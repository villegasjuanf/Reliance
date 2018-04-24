library(dplyr)
library(tidyr)
library(survival)
library(progress)
library(Rmpfr)

purge_few<-function(id,more.than=5){
  x<-data.frame(id=id)%>%group_by(id)%>%summarise(freq=n())%>%filter(freq>more.than)
  return(x$id)
}

add_Status<-function(modelo.coef,data,Fecha,id=NULL){
  if(!is.null(id)){
    data<-cbind(Ref=id,data)
  }
  return(left_join(modelo.coef,data%>%group_by(Ref)%>%
                     summarise(Fini=min(Date))%>%
                     mutate(t=as.numeric(Fecha-Fini))))
}

dummyVar<-function(x){
  y<-data.frame(rep(0,nrow(x)))
  for (i in 1:ncol(x)) {
    for (j in levels(x[,i])) {
      y<-cbind(y,a=rep(0,nrow(x)))
      names(y)[length(names(y))]<-j
      y[x[,i]==j,ncol(y)]<-1
    }
  }
  return(y[,-1])
}

ttl<-function(date,id){
  x<-data.frame(id=id,date=date)
  y<-x%>%group_by(id)%>%mutate(t=as.numeric(difftime(date,min(date),units = "days")))
  return(y$t)
}

days2date<-function(id,fecha,fecha_ref){
  x<-data.frame(id=id,fecha=fecha)%>%
    group_by(id)%>%
    summarise(days=difftime(fecha_ref,min(fecha),units = "days"))
  return(x)
}


Weibull_model<-function(time,status,shift.optimize=F){
  data<-Surv(time,status)
  data.surv<-survfit(formula = data~1)
  x<-data.frame(time=data.surv$time,
                surv=data.surv$surv,
                lower=data.surv$lower,
                upper=data.surv$upper)
  x<-x[-which(x$surv==0),]

  W.model<-function(x,opt=0){
    x<-x%>%mutate(log_time=log(time+opt),
                  log_R=log(-log(surv)),
                  log_L=log(-log(lower)),
                  log_U=log(-log(upper)))
    x.model<-lm(data = x[x$surv<1,],log_R~log_time)
    low.model<-lm(data = x[x$lower<1,],log_L~log_time)
    upp.model<-lm(data = x[x$upper<1,],log_R~log_time)
    return(list(est=x.model,low=low.model,upp=upp.model,n=x.model$rank+x.model$df.residual))
  }
  if(shift.optimize){
    opt.shift<-optimize(function(shift) summary(W.model(x,shift)$est)$r.squared,c(min(time),max(time)),maximum = T)$maximum
    return(W.model(x,opt.shift))
  }else{
    return(W.model(x))
  }
}

Weibull_models<-function(time,status,id,shift.optimize=F,echo=F){
  t0.index<-which(time==0)
  time<-time[-t0.index]
  status<-status[-t0.index]
  id<-id[-t0.index]
  ids<-levels(id)
  modelos.ref<-list()
  modelos.coef<-data.frame(id=NULL,Shape=NULL,Scale=NULL,r2=NULL,n=NULL)
  if(!echo)
    pb<-progress_bar$new(total=length(ids),show_after = 3)

  for(i in ids){
    index<-which(id==i)
    x<-Weibull_model(time[index],status[index],shift.optimize)
    c0<-Weibul_coef(x$est)
    r2<-summary(x$est)$r.squared
    if(echo)
      pb$tick()

    modelos.ref[[length(modelos.ref)+1]]<-list(id=i,Model=x,Coef=c0)
    modelos.coef<-rbind(modelos.coef,data.frame(id=as.factor(i),Shape=c0[2],Scale=c0[1],R2=r2,n=x$n))
  }
  return(list(ref=modelos.ref,coef=modelos.coef))
}

Weibul_coef<-function(weibull.model){
  x<-weibull.model$coefficients
  x[1]<-exp(-weibull.model$coefficients[1]/weibull.model$coefficients[2])
  return(x)
}



f_fun<-function(t,shape,scale,precision=NULL){
  if(!is.null(precision))
    t<-mpfr(t,precision)
  return(shape/scale*(t/scale)^(shape-1)*exp(-(t/scale)^shape))
}

F_fun<-function(t,shape,scale,precision=NULL){
  if(!is.null(precision))
    t<-mpfr(t,precision)
  return(1-exp(-(t/scale)^shape))
}

R_fun<-function(t,shape,scale,precision=NULL){
  return(1-F_fun(t,shape,scale,precision))
}

r_fun<-function(t,shape,scale,precision=NULL){
  return(f_fun(t,shape,scale,precision)/R_fun(t,shape,scale,precision))
}

H_fun<-function(tp,shape,scale=1,precision=NULL){
  H_apply<-function(y){
    z<-tryCatch(integrateR(function(x) r_fun(x,shape,scale,precision),1,y),error=function(e) list(value=NaN))
    return(asNumeric(z$value))
  }
  return(sapply(tp,H_apply))
}

M_fun<-function(tp,shape,scale=1,precision=NULL){
  M_apply<-function(y){
    z<-tryCatch(integrateR(function(x) x*f_fun(x,shape,scale,precision),1,y),error=function(e) list(value=NaN))
    return(asNumeric(z$value/(F_fun(y,shape,scale,precision))))
  }
  return(sapply(tp,M_apply))
}

UEC_Poli_I<-function(tp,shape,scale,Cp,Cc){
  UEC<-(Cp*R_fun(tp,shape,scale)+Cc*F_fun(tp,shape,scale))/(tp*R_fun(tp,shape,scale)+M_fun(tp,shape,scale)*F_fun(tp,shape,scale))
  return(UEC)}


UEC_Poli_II<-function(tp,shape,scale,Cp,Cc){
  UEC<-(Cp+Cc*H_fun(tp,shape,scale))/tp
  return(UEC)}


UEC_Poli_I_opt<-function(tp,shape,scale,Cp,Cc){
  return(tp,optim(function(x){UEC_Poli_I(x,shape,scale,Cp,Cc)}))
}


UEC_Poli_II_opt<-function(tp,shape,scale,Cp,Cc){
  return(tp,optim(function(x){UEC_Poli_II(x,shape,scale,Cp,Cc)}))
}

