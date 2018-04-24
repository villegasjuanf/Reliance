library(dplyr)
library(tidyr)
library(survival)
library(plotly)
library(ReliaEng)
library(progress)
library(Rmpfr)

#Constantes ------------------------------------------------------------
F_CORTE_DATA<-strptime("2018-01-01",format = "%Y-%m-%d")
F_ESTUDIO<-strptime(c("2016-01-01","2016-12-31"),format = "%Y-%m-%d")
FORMATO_FECHA<- "%Y-%m-%d %H:%M:%S"
PRECISION<-NULL
TRAIN_DATA<-T
TPO_FPROB<-seq(0,150,1)
WEIBULL_3_PARS<-T
ECHO<-F


#Lectura de datos-------------------------------------
if(TRAIN_DATA){
  data<-read.csv2("train.csv",stringsAsFactors = T)
}else{
  data<-read.csv2("data.csv",stringsAsFactors = T)
}
data$Date<-as.POSIXct(strptime(data$Date,format = FORMATO_FECHA))
#data<-data%>%filter()

#Modelamiento estimador Km y linealización Weibull ------------------------------------
data.model<-Weibull_models(time=ttl(data$Date,data$Ref),status=data$Event,id=data$Ref,shift.optimize=WEIBULL_3_PARS,echo = ECHO)
data.coef<-left_join(data.model$coef,
                     data%>%select(Ref,Cat1,Cat2,Cat3)%>%unique(),
                     by=c("id"="Ref"))

#Gráfico scatter de parámetros [Scale vs Shape] ------------------------------------------
coef.plot<-ggplot(data.coef,aes(x=Shape,y=Scale,colour=Cat1,shape=Cat2))+geom_point(alpha=0.7)
ggplotly(coef.plot)

#Construcción de curvas probabilidad -----------------------------------------------------
coef.model<-data.coef%>%group_by(Cat1,Cat2)%>%summarise(Shape=mean(Shape),Scale=mean(Scale))

t<-TPO_FPROB
funs.data<-data.frame(t=NULL,f_fun=NULL,F_fun=NULL,r_fun=NULL,R_fun=NULL,H_fun=NULL,M_fun=NULL,Cat1=NULL,Cat2=NULL)
pb<-progress_bar$new(total=nrow(coef.model)*7,incomplete = ".",show_after = 1)
for (i in 1:nrow(coef.model)) {
  f_fun.tmp=f_fun(t,coef.model$Shape[i],coef.model$Scale[i],PRECISION)
  pb$tick()
  F_fun.tmp=F_fun(t,coef.model$Shape[i],coef.model$Scale[i],PRECISION)
  pb$tick()
  R_fun.tmp=1-F_fun.tmp
  pb$tick()
  r_fun.tmp=f_fun.tmp/R_fun.tmp
  pb$tick()
  H_fun.tmp=H_fun(t,coef.model$Shape[i],coef.model$Scale[i],PRECISION)
  pb$tick()
  M_fun.tmp=M_fun(t,coef.model$Shape[i],coef.model$Scale[i],PRECISION)
  pb$tick()
  funs.data<-rbind(funs.data,
                   data.frame(t=t,
                              f_fun=f_fun.tmp%>%as.numeric(),
                              F_fun=F_fun.tmp%>%as.numeric(),
                              r_fun=r_fun.tmp%>%as.numeric(),
                              R_fun=R_fun.tmp%>%as.numeric(),
                              H_fun=H_fun.tmp%>%as.numeric(),
                              M_fun=M_fun.tmp%>%as.numeric(),
                              Cat1=coef.model$Cat1[i],
                              Cat2=coef.model$Cat2[i]
                   )
  )
  pb$tick()
}
rm(f_fun.tmp,F_fun.tmp,r_fun.tmp,R_fun.tmp,H_fun.tmp,M_fun.tmp,pb)
#rm(data,coef.model)
funs.data<-funs.data%>%mutate(id=paste0(Cat1,Cat2))

#Gráficas de probabilidad
if(F){
  fprob.plot<-ggplot(funs.data,aes(x=t,y=f_fun,group=id))+geom_line()+facet_grid(Cat1~Cat2)
  ggplotly(fprob.plot)
  Fprob.plot<-ggplot(funs.data,aes(x=t,y=F_fun,group=id))+geom_line()+facet_grid(Cat1~Cat2)
  frame()
  ggplotly(Fprob.plot)
  Rprob.plot<-ggplot(funs.data,aes(x=t,y=R_fun,group=id))+geom_line()+facet_grid(Cat1~Cat2)
  frame()
  ggplotly(Rprob.plot)
  rprob.plot<-ggplot(funs.data,aes(x=t,y=r_fun,group=id))+geom_line()+facet_grid(Cat1~Cat2)
  frame()
  ggplotly(rprob.plot)
  Hprob.plot<-ggplot(funs.data,aes(x=t,y=H_fun,group=id))+geom_line()+facet_grid(Cat1~Cat2)
  frame()
  ggplotly(Hprob.plot)
  Mprob.plot<-ggplot(funs.data,aes(x=t,y=M_fun,group=id))+geom_line()+facet_grid(Cat1~Cat2)
  frame()
  ggplotly(Mprob.plot)
  
}

#Proyecciones del estudio -----------------------------------------------------------------------
dataEst<-left_join(data.coef,days2date(data$Ref,data$Date,F_ESTUDIO[1]),by=c("id"="id"))%>%
  left_join(.,days2date(data$Ref,data$Date,F_ESTUDIO[2]),by=c("id"="id"))
names(dataEst)[ncol(dataEst)-1:0]<-c("dias_ini","dias_fin")
dataEst$dias_ini<-ifelse(dataEst$dias_ini<0,0,dataEst$dias_ini)
dataEst$dias_fin<-ifelse(dataEst$dias_fin<0,0,dataEst$dias_fin)

data.coef<-dataEst%>%mutate(diff.F=F_fun(as.numeric(dias_fin),Shape,Scale)-F_fun(as.numeric(dias_ini),Shape,Scale))

Falla_est<-data.coef%>%group_by(Cat1,Cat2)%>%summarise(dias=dias_fin-dias_ini,diff.F=sum(diff.F))

rm(dataEst,i,t)

