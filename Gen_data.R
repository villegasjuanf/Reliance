library(dplyr)

#Split de Ref: Dispositivo{3}-Marca{3}-Serie{3}
testCat<-list(Cat2=c("MED-","BOG-"),
              Cat1=c("VOLV","GMC-"))
Ref.Format<-list(c1=c(1,4),
                 c2=c(5,8),
                 c3=c(9,11))
caracteristicas <- list(c1 = 100:105, 
                        c2 = c("CLO-","BOG-","BQA-","MED-"), 
                        c3 = c("VOLV","MERC","GMC-"))

#Función generadora de datos


Gen.data <- function() {
  
  FFORMA <- c(0.1, 6.5) #Rango del factor de forma para dispositivo
  FESCALA <- c(100, 5000) #Rango del factor de escala para dispositivo
  N.NAS <- 0 #Cantidad de NA por serie
  n=50 #Cantidad de datos por serie
  pOnes=1 #
  dc<-c(.8,.4) #Factor de dispersión para forma en serie y marca
  de<-c(.8,.4) #Factor de dispersión para escala en serie y marca
  Costo=list(Prev=c(100,10), #Costos de mantenimiento y dispersión
             Corr=c(3000,400))

  
  #Determinación de coeficientes por serie
  carac.forma <- list(c1 = runif(length(caracteristicas$c1),dc[1], 1),
                      c2 = runif(length(caracteristicas$c2), dc[2], 1),
                      c3 = runif(length(caracteristicas$c3), FFORMA[1], FFORMA[2]))
  
  
  carac.escala <- list(c1 = runif(length(caracteristicas$c1),de[1], 1),
                       c2 = runif(length(caracteristicas$c2), de[2], 1),
                       c3 = runif(length(caracteristicas$c3), FESCALA[1], FESCALA[2]))
  
  carac.cost <- list(c1 = runif(length(caracteristicas$c1),de[1], (1-de[1])*2+de[1]), 
                     c2 = runif(length(caracteristicas$c2), de[2], (1-de[2])*2+de[2]), 
                     cc = runif(length(caracteristicas$c3), Costo$Corr[1], Costo$Corr[2]),
                     cp = runif(length(caracteristicas$c3),Costo$Prev[1],Costo$Prev[2]))
  
  print(carac.forma)
  print(carac.escala)
  print(carac.cost)
  
  #Generación de datos
  coef.data<-data.frame(Cat1=NULL,Cat2=NULL,shape=NULL,scale=NULL)
  data <- x <- data.frame(Ref = NULL, t = NULL, Event = NULL,stringsAsFactors = F)
  seed<-Sys.time()%>%as.numeric()
  for (i in 1:length(caracteristicas$c1)) {
    for (j in 1:length(caracteristicas$c2)) {
      for (k in 1:length(caracteristicas$c3)) {
        shape=carac.forma$c3[k]*carac.forma$c2[j]* carac.forma$c1[i]
        scale=carac.escala$c3[k]* carac.escala$c2[j]* carac.escala$c1[i]
        Cc=carac.cost$cc[k]*carac.cost$c2[j]*carac.cost$c1[i]
        Cp=carac.cost$cp[k]*carac.cost$c2[j]*carac.cost$c1[i]
        NAcost<-rbinom(n,1,0.5)
        set.seed(seed)
        t<-rweibull(n,shape,scale)
        x <- data.frame(
          Ref = rep(paste(sep = "",caracteristicas$c3[k], caracteristicas$c2[j],caracteristicas$c1[i]), n), 
          t = t, 
          Event = as.logical(rbinom(n,1,pOnes)),
          C_Corr=rnorm(n,Costo$Corr[1],Costo$Corr[2]),
          C_Prev=rnorm(n,Costo$Prev[1],Costo$Prev[2]))
        initdate<-as.POSIXct(runif(1,0,365*24*3600),origin="2015-01-01",tz="GMT")
        x<-x%>%mutate(Date=t*3600*24+initdate)
        cat(paste(sep = "",caracteristicas$c3[k], caracteristicas$c2[j],caracteristicas$c1[i]),"|",shape,"|",scale,"\n")
        coef.data<-rbind(coef.data,data.frame(Cat1=caracteristicas$c3[k],Cat2=caracteristicas$c2[j],shape=shape,scale=scale))
        if(N.NAS>0)
          x$t[runif(runif(1,2,N.NAS),1,n)]<-NA
        data <- rbind(data, x)
      }
    }
    
  }
  write.csv2(coef.data%>%unique(),"coefs.csv")
  data<-data%>%mutate(Category=substr(Ref,Ref.Format$c1[1],Ref.Format$c2[2]),
                      Cat1=substr(Ref,Ref.Format$c1[1],Ref.Format$c1[2]),
                      Cat2=substr(Ref,Ref.Format$c2[1],Ref.Format$c2[2]),
                      Cat3=substr(Ref,Ref.Format$c3[1],Ref.Format$c3[2])) 
  return(data)
}
data<-Gen.data()%>%select(-t)%>%sample_frac(.7)
write.csv2(data,"data.csv")
testIndex<-which((data$Cat1==testCat$Cat1[1]&data$Cat2==testCat$Cat2[1])|(data$Cat1==testCat$Cat1[2]&data$Cat2==testCat$Cat2[2]))
write.csv2(data[testIndex,],"test.csv")
write.csv2(data[-testIndex,],"train.csv")
rm(testIndex,data,Ref.Format,caracteristicas,Gen.data)
