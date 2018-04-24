library(neuralnet)

#Constantes ------------------------------------------------------------
HIDDEN_SIZE<-c(3,3) #Neuronas en capa oculta
THRESHOLD<-1e-4 #Tolerancia absoluta de la NNet
LEARN_RATE<-.3 #Tasa de variación de tasa de aprendizaje
REP_NNET<-5 #Repeticiones del entrenamiento de la NNet

#Validar con subconjunto de TEST data -----------------------------------
if(TRAIN_DATA){
  test<-read.csv2("test.csv")
  test$Date<-as.POSIXct(test$Date)
  test.model<-Weibull_models(ttl(test$Date,test$Ref),test$Event,test$Ref,shift.optimize = T)
  test.coef<-left_join(test.model$coef,
                       test%>%select(Ref,Cat1,Cat2,Cat3)%>%unique(),
                       by=c("id"="Ref"))
}else{
  test.coef<-data.coef
  test.model<-data.model
}

#Preprocesamiento de entradas y salidas de NNet ----------------------------------------
coef.scales<-list(Shape=max(data.coef$Shape),Scale=max(data.coef$Scale))
coef.nnet<-dummyVar(data.coef%>%select(Cat1,Cat2))%>%cbind(
  data.frame(Shape=data.coef$Shape/coef.scales$Shape,Scale=data.coef$Scale/coef.scales$Scale))

#Entrenamiento de NNet ------------------------------------------------------

model.nnet<-neuralnet(formula = paste0("`",names(coef.nnet%>%select(-c(Shape,Scale))),"`")%>%
                        paste(collapse = "+")%>%
                        paste0("Shape+Scale~",.)%>%
                        as.formula(),
                      data=coef.nnet,
                      threshold = THRESHOLD,
                      rep = REP_NNET,
                      hidden = HIDDEN_SIZE,
                      learningrate = LEARN_RATE,
                      lifesign = T)

#Procesamiento de datos de inferencia -------------------------------------------------------------
in.nnet<-array(rep(0,18),c(2,9))%>%as.data.frame() #array vacio en 0s
names(in.nnet)<-names(coef.nnet)
in.nnet[,names(test.coef%>%select(Cat1,Cat2)%>%unique()%>%dummyVar())]<-test.coef%>%select(Cat1,Cat2)%>%unique()%>%dummyVar()
out.nnet<-compute(model.nnet,in.nnet%>%select(-c(Shape,Scale)))%>%as.data.frame()%>%select(contains("net.result")) #prediccion
names(out.nnet)<-c("Shape","Scale")
out.nnet<-cbind(test.coef%>%select(Cat1,Cat2)%>%unique(),out.nnet)
out.nnet$Shape<-out.nnet$Shape*coef.scales$Shape # desescalado
out.nnet$Scale<-out.nnet$Scale*coef.scales$Scale


#Grafico de inferencia de parámetros ---------------------------------------------------
nnet.plot<-coef.plot+
  geom_point(data = out.nnet,
             aes(x=Shape,y=Scale,colour=Cat1,shape=Cat2),
             size=4)

test.plt<-ggplot(test.coef,aes(x=Shape,y=Scale,colour=Cat1,shape=Cat2))+
  geom_point(alpha=0.7)+
  geom_point(data=out.nnet,aes(x=Shape,y=Scale,colour=Cat1,shape=Cat2),size=4)+
  coord_cartesian(xlim = layer_scales(nnet.plot)$x$range$range,ylim = layer_scales(nnet.plot)$y$range$range)

subplot(nnet.plot,test.plt,nrows = 2)

#Gráfico de curvas de distribución -------------------------------------------------------------
pdist<-data.frame(t=TPO_FPROB)
mean.pars1<-apply(test.coef%>%filter(Cat1=="GMC-"&Cat2=="BOG-")%>%select(Shape,Scale)%>%as.data.frame(),2,mean)
mean.pars2<-apply(test.coef%>%filter(Cat1=="VOLV"&Cat2=="MED-")%>%select(Shape,Scale)%>%as.data.frame(),2,mean)
pdist<-pdist%>%mutate(p1=dweibull(t,out.nnet$Shape[1],out.nnet$Scale[1]),
                      p2=dweibull(t,mean.pars1[1],mean.pars1[2]),
                      p3=dweibull(t,out.nnet$Shape[2],out.nnet$Scale[2]),
                      p4=dweibull(t,mean.pars2[1],mean.pars2[2]))
                                  
pdist1.plt<-ggplot(pdist,aes())+
  geom_line(aes(x=t,y=p1),color="red")+
  geom_line(aes(x=t,y=p2),color="green")+
  geom_line(aes(x=t,y=p3),color="blue")+
  geom_line(aes(x=t,y=p4),color="purple")
ggplotly(pdist1.plt)

rm(in.nnet,pdist,mean.pars1,mean.pars2,test,out.nnet,coef.scales,coef.nnet)
