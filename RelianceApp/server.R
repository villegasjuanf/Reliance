library(shiny)
library(dplyr)
library(tidyr)
library(ReliaEng)
library(progress)
library(survival)
library(plotly)
library(neuralnet)
library(Rmpfr)

#Constantes ------------------------------------------------------------
HIDDEN_SIZE<-c(3,3) #Neuronas en capa oculta
THRESHOLD<-1e-4 #Tolerancia absoluta de la NNet
LEARN_RATE<-.3 #Tasa de variacion de tasa de aprendizaje
REP_NNET<-10 #Repeticiones del entrenamiento de la NNet
FORMATO_FECHA<- "%Y-%m-%d %H:%M:%S" #Formato de fecha para importacion de datos
PDIST_X_DOTS<-200 #Puntos de la grafica de probabilidad
WEIBULL_3_PARS<-F #Regresion Weibull de 3 parametros
PRECISION<-NULL #Precision de calculo en numero de bits (NULL = double de 53 bits)
FUNS_TOL<-1e-9 #valor minimo para calculo de funciones de probabilidad f,F,R,r,H,M

#Weibull_models con barra de progreso
Weibull_models_shiny<-function(time,status,id,shift.optimize=F,echo=T){
  t0.index<-which(time==0)
  time<-time[-t0.index]
  status<-status[-t0.index]
  id<-id[-t0.index]
  ids<-levels(id)
  modelos.ref<-list()
  modelos.coef<-data.frame(id=NULL,Shape=NULL,Scale=NULL,r2=NULL,n=NULL)
  withProgress(message = "Procesando modelos",detail = "esto tomara unos segundos...",{
    for(i in ids){
      incProgress(1/length(ids))
      index<-which(id==i)
      x<-Weibull_model(time[index],status[index],shift.optimize)
      c0<-Weibul_coef(x$est)
      r2<-summary(x$est)$r.squared
      
      modelos.ref[[length(modelos.ref)+1]]<-list(id=i,Model=x,Coef=c0)
      modelos.coef<-rbind(modelos.coef,data.frame(id=as.factor(i),Shape=c0[2],Scale=c0[1],R2=r2,n=x$n))
    }
  })
  return(list(ref=modelos.ref,coef=modelos.coef))
}


#Shiny Server --------------------------------------------------------------------------------------
#...................................................................................................

shinyServer(function(input, output) {
  
  #Valores iniciales ---------------------------------------------------------------------
estados<-reactiveValues(process=F,inference=F)

  #Trace de variables --------------------------------------------------------------------
 
  #Objetos del panel 1 --------------------------------------------------------------------
  
  #importar datos
  datax<-reactive({
    if(is.null(input$file$datapath))
      datax<-read.csv2("./tmp/test_tmp.csv")%>%select(-X) #carga de ultimo valor guardado
    else{
      datax<-read.csv2(input$file$datapath)%>%select(-X)
      write.csv2(datax,"./tmp/test_tmp.csv")
    }
    datax$Date<-as.POSIXct(strptime(datax$Date,format = FORMATO_FECHA))
    estados$process<-F
    estados$inference<-F
    return(datax)
  })
  
  #Mostrar data
  output$data<-renderDataTable({
    datax()%>%transmute(Marca=Cat1,
                        Ciudad=Cat2,
                        Serie=Cat3,
                        Falla=ifelse(Event,"Si","No"),
                        Fecha=Date%>%as.Date(),
                        Costo_Corr=C_Corr%>%round()%>%format(digits=6,big.mark=".",decimal.mark=",",justify="right")%>%paste("$",.),
                        Costo_Prev=C_Prev%>%round()%>%format(digits=6,big.mark=".",decimal.mark=",",justify="right")%>%paste("$",.)
                        )
  })
  
  #Mostrar coef
  output$resData<-renderTable({
    datax()%>%group_by(Marca=Cat1,Ciudad=Cat2)%>%summarise(Nro_datos=n())
  })
  
  
  #Objetos del panel 2 --------------------------------------------------------------------
  
  #procesar datos en coef
  data.coef<-eventReactive(input$procesar,ignoreInit = T,ignoreNULL = T,{
    data.model<-Weibull_models_shiny(time=ttl(date=datax()$Date,
                                              id=datax()$Ref),
                                     status=datax()$Event,
                                     id=datax()$Ref,
                                     shift.optimize=WEIBULL_3_PARS)
    data.coef<-left_join(data.model$coef,datax()%>%select(Ref,Cat1,Cat2,Cat3)%>%unique(),by=c("id"="Ref"))
    write.csv2(data.coef,"./tmp/data_model_tmp.csv")
    estados$inference<-F
    estados$process<-T
    return(data.coef)
  })
  
  #carga de datos temporales de la pestana 2
  data.coef_<-reactive({
     if(input$procesar==0){
       data.coef_<-read.csv2("./tmp/data_model_tmp.csv")%>%select(-X)}
    else{
      data.coef_<-data.coef()
    }
    
  })
  
  #Datos seleccionados en pestana de modelos estadisticos
  seldataModel1<-reactive({return(data.coef_()%>%filter((Cat1==input$selCat1)&(Cat2==input$selCat2)&(Cat3==input$selCat3)))})
  seldataModel2<-reactive({return(data.coef_()%>%filter((Cat1==input$selCat4)&(Cat2==input$selCat5)&(Cat3==input$selCat6)))})
  
  #grafico del panel de dist prob
  output$distPlot<-renderPlotly({
    
    #data para graficos
    xydata<-data.frame(
      t=seq(0,qweibull(0.995,shape=seldataModel1()$Shape,scale=seldataModel1()$Scale)*2,
            qweibull(0.995,seldataModel1()$Shape,seldataModel1()$Scale)/PDIST_X_DOTS))%>%
      mutate(d_prob1=dweibull(t,seldataModel1()$Shape,seldataModel1()$Scale),
             P_prob1=pweibull(t,seldataModel1()$Shape,seldataModel1()$Scale),
             d_prob2=dweibull(t,seldataModel2()$Shape,seldataModel1()$Scale),
             P_prob2=pweibull(t,seldataModel2()$Shape,seldataModel1()$Scale)
             )
    #grafico de dens prob
    dplot<-ggplot(xydata,aes(x=t,y=d_prob1))+
      geom_line(colour="red",size=1.2)+
      geom_line(aes(x=t,y=d_prob2),colour="green",size=1.2)+
      coord_cartesian(xlim = c(0,input$sliderX))
    #grafico de prob acum
    Pplot<-ggplot(xydata,aes(x=t,y=P_prob1))+
      geom_line(colour="red",size=1.2)+
      geom_line(aes(x=t,y=P_prob2),colour="green",size=1.2)+
      coord_cartesian(xlim = c(0,input$sliderX))
    
    subplot(dplot,Pplot,nrows = 2)
  })
  
  #tabla de coeficientes del panel de dist prob
  output$R2.1<-renderTable({
    x<-data.frame(Parametro=c("Coeficiente R2","Factor de forma","Factor de escala","Nro datos"),
                  Valor=c(seldataModel1()$R2,seldataModel1()$Shape,seldataModel1()$Scale,seldataModel1()$n))
    x$Valor[1:3]<-format(x$Valor[1:3],digits = 10)
    return(x)
  })
  output$R2.2<-renderTable({
    x<-data.frame(Parametro=c("Coeficiente R2","Factor de forma","Factor de escala","Nro datos"),
                  Valor=c(seldataModel2()$R2,seldataModel2()$Shape,seldataModel2()$Scale,seldataModel2()$n))
    x$Valor[1:3]<-format(x$Valor[1:3],digits = 10)
    return(x)
  })
  
  #Sliderbar de zoom del eje X
  output$sliderX<-renderUI({
    mid<-qweibull(0.995,shape=seldataModel1()$Shape,scale=seldataModel1()$Scale)
    sliderInput("sliderX","Zoom en Eje X",min=round(mid/10,0),max=round(mid*2,0),value=round(mid,0),ticks = F,sep = "")
  })
  
  #seleccion de graficas en dist prob
  output$selCat1<-renderUI({selectInput("selCat1","Categoria 1 - item 1",data.coef_()%>%select(Cat1)%>%unique())})
  output$selCat2<-renderUI({selectInput("selCat2","Categoria 2 - item 1",data.coef_()%>%select(Cat2)%>%unique())})
  output$selCat3<-renderUI({selectInput("selCat3","Categoria 3 - item 1",data.coef_()%>%select(Cat3)%>%unique())})
  output$selCat4<-renderUI({selectInput("selCat4","Categoria 1 - item 2",data.coef_()%>%select(Cat1)%>%unique())})
  output$selCat5<-renderUI({selectInput("selCat5","Categoria 2 - item 2",data.coef_()%>%select(Cat2)%>%unique())})
  output$selCat6<-renderUI({selectInput("selCat6","Categoria 3 - item 2",data.coef_()%>%select(Cat3)%>%unique())})
  
  #Objetos del panel 3 --------------------------------------------------------------------
  
   #mostrar tabla de coeficientes
  output$coefTable<-renderDataTable({
    if(!estados$inference)
      coef<-data.coef_()
    else{
      coef<-inference()
      #estados$inference<-F
      }
    coef%>%group_by(Cat1,Cat2)%>%summarise(Shape=mean(Shape),Scale=mean(Scale),R2_min=min(R2),n=sum(n))
  })
  
  
  #grafico del panel de inferencia
  output$coefPlot<-renderPlotly({
    
    if(!estados$inference){ 
      ggplot(data.coef_(),aes(x=Shape,y=Scale))+
        geom_point(aes(color=Cat1,shape=Cat2))
      }
    else{
      #estados$inference<-F
      coef<-inference()%>%mutate(is.infer=ifelse(is.na(n),3,1))
      ggplot(coef,aes(x=Shape,y=Scale))+
        geom_point(aes(color=Cat1,shape=Cat2,size=is.infer))
      }
  })
 
  #seleccion de patrones para inferencia
  output$selInference<-renderUI({
    catList<-expand.grid(Cat1=datax()$Cat1%>%levels(),Cat2=datax()$Cat2%>%levels())
    inferenceChoices<-dplyr::setdiff(catList,data.coef_()%>%select(Cat1,Cat2)%>%unique())
    checkboxGroupInput("inferenceData","inferencia de datos",
                       choiceNames = paste(inferenceChoices$Cat1,inferenceChoices$Cat2),
                       choiceValues = paste(inferenceChoices$Cat1,inferenceChoices$Cat2,sep = "~"))
  })
  
  #inferencia de datos
  inference<-eventReactive(input$R.inferencia,{
    if(input$metodo=="nnet"|T){ #QUITAR CONDICION TRUE PARA HACER FUNCIONAR EL IF .....................................
      
      #Preprocesamiento de entradas y salidas de NNet
      coef.scales<-list(Shape=max(data.coef_()$Shape),Scale=max(data.coef_()$Scale)) #Factores de escalado
      coef.nnet<-dummyVar(data.coef_()%>%select(Cat1,Cat2))%>%cbind(
        data.frame(Shape=log(data.coef_()$Shape)/coef.scales$Shape,Scale=data.coef_()$Scale/coef.scales$Scale))
      test.coef<-input$inferenceData%>%strsplit("~")%>%as.data.frame()%>%t()%>%as.data.frame()
      colnames(test.coef)<-c("Cat1","Cat2")
      rownames(test.coef)<-NULL
      
      #entrenamiento de red neuronal
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
      
      #Procesamiento de datos de inferencia en la NNet
      in.nnet<-array(rep(0,nrow(test.coef)*ncol(coef.nnet)),c(nrow(test.coef),ncol(coef.nnet)))%>%as.data.frame()
      colnames(in.nnet)<-names(coef.nnet)
      in.nnet[,names(test.coef%>%select(Cat1,Cat2)%>%unique()%>%dummyVar())]<-test.coef%>%select(Cat1,Cat2)%>%unique()%>%dummyVar()
      out.nnet<-compute(model.nnet,in.nnet%>%select(-c(Shape,Scale)))%>%as.data.frame()%>%select(contains("net.result")) #prediccion
      names(out.nnet)<-c("Shape","Scale")
      out.nnet<-cbind(test.coef%>%select(Cat1,Cat2)%>%unique(),out.nnet)%>%cbind(id=NA,R2=NA,n=NA,Cat3=NA)
      out.nnet$Shape<-exp(out.nnet$Shape*coef.scales$Shape) # desescalado de los datos
      out.nnet$Scale<-out.nnet$Scale*coef.scales$Scale
      out.nnet<-data.coef_()%>%rbind(out.nnet)
    }
    else{ #Condicion para test de Cox
      
    }
    estados$inference<-T
    return(out.nnet)
   })
  
  #Tabla de coefcientes de inferencia
  output$trace<-renderTable(inference()%>%filter(is.na(n)&is.na(R2))%>%select(Cat1,Cat2,Shape,Scale))
  
  observe({
    if(input$save){
      assign("data.coef_",data.coef_(),envir = .GlobalEnv)
      assign("datax",datax(),envir = .GlobalEnv)
      assign("diasUso",diasUso(),envir = .GlobalEnv)
      assign("FallasEst",FallasEst(),envir = .GlobalEnv)
      print("datos guardados")
    }
  })
  
  # Objetos del panel 4 ---------------------------------------------------------------------------------
  
  #Calculo de funciones de confiabilidad
  reliaFuncs<-reactive({
    Cost<-datax()%>%group_by(Ref)%>%summarise(C_Prev=mean(C_Prev),C_Corr=mean(C_Corr))
    data.coef<-data.coef_()%>%filter(id==input$selReliaId)%>%left_join(Cost,by=c("id" = "Ref"))
    
    i<-1
    max_t<-qweibull(1-FUNS_TOL,data.coef$Shape[i],data.coef$Scale[i])
    funs.data<-data.frame(t=seq(1,max_t,length.out=PDIST_X_DOTS))
    summary(data.coef)
    funs.data<-funs.data%>%mutate(f.fun=f_fun(t,data.coef$Shape,data.coef$Scale,PRECISION)%>%as.numeric(),
                                  F.fun=F_fun(t,data.coef$Shape,data.coef$Scale,PRECISION)%>%as.numeric(),
                                  R.fun=(1-F.fun)%>%as.numeric(),
                                  r.fun=(f.fun/R.fun)%>%as.numeric(),
                                  H.fun=H_fun(t,data.coef$Shape,data.coef$Scale,PRECISION)%>%as.numeric(),
                                  M.fun=M_fun(t,data.coef$Shape,data.coef$Scale,PRECISION)%>%as.numeric(),
                                  Poli.I=(data.coef$C_Prev*R.fun+data.coef$C_Corr*F.fun)/(t*R.fun+M.fun*F.fun)*365,
                                  Poli.II=(data.coef$C_Prev+data.coef$C_Corr*H.fun)/t)*365
    return(funs.data)
  })
  
  output$selRelia<-renderUI({selectInput("selReliaId","Seleccionar Id:",choices = data.coef_()$id)})
  
  output$reliaPlt<-renderPlotly({
    data<-reliaFuncs()
    print(data)
    plt<-ggplot(data,aes(x=t))+theme_dark()
    plt.f<-plt+geom_line(aes(y=f.fun))
    plt.F<-plt+geom_line(aes(y=F.fun))
    plt.r<-plt+geom_line(aes(y=r.fun))
    plt.H<-plt+geom_line(aes(y=H.fun))
    plt.I<-plt+geom_line(aes(y=Poli.I))
    plt.II<-plt+geom_line(aes(y=Poli.II))
    
    subplot(plt.f,plt.F,plt.r,plt.H,plt.I,plt.II,nrows = 3,shareX = T)
    })
  
  
  #dias y probabilidad de falla en periodo de tiempo de estudio
  FallasEst<-reactive({
    dataEst<-left_join(data.coef_(),
                       days2date(datax()$Ref,datax()$Date,as.POSIXct(input$dates[1])),
                       by=c("id"="id"))%>%
      left_join(days2date(datax()$Ref,datax()$Date,as.POSIXct(input$dates[2])),
                by=c("id"="id"))
    
    names(dataEst)[ncol(dataEst)-1:0]<-c("dias_ini","dias_fin")
    dataEst$dias_ini<-ifelse(dataEst$dias_ini<0,0,dataEst$dias_ini)
    dataEst$dias_fin<-ifelse(dataEst$dias_fin<0,0,dataEst$dias_fin)
    
    data.coef<-dataEst%>%mutate(diff.F=F_fun(as.numeric(dias_fin),Shape,Scale)-F_fun(as.numeric(dias_ini),Shape,Scale))
    
    Falla_est<-data.coef%>%
      group_by(Cat1,Cat2)%>%
      summarise(diff.F=sum(diff.F))
    return(Falla_est)
  })
  
  #dias de uso de las ids
  diasUso<-reactive({
    diasUso<-datax()%>%
      group_by(Ref)%>%
      mutate(dias_uso=ifelse(difftime(input$dates[2],Date,units="days")<0,NA,difftime(input$dates[2],Date,units="days")))
    return(diasUso)
  })
  
  
  #Grafico de dias de uso de dispositivos
  output$diasUso<-renderPlotly({
    data<-diasUso()%>%select(Ref,dias_uso,Cat1,Cat2)%>%mutate(Cat0=paste(Cat1,Cat2))
    ggplot(data,aes(x=dias_uso%>%as.numeric(),fill=Cat0))+geom_histogram(stat="bin",binwidth = 30,position = "stack")
  })
  
  #Grafico de fallas acumuladas del periodo
  output$fallasEst<-renderPlotly({
    data<-FallasEst()%>%
      mutate(Cat0=paste(Cat1,Cat2))%>%select(Cat0,diff.F)
    ggplot(data,aes(x=reorder(Cat0,diff.F),y=diff.F,fill=Cat0))+geom_col(width = 0.8)+geom_rug(sides = "left",color="gray")
    
  })
  
  
})

