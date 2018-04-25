library(shiny)
library(plotly)

shinyUI(navbarPage(
  title="Reliance",
  tabPanel("1. Datos",sidebarLayout(
    sidebarPanel(
      tableOutput("resData")
    ),
    mainPanel(
      fileInput("file","Archivo"),
      dataTableOutput("data")
    )
  )),
  tabPanel("2. Modelos",sidebarLayout(
    sidebarPanel(
      actionButton("procesar",label="Crear modelos"),
      h4("Item 1"),
      uiOutput("selCat1"),
      uiOutput("selCat2"),
      uiOutput("selCat3"),
      h4("Item 2"),
      uiOutput("selCat4"),
      uiOutput("selCat5"),
      uiOutput("selCat6"),
    width=3),
    mainPanel(
      uiOutput("sliderX"),
      plotlyOutput("distPlot"),
      splitLayout(
        verticalLayout(
          h5("Item 1 (Rojo)"),
          tableOutput("R2.1"),
          fluid=F),
        verticalLayout(
          h5("Item 2 (Verde)"),
          tableOutput("R2.2"),
          fluid = F)
        )
    )
  )),
  tabPanel("3. Inferencia",sidebarLayout(
    sidebarPanel(
      uiOutput("selInference"),
      selectInput("metodo","Metodo",c("Modelo de Cox" = "cox","Red Neuronal" = "nnet"),selected = "nnet"),
      actionButton("R.inferencia","Realizar inferencia")
    ),
    mainPanel(tabsetPanel(
      tabPanel("Grafico",plotlyOutput("coefPlot"),tableOutput("trace")),
      tabPanel("Tabla",dataTableOutput("coefTable"))
    ))
  )),
  tabPanel("4. Proyecciones",verticalLayout(
    actionButton("save","salvar variables"),
    dateRangeInput("dates","Rango del estudio",language = "es",start = "2000-01-01",end = "2020-01-01"),
    plotlyOutput("diasUso"),
    plotlyOutput("fallasEst")
  )),
  tabPanel("5. Pronosticos",verticalLayout(
    uiOutput("selRelia"),
    plotlyOutput("reliaPlt",height = "600px")
  ))
))
