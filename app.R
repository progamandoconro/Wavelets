
library(shiny)
library(WaveletComp)
library(wavelets)
PAGE_TITLE<-"Análisis wavelet de la fenología de un bosque nublado en Venezuela"
x<-read.csv("dataSaul2018.csv")
d<-x[,3:40]
ui <- fluidPage(
  titlePanel(windowTitle =PAGE_TITLE  ,title = div(PAGE_TITLE,
                                                   img(
                                                     src = "my_logo.png",
                                                     height = 150,
                                                     width = 75)
  )),
  
  
  h5("Creador: Rodrigo Díaz-Lupanow "),
  h5("Colaboradores científicos: Saúl Flores, Carlos Méndez, Esther Gutierrez, Alejandro Ramirez-Rojas "),
  h5("email: rodlupanow@gmail.com"),
  
  sidebarLayout(
    sidebarPanel(
  
h5("Univariada"),
      selectInput("selection3", "Variables (Clima/Flores/Frutos):",
                  choices = colnames(d),selected = "A.fendleri"),
h5("Bivariada"),
selectInput("selection", "Variables (Clima/Flores/Frutos):",
            choices = colnames(d),selected = "Precip")
    ),
    
    
    mainPanel(
     
      plotOutput("distPlot3"),
      plotOutput("distPlot2")
    )
  )
)
server <- function(input, output) {
  
  
  output$distPlot3 <- renderPlot({
    
    my.w <- analyze.wavelet(d, input$selection3,
                            loess.span = 0,
                            dt = 1,
                            lowerPeriod = 1, upperPeriod = 80,
                            make.pval = TRUE, n.sim = 10)
    
   wt.image(my.w
                  , color.key = "interval", n.levels = 250,
             legend.params = list(lab = "wavelet power levels"),
             periodlab = "Periodo (meses)")

  })
  
  output$distPlot2 <- renderPlot({
    
   
    my.wc <- analyze.coherency(d, my.pair = c(input$selection3,input$selection),
                               loess.span = 0,
                               dt = 1,
                               lowerPeriod = 8,
                               upperPeriod = 80,
                               make.pval = TRUE, n.sim = 10)
    
    wc.image(my.wc, n.levels = 250,
             legend.params = list(lab = "cross-wavelet power levels"),
             timelab = "", periodlab = "Periodo (meses)")
    
    
  })
  
}

shinyApp(ui = ui, server = server)

