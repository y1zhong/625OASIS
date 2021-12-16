# load the shiny package
library(shiny)
library(oro.nifti)
library(stringr)
library(dplyr) 
library(ggplot2)
library(glmnet)

reshapeData <- function(path) {
  img_raw = oro.nifti::readANALYZE(path)@.Data[,,88,1]
  img = reshape2::melt(img_raw)
  return(img)
}

readData <- function(path) {
  img_raw = oro.nifti::readANALYZE(path)@.Data[,,88,1]
  img_raw2 = img_raw[apply(img_raw != 0, 1, any), apply(img_raw != 0, 2, any)]
  return(img_raw2)
}


ui <- fluidPage(
  titlePanel(p("Predication APP for Demantia")),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload image information.", accept = ".zip"),
      actionButton("showButton", "Showing your MRI"),
      
      tags$hr(),
      radioButtons("m.f", "Your gender:",
                   choices = c(Male= "M",
                               "Female" = 'F'),
                   selected = 'F'),
      numericInput('age', 'Your age:', 70, min = 60, max = 100),
      radioButtons("educ", "Your education level:",
                   choices = c('less than high school grad.'= 1,
                               "high school grad." = 2,
                               "some college" = 3,
                               "college grad." = 4,
                               "beyond college" = 5),
                   selected = 1),
      sliderInput("SES",
                  "Your socioeconomic status:",
                  min = 1,  max = 5, value = 3),
      actionButton("goButton", "Go")
    ),

    mainPanel(
      plotOutput("plot2"),
      h3('Results of Demantia Prediction'),
      h4('Based on value, your predicted Demantia status is'),
      verbatimTextOutput("check")
    )
  )
)

server <- function(input,output){
  load("ridge_fit.Rdata")
  best.fit = ridge.fit
  AD_check <- function(fit = best.fit,data) {
    ridge.pred.tst <- predict(fit, as.vector(data), type="response")
    res = as.character(ifelse(ridge.pred.tst==0,'No Demantia','Possible to have Demantia'))
    return(res)
  }
  
  reshapeDat <- eventReactive(input$showButton,{
    uz = unzip(input$file1$datapath)
    path = grep('(\\masked_gfc.hdr$)', uz, value = TRUE)[1]
    out = reshapeData(path)
  })
  
  output$plot2<-renderPlot({
    ggplot(reshapeDat(), aes(x = Var1, y = Var2)) + geom_raster(aes(fill=value)) + theme(axis.title.x = element_blank(), axis.title.y = element_blank())+ theme(legend.position = "none")} , width = 400, height = 400)
  
  Dat <- eventReactive(input$goButton,{
    uz = unzip(input$file1$datapath)
    path = grep('(\\masked_gfc.hdr$)', uz, value = TRUE)[1]
    out = readData(path)
  })
  # user_data <- cbind.data.frame(
  #   x = Dat(),
  #   "m.f" =input$m.f,
  #   "age" =input$age,
  #   "educ" =input$educ,
  #   "SES" =input$SES
  # )
  
  output$check <- renderPrint({AD_check(best.fit,Dat())})
}

# call to shinyApp() which returns the Shiny app
shinyApp(ui = ui, server = server)
