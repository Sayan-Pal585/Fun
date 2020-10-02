library("shiny", lib.loc="~/R/win-library/3.4")

ui<-fluidPage("Hello World")

server<-function(input,output){}

shinyApp(ui=ui,server=server)

##slider input type
ui<-fluidPage(sliderInput(inputId = "num",label = "choose a number",value =67,min=1,max=100))

server<-function(input,output){}

shinyApp(ui=ui,server=server)

##plot input & output type
ui<-fluidPage(sliderInput(inputId = "num",label = "choose a number",value =67,min=1,max=1000),plotOutput(outputId = "hist"))

server<-function(input,output){
  output$hist<-renderPlot({
    title<-"Histogram plot for rnorm"
    hist(rnorm(input$num),main = title)
  })
}

shinyApp(ui=ui,server=server)

##
