#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Rejection sampling"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("major", label = "Majorizing:",
                  choices = c("Uniform","Beta( 1.5, 1.5 )","Beta( 2, 2 )"), selected = "Uniform"),
  
      selectInput("N", label = "Samples:",
                  choices = c(1,10,100,1000), selected = 1),
      
      actionButton("do_sample", "Sample", icon = icon("play")),
      
      checkboxInput("show_major", "Show majorizing samples density?" ),
                    
      tags$hr(),
      
      textOutput("acc_out")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
