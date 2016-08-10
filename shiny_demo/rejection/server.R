#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source('settings.R')

values <- reactiveValues(samples=numeric(), 
                         majorizing = numeric(), 
                         z = numeric(),
                         acc = logical() )

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  getMajor = function(){
    switch( input$major,
            "Uniform" = function(x) x*0+1,
            "Beta( 1.5, 1.5 )" = function(x) dbeta(x, 1.5, 1.5),
            "Beta( 2, 2 )" = function(x) dbeta(x, 2, 2)
    )
  } 

  randomMajor = function(){
    switch( input$major,
            "Uniform" = function() rbeta(input$N, 1, 1 ),
            "Beta( 1.5, 1.5 )" = function() rbeta(input$N, 1.5, 1.5 ),
            "Beta( 2, 2 )" = function() rbeta(input$N, 2, 2 )
    )
  } 
  
    
  observeEvent(input$major, {
    values$samples=numeric()
    values$majorizing = numeric()
    values$acc = logical()
    values$z = numeric()
  })
  
  observeEvent(input$do_sample, {
    x = randomMajor()()
    mjd = getMajor()(x) / getMajor()(.5)
    z = runif(length(x), 0, mjd)
    acc = z < sin( pi * x )
    new.samples = x[ acc ]
    values$samples <- c( values$samples, new.samples )
    values$z <- c( values$z, z )
    values$majorizing <- c( values$majorizing, x )
    values$acc <- c( values$acc, acc )
  })
  
  output$acc_out <- renderText({
    Nsamp <- length(values$acc)
    Nacc <- sum(values$acc)
    paste("Acceptance rate: ",Nacc," / ",Nsamp," ( ",round(Nacc/Nsamp*100,1),"%; theoretical ", 
          round(2 / pi * getMajor()(.5) * 100, 1 ) ,"% )", sep=" ")
  })
  
  output$distPlot <- renderPlot({
    
    m = 2/pi
    Nsamp = length(values$majorizing)
    cand = values$majorizing[ Nsamp ]
    z = values$z[ Nsamp ]
    acc = values$acc[ Nsamp ]
    
    layout( matrix( c(1,1,1,2), 1 ) )
    
    par( par.list )

    xx = seq( 0 , 1 , len = 200 )
    
    plot(xx, sin( pi * xx ), ylab="", xlab="Value", 
         pch=21, col=cols1[3], ty='l', xlim = c(0,1),
         ylim = c(0, 1.3 ), axes=FALSE, lwd = 2 )
    
    
    lines( xx, getMajor()(xx) / getMajor()(.5), lty=2 )
    
    axis(1)
    
    if( length(values$majorizing) > 0 ){
      abline(v = cand, lty=2)
      if( input$show_major ){
        dn = density(values$majorizing)
        lines( dn$x, dn$y / getMajor()(.5), col = rgb(0,0,0, .66), lwd = 1, lty=3 )
      }
    }
    
    if( length(values$samples) > 0 ){
      points( values$samples, values$samples * 0 + .01, pch = 19, col = paste0( cols1[6], "66" ) )
    }
    if( length(values$samples) > 1 ){
      dn = density(values$samples)
      lines( dn$x, m * dn$y, col = paste0( cols1[6], "66" ), lwd = 1)
    }
    
    plot(0, 0, ylim=c(0,1.3), xlim=c(0,1), axes=FALSE,ylab="",xlab=ifelse( acc, "Accepted", "Rejected" ), ty='n' )
    if( length(values$majorizing) > 0 ){
      abline(h = getMajor()( cand ) / getMajor()(.5), col="black")
      abline(h = sin( pi * cand ), col = cols1[3] )
      abline(h = z, col=paste0( cols1[4], "66" ))
    }
    
  })
  
})
