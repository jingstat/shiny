library(lattice)
library(Matrix)
library(coda)
library(rjags)
library(ggplot2)
source("D:/mystuff/Adaptive_Design_MM_project/Bayesian Hirachi/ComboBayFit.r")



N <- 12
X <- diag(rep(1,12))
y <-rep(0,12)
n <- rep(0,12)

compound <- c(rep(1,4),rep(2,4),rep(3,4))
backbone <- rep(c(1:4),3)

modlist<- list(mod_string1, mod_string2,mod_string3,mod_string4, mod_string5, mod_string6)

xlist<- list('x.1' = X[,1], 'x.2' = X[,2], 'x.3'=X[,3],
             'x.4' = X[,4], 'x.5' = X[,5], 'x.6'=X[,6],
             'x.7' = X[,7], 'x.8' = X[,8], 'x.9' = X[,9], 
             'x.10'=X[,10], 'x.11' = X[,11], 'x.12' = X[,12],'N' = N, 'y'=y, 'n'=n)



server <- function(input, output){
  observeEvent(input$do,
    {
    para1 <- reactive({
    para<-   c(input$p1, input$p2,input$p3,input$p4,input$p5,input$p6,
               input$p7, input$p8,input$p9,input$p10,input$p11,input$p12  )
       para})

    
    fit <- reactive({discrpt <- "Main compound effect and backbone efect"
      modfix<- list("mod4"= mod_string4)
      psoc <- 0.7
    
      set.seed(301)
      fit <- ComboBayFit(n1=10, n2=10, n=30, para=para1(), modstr=modlist, xlist=xlist, delta=psoc,epsL=0.1,Penalty=1, selection=TRUE)
    
      event <- fit[[3]]
      predp <- fit[[4]]
      
      test<- data.frame(compound, backbone, predp,para1(), M=which(fit[[5]],TRUE))
      test
     })
     output$plot1 <-  renderPlot({
    
      
      a<- ggplot(fit(), aes(x=backbone, y= predp))+ geom_point(aes(colour = factor(compound)),size=4 ) + theme_bw()
      a
      })
     output$summary<- renderPrint({
       fit()
     })
    })
    
}