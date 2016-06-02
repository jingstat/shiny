require(lattice)
require(Matrix)
require(coda)
require(rjags)
require(ggplot2)
#source("D:/mystuff/myshiny/test1/AppComboBay/VectorFit.R")
source("VectorFit.R")
source("source.R")
server <- function(input, output,session){
  myData <- reactive({
    inFile <- input$file1
  
    if (is.null(inFile))
      return(NULL)
  
    mytable<- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                     quote=input$quote)
    mytable
  })


  output$contents <- DT::renderDataTable({
  
    DT::datatable(myData(), options=list(orderClassess = TRUE))
  })


  output$summary1 <- renderPrint({
   # summary(myData()), 
    matrix(myData()$y/myData()$n, input$combos,input$backbones) 
  })
  
  
  

  modlist<- list(mod_string1,mod_string2,mod_string3, mod_string4, mod_string5,mod_string6)
  modfix<- list("mod4"= mod_string4)
 

   modstrInput <- reactive({
     switch(input$select, "Auto"=modlist,
            "Hierarchy"=modfix)
   })

      fit <- reactive({
        
        cn <- input$combos
        bn <- input$backbones
        compound <- rep(c(1:cn),bn)
        backbone <- rep(1:bn, each=cn)
        
        nmatrix <- matrix(myData()$n,cn,bn)
        ymatrix <- matrix(myData()$y,cn,bn)
        
        xlist<- list('y'=ymatrix,'n'=nmatrix,'cn' = cn, 'bn'=bn)  
        output<- VecFit(modstr=modstrInput(), xlist=xlist)

      
      armctn <- output[[1]] 
      predp <- output[[2]]
      mod.select <- output[[3]]
      test<- data.frame(compound, backbone, armctn,predp,mod.select)
      test
      })
      
    
     output$plot1 <-  renderPlot({
       a<- ggplot(fit(), aes(x=backbone, y= predp))+ geom_point(aes(colour = factor(compound)),size=3)+
           geom_point(data=fit()[fit()$armctn==0,],shape=4,size=6)
       a
      })
     output$summary2<- renderPrint({
            fit()
     })
     
     simufit <- eventReactive(
       input$sim.start, {
         cn.sim <- input$combos
         bn.sim <- input$backbones
         para<- myData()$Phat 
        #   rep(c(0.1,0.5,0.8),4)
         k.itm <- input$k.itm
         stop.itm <- input$stop.itm
         nsubj <- rep(input$npitm, k.itm)
         BayMSD(cn=cn.sim, bn=bn.sim, para=para, stop.itm=stop.itm, k.itm=k.itm, modlist=modlist,n=nsubj)
         }
     )
     
     
     output$summary3 <- renderPrint({ 
           simufit()
     })
     
     
}