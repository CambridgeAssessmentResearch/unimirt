#VERSION THAT ALLOWS INTERACTIVE DATA UPLOAD
#AND CALCULATION OF CLASSICAL STATISTICS

#Global statements
library(shiny)
library(ggplot2)
library(reshape2)
library(equate)

#SERVER
#SERVER
#SERVER
#SERVER
#SERVER
#SERVER
#SERVER
server <- function(input, output,session) {

  #SINGLE GROUP DESIGN
  sgdata=reactiveValues(data=NULL)
  sgfile=reactiveValues(datapath=NULL)
  observeEvent(input$sgfile, {sgfile$datapath <- input$sgfile$datapath})
  observeEvent(sgfile$datapath, {
    if (is.null(sgfile$datapath)) {
      # User has not uploaded a file yet
      sgdata$data=NULL
    }
    if (!is.null(sgfile$datapath)) {
      sgdata$data=data.frame(read.csv(sgfile$datapath))
    }
  })
  
  output$sgtt <- renderTable({
    if(is.null(sgdata$data)){return(NULL)}
    head(sgdata$data)
  })

  output$sgmax1 <- renderUI({
    numericInput("sgmax1","Specificy the maximum possible score on test 1",max(sgdata$data[,1]))
  })
  
  output$sgmax2 <- renderUI({
    numericInput("sgmax2","Specificy the maximum possible score on test 2",max(sgdata$data[,2]))
  })
  
  sgdistdata1=reactive({
    if(is.null(sgdata$data)){return(NULL)}
    
    tots=sgdata$data[,1]
    max1=input$sgmax1
    distdata1=data.frame(Score=0:max1,
                         N=tabulate(tots+1,max1+1))
    distdata1$Per_cent=floor(1000*distdata1$N/sum(distdata1$N)+0.5)/10
    distdata1$Cum_per_cent=floor(1000*sapply(distdata1$Score
                                             ,function(i){0+mean(tots>=i)})+0.5)/10
    distdata1
  })

  sgdistdata2=reactive({
    if(is.null(sgdata$data)){return(NULL)}
    
    tots=sgdata$data[,2]
    max1=input$sgmax2
    distdata1=data.frame(Score=0:max1,
                         N=tabulate(tots+1,max1+1))
    distdata1$Per_cent=floor(1000*distdata1$N/sum(distdata1$N)+0.5)/10
    distdata1$Cum_per_cent=floor(1000*sapply(distdata1$Score
                                             ,function(i){0+mean(tots>=i)})+0.5)/10
    distdata1
  })
  output$sgdisttab1=renderTable({sgdistdata1()})
  output$sgdisttab2=renderTable({sgdistdata2()})
  
  sgeqtab=reactive({
    if(is.null(sgdata$data)){return(NULL)}
    nx <- freqtab(sgdata$data
                  ,scales=list(0:input$sgmax1,0:input$sgmax2))

    #d1=read.csv("//SPINFFS001/AsmntDir/RD/Internal/Active Projects/Activities/ExternalTrainingForNetwork/A203 Psychometrics/Example data/Equating - single group design.csv")
    #nx <- freqtab(d1,scales=list(0:25,0:25))
    #neq1 <- equate(nx,type="equipercentile")
    
    neq1 <- equate(nx,type=input$sgeqtype,smoothmethod="bump",jmin=1e-06)
    out=neq1$concordance[,1:2]
    out
  })
  
  output$sgeqtab=renderTable({sgeqtab()})  
  output$downloadsgeqtab <- downloadHandler(
    filename = function() {"Single group equating results.csv"},
    content = function(file) {
      write.csv(sgeqtab(), file, row.names = FALSE)
    })
  
  output$sgeqplot=renderPlot({
    if(is.null(sgdata$data)){return(NULL)}
    ggplot(data=data.frame(sgeqtab()),aes(x=scale,y=yx))+geom_line()+
      scale_x_continuous(limits=c(0,input$sgmax1))+
      scale_y_continuous(limits=c(0,input$sgmax2))+
      labs(x="Score on test 1",y="Equivalent score on test 2")+
      geom_abline(lty=3)

    #ggplot(data=data.frame(out),aes(x=scale,y=yx))+geom_line()+
    #  scale_x_continuous(limits=c(0,25))+
    #  scale_y_continuous(limits=c(0,25))+
    #  labs(x="Score on test 1",y="Equivalent score on test 2")+
    #  geom_abline(lty=3)
    
      })  
  
  #RANDOM GROUPS DESIGN
  #first data set
  rgdata1=reactiveValues(data=NULL)
  rgfile1=reactiveValues(datapath=NULL)
  observeEvent(input$rgfile1, {rgfile1$datapath <- input$rgfile1$datapath})
  observeEvent(rgfile1$datapath, {
    if (is.null(rgfile1$datapath)) {
      # User has not uploaded a file yet
      rgdata1$data=NULL
    }
    if (!is.null(rgfile1$datapath)) {
      rgdata1$data=data.frame(read.csv(rgfile1$datapath))
    }
  })
  output$rgtt1 <- renderTable({
    if(is.null(rgdata1$data)){return(NULL)}
    head(rgdata1$data)
  })
  #second data set
  rgdata2=reactiveValues(data=NULL)
  rgfile2=reactiveValues(datapath=NULL)
  observeEvent(input$rgfile2, {rgfile2$datapath <- input$rgfile2$datapath})
  observeEvent(rgfile2$datapath, {
    if (is.null(rgfile2$datapath)) {
      # User has not uploaded a file yet
      rgdata2$data=NULL
    }
    if (!is.null(rgfile2$datapath)) {
      rgdata2$data=data.frame(read.csv(rgfile2$datapath))
    }
  })
  output$rgtt2 <- renderTable({
    if(is.null(rgdata2$data)){return(NULL)}
    head(rgdata2$data)
  })
  
    
  output$rgmax1 <- renderUI({
    numericInput("rgmax1","Specificy the maximum possible score on test 1",max(rgdata1$data[,1]))
  })
  
  output$rgmax2 <- renderUI({
    numericInput("rgmax2","Specificy the maximum possible score on test 2",max(rgdata2$data[,1]))
  })
  
  #actual equating stuff
  rgdistdata1=reactive({
    if(is.null(rgdata1$data)|is.null(rgdata2$data)){return(NULL)}
    
    tots=rgdata1$data[,1]
    max1=input$rgmax1
    distdata1=data.frame(Score=0:max1,
                         N=tabulate(tots+1,max1+1))
    distdata1$Per_cent=floor(1000*distdata1$N/sum(distdata1$N)+0.5)/10
    distdata1$Cum_per_cent=floor(1000*sapply(distdata1$Score
                                             ,function(i){0+mean(tots>=i)})+0.5)/10
    distdata1
  })
  
  rgdistdata2=reactive({
    if(is.null(rgdata1$data)|is.null(rgdata2$data)){return(NULL)}
    
    tots=rgdata2$data[,1]
    max1=input$rgmax2
    distdata1=data.frame(Score=0:max1,
                         N=tabulate(tots+1,max1+1))
    distdata1$Per_cent=floor(1000*distdata1$N/sum(distdata1$N)+0.5)/10
    distdata1$Cum_per_cent=floor(1000*sapply(distdata1$Score
                                             ,function(i){0+mean(tots>=i)})+0.5)/10
    distdata1
  })
  output$rgdisttab1=renderTable({rgdistdata1()})
  output$rgdisttab2=renderTable({rgdistdata2()})
  
  rgeqtab=reactive({
    if(is.null(rgdata1$data)|is.null(rgdata2$data)){return(NULL)}
    nx <- freqtab(rgdata1$data[,1],scales=list(0:input$rgmax1))
    ny <- freqtab(rgdata2$data[,1],scales=list(0:input$rgmax2))
    neq1 <- equate(nx,ny,type=input$rgeqtype,smoothmethod="bump",jmin=1e-06)
    out=neq1$concordance[,1:2]
    out
  })
  output$downloadrgeqtab <- downloadHandler(
    filename = function() {"Random groups equating results.csv"},
    content = function(file) {
      write.csv(rgeqtab(), file, row.names = FALSE)
    })
  
  output$rgeqtab=renderTable({rgeqtab()})  
  
  output$rgeqplot=renderPlot({
    if(is.null(rgdata1$data)|is.null(rgdata2$data)){return(NULL)}
    ggplot(data=data.frame(rgeqtab()),aes(x=scale,y=yx))+geom_line()+
      scale_x_continuous(limits=c(0,input$rgmax1))+
      scale_y_continuous(limits=c(0,input$rgmax2))+
      labs(x="Score on test 1",y="Equivalent score on test 2")+
      geom_abline(lty=3)
    
  })  
  
  #NEAT DESIGN
  #first data set
  neatdata1=reactiveValues(data=NULL)
  neatfile1=reactiveValues(datapath=NULL)
  observeEvent(input$neatfile1, {neatfile1$datapath <- input$neatfile1$datapath})
  observeEvent(neatfile1$datapath, {
    if (is.null(neatfile1$datapath)) {
      # User has not uploaded a file yet
      neatdata1$data=NULL
    }
    if (!is.null(neatfile1$datapath)) {
      neatdata1$data=data.frame(read.csv(neatfile1$datapath))
    }
  })
  output$neattt1 <- renderTable({
    if(is.null(neatdata1$data)){return(NULL)}
    head(neatdata1$data)
  })
  #second data set
  neatdata2=reactiveValues(data=NULL)
  neatfile2=reactiveValues(datapath=NULL)
  observeEvent(input$neatfile2, {neatfile2$datapath <- input$neatfile2$datapath})
  observeEvent(neatfile2$datapath, {
    if (is.null(neatfile2$datapath)) {
      # User has not uploaded a file yet
      neatdata2$data=NULL
    }
    if (!is.null(neatfile2$datapath)) {
      neatdata2$data=data.frame(read.csv(neatfile2$datapath))
    }
  })
  output$neattt2 <- renderTable({
    if(is.null(neatdata2$data)){return(NULL)}
    head(neatdata2$data)
  })
  
  
  output$neatmax1 <- renderUI({
    numericInput("neatmax1","Specificy the maximum possible score on test 1",max(neatdata1$data[,1]))
  })
  
  output$neatmax2 <- renderUI({
    numericInput("neatmax2","Specificy the maximum possible score on test 2",max(neatdata2$data[,1]))
  })
  
  output$neatmaxanc <- renderUI({
    numericInput("neatmaxanc","Specificy the maximum possible score on anchor test",max(c(neatdata1$data[,2],neatdata2$data[,2])))
  })
  
  #actual equating stuff
  neatdistdata1=reactive({
    if(is.null(neatdata1$data)|is.null(neatdata2$data)){return(NULL)}
    
    tots=neatdata1$data[,1]
    max1=input$neatmax1
    distdata1=data.frame(Score=0:max1,
                         N=tabulate(tots+1,max1+1))
    distdata1$Per_cent=floor(1000*distdata1$N/sum(distdata1$N)+0.5)/10
    distdata1$Cum_per_cent=floor(1000*sapply(distdata1$Score
                                             ,function(i){0+mean(tots>=i)})+0.5)/10
    distdata1
  })
  neatdistdata1anc=reactive({
    if(is.null(neatdata1$data)|is.null(neatdata2$data)){return(NULL)}
    
    tots=neatdata1$data[,2]
    max1=input$neatmaxanc
    distdata1=data.frame(Score=0:max1,
                         N=tabulate(tots+1,max1+1))
    distdata1$Per_cent=floor(1000*distdata1$N/sum(distdata1$N)+0.5)/10
    distdata1$Cum_per_cent=floor(1000*sapply(distdata1$Score
                                             ,function(i){0+mean(tots>=i)})+0.5)/10
    distdata1
  })
  
  neatdistdata2=reactive({
    if(is.null(neatdata1$data)|is.null(neatdata2$data)){return(NULL)}
    
    tots=neatdata2$data[,1]
    max1=input$neatmax2
    distdata1=data.frame(Score=0:max1,
                         N=tabulate(tots+1,max1+1))
    distdata1$Per_cent=floor(1000*distdata1$N/sum(distdata1$N)+0.5)/10
    distdata1$Cum_per_cent=floor(1000*sapply(distdata1$Score
                                             ,function(i){0+mean(tots>=i)})+0.5)/10
    distdata1
  })
  neatdistdata2anc=reactive({
    if(is.null(neatdata1$data)|is.null(neatdata2$data)){return(NULL)}
    
    tots=neatdata2$data[,2]
    max1=input$neatmaxanc
    distdata1=data.frame(Score=0:max1,
                         N=tabulate(tots+1,max1+1))
    distdata1$Per_cent=floor(1000*distdata1$N/sum(distdata1$N)+0.5)/10
    distdata1$Cum_per_cent=floor(1000*sapply(distdata1$Score
                                             ,function(i){0+mean(tots>=i)})+0.5)/10
    distdata1
  })

  output$neatdisttab1=renderTable({neatdistdata1()})
  output$neatdisttab2=renderTable({neatdistdata2()})
  output$neatdisttab1anc=renderTable({neatdistdata1anc()})
  output$neatdisttab2anc=renderTable({neatdistdata2anc()})
  
  neateqtab=reactive({
    if(is.null(neatdata1$data)|is.null(neatdata2$data)){return(NULL)}
    nx <- freqtab(neatdata1$data[,1:2],scales=list(0:input$neatmax1,0:input$neatmaxanc))
    ny <- freqtab(neatdata2$data[,1:2],scales=list(0:input$neatmax2,0:input$neatmaxanc))
    
    if(input$neateqfulltype=="Identity"){return(data.frame(scale=0:input$neatmax1,yx=0:input$neatmax1))}

    if(input$neateqfulltype=="Tucker linear"){
      type="linear"
      method="tucker"
    }
    if(input$neateqfulltype=="Chained linear"){
      type="linear"
      method="chained"
    }
    if(input$neateqfulltype=="Frequency estimation equipercentile"){
      type="equipercentile"
      method="frequency estimation"
    }  
    if(input$neateqfulltype=="Chained equipercentile"){
      type="equipercentile"
      method="chained"
    }  
    if(input$neateqfulltype=="Chained circle-arc"){
      type="circle-arc"
      method="chained"
    }  
  
    neq1 <- equate(nx,ny,type=type,method=method,smoothmethod="bump",jmin=1e-06)
    out=neq1$concordance[,1:2]
    out
  })
  
  output$downloadneateqtab <- downloadHandler(
    filename = function() {"NEAT equating results.csv"},
    content = function(file) {
      write.csv(neateqtab(), file, row.names = FALSE)
    })
  output$neateqtab=renderTable({neateqtab()})  
  
  output$neateqplot=renderPlot({
    if(is.null(neatdata1$data)|is.null(neatdata2$data)){return(NULL)}
    ggplot(data=data.frame(neateqtab()),aes(x=scale,y=yx))+geom_line()+
      scale_x_continuous(limits=c(0,input$neatmax1))+
      scale_y_continuous(limits=c(0,input$neatmax2))+
      labs(x="Score on test 1",y="Equivalent score on test 2")+
      geom_abline(lty=3)
    
  })  
  

  
  ###END OF MAIN SERVER STUFF  
  
    session$onSessionEnded(function() {
   stopApp()
 })

}

###UI
###UI
###UI
###UI
###UI
###UI
###UI
###UI
ui <- fluidPage(
  # Application title
  titlePanel("Classical test equating"),
  navlistPanel(
    tabPanel("Single group design"
             ,fluidRow("First upload a file of data from the tests you want to equate. 
                    Each row should relate to an individual pupils.
                    There should be two columns denoting the two pupils'
                    scores on the tests you want to equate.
                    No extraneous information 
                      (e.g. pupil IDs) should be included.")
             ,br(),br()
             ,fileInput('sgfile', 'CSV file of item data',
                        accept=c('text/csv'
                                 , 'text/comma-separated-values,text/plain'))
             ,fluidRow("Below is a preview of the first few rows of your data set (if uploaded).")
             ,tableOutput("sgtt")
             ,br(),br()
             ,fluidRow(uiOutput("sgmax1"))
             ,fluidRow(uiOutput("sgmax2"))
             ,fluidRow(selectInput("sgeqtype","Specify the equating method you want to use"
                                   ,c("linear","equipercentile"
                                      ,"circle-arc","mean","identity")))
             ,br(),br()
             ,fluidRow("The equating relationship between the two tests is shown below.")
             ,tableOutput("sgeqtab")
             ,downloadButton("downloadsgeqtab", "Download table")
             ,br(),br()
             ,fluidRow("The same equating relationship is shown graphically below. The dotted line is a line of equality.")
             ,plotOutput("sgeqplot")
             ,br(),br()
             ,fluidRow("The tables below gives the score distributions on each test.")
             ,fluidRow("Test 1.")
             ,tableOutput("sgdisttab1")
             ,fluidRow("Test 2.")
             ,tableOutput("sgdisttab2")
    )
    ,
    tabPanel("Random groups design"
             ,fluidRow("First upload two files of data from the tests you want to equate. 
                       Each data set should consist of a single column with rows containing the test scores of individual pupils.
                       No extraneous information 
                       (e.g. pupil IDs) should be included.")
             ,br(),br()
             ,fileInput('rgfile1', 'CSV file of item data',
                        accept=c('text/csv'
                                 , 'text/comma-separated-values,text/plain'))
             ,fileInput('rgfile2', 'CSV file of item data',
                        accept=c('text/csv'
                                 , 'text/comma-separated-values,text/plain'))
             ,fluidRow("Below is a preview of the first few rows of your data set (if uploaded).")
             ,fluidRow("Test 1")
             ,tableOutput("rgtt1")
             ,fluidRow("Test 2")
             ,tableOutput("rgtt2")
             ,br(),br()
             ,fluidRow(uiOutput("rgmax1"))
             ,fluidRow(uiOutput("rgmax2"))
             ,fluidRow(selectInput("rgeqtype","Specify the equating method you want to use"
                                   ,c("linear","equipercentile"
                                      ,"circle-arc","mean","identity")))
             ,br(),br()
             ,fluidRow("The equating relationship between the two tests is shown below.")
             ,tableOutput("rgeqtab")
             ,downloadButton("downloadrgeqtab", "Download table")
             ,br(),br()
             ,fluidRow("The same equating relationship is shown graphically below.
                       The dotted line is a line of equality.")
             ,plotOutput("rgeqplot")
             ,br(),br()
             ,fluidRow("The tables below gives the score distributions on each test.")
             ,fluidRow("Test 1.")
             ,tableOutput("rgdisttab1")
             ,fluidRow("Test 2.")
             ,tableOutput("rgdisttab2")
             )
    ,
    tabPanel("NEAT design"
             ,fluidRow("This tool can be used to equate tests using a non-equivalent groups with anchor test (NEAT) design.")
             ,fluidRow("First upload two files of data from the tests you want to equate. 
                       Each data set should consist of two columns. Each row should provide the score on the test you want
                        to equate in the first column and the score
                        on the anchor test in the second column.
                       No extraneous information 
                       (e.g. pupil IDs) should be included.")
             ,br(),br()
             ,fileInput('neatfile1', 'CSV file of item data',
                        accept=c('text/csv'
                                 , 'text/comma-separated-values,text/plain'))
             ,fileInput('neatfile2', 'CSV file of item data',
                        accept=c('text/csv'
                                 , 'text/comma-separated-values,text/plain'))
             ,fluidRow("Below is a preview of the first few rows of your data set (if uploaded).")
             ,fluidRow("Test 1")
             ,tableOutput("neattt1")
             ,fluidRow("Test 2")
             ,tableOutput("neattt2")
             ,br(),br()
             ,fluidRow(uiOutput("neatmax1"))
             ,fluidRow(uiOutput("neatmax2"))
             ,fluidRow(uiOutput("neatmaxanc"))
             #,fluidRow(selectInput("neateqtype","Specify the equating method you want to use"
            #                       ,c("linear","equipercentile"
             #                         ,"circle-arc","mean","identity")))
             #,fluidRow(selectInput("neateqmethod","Specify the linking method you want to use"
              #                     ,c("tucker","frequency estimation","chained")))
            ,fluidRow(selectInput("neateqfulltype","Specify the equating method you want to use"
                                   ,c("Tucker linear"
                                      ,"Chained linear"
                                      ,"Frequency estimation equipercentile"
                                      ,"Chained equipercentile"
                                      ,"Chained circle-arc"
                                      ,"Identity"
                                      )))
            ,br(),br()
             ,fluidRow("The equating relationship between the two tests is shown below.")
             ,tableOutput("neateqtab")
            ,downloadButton("downloadneateqtab", "Download table")
            ,br(),br()
             ,fluidRow("The same equating relationship is shown graphically below.")
             ,plotOutput("neateqplot")
             ,br(),br()
             ,fluidRow("The tables below gives the score distributions on the main test and anchor test in each group.")
             ,fluidRow(column(6,"Test 1."),column(6,"Anchor"))
             ,fluidRow(column(6,tableOutput("neatdisttab1")),column(6,tableOutput("neatdisttab1anc")))
             ,fluidRow(column(6,"Test 2."),column(6,"Anchor"))
             ,fluidRow(column(6,tableOutput("neatdisttab2")),column(6,tableOutput("neatdisttab2anc")))
    )
    )
)

# Run the application 
shinyApp(ui = ui, server = server)


