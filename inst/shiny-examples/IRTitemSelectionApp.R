#Global statements
library(ggplot2)
library(mirt)
library(shiny)
library(ROI.plugin.lpsolve)
library(ROI)
library(ompr.roi)
library(ompr)
library(dplyr)
globls=ls(name=".GlobalEnv")
classes=NULL
for(i in 1:length(globls)){classes=c(classes,as.vector(paste(class(get(globls[i])),collapse="") ))}
loadofmirts=ls(name=".GlobalEnv")[classes=="SingleGroupClass"]
print(loadofmirts)

#UI
#UI
#UI
#UI
#UI
#UI
ui <- fluidPage(
  titlePanel("Item selection/test construction"),
navlistPanel(
  tabPanel("Manual selections",
           selectInput("sel1", "First select an IRT analysis",loadofmirts)
           ,br(),br()
           ,"Can optionally alter the desired population ability distribution"
           ,splitLayout(numericInput("thetamean","Mean",value=0,step=0.1,min=-3,max=3)
                        ,numericInput("thetasd","SD",value=1,step=0.1,min=0.1,max=3.5))
           ,br(),br()
           ,"Here is a list of the indices of the items currently included in the test."
           ,br()
           ,textOutput("which.items")
           ,br(),br()
           ,"Can now manually select items to include in test and see impact on score
           distribution and other things. When the app is started, by default,
           all items are included in the constructed test once.
           Note that a separate tab allows optimal item selection subject to some criteria."
           ,br(),br()
           ,splitLayout(actionButton("AllZero", label = "Remove ALL items",default=1)
                        ,actionButton("AllOne", label = "Include ALL items (once)",default=1))
           ,br()
           ,uiOutput("item_selections")
  )
  ,
  tabPanel("Score distribution - selected",br(),br()
                    ,"The chart below shows the estimated total score distribution for a test
                    consisting of the items you have selected (and the chosen ability distribution).
                    You can manually add or remove items (and alter the ability distribution)
                    using the panel on the left."
                    ,br(),br()
                    ,plotOutput("totalscoredistplot")
                    ,"The table below gives some estimated basic statistics about the
                    test you have constructed."
                    ,tableOutput("totalscoredisttable")
                    ,"The table below shows an estimate of the score distribution.
                    The cumulative percent is defined as the percentage of students
                    expected to be at or above each score.
            Also shown is the expected mean and 
                 standard deviation of ability at each raw score
           based on the fitted IRT model. Finally the
           table shows the ability estimate that corresponds
           to each total score on the test characteristic curve."
           
                    ,tableOutput("scoredisttab2")
           )
           ,
           tabPanel("Test information - selected",br()
                    ,"The chart below shows the total test information based on the selected items."
                    ,plotOutput("totalinfoplot"))
           ,
           tabPanel("Test characteristic curve - selected",br()
                    ,"The chart below shows the test characteristic curve based on the selected items.
                    This shows the score we expect candidates at different ability level to achieve."
                    ,plotOutput("tcc"))
           ,
           tabPanel("Optimal item selection",br()
                    ,"This tab allows the automation of the item selection problem.
                    To begin with you may wish to upload further information about items."
                    ,fileInput('datafile', '(Optional) Choose CSV file of additional item information',
                              accept=c('text/csv'
                                       , 'text/comma-separated-values,text/plain'))
                    ,br(),br()
                    ,"(Optional) You may also wish to calculate the expected item scores
                    at particular ability levels. This can be used to help ensure
                    that the mean total achieved by a particular type of student is at a target value.
                    Expected scoress on each item at this level will be stored in the variables
                    'mean.at.chosen.ability' and 'mean.at.chosen.ability2'."
                    ,splitLayout(numericInput("targthetaei","Ability level 1",0,step=0.1,min=-3,max=3)
                                 ,numericInput("targthetaei2","Ability level 2",0,step=0.1,min=-3,max=3))
                    ,br(),br()
                    ,"Item selection is done to maximise the test information 
                    (i.e. the area under the test information curve) within
                    a particular ability range subject to the constraints specified below.
                    Select the an ability range you are most interested in. For example,
                    this might be the range in which we expect to find most of the students."
                    ,splitLayout(numericInput("abilmin","Min target ability (reliability)",-1)
                                 ,numericInput("abilmax","Max target ability (reliability)",1))
                    ,br(),br()
                    ,"Now add some constraints on item selection. Each constraint
                    is specified in terms of the minimum and maximum level that
                    a given variable can sum to across the items.
                    At the very least it is usually worth adding a constraint on the
                    number of marks (variable 'Max') that you want included."
                    ,numericInput("nconstr","Number of constraints on optimisation",1)
                    ,uiOutput("constr_selections")
                    ,actionButton("startopt", label = "Start Optimisation",default=1)
                    ,br(),br()
                    ,"The table below shows the sum of the variables 
                    used for optimisation and in the constraints for the selected items.
                    If this table is missing it was not possible to find
                    a selection of items matching the constraints."
                    ,tableOutput("idata1aoptsum")
                    ,br(),br()
                    ,"The table below shows the selected items.
                    If this table is missing it was not possible to find
                    a selection of items matching the constraints."
                    ,tableOutput("idata1aopt")
                    ,br(),br()
                    ,"The table below simply list all of the data available 
                    for all of the items that can be used in optimisation."
                    ,tableOutput("idata1")
           )
           ,
           tabPanel("Estimated classical parameters",br()
                    ,"For information the table below shows the estimated classical
                    item parameters of all items. These are estimated
                    based on the IRT model."
                    ,tableOutput("estclass"))
           ,
           tabPanel("IRT coefficients",br()
                    ,"For information the table below shows the IRT
                    item parameters of all items."
                    ,tableOutput("coefs"))
    ))
    

#SERVER
#SERVER
#SERVER
#SERVER
#SERVER
#SERVER
server <- function(input, output,session) {
  
  revals=reactiveValues(optsel=NULL)
  tempmirt1=reactive({get(input$sel1)})
  coefs=reactive({coef(tempmirt1(),simplify=TRUE,IRTpars=TRUE)$items})
  nites <- reactive({nrow(coefs())})
  #thetas=reactive({tempmirt1()@Model$Theta})
  thetas=reactive({tempmirt1()@Model$Theta
    as.matrix(seq(-6,6,length=201))})
  #qwts=reactive({extract.mirt(tempmirt1(),"Prior")[[1]]})
  qwts=reactive({temp1=dnorm(as.numeric(thetas()),input$thetamean,input$thetasd)
                temp1/sum(temp1)})
  estclass=reactive({MirtToEstimatedClassical(tempmirt1()
                                              ,theta=thetas()
                                              ,qwts=qwts())})
  targeis=reactive({sapply(1:nites(),
                           function(i) expected.item(extract.item(
                             tempmirt1(),i),Theta=input$targthetaei)
                           )})
  
  targeis2=reactive({sapply(1:nites(),
                           function(i) expected.item(extract.item(
                             tempmirt1(),i),Theta=input$targthetaei2)
  )})

    filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  idata1=reactive({
    tempi1=estclass()
    tempi1$mean.at.chosen.ability=targeis()
    tempi1$mean.at.chosen.ability2=targeis2()
    if(!is.null(filedata())){tempi1=merge(tempi1,filedata())}
    tempi1$itecount=1
    tempi1
  })
  idata1a=reactive({
    auc=sapply(1:extract.mirt(tempmirt1(),"nitems"),
               function(i) areainfo(tempmirt1()
                                    ,c(input$abilmin,input$abilmax)
                                    ,which.items = i)$Info)
    auc=data.frame(Item=estclass()$Item,auc=auc)  
    merge(idata1(),auc)
  })

  
  numcols=reactive({unlist(lapply(idata1(), is.numeric))})
  output$idata1=renderTable({idata1a()})

  output$constr_selections <- renderUI({
    buttons1=list()
    if(input$nconstr>0){buttons1 <- as.list(1:input$nconstr)}
    buttons1 <- lapply(buttons1, function(i){
      fluidRow(
        column(4,selectInput(paste0("constrvars",i), "Constraint variable",names(idata1())[numcols()]))
        ,column(2,numericInput(paste0("constrmins",i), label = "Min", value=0))
        ,column(2,numericInput(paste0("constrmaxs",i), label = "Max", value=50))
      )
    }
    )
  })
  constrvars=reactive({
    temp1=NULL
    if(input$nconstr>0){
    for(iz in 1:input$nconstr){temp1=c(temp1,input[[paste0("constrvars",iz)]])}
    }
    temp1
  })
  constrmins=reactive({
    temp1=NULL
    if(input$nconstr>0){
      for(iz in 1:input$nconstr){temp1=c(temp1,input[[paste0("constrmins",iz)]])}
    }
    temp1
  })
  constrmaxs=reactive({
    temp1=NULL
    if(input$nconstr>0){
      for(iz in 1:input$nconstr){temp1=c(temp1,input[[paste0("constrmaxs",iz)]])}
    }
    temp1
  })
  output$constr=renderTable({data.frame(a=constrvars(),b=constrmins(),c=constrmaxs())})
    
    observeEvent(input$startopt, {
      revals$optsel=tryCatch(ChooseItems(idata1a(),"auc"
                                ,constrvars()
                                ,constrmins()
                                ,constrmaxs())
                    ,error=function(e) NULL)
    
      for (iz in 1:nites()){
        updateNumericInput(session, paste0("item_num",iz),  value = 0)
        if(estclass()$Item[iz]%in%idata1a()$Item[revals$optsel]){updateNumericInput(session, paste0("item_num",iz),  value = 1)}
      }
    })

    idata1aopt=reactive({temp1=NULL
      if(!is.null(revals$optsel)){temp1=idata1a()[revals$optsel,]}
      temp1
      })
    idata1aoptsum=reactive({temp1=NULL
      if(!is.null(revals$optsel)){temp1=apply(idata1aopt()[,c(constrvars(),"auc")],2,sum)}
      temp1
      })
    output$idata1aopt=renderTable({idata1aopt()})
    output$idata1aoptsum=renderTable({
      temp1=NULL
      if(!is.null(revals$optsel)){
      temp1=idata1aopt()[1,c(constrvars(),"auc")]
      temp1[1,]=idata1aoptsum()
      }
      temp1})

  output$item_selections <- renderUI({
    buttons <- as.list(1:nites())
    buttons <- lapply(buttons, function(i){
      wellPanel(splitLayout(cellWidths = c("75%", "25%"),cellArgs = list(style = "padding: 1px")
        ,div(style= "font-size:80%",renderTable({
          estclass()[i,c("Item","Max","Facility","R_abil")]}
                            ,width=160,spacing="xs"))
        ,div(style= "font-size:80%",numericInput(paste0("item_num",i)
                      , label = "Include", value=1,min=0,width='60px'))
      ))
    }
    )
  })
  
  observeEvent(input$AllZero, {
    for (iz in 1:nites()){
    updateNumericInput(session, paste0("item_num",iz),  value = 0)
    }
  })

  observeEvent(input$AllOne, {
    for (iz in 1:nites()){
      updateNumericInput(session, paste0("item_num",iz),  value = 1)
    }
  })
  
  itesels=reactive({
    temp1=NULL
    for(iz in 1:nites()){temp1=c(temp1,input[[paste0("item_num",iz)]])}
    temp1
  })
  
  output$vals=renderTable({itesels()})
  output$estclass=renderTable({estclass()})
  output$coefs=renderTable({coefs()},rownames = TRUE)

  which.items=reactive({rep(1:nites(),times=itesels())})
  output$which.items=renderText({which.items()})
  
  dist1=reactive({
    tempm1=sum(thetas()*qwts())
    tempsd1=sqrt(sum(thetas()*thetas()*qwts())-tempm1^2)
    ScoreDistFromMirt(tempmirt1(),which.items=which.items()
                                    ,theta.mean.sd=c(tempm1,tempsd1))})
  output$totalscoredistplot<-renderPlot({
    ggplot(data=dist1(),aes(x=score,y=prob))+
          geom_bar(stat="identity")+labs(x="raw.score",y="proportion")
    })

  et=reactive({tempet=0*thetas()
    if(sum(itesels())>0){
      tempet=expected.test(tempmirt1(),Theta=thetas(),which.items = which.items())
      }
    tempet
    })
  
  output$tcc=renderPlot({
    ggplot(data.frame(Ability=thetas(),Expected.Score=et())
           ,aes(x=Ability,y=Expected.Score))+geom_line()+
      coord_cartesian(ylim=c(0,max(dist1()$score)),xlim=c(-3,3))
    })
  
  output$totalscoredisttable<-renderTable({
    
    n.items=sum(itesels())
    var1=sum((dist1()$score^2)*dist1()$prob)-sum(dist1()$score*dist1()$prob)^2
    itevarsums=sum(estclass()$SD[which.items()]^2)
    cronbach.alpha=(n.items/(n.items-1))*(1-(itevarsums/var1))
    
    vet=sum(et()*et()*qwts())-sum(et()*qwts())^2
    truereliability=vet/var1
    
    data.frame(n.items=sum(itesels())
               ,test.max=max(dist1()$score)
               ,test.mean=sum(dist1()$score*dist1()$prob)
               ,test.sd=sqrt(var1),cronbach.alpha=cronbach.alpha
             ,true.reliability=truereliability)
    
  })
  
  output$scoredisttab2<-renderTable({
    disttable=dist1()
    TCCstuff=TCClookup(tempmirt1(),which.items = which.items())
    disttable$TCC_theta=TCCstuff$TCCabil
    disttable=disttable[order(-disttable$score),]
    disttable$predicted_percent=100*disttable$prob
    disttable$predicted_cum_percent=cumsum(disttable$predicted_percent)
    disttable$expected_theta=disttable$expectedtheta
    disttable$sd_theta=disttable$sdtheta
    disttable=disttable[,c("score","predicted_percent"
                           ,"predicted_cum_percent"
                           ,"expected_theta","sd_theta","TCC_theta")]
    disttable
  })
  
  
  iteminfs=reactive({t(simplify2array(lapply(1:nites(),function(i) iteminfo(extract.item(tempmirt1(),i),thetas()))))})
  
  output$totalinfoplot<-renderPlot({
    
    if(sum(itesels())==1){totinf=iteminfs()[which.items(),]}
    if(sum(itesels())!=1){totinf=colSums(iteminfs()[which.items(),])}
    
    ggplot(data=data.frame(x=thetas(),y=totinf),aes(x=x,y=y))+
      geom_line()+labs(x="Ability",y="Total test information")
  })
  
#code to close app
 session$onSessionEnded(function() {
   stopApp()
 })

 
  }

# Run the application 
shinyApp(ui = ui, server = server)

