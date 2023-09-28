#VERSION THAT ALLOWS INTERACTIVE DATA UPLOAD
#AND CALCULATION OF CLASSICAL STATISTICS

#Global statements
library(shiny)
library(unimirt)
library(ggplot2)
library(reshape2)

#Guttman L4 functions
#Guttman L4 functions
#Guttman L4 functions
MaxSplitHalf<-function(data,xal){
  #data - matrix of items scores (row=candidates,column=items)
  #xal - vector of 0s and 1s specifying initial split
  #data=split_ft
  #  xal=c(1,rep(0:1,21))
  nite<-ncol(data)
  xal=rep(0,nite)
  cov1<-cov(data)
  v<-diag(cov1)
  yal<-1-xal
  ones<-rep(1,nite)
  covxy<-t(xal)%*%cov1%*%yal
  #Code to examine all possible swaps
  maxchg1=9
  while(maxchg1>0){
    #Calculate change for swapping items in X and Y;
    #This is equal to 2covxiyj+covxix+covyyj-vx-vy-covxiy-covxyj;
    covxiyj<-cov1
    covxix<-(cov1%*%xal)%*%t(ones)
    covyyj<-ones%*%(yal%*%cov1)
    vx<-v%*%t(ones)
    vy<-t(vx)
    covxiy<-(cov1%*%yal)%*%t(ones)
    covxyj<-ones%*%(xal%*%cov1)
    result<-2*covxiyj+covxix+covyyj-vx-vy-covxiy-covxyj
    for (i in 1:nite){for (j in 1:nite){if (xal[i]==xal[j]){result[i,j]=0}}}
    for (i in 1:nite){if(xal[i]==0){result[i,]=0}}
    for (i in 1:nite){if(xal[i]==1){result[,i]=0}}
    #Add bits for swapping with no other item
    result<-cbind(result,as.vector(cov1%*%xal-cov1%*%yal-v)*xal)
    result<-rbind(result,c(as.vector(cov1%*%yal-cov1%*%xal-v)*yal,0))
    #find indices of maximum change;
    maxchg=0
    maxchgx=0
    maxchgy=0
    which1=which(result==max(result),arr.ind=TRUE)[1,]
    if (result[which1[1],which1[2]]>0){maxchgx=which1[1]
    maxchgy=which1[2]
    maxchg=result[which1[1],which1[2]]}
    maxchg1<-maxchg
    if (maxchgx>0 & maxchgx<(nite+1)) {xal[maxchgx]=0
    yal[maxchgx]=1}
    if (maxchgy>0 & maxchgy<(nite+1)) {xal[maxchgy]=1
    yal[maxchgy]=0}
    covxy<-t(xal)%*%cov1%*%yal
    #print(xal)
    #print(maxchg)
  }
  guttman<-4*covxy/sum(cov1)
  pites<-sum(xal)/nite
  raju<-covxy/(sum(cov1)*pites*(1-pites))
  v1<-t(xal)%*%cov1%*%xal
  v2<-t(yal)%*%cov1%*%yal
  feldt<-4*covxy/(sum(cov1)-((v1-v2)/sqrt(sum(cov1)))**2);
  res<-list(guttman=as.vector(guttman),
            raju=as.vector(raju),
            feldt=as.vector(feldt),
            xal=xal)
  return(res)}
MaxSplitHalfHad12<-function(data){
  #data - matrix of items scores (row=candidates,column=items)
  #start with odd vs even
  nite<-ncol(data)
  sequence<-1:nite
  xal<-(sequence%%2)
  res1<-MaxSplitHalf(data,xal)
  #now try 12 further splits based on a 12*12 Hadamard matrix
  had=matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
               1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1,
               1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0,
               1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0,
               1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1,
               1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0,
               1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0,
               1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0,
               1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1,
               1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1,
               1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1,
               1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0),ncol=12)
  for (iz in 1:12){
    nextra<-max(nite-12,0)
    resrand<-MaxSplitHalf(data,c(had[,iz],rep(0,nextra))[1:nite])
    if (resrand$guttman>res1$guttman){res1<-resrand}}
  return(res1)}
##END OF Guttman L4 functions
##END OF Guttman L4 functions
##END OF Guttman L4 functions
##END OF Guttman L4 functions


#MAIN WORK
#MAIN WORK
server <- function(input, output,session) {

  #INPUT AND STUFF
  #INPUT AND STUFF
  filedata=reactiveValues(data=NULL,scoreddata=NULL)
  infile=reactiveValues(datapath=NULL)
  observeEvent(input$datafile, {infile$datapath <- input$datafile$datapath})
  observeEvent(infile$datapath, {
    if (is.null(infile$datapath)) {
      # User has not uploaded a file yet
      filedata$data=NULL
      filedata$scoreddata=NULL
    }
    if (!is.null(infile$datapath)) {
      filedata$data=read.csv(infile$datapath)
      filedata$scoreddata=filedata$data
      filedata$scoreddata[is.na(filedata$scoreddata)]=0
    }
  })
  
  output$tt <- renderTable({
    if(is.null(filedata$data)){return(NULL)}
    head(filedata$data)
  })

  #Classical ILD
  ild <- reactive({
    if(is.null(filedata$scoreddata)){return(NULL)}
    
    tots=rowSums(filedata$scoreddata)
    rtots=as.numeric(cor(filedata$scoreddata,tots))
    discfunc=function(i){cor(filedata$scoreddata[,i],tots-filedata$scoreddata[,i])}
    rrests=sapply(1:ncol(filedata$scoreddata),discfunc)
        
    ildtab=data.frame(
      Item=names(filedata$data)
      ,Max.mark=apply(filedata$scoreddata,2,max)
      ,Mean=floor(100*apply(filedata$scoreddata,2,mean)+0.5)/100
      ,SD=floor(100*apply(filedata$scoreddata,2,sd)+0.5)/100
      ,Facility=floor(1000*(apply(filedata$scoreddata,2,mean)/apply(filedata$scoreddata,2,max))+0.5)/10
      ,PC.omit=floor(1000*apply(is.na(filedata$data),2,mean)+0.5)/10
      ,R_tot=rtots
      ,R_rest=rrests
    )
    ildtab
    })
  output$ild=renderTable({ild()})
  output$downloadild <- downloadHandler(
    filename = function() {"Classical item statistics.csv"},
    content = function(file) {
      write.csv(ild(), file, row.names = FALSE)
    })
    
  #Whole test statistics
  guttman=reactive({
    if(is.null(filedata$scoreddata)){return(NULL)}
    #MaxSplitHalfHad12(filedata$scoreddata)    
    sequence<-1:ncol(filedata$scoreddata)
    xal<-(sequence%%2)
    MaxSplitHalf(filedata$scoreddata,xal)
  })
  
  reltab=reactive({
    if(is.null(filedata$scoreddata)){return(NULL)}
    alpha=alfunc(filedata$scoreddata)
    tots=rowSums(filedata$scoreddata)
    sd1=sd(tots)
    data.frame(
      Statistic=c("Number of items"
                  ,"Maximum available score"
                  ,"Mean test score"
                  ,"SD of test scores"
                  ,"Cronbach's Alpha"
                  ,"Guttman's L4"
                  ,"Standard error of measurement (based on alpha)"
                  ,"Standard error of estimation (based on alpha)"
                  ,"Standard error of prediction (based on alpha)"
                  ,"Standard error of measurement (based on L4)"
                  ,"Standard error of estimation (based on L4)"
                  ,"Standard error of prediction (based on L4)"
      )
      ,
      Value=c(ncol(filedata$scoreddata),sum(ild()$Max.mark)
              ,floor(100*mean(tots))/100
              ,floor(100*sd(tots))/100
              ,floor(100*alpha+0.5)/100
              ,floor(100*guttman()$guttman+0.5)/100
              ,floor(100*sd1*sqrt(1-alpha)+0.5)/100
              ,floor(100*sd1*sqrt(alpha*(1-alpha))+0.5)/100
              ,floor(100*sd1*sqrt(1-alpha^2)+0.5)/100
              ,floor(100*sd1*sqrt(1-guttman()$guttman)+0.5)/100
              ,floor(100*sd1*sqrt(guttman()$guttman*(1-guttman()$guttman))+0.5)/100
              ,floor(100*sd1*sqrt(1-guttman()$guttman^2)+0.5)/100
      )
    )
  })  
  output$reltab=renderTable({reltab()})
  
  output$text1 <- renderPrint({
    if(is.null(filedata$scoreddata)){return(NULL)}
    #alfunc(filedata$scoreddata)
    paste0("The questions select for one half by Guttman's L4 were: "
           ,paste(names(filedata$scoreddata)[guttman()$xal==1],collapse=","))
  })

  output$text2 <- renderPrint({
    if(is.null(filedata$scoreddata)){return(NULL)}
    #alfunc(filedata$scoreddata)
    paste0("The questions select for the other half by Guttman's L4 were: "
           ,paste(names(filedata$scoreddata)[guttman()$xal==0],collapse=","))
  })
  
  #Score distribution
  distdata=reactive({
    if(is.null(filedata$scoreddata)){return(NULL)}
    
    tots=rowSums(filedata$scoreddata)
    max1=sum(ild()$Max.mark)
    distdata1=data.frame(Score=0:max1,
                        N=tabulate(tots+1,max1+1))
    distdata1$Per_cent=floor(1000*distdata1$N/sum(distdata1$N)+0.5)/10
    distdata1$Cum_per_cent=floor(1000*sapply(distdata1$Score
                                 ,function(i){0+mean(tots>=i)})+0.5)/10
    distdata1

      })
  output$scoredisttab=renderTable({distdata()})
  output$scoredistplot=renderPlot({
    ggplot(data=distdata(),aes(x=Score,y=N))+geom_bar(stat="identity")
  })
  
  #Item characteristic curves
  output$plotsel <- renderUI({
    selectInput("plotsel","Choose item(s)",names(filedata$scoreddata),multiple=TRUE)
  })
  output$iccplot=renderPlot({
    if(is.null(filedata$scoreddata)){return(NULL)}
    if(is.null(input$plotsel)){return(NULL)}
    tots=rowSums(filedata$scoreddata)
    #quartiles=findInterval(tots,quantile(tots,c(0.25,0.5,0.75)))
    iccngroup=as.numeric(input$iccngroup)
    qcuts=seq(1/iccngroup,1-1/iccngroup,1/iccngroup)
    quartiles=findInterval(tots,quantile(tots,qcuts))

    #ite=filedata$scoreddata[[input$plotsel]]
    #pdat=data.frame(Total.Score.Quartile=0:3
    #                ,Mean.item.score=c(
    #                  mean(ite[quartiles==0])
    #                  ,mean(ite[quartiles==1])
    #                  ,mean(ite[quartiles==2])
    #                  ,mean(ite[quartiles==3])
    #                )
    #                )
    #ggplot(data=pdat,aes(x=Total.Score.Quartile,Score
    #                     ,y=Mean.item.score))+geom_point()+geom_line()+ylim(0,NA)
    
    cdat1=filedata$scoreddata
    cdat1$tempid=1:nrow(cdat1)
    cdat1$score=tots
    cdat1$scoregroup=quartiles
    
    cdat2=reshape2::melt(cdat1,id.vars=c("tempid","score","scoregroup"))
    pdat=aggregate(cdat2[,c("value","score")]
                   ,by=list(variable=cdat2$variable,scoregroup=cdat2$scoregroup)
                   ,mean)
    names(pdat)[3:4]=c("Mean.item.score","Mean.total.score.in.group")
    pdat2=aggregate(cdat2[,c("value","score")]
                   ,by=list(variable=cdat2$variable,scoregroup=cdat2$scoregroup)
                   ,length)
    names(pdat2)[3:4]=c("N","dontcare")
    pdat=merge(pdat,pdat2)
    print(pdat)
    
    ggplot(data=pdat[pdat$variable%in%input$plotsel,],aes(x=Mean.total.score.in.group
                         ,y=Mean.item.score,col=variable,lty=variable))+
      geom_point(aes(size=N))+geom_line()+
      ylim(0,NA)+xlim(0,reltab()[2,2])+
      scale_size_continuous(limits=c(0,NA))
  })
  
  ##
  
 session$onSessionEnded(function() {
   stopApp()
 })

}

###ACTUAL OUTPUT
ui <- fluidPage(
  # Application title
  titlePanel("Classical item level statistics"),
  navlistPanel(
    tabPanel("Data upload"
             ,fluidRow("First upload a file of assessment data to analyse. 
                    The rows should relate to pupils and the columns
                      to item scores. No extraneous information 
                      (e.g. pupil IDs) should be included.
                       The analysis assumes that all of the pupils had the opportunity to answer all of the items.")
             ,br(),br()
             ,fileInput('datafile', 'CSV file of item data',
                        accept=c('text/csv'
                                 , 'text/comma-separated-values,text/plain'))
             ,fluidRow("Below is a preview of the first few rows of your data set (if uploaded).")
             ,tableOutput("tt")
             ,br(),br()
    )
    ,
    tabPanel("Classical item level statistics",
             "The table below provides some of the typical classical item level data for your test."
             ,br(),br()
             ,tableOutput("ild")
             ,downloadButton("downloadild", "Download table")
             )
    ,
    tabPanel("Whole test statistics"
             ,"The table below shows a variety of whole test statistics.
            Reliability coefficients have been calculated both using
             Cronbach's alpha and by finding the best split-half reliability (Guttman's Lambda4).
             For the purposes of speed, the 'best' split half is estimated
             starting from a split of odd and even numbered items and
             applying a greedy search algorithm from there."
             ,br(),br()
             ,tableOutput("reltab")
             ,verbatimTextOutput("text1")
             ,verbatimTextOutput("text2")
    )
    ,tabPanel("Score distribution"
              ,"The chart and table below give full details of the score distribution"
              ,plotOutput("scoredistplot")
              ,tableOutput("scoredisttab")
    )
    ,tabPanel("Item characteristic curves"
              ,"Select items to explore their item characteristic curves."
              ,br(),br()
              ,fluidRow(uiOutput("plotsel"))
              ,fluidRow(selectInput("iccngroup"
                        ,"Select number of total score groups to use"
                        ,choices=3:10
                        ,selected=4))
              ,plotOutput("iccplot")
    )
  )
)

# Run the application 
shinyApp(ui = ui, server = server)


