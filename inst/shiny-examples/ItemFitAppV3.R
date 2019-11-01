#version to work with models fitted using syntax
#also uses WLE ability estimates rather than plausible value throughout
#this can be a little slower

#Global statements
library(shiny)
library(unimirt)
library(ggplot2)
library(reshape2)
globls=ls(name=".GlobalEnv")
#print(globls)
classes=NULL
for(i in 1:length(globls)){classes=c(classes,as.vector(paste(class(get(globls[i])),collapse="") ))}
loadofmirts=ls(name=".GlobalEnv")[classes%in%c("SingleGroupClass")]
print(loadofmirts)

#MAIN WORK
#MAIN WORK
server <- function(input, output,session) {

  tempmirt1=reactive({get(input$sel1)})
  thetas=reactive({
    as.matrix(data.frame(fscores(tempmirt1(),method="WLE"))$F1)
  })
  
  anymiss=reactive({sum(is.na(GetDataFromMirt(tempmirt1())))>0})
 
  coefs=reactive({
    tempcoef=MirtTidyCoef(tempmirt1())
    if(anymiss()==FALSE){
        tempcoef=cbind(tempcoef
            ,MirtToClassical(tempmirt1())[,-1])
      }
    tempcoef
    })
  
  fits=reactive({
    
    if(sum(is.na(tempmirt1()@Data$data))>0){
      quickfits=itemfit(tempmirt1(),fit_stats=c("X2"),Theta=thetas(),mincell.X2=0)
    }
    if(sum(is.na(tempmirt1()@Data$data))==0){
      quickfits=itemfit(tempmirt1(),fit_stats=c("X2","infit"),Theta=thetas(),mincell.X2=0)
    }
    quickfits
  })
  
  output$plotsel <- renderUI({
    selectInput("plotsel","Choose item",colnames(tempmirt1()@Data$data))
  })
  
  output$plotsel2 <- renderUI({
    selectInput("plotsel2","Choose item(s)",colnames(tempmirt1()@Data$data)
                ,selected=colnames(tempmirt1()@Data$data)[1]
                ,multiple=TRUE
                )
  })
  output$plotsel3 <- renderUI({
    selectInput("plotsel3","Choose item",colnames(tempmirt1()@Data$data))
  })
  output$plotsel4 <- renderUI({
    selectInput("plotsel4","Choose item",colnames(tempmirt1()@Data$data))
  })
  
  
  plotselnum=reactive({(1:ncol(tempmirt1()@Data$data))[colnames(tempmirt1()@Data$data)==input$plotsel]})
  plotcoef=reactive({coefs()[plotselnum(),]})
  plotfits=reactive({fits()[plotselnum(),]})
  
  #IRT parameters
  output$coeftable<-renderTable({coefs()},rownames=TRUE)
  output$downloadcoef <- downloadHandler(
    filename = function() {"IRTcoefficients.csv"},
    content = function(file) {
      write.csv(coefs(), file, row.names = FALSE)
    }
  )
  
  #Item fit
  output$fittable<-renderTable({fits()},rownames=TRUE)
  output$downloadfit <- downloadHandler(
    filename = function() {"IRTfits.csv"},
    content = function(file) {
      write.csv(fits(), file, row.names = FALSE)
    }
  )
  #IRT fit plot
  output$plot1<-renderPlot({itemfit(tempmirt1(),empirical.plot=plotselnum(),Theta=thetas())})
  #IRT parameters
  output$plotcoeftable<-renderTable({plotcoef()},rownames=TRUE)
  #Item fit
  output$plotfittable<-renderTable({plotfits()},rownames=TRUE)
  #Alternative item plots
  plotselnum2=reactive({(1:ncol(tempmirt1()@Data$data))[colnames(tempmirt1()@Data$data)%in%input$plotsel2]})
  #output$plot2<-renderPlot({plot(tempmirt1()
  #                               ,which.items=plotselnum2()
  #                               ,type=input$plottype
  #                               ,facet_items=FALSE)})
  output$plot2<-renderPlot({unimirt.plot(tempmirt1()
                                 ,which.items=plotselnum2()
                                 ,type=input$plottype
                                ,thetamin=input$thetamin
                                ,thetamax=input$thetamax)})
  plotcoef2=reactive({coefs()[plotselnum2(),]})
  output$plotcoeftable2<-renderTable({plotcoef2()},rownames=TRUE)

  #Empirical ICC plots
  plotselnum3=reactive({(1:ncol(tempmirt1()@Data$data))[colnames(tempmirt1()@Data$data)==input$plotsel3]})
  
  EICC=reactive({
    tempEICC=list(modelchartdat=NULL)
    if(anymiss()==FALSE){
      tempEICC=EmpiricalICCfit(tempmirt1(),plotselnum3(),ngroups=as.numeric(input$ngroups))
    }
    tempEICC
  })
  
    output$plot3<-renderPlot({
      tempplot=ggplot()
      if(anymiss()==FALSE){
        tempplot=EICC()$plot1
      }
      tempplot
      })
  plotcoef3=reactive({coefs()[plotselnum3(),]})
  output$plotcoeftable3<-renderTable({plotcoef3()},rownames=TRUE)
  plotfits3=reactive({fits()[plotselnum3(),]})
  output$plotfittable3<-renderTable({plotfits3()},rownames=TRUE)

  output$EICCtable=renderTable({EICC()$modelchartdat})
  
  #Estimated classical parameters (useful if real ones are not available)
  estclasstable1=reactive({MirtToEstimatedClassical(tempmirt1())})
  output$estclasstable<-renderTable({estclasstable1()})
  output$downloadEstClass <- downloadHandler(
    filename = function() {"EstimatedClassicalStatistics.csv"},
    content = function(file) {
      write.csv(estclasstable1(), file, row.names = FALSE)
    }
  )
  
  
  #Ability distribution plots
  output$abilplot<-renderPlot({
    dens1=density(thetas()[,1])
    ggplot(data=data.frame(Ability=tempmirt1()@Model$Theta[,1]
                         ,density=extract.mirt(tempmirt1(),"Prior")[[1]]/
                           mean(as.numeric(stats::filter(tempmirt1()@Model$Theta[,1],c(1,-1))),na.rm=TRUE))
           ,aes(x=Ability,y=density))+geom_line()+
      geom_area(data=data.frame(Ability=dens1$x,density=dens1$y),alpha=0.5)
  })
  #Ability parameters
  output$abilpars<-renderTable({data.frame(coef(tempmirt1())$GroupPars)[1,]})

  #Score distribution across all items plot
  #code to app a score dist plot
  dist1=reactive({ScoreDistFromMirt(tempmirt1())})
  output$totalscoredistplot<-renderPlot({
      ggdist1=ggplot(data=dist1(),aes(x=score,y=prob))+
    #    geom_bar(stat="identity")+labs(x="raw.score",y="proportion")
    geom_line(col="blue",size=1.2)+labs(x="raw.score",y="proportion")
  if(anymiss()==FALSE){
    empiricaldist=data.frame(score=dist1()$score
                             ,prob=tabulate(1+rowSums(GetDataFromMirt(tempmirt1()))
                                            ,nrow(dist1()))/nrow(GetDataFromMirt(tempmirt1()))
    )
    #  ggdist1=ggdist1+geom_line(data=empiricaldist,col="blue")
    ggdist1=ggdist1+geom_bar(stat="identity",data=empiricaldist,alpha=0.5)
  }
  
  ggdist1
  })
  totalscoredisttable<-reactive({disttable=dist1()
  TCCstuff=TCClookup(tempmirt1())
  disttable$TCC_theta=TCCstuff$TCCabil
  disttable=disttable[order(-disttable$score),]
  disttable$predicted_percent=100*disttable$prob
  disttable$predicted_cum_percent=cumsum(disttable$predicted_percent)
  disttable$expected_theta=disttable$expectedtheta
  disttable$sd_theta=disttable$sdtheta
  disttable=disttable[,c("score","predicted_percent"
                         ,"predicted_cum_percent"
                         ,"expected_theta","sd_theta","TCC_theta")]
  })
  output$totalscoredisttable=renderTable(totalscoredisttable())

  output$downloadScoreDist <- downloadHandler(
    filename = function() {"TotalScoreDistribution.csv"},
    content = function(file) {
      write.csv(totalscoredisttable(), file, row.names = FALSE)
    }
  )
  
  #Item score distributions
  plotselnum4=reactive({(1:ncol(tempmirt1()@Data$data))[colnames(tempmirt1()@Data$data)==input$plotsel4]})
  idisttab=reactive({EstimatedItemDistribution(tempmirt1(),plotselnum4())})
  output$idisttab<-renderTable({idisttab()})
  output$idistplot<-renderPlot({
    idisttab2=reshape2::melt(idisttab(),id.vars=c("Item","Score")
                                                         ,variable.name="Type",value.name="Per_cent")
    ggplot(data=idisttab2,aes(x=as.factor(Score),y=Per_cent,fill=Type))+
      geom_bar(stat="identity",position="dodge")+
  	  labs(x="Item Score",Y="Per cent")+
  	  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+
  	  theme_minimal()+theme(text=element_text(size=14))
  })

  #Basic information about inputs and model fit
  output$overallinfo=renderTable({
    data.frame(N.persons=tempmirt1()@Data$N
               ,N.items=tempmirt1()@Data$nitems
               ,Fit.LogLikelihood=tempmirt1()@Fit$logLik
               ,Fit.AIC=tempmirt1()@Fit$AIC
               ,Fit.BIC=tempmirt1()@Fit$BIC)
  })
  output$itemtypesinfo=renderTable({
    itypedat=data.frame(table(extract.mirt(tempmirt1(),"itemtype")))
    names(itypedat)=c("item.type","Freq")
    itypedat
  })
  
  output$itemmaxesinfo=renderTable({
    imaxdat=data.frame(table(extract.mirt(tempmirt1(),"K")-1))
    names(imaxdat)=c("item.maximum","Freq")
    imaxdat
  })


  #Wright map plot
  difslong=reactive({
      if(input$threshtype!="Thurstonian Thresholds"){
      difslong1=melt(coefs()[,c("Item",names(coefs())[substr(names(coefs()),1,1)=="b"])]
                ,id.vars="Item",na.rm=TRUE)
      }

    if(input$threshtype=="Thurstonian Thresholds"){
      tt1=ThurstonianThresh(tempmirt1())
      names(tt1)[2:ncol(tt1)]=paste0("tt.",1:(ncol(tt1)-1))
      difslong1=melt(tt1,id.vars="Item",na.rm=TRUE)  
    }
    
    difslong1
    
      })

  output$WrightMap<-renderPlot({
	difnames=paste(difslong()$Item,"_",difslong()$variable,sep="")
	difs1=difslong()$value
	wrightp1=ggplot(data=data.frame(difs=difs1),aes(x=difs))+geom_density()
	wrightp1
	wrightp1vals=ggplot_build(wrightp1)$data[[1]][,c("x","y")]
    set.seed(293472111)
	iheights=runif(length(difs1),0,approx(wrightp1vals$x,wrightp1vals$y,difs1)$y)
    set.seed(as.numeric(Sys.time()))
	wrightp1=wrightp1+
    		geom_text(data=data.frame(x=difs1,y=iheights,label=difnames)
	                      ,aes(x=x,y=y,label=label),size=input$itetextsize)+
 	geom_point(data=data.frame(x=difs1,y=iheights),aes(x=x,y=y),alpha=0.3)+
  	geom_density(data=data.frame(Ability=thetas()),aes(x=Ability),alpha=0.2,fill="red",col="red")+
  	labs(x="Item difficulties \n(abilities in red shaded area)")
  if(input$WrightZoom){wrightp1=wrightp1+coord_cartesian(xlim=c(input$WrightMin,input$WrightMax))}
	wrightp1
	})
  
  output$WrightSelectTable<-renderTable({
    SelItems=difslong()$Item[difslong()$value>=input$plot_brush$xmin
                             & difslong()$value<=input$plot_brush$xmax]
    SelItems
    outcoef=coefs()
    if(input$threshtype=="Thurstonian Thresholds"){
      outcoef=cbind(outcoef,ThurstonianThresh(tempmirt1())[,-1])
      }
    outcoef[outcoef$Item%in%SelItems,]
  })
  
  #Item slopes plot
  
  output$ParPlot<-renderPlot({
    if(input$parametertype=="Slopes"){plot1=SlopePlot(tempmirt1())}
    if(input$parametertype=="Difficulties"){plot1=DifficultyPlot(tempmirt1())}
    plot1
  })
  #overallinfo,itemtypesinfo,itemmaxesinfo
  output$ParTable=renderTable({
    tab1=tryCatch(MirtTidyCoefSE(tempmirt1())
             ,error = function(e) MirtTidyCoef(tempmirt1()))
    if(!"Item"%in%names(tab1)){tab1$Item=rownames(tab1)}
    othcols=names(tab1)[!names(tab1)=="Item"]
    tab1=tab1[,c("Item",othcols)]
    tab1
  })
  
  #Thurstonian Thresholds
  thursttable=reactive({ThurstonianThresh(tempmirt1(), prob = input$threshprob)})
  output$thurstthreshtable=renderTable({thursttable()})
  output$downloadThurst <- downloadHandler(
    filename = function() {"ThurstoninanThresholds.csv"},
    content = function(file) {
      write.csv(thursttable(), file, row.names = FALSE)
    }
  )
  
 session$onSessionEnded(function() {
   stopApp()
 })

}

###ACTUAL OUTPUT
ui <- fluidPage(
  # Application title
  titlePanel("Basic IRT results"),
  selectInput("sel1", "Select an IRT object",loadofmirts),
  navlistPanel(
    tabPanel("Basic Information",
             "Information about the numbers of persons and items 
             included in analysis is below alongside measures of model fit.
             The number of items with each maximum number of marks
             and with each estimated item type is also displayed."
             ,br(),br()
             ,tableOutput("overallinfo")
             ,br(),br()
             ,tableOutput("itemtypesinfo")
             ,br(),br()
             ,tableOutput("itemmaxesinfo"))
    ,
    tabPanel("Coefficients",
             "The table below shows the IRT parameters
            of all items in the analysis.
            IRT parameters are prefixed by 'a', 'g' or 'b'.
             Slope parameters are prefixed with an 'a',
              difficulty parameters with a 'b' and guessing parameters are labelled 'g'.
            If an item has more than one mark then the difficulty parameters
            for successive items have the suffixes 1, 2, 3, and so on.
             If all pupils have valid scores for all items then the
             usual classical statistics will also be shown on the
             right hand side of the table below."
             ,
             tableOutput("coeftable")
             ,downloadButton("downloadcoef", "Download")
             )
        ,
        tabPanel("Item fit summary"
                 ,"The table below gives an estimate of the fit of
                 each item to the model. Fit is calculated using a
                 chi-square test comparing how expected achievement
                 on each item given ability relates to actual achievement
                 for groups of pupils with different levels of ability (estimated using WLE estimates)."
                 ,br(),br()
                 ,"Where a Rasch model has been fitted and data is available for all items
                 for all candidates, INFIT and OUTFIT statistics are also provided."
                 ,tableOutput("fittable")
                 ,downloadButton("downloadfit", "Download")
                 )
        ,
        tabPanel("Ability Fit Plots"
                 ,"The chart below gives a visual representation of item fit
                 and how the fit statistics are calculated for each item"
                 ,fluidRow(uiOutput("plotsel"))
                  ,
                fluidRow(plotOutput("plot1"))
                  ,
                fluidRow("Item parameters")
                  ,tableOutput("plotcoeftable"),
                fluidRow("Fit statistics")
                  ,tableOutput("plotfittable"))
        ,
        tabPanel("Item plots"
                 ,"Various plots relating to (groups of) items.
                 Type 'help(unimirt.plot)' in the console for more information
                 about what the various options mean."
                 ,fluidRow(uiOutput("plotsel2"))
                 ,selectInput("plottype","Plot type",c("trace","cumtrace","itemscore","infotrace","score","info","SE"))
                 ,
                 fluidRow(plotOutput("plot2"))
                 ,
                 fluidRow(column(6,numericInput("thetamin","Min Ability",-4))
                          ,column(6,numericInput("thetamax","Max Ability",4)))
                 ,
                 fluidRow("Item parameters")
                 ,tableOutput("plotcoeftable2"))
        ,
        tabPanel("Empirical ICCs",
                 fluidRow("Note: This chart is only displayed if all 
                          pupils have valid scores for all items")
                 ,
                 fluidRow(column(6,uiOutput("plotsel3"))
                          ,column(6,selectInput("ngroups","Number of empirical groups",1:30)))
                 ,
                 fluidRow("The chart below provides another way
                  of assessing model fit.
                  The line is the theoretically
                  expected relationship between total scores and  
                  individual item scores given the fitted IRT model.
                  The points show the empirical relationship with 
                  actual observed total scores. Specifically, persons
                  are split into groups based on  their total raw scores
                  and then, in each group, mean total scores are plotted
                  against mean item scores.
                  This plot is useful as the empirical relationships
                  (the points)
                  are derived without any reference to the model parameters.
                  This plot will not be displayed if there are
                  any items with missing data (i.e. not all persons taking the same items).
		To see the empirical relationship for each specific total number of marks
		set the number of empirical groups to 1.")
                 ,br(),br()
                 ,fluidRow(plotOutput("plot3"))
                 ,
                 fluidRow("Item parameters"),tableOutput("plotcoeftable3")
                 ,
                 fluidRow("Fit statistics"),tableOutput("plotfittable3")
                 ,
                 fluidRow("The theoretical relationship between total test score 
                          and item score is reproduced in tabular form below.")
                  ,tableOutput("EICCtable")
        )
        ,
        tabPanel("Estimated classical statistics",
                 "The table below attempts to re-estimate equivalents
                 of classical item statistics from the IRT parameters.
                 The statistics here may not exactly match direct estimates
                 (as opposed to these model based estimates) of the same quantities.
                 However, they may be useful in cases where different candidates have taken 
                 different sets of questions so that direct calculation of classical statistics
                 is not possible.
                 "
                 ,
                 br(),br(),
                 "The column labelled 'R_abil' is based on the percentage of the variance
                 in item scores that can be attributed to actual changes in ability.
                 It can be interpreted in a similar way to classical corrected item-total
                 correlations. Note that, like these classical statistics, it is population dependent."
                 ,
                 br(),br(),
                 "The column labelled 'R_abil_best' gives the value of
                 'R_abil' in a population with an ideal level of mean ability for this item
                 and a standard deviation equal to that found in the current population.
                 It is intended to give a measure of item quality that is slightly less
                 population dependent."
                 ,
                 br(),br()
                 ,
                 tableOutput("estclasstable")
                 ,downloadButton("downloadEstClass", "Download")
                 )
        ,
        tabPanel("Ability distributions"
                 ,"The line shows the theoretical ability distribution.
                  The shaded area shows the distribution of estimated WLE values
                 for each pupil. Very large discrepancies in shape may indicate that
                 the theoretical shape of the ability distribution is not quite correct."
                 ,br(),br()
                 ,fluidRow(plotOutput("abilplot"))
                 ,"Ability distribution parameters"
                 ,tableOutput("abilpars")
                 )
        ,
        tabPanel("Wright Map"
                 ,"The black line shows the (smoothed) distribution of item difficulties (or Thurstonian Thresholds).
                  The red shaded area shows the distribution of estimated abilities (WLE estimates). 
                This allows a assessment of how the general difficulty of items relates to the
                 ability of the cohort."
                 ,br(),br()
                 ,"The difficulties (or thresholds) of individual items are also shown on the chart.
                  Note that the heights of the item labels
                 are just chosen to help fill the space of the density plot.
                 The height of individual item difficulties has no particular meaning."
                 ,br(),br()
                 ,selectInput("threshtype","What should be plotted?",c("Difficulty parameters","Thurstonian Thresholds"))
                 ,br(),br()
                 ,fluidRow(plotOutput("WrightMap",brush = brushOpts(id = "plot_brush")))
                 ,br(),br()
                 ,fluidRow(column(4,checkboxInput("WrightZoom", label = "Apply custom chart min and max (zoom)", value = FALSE))
                    ,column(4,numericInput("WrightMin", label ="Chart min", value = -5))
                           ,column(4,numericInput("WrightMax", label ="Chart max", value = 5)))
                 ,fluidRow(numericInput("itetextsize", label ="Size of item labels", value = 2.5,step=0.1))
                ,br(),br()
                 ,"You can select an area on the plot and all of the
                 coeffcients for items with difficulties in the given range
                 will be displayed below. 
                 The height of the area you select does not matter."
                 ,br(),br()
                 ,fluidRow(tableOutput("WrightSelectTable"))
                 )
        ,
        tabPanel("Total score distribution"
                 ,fluidRow("The line shows the estimated total score distribution
                  across all items based upon the fitted IRT model.
                  If all items were taken by all pupils then grey bars
                  will also be shown allowing a comparison with the actual
                  score distribution.")
                 ,br(),br()
                 ,fluidRow(plotOutput("totalscoredistplot"))
                 ,br(),br()
                 ,"The table below shows the predicted percentage of students at
                 each score. based on the model. It is sorted from the highest scores to the lowest.
                  Also shown is the expected mean and 
                 standard deviation of ability at each raw score
                 based on the fitted IRT model. Finally the
                 table shows the ability estimate that corresponds
                 to each total score on the test characteristic curve."
                 ,fluidRow(tableOutput("totalscoredisttable"))
                 ,downloadButton("downloadScoreDist", "Download")
        )
    ,
        tabPanel("Item score distributions"
                 ,fluidRow(uiOutput("plotsel4"))
                 ,"The chart and table show the expected score distribution for the selected item based upon the fitted model.
                 If the selected item was taken by everyone in the data set (no missing data) then the actual score distribution is also
                 shown."
                 ,br(),br()
                 ,fluidRow(plotOutput("idistplot"))
                 ,tableOutput("idisttab")
                 )
      ,tabPanel("Comparing item parameters"
                ,fluidRow(selectInput("parametertype","Parameters to include in chart",c("Difficulties","Slopes")))
                ,fluidRow("The plot below displays
                          the selected item parameters for all items.
                          The dotted line gives the median 
                          parameter value across all items.
                          If the IRT models were estimated with
                          'SE=TRUE' then 95 per cent confidence 
                          intervals for the slope parameters will also
                          be displayed.
                          If the model was fitted using the
                          generalised partail credit model (gpcm)
                          then looking at slope parameters may reveal if items
                          are potentially being under or over rewarded.
                          ")
                ,br(),br()
                ,fluidRow(plotOutput("ParPlot",height="1500px"))
                ,fluidRow(tableOutput("ParTable")))
    ,
    tabPanel("Thurstonian thresholds",
             "The Thurstonian threshold for a score category is
             defined as the ability at which the probability of 
             achieving that score or higher reaches the user-defined
             probability (0.5 by default).
             If the selected probability=0.5,
             for items analysed using the graded response model
             or for dichotomous items analysed as part of a model
             using the (generalised) partial credit model (or Rasch)
             these will be equal to the usual difficulty parameters.
             However, Thurstonian thresholds may be a useful way of
             understanding the difficulty of marks within polytomous items
             under the (generalised) partial credit model. "
             ,
             br(),br()
             ,
             fluidRow(sliderInput("threshprob","Probability of success",min=0.05,max=0.95,value=0.5))
             ,
             br(),br()
             ,
             tableOutput("thurstthreshtable")
             ,downloadButton("downloadThurst", "Download"))
    
  ,widths=c(3,9))
)

# Run the application 
shinyApp(ui = ui, server = server)



