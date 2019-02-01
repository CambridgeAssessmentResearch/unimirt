#Global statements
library(shiny)
library(mirt)
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
    set.seed(293472397)
    #fs1=fscores(tempmirt1(),method="plausible"
    #            ,use_dentype_estimate = TRUE
    #            )
    fs1=as.matrix(MirtUniPVs(tempmirt1(),excludefake=FALSE))
    set.seed(as.numeric(Sys.time()))
    fs1})
  anymiss=reactive({sum(is.na(GetDataFromMirt(tempmirt1())))>0})
 
  coefs=reactive({
    tempcoef=MirtTidyCoef(tempmirt1())
    if(anymiss()==FALSE){
        tempcoef=cbind(tempcoef
            ,MirtToClassical(tempmirt1())[,-1])
      }
    tempcoef
    })
  
  fits=reactive({itemfit(tempmirt1(),fit_stats="X2",Theta=thetas(),mincell.X2=0)})
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
  
  
  plotselnum=reactive({(1:ncol(tempmirt1()@Data$data))[colnames(tempmirt1()@Data$data)==input$plotsel]})
  plotcoef=reactive({coefs()[plotselnum(),]})
  plotfits=reactive({fits()[plotselnum(),]})
  
  #IRT parameters
  output$coeftable<-renderTable({coefs()},rownames=TRUE)
  #Item fit
  output$fittable<-renderTable({fits()},rownames=TRUE)
  #IRT plot
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
  output$estclasstable<-renderTable({MirtToEstimatedClassical(tempmirt1())})
  
  
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
  output$abilpars<-renderTable({coef(tempmirt1())$GroupPars})

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
  output$totalscoredisttable<-renderTable({disttable=dist1()
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
  difslong=reactive({melt(coefs()[,c("Item",names(coefs())[substr(names(coefs()),1,1)=="b"])]
                ,id.vars="Item",na.rm=TRUE)})

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
    coefs()[coefs()$Item%in%SelItems,]
  })
  
  #overallinfo,itemtypesinfo,itemmaxesinfo

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
             "Information about the numbers of person and items 
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
             tableOutput("coeftable"))
        ,
        tabPanel("Item fit summary"
                 ,"The table below gives an estimate of the fit of
                 each item to the model. Fit is calculated using a
                 chi-square test comparing how expected achievement
                 on each item given ability relates to actual achievement
                for groups of pupils with different levels of ability (estimated using plausible values)"
                 ,tableOutput("fittable"))
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
                 Type 'help(itemplot)' in the console for more information
                 about what the various options mean."
                 ,fluidRow(uiOutput("plotsel2"))
                 ,selectInput("plottype","Plot type",c("trace","itemscore","infotrace","score","info","SE"))
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
                 tableOutput("estclasstable"))
        ,
        tabPanel("Ability distributions"
                 ,"The line shows the theoretical ability distribution.
                  The shaded area shows the distribution of estimated plausible ability values
                 for each pupil. Obvious discrepancies may indicate that
                 the theoretical shape of the ability distribution is not quite correct."
                 ,br(),br()
                 ,fluidRow(plotOutput("abilplot"))
                 ,"Ability distribution parameters"
                 ,tableOutput("abilpars")
                 )
        ,
        tabPanel("Wright Map"
                 ,"The black line shows the (smoothed) distribution of item difficulties.
                  The red shaded area shows the distribution of estimated plausible ability values
                 (abilities). This allows a assessment of how the general difficulty of items relates to the
                 ability of the cohort."
                 ,br(),br()
                 ,"The difficulties of individual items are also shown on the chart.
                  Note that the heights of the item labels
                 are just chosen to help fill the space of the density plot.
                 The height of individual item difficulties has no particular meaning."
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
                 ,fluidRow(tableOutput("totalscoredisttable")))
    ,widths=c(3,9))
)

# Run the application 
shinyApp(ui = ui, server = server)



