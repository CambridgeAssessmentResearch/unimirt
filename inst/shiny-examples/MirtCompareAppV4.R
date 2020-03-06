#App to compare two mirt objects
#in terms of item parameters and expected scores on common items

#Global statements
library(shiny)
library(unimirt)
library(ggplot2)
library(reshape2)
globls=ls(name=".GlobalEnv")
#print(globls)
#classes=NULL
#for(i in 1:length(globls)){classes=c(classes,as.vector(paste(class(get(globls[i])),collapse="") ))}
#loadofmirts=ls(name=".GlobalEnv")[classes%in%c("SingleGroupClass")]
#print(loadofmirts)

#MAIN WORK
#MAIN WORK
server <- function(input, output,session) {

  #KEEP TRACK OF WHETHER RECALIBRATION HAS BEEN DONE OR NOT
  recalibration=reactiveValues(state=data.frame(RecalibrationComplete="NO"))
  output$recstate=renderTable({recalibration$state})
    
  #UPLOAD OF MODEL 1
  model1=reactiveValues(model=NULL)
  observeEvent(input$model1, {
    modin=input$model1
    if(is.null(modin)){model1$model=NULL}
    if(!is.null(modin)){
      modfile <- input$model1$datapath
      e = new.env()
      load(modfile,envir=e)
      model1$model=e[["irt.model1"]]
      recalibration$state=data.frame(RecalibrationComplete="NO")    
    }
  })
  
  #UPLOAD OF MODEL 2
  model2=reactiveValues(model=NULL)
  observeEvent(input$model2, {
    modin=input$model2
    if(is.null(modin)){model2$model=NULL}
    if(!is.null(modin)){
      modfile <- input$model2$datapath
      e = new.env()
      load(modfile,envir=e)
      model2$model=e[["irt.model1"]]
      recalibration$state=data.frame(RecalibrationComplete="NO")    
    }
  })

  #ACTIONS ON PRESSING RECALIBRATION BUTTON
  observeEvent(input$recalibrate,{
    if(!is.null(model1$model) & !is.null(model2$model)){
      model2$model=MirtObjectRecalibrate(model1$model, model2$model)
      recalibration$state=data.frame(RecalibrationComplete="YES")    
    }
  })
  
  #ACTIONS ON PRESSING DOWNLOAD MODEL BUTTON
  observe({if(!is.null(tempmirt2()))  isolate(irt.model1 <<- tempmirt2())})
  output$downloadModel <- downloadHandler(
    filename <- function(){
      paste0(input$modelfilename,".RData")
    },
    content = function(file) {
      save(irt.model1, file = file)
    }
  )

  output$model1info=renderTable({
    if(is.null(model1$model)){return(NULL)}
    data.frame(N.persons=model1$model@Data$N
               ,N.items=model1$model@Data$nitems
               ,Fit.LogLikelihood=model1$model@Fit$logLik
               ,Fit.AIC=model1$model@Fit$AIC
               ,Fit.BIC=model1$model@Fit$BIC)
  })
  
  output$model2info=renderTable({
    if(is.null(model2$model)){return(NULL)}
    data.frame(N.persons=model2$model@Data$N
               ,N.items=model2$model@Data$nitems
               ,Fit.LogLikelihood=model2$model@Fit$logLik
               ,Fit.AIC=model2$model@Fit$AIC
               ,Fit.BIC=model2$model@Fit$BIC)
  })
  output$model1itemtypesinfo=renderTable({
    if(is.null(model1$model)){return(NULL)}
    itypedat=data.frame(table(extract.mirt(model1$model,"itemtype")))
    names(itypedat)=c("item.type","Freq")
    itypedat
  })
  output$model2itemtypesinfo=renderTable({
    if(is.null(model2$model)){return(NULL)}
    itypedat=data.frame(table(extract.mirt(model2$model,"itemtype")))
    names(itypedat)=c("item.type","Freq")
    itypedat
  })
  
  tempmirt1=reactive({model1$model})
  tempmirt2=reactive({model2$model})
  
  coefs1=reactive({MirtTidyCoef(tempmirt1())})
  coefs2=reactive({MirtTidyCoef(tempmirt2())})

  output$commoncoeftable1<-renderTable({merge(coefs1(),coefs2(),by="Item")},rownames=TRUE)
  output$coeftable1<-renderTable({coefs1()},rownames=TRUE)
  output$coeftable2<-renderTable({coefs2()},rownames=TRUE)

  output$eplot<-renderPlot({ExpectedScoreCompare(tempmirt1(),tempmirt2(),lab1="Model 1",lab2="Model 2")})
  output$iteplot<-renderPlot({ItemParameterCompare(tempmirt1(),tempmirt2(),compare=input$comparesel)})
  output$iteplot2<-renderPlot({
	ParComparePlot(list(tempmirt1(),tempmirt2()),c(input$sel1,input$sel2),compare=input$comparesel2)
	})
  
#code to close app
 session$onSessionEnded(function() {
   stopApp()
 })

}

###ACTUAL OUTPUT
ui <- fluidPage(
  # Application title
  titlePanel("Comparing IRT objects of the same type"),
  navlistPanel(
    tabPanel("Basic information",
             column(11,"Select IRT models to compare. They must be of the same type."),
             br(),br(),
             fileInput('model1', '.RData file with first IRT model'),
             fileInput('model2', '.RData file with second IRT model'),
             column(11,"Basic information about the two loads you have loaded is below.
                       Please check that the IRT model types match.")
             ,br(),br()
             ,column(11,"Model 1")
             ,tableOutput("model1info")
             ,tableOutput("model1itemtypesinfo")
             ,br(),br()
             ,column(11,"Model 2")
             ,tableOutput("model2info")
             ,tableOutput("model2itemtypesinfo")
             ,br(),br()
             ,actionButton("recalibrate","Ready to recalibrate model 2?")
             ,tableOutput("recstate")
             ,textInput("modelfilename", "Filename for saving recalibrated model2")
             ,downloadButton("downloadModel", "Download recalibrated model")
            ),
    tabPanel("Compare coefficients",
             column(11,"IRT parameters are prefixed by 'a', 'g' or 'b'.
             Slope parameters are prefixed with an 'a',
              difficulty parameters with a 'b' and guessing parameters are labelled 'g'.
             In the table of common items the coefficients from
             the first object have the suffix 'x' and those from the second
             one have the suffix 'y'."
             ,br(),br()
             ,"Common items",br(),br()
             ,
             tableOutput("commoncoeftable1")
             ,br(),br()
		,"First IRT object",br(),br()
		,
		tableOutput("coeftable1")
		,"Second IRT object",br(),br()
             ,
             tableOutput("coeftable2")
	))
        ,
        tabPanel("Coefficient plots",column(11
		,"The plot below compares item parameters on common items between the two selected IRT objects.
		If you select to compare item difficulties then separate marks within each item will be identified
		by the number after the suffix 'b'. Common items are automatically identified as all those with identical
		variable names between the two objects."
		,br(),br(),
                 fluidRow(selectInput("comparesel","Which item parameters do you want to compare?"
			,c("Difficulties","Slopes")))
		,br(),br(),
                fluidRow(plotOutput("iteplot"))
		))
        ,
        tabPanel("Expected score plots",column(11
		,"The plot below compares expected total scores on the common items between the two selected IRT objects
		agains the ability scale. If the objects have been calibrated to the same scale the two lines
		should be very close to one another.
		Common items are automatically identified as all those with identical
		variable names between the two objects."
                  ,br(),br()
                ,fluidRow(plotOutput("eplot"))
		))
	,
	tabPanel("Parameter comparisons",column(11
	                                       ,"The plot below
	                                       provides another method to compare
	                                       parameters values across models.
	                                       If these have been estimated with 
	                                       'SE=TRUE' then the chart includes
	                                       95 per cent confidence intervals
	                                       for the selected parameters."
	                                       ,br(),br(),
	                                       fluidRow(selectInput("comparesel2","Which item parameters do you want to compare?"
	                                                            ,c("Difficulties","Slopes","Guessing")))
	                                       ,br(),br()
	                                       ,fluidRow(plotOutput("iteplot2"))
	))
  )
)

# Run the application 
shinyApp(ui = ui, server = server)



