#App to compare two mirt objects
#in terms of item parameters and expected scores on common items

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
  tempmirt2=reactive({get(input$sel2)})
  
  coefs1=reactive({MirtTidyCoef(tempmirt1())})
  coefs2=reactive({MirtTidyCoef(tempmirt2())})

  output$commoncoeftable1<-renderTable({merge(coefs1(),coefs2(),by="Item")},rownames=TRUE)
  output$coeftable1<-renderTable({coefs1()},rownames=TRUE)
  output$coeftable2<-renderTable({coefs2()},rownames=TRUE)

  output$eplot<-renderPlot({ExpectedScoreCompare(tempmirt1(),tempmirt2(),lab1=input$sel1,lab2=input$sel2)})
  output$iteplot<-renderPlot({ItemParameterCompare(tempmirt1(),tempmirt2(),compare=input$comparesel)})

#code to close app
 session$onSessionEnded(function() {
   stopApp()
 })

}

###ACTUAL OUTPUT
ui <- fluidPage(
  # Application title
  titlePanel("Comparing IRT objects"),
  selectInput("sel1", "Select first IRT object",loadofmirts),
  selectInput("sel2", "Select second IRT object",loadofmirts),
  navlistPanel(
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
)
)

# Run the application 
shinyApp(ui = ui, server = server)



