
# setwd("C:\\Users\\insig\\Documents\\Projects\\Internal\\P0002_DeployRShiny\\sandbox-play")


library(shiny)
library(UpSetR)
# library(knitr)

# rmdfiles <- c("RMarkdownFile.rmd")
# sapply(rmdfiles, knit, quiet = T)

ui <- shinyUI(


    fluidPage(
	
	tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "InsightStats.css")
	),
   
    markdown("
		## A toy app to play with
		Upload some data or use the iris data by default
		"
		),
		
		sidebarPanel(
		  numericInput('n', 'Some numeric parameter', 3),
		  textAreaInput("text", "IDs", "Eeny\nMeeny\nMiny\nMoe"),
		  selectInput('select', 'Make a choice', choices=c("One", "Two")),
			downloadLink('downloadData', 'Download all matches'),
		  fileInput('file1', 'Some input file - csv',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
				width=3
		),
		
		mainPanel(
		 tabsetPanel(
		  tabPanel("Plot", plotOutput("distPlot")),
		  tabPanel("Table", tableOutput("userTable")),
		  tabPanel("UpSet", plotOutput("upsetPlot"))),
		  width=5
		)
		)
  )


server <- function(input, output) {

 output$distPlot <- renderPlot({

  inFile <- input$file1
  
  if (is.null(inFile))
      	return(plot(iris[,1:4], col=iris[,5], main="iris data"))

  dat = read.csv(inFile$datapath)
  boxplot(as.numeric(dat[,2]) ~ dat[,1])
  
	
  })
  
  
  output$upsetPlot <- renderPlot({

  inFile <- input$file1
  
  if (is.null(inFile))
      	return(boxplot(iris[,1:4], col=rainbow(4), las=2, main="iris data"))

  dat.View = read.csv(inFile$datapath)
  
  upset(dat.View, sets = colnames(dat.View)[-1], order.by = "freq", sets.bar.color = "#56B4E9", empty.intersections = "off")

	
  })
  
   output$userTable <- renderTable({
 
	inFile <- input$file1

    if (is.null(inFile))
      	return(iris[1:10,])
    
    head(read.csv(inFile$datapath))
	
  })
  
}

shinyApp(ui, server)

