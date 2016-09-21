library("shiny")
library("ggplot2")

ui = fluidPage(
	titlePanel("nospaRge: brewhouse efficiency calculator"),
	sidebarLayout(
		sidebarPanel(
			sliderInput("batchsize",
						"Batch Size (gallons)",
						min = 1,
						max = 50,
						value = 10,
						step = 0.25),
			sliderInput("deadloss1",
						"Equipment loss from boil to fermentation (gallons)",
						min = 0,
						max = 5,
						value = 1,
						step = 0.125),
			sliderInput("boiloff",
						"Boil off rate (gallons/hour)",
						min = 0,
						max = 5,
						value = 1,
						step = 0.125),
			sliderInput("deadloss2",
						"Equipment loss from mash to boil (gallons)",
						min = 0,
						max = 5,
						value = 1,
						step = 0.125),
			sliderInput("retention",
						"Grain absorption (gallons/pound)",
						min = 0.1,
						max = 0.15,
						value = 0.125,
						step = 0.005),
			sliderInput("extracteff",
						"Extraction efficiency (%)",
						min = 70,
						max = 100,
						value = 95,
						step = 1),
			sliderInput("ppg",
						"Sugar content of grist (ppg)",
						min = 1.030,
						max = 1.040,
						value = 1.035,
						step = 0.001)
			),	
		mainPanel(
			plotOutput("ogPlot", height="400px", width="600px"),
			plotOutput("effPlot", height="400px", width="600px")
			#dataTableOutput("brewdata")
			)
		)
	)

server = function(input, output){
	brewcalc = function(){
		grain = seq(floor(0.5*input$batchsize), ceiling(4*input$batchsize))
		shrinkage = 4
		postboilvol = (input$batchsize + input$deadloss1) * (100/(100-shrinkage))
		preboilvol = postboilvol + input$boiloff
		totalwater = preboilvol + input$deadloss2 + input$retention * grain
		preboilgrav = 1 + ((input$ppg - 1) * (input$extracteff/100) * grain / totalwater)
		OG = 1 + ((preboilgrav - 1) * preboilvol / postboilvol)
		maxOG = 1 + ((input$ppg - 1) * grain / input$batchsize)
		efficiency = (OG - 1)/(maxOG - 1) * 100
		data.frame(grain, efficiency, OG)
	}
	brewx = reactive(brewcalc())
	output$ogPlot = renderPlot({
		ggplot(brewx(), aes(x=grain, y=OG)) + 
			geom_line(size=3) + 
			labs(x="Total grain bill (lbs)", y="Original gravity") +
			scale_y_continuous(limits = c(1.010, 1.090))
	})
	output$effPlot = renderPlot({
		ggplot(brewx(), aes(x=grain, y=efficiency)) + 
			geom_line(size=3) + 
			labs(x="Total grain bill (lbs)", y="Brewhouse Efficiency") +
			scale_y_continuous(limits = c(50, 100))
	})
	#output$brewdata = renderDataTable({brewx()})
}

shinyApp(ui, server)