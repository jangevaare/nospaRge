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
			sliderInput("deadloss1a",
						"Cool wort loss from boil to fermentation (gallons)",
						min = 0,
						max = 5,
						value = 0,
						step = 0.125),
			sliderInput("deadloss1b",
						"Hot wort loss from boil to fermentation (gallons)",
						min = 0,
						max = 5,
						value = 0,
						step = 0.125),
			sliderInput("boiloff",
						"Boil off (gallons)",
						min = 0,
						max = 5,
						value = 1,
						step = 0.125),
			sliderInput("deadloss2",
						"Equipment loss from mash to boil (gallons)",
						min = 0,
						max = 5,
						value = 0,
						step = 0.125),
			sliderInput("retention",
						"Grain absorption (gallons/pound)",
						min = 0.05,
						max = 0.15,
						value = 0.100,
						step = 0.01),
			sliderInput("converteff",
						"Conversion efficiency (%)",
						min = 50,
						max = 100,
						value = 95,
						step = 1)
			),	
		mainPanel(
			plotOutput("effPlot", height="400px", width="600px")
			)
		)
	)

server = function(input, output){
	brewcalc = function(){
		grain = seq(floor(0.5*input$batchsize), ceiling(4*input$batchsize))
		shrinkage = 4
		postboilvol = ((input$batchsize + input$deadloss1a) * (100/(100-shrinkage))) + input$deadloss1b
		preboilvol = postboilvol + input$boiloff
		totalwater = preboilvol + input$deadloss2 + (input$retention * grain)
		lauterefficiency = preboilvol / totalwater
		mashefficiency = lauterefficiency*(input$converteff/100)
		brewhouseefficiency = mashefficiency * ((input$batchsize * (100/(100-shrinkage))) / postboilvol)
		data.frame(Grain=grain, Efficiency=brewhouseefficiency*100)
	}
	brewx = reactive(brewcalc())
	output$effPlot = renderPlot({
		ggplot(brewx(), aes(x=Grain, y=Efficiency)) + 
			geom_line(size=3) + 
			labs(x="Total grain bill (lbs)", y="Brewhouse efficiency (%)") +
			scale_y_continuous(limits = c(40, 100))
	})
}

shinyApp(ui, server)