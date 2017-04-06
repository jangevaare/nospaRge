# Assumes 4% moisture content of grain

library("shiny")
library("ggplot2")

ui = fluidPage(
	titlePanel("nospaRge: brewhouse efficiency calculator"),
	sidebarLayout(
		sidebarPanel(
			numericInput("batchsize",
						"Batch Size (gallons)",
						min = 1,
						max = 100,
						value = 10,
						step = 0.25),
			numericInput("deadloss1a",
						"Cool wort loss from boil to fermentation (gallons)",
						min = 0,
						max = 10,
						value = 0,
						step = 0.125),
			numericInput("deadloss1b",
						"Hot wort loss from boil to fermentation (gallons)",
						min = 0,
						max = 5,
						value = 0,
						step = 0.125),
			numericInput("boiloff",
						"Boil off (gallons)",
						min = 0,
						max = 5,
						value = 1,
						step = 0.125),
			numericInput("deadloss2",
						"Equipment loss from mash to boil (gallons)",
						min = 0,
						max = 5,
						value = 0,
						step = 0.125),
			numericInput("retention",
						"Apparent grain absorption (gallons/pound)",
						min = 0.05,
						max = 0.15,
						value = 0.100,
						step = 0.01),					
			numericInput("converteff",
						"Conversion efficiency (%)",
						min = 10,
						max = 100,
						value = 95,
						step = 0.5),
			numericInput("ppg",
						"Grain potential (ppg)",
						min = 20,
						max = 40,
						value = 36,
						step = 0.5)	
			),	
		mainPanel(
			plotOutput("effPlot", height="400px", width="600px"),
			br(),
			p("You can approximate your brewhouse efficiency with the following linear equation:"),
			h4(textOutput("equation"))
			)
		)
	)

server = function(input, output){
	brewcalc = function(){
		t100density = 0.960
		grainmoisture = 0.04
		grain = seq(floor(0.75*input$batchsize), ceiling(3*input$batchsize))
		drygrain = grain * (1-grainmoisture)
		postboilvol = ((input$batchsize + input$deadloss1a) / t100density) + input$deadloss1b
		preboilvol = postboilvol + (input$boiloff * t100density)
		totalliquid = (preboilvol + input$deadloss2 + (input$retention*grain))*t100density + (drygrain*input$ppg*0.00160054653980182) + grain*grainmoisture/8.344
		lauterefficiency = (preboilvol * t100density) / totalliquid
		mashefficiency = input$converteff * lauterefficiency
		brewhouseefficiency = mashefficiency * (input$batchsize / (postboilvol * 0.960))
		data.frame(Grain=grain, Efficiency=brewhouseefficiency)
	}
	brewx = reactive(brewcalc())
	brewlm = reactive(lm(Efficiency~Grain, data=brewx()))
	output$effPlot = renderPlot({
		ggplot(brewx(), aes(x=Grain, y=Efficiency)) + 
			geom_line(size=3) + 
			labs(x="Total grain bill (lbs)", y="Brewhouse efficiency (%)") +
			scale_y_continuous(limits = c(40, 100))
	})
	output$equation = renderText({
		paste("Efficiency = (", round(brewlm()$coef[2], 4), " x Grain) + ", round(brewlm()$coef[1], 4))
	})

}

shinyApp(ui, server)