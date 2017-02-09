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
						"Conversion efficiency",
						min = 0.50,
						max = 1.00,
						value = 0.95,
						step = 0.01)
					
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
		ppg = 1.030
		grain = seq(floor(0.75*input$batchsize), ceiling(3*input$batchsize))
		totalsugar = (ppg - 1)*grain
		postboilvol = ((input$batchsize + input$deadloss1a) * (1.000/t100density)) + input$deadloss1b
		preboilvol = postboilvol + input$boiloff
		totalwater = preboilvol + input$deadloss2 + (input$retention * grain)
		preboilgrav = 1 + (totalsugar * input$converteff / totalwater)
		postboilgrav = 1 + (preboilgrav - 1) * preboilvol/postboilvol
		brewhouseefficiency = (postboilgrav - 1) * input$batchsize / totalsugar
		data.frame(Grain=grain, Efficiency=brewhouseefficiency*100)
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
		paste("Efficiency = ", round(brewlm()$coef[2], 4), " x Grain (lbs) + ", round(brewlm()$coef[1], 4))
	})

}

shinyApp(ui, server)