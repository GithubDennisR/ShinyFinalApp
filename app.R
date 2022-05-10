library(shiny)
library(fpp3)
library(ggthemes)
library(ggplot2)
library(shinydashboard)

ui <- dashboardPage( skin = "blue",
                     dashboardHeader(title = "US Gasoline Exports", titleWidth = 400),
                     dashboardSidebar( width = 200,
                                       sidebarMenu(
                                           menuItem("Instructions", tabName = "instructions",
                                                    icon = icon("chart-line")),
                                           menuItem("Full Time Series", tabName = "fulltimeseries", 
                                                    icon = icon("chart-line")),
                                           menuItem("Secondary Plots", tabName = "SecondaryPlots", 
                                                    icon = icon("chart-line")),
                                           menuItem("Other Seasonality Plots", tabName = "SeasonalityOther", 
                                                    icon = icon("chart-line")),
                                           menuItem("Simple Models", tabName = "SimpleModels", 
                                                    icon = icon("chart-line")),
                                           menuItem("Holts/Holts-Winter Models", tabName = "HoltsWinterModels", 
                                                    icon = icon("chart-line")),
                                           menuItem("ARIMA Model", tabName = "ARIMAModel", 
                                                    icon = icon("chart-line"))
                                       )
                     ),
                     dashboardBody(
                         tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
                         tabItems(
                             # First tab content
                             tabItem(tabName = "instructions",
                                     h3("Use the application by clicking on the menu triple bar 
                                icon at the top and selecting the graphs you would 
                                like to see on the left side") 
                             ),
                             
                             # Second tab content
                             tabItem(tabName = "fulltimeseries",
                                     h1("Plot of Full Time Series"),
                                     hr(),
                                     plotOutput("FullTimeSeries")       
                                     
                             ),
                             
                             # Third tab content
                             tabItem(tabName = "SecondaryPlots",
                                     selectInput(inputId = "plotChoice", label = "Choose a Plot", 
                                                 choices = c("Seasonality" = "Season", 
                                                             "Autocorrelation" = "Auto", 
                                                             "Decomposition" = "Decomp")),
                                     verbatimTextOutput("SecondaryOutput"),
                                     plotOutput("SecondaryPlotOutput")
                                     
                                     
                             ),
                             # Fourth tab content
                             tabItem(tabName = "SeasonalityOther",
                                     
                                     plotOutput("SeasonalityOtherVar"),
                                     
                                     
                                     
                             ),
                             tabItem(tabName = "SimpleModels",
                                     h1("Naive Model"),
                                     plotOutput("NaiveModel"),
                                     hr(),
                                     h1("Seasonal Naive Model"),
                                     plotOutput("SeasonNaive"),
                                     h1("Mean Model"),
                                     plotOutput("Mean"),
                                     h1("Drift Model"),
                                     plotOutput("Drift")
                                     
                                     
                             ),
                             
                             tabItem(tabName = "HoltsWinterModels",
                                     h1("Holts Model"),
                                     plotOutput("Holts"),
                                     h1("Holts-Winter Model"),
                                     plotOutput("HoltsWinter")
                                     
                                     
                                     
                             ),
                             tabItem(tabName = "ARIMAModel",
                                     h1("ARIMA Model"),
                                     h3("Model takes some time to generate"),
                                     plotOutput("ARIMA"),
                                    
                                     
                                     
                                     
                             )
                         )
                         
                         
                         
                         
                     )
                     
)  



server <- function(input, output) {
    
    output$SeasonalityOtherVar <- renderPlot({
        us_gasoline %>%
            gg_subseries(Barrels) +
            labs(
                y = "Barrels (Millions)",
                title = "US finished motor gasoline product supplied"
            )
    })
    
    output$SecondaryOutput <- renderText(
        if(input$plotChoice == "Season"){
            noquote(paste("The seasonal graph of the US gasoline production indicates that there isn't much of a seasonal trend amongnst the data but there is a clear increase in barrels produced over the time span."))
        } else if (input$plotChoice == "Auto"){
            noquote(paste("All points on the first lag being above the significance line indicates that the last month of data is reliable enough to predict the next month of data"))
        } else if (input$plotChoice == "Decomp"){
            noquote(paste("The decomposition graph shows that there is not much seasonal impact and that there is a slight impact from randomness."))
        }
    )
    output$FullTimeSeries <- renderPlot({
        USGPlot <- us_gasoline
        autoplot(USGPlot, Barrels) +
            labs(title = "US finished motor gasoline product supplied",
                 subtitle = "US Energy Information Administration",
                 y = "Barrels (Millions)")
    })
    output$SecondaryPlotOutput <- renderPlot({
        if(input$plotChoice == "Season"){
            gg_season(us_gasoline) +
                labs(title = "US finished motor gasoline product supplied",
                     subtitle = "US Energy Information Administration",
                     y = "Barrels (Millions)")
            
        } else if (input$plotChoice == "Auto") {
            us_gasoline %>%
                ACF(Barrels, lag_max = 48) %>%
                autoplot() +
                labs(title="US finished motor gasoline product supplied")
        } else if (input$plotChoice == "Decomp"){
            us_gasoline %>%
                model(
                    classical_decomposition(Barrels, type = "additive")
                ) %>%
                components() %>%
                autoplot() +
                labs(title = "Classical additive decomposition of total finished US motor gasoline")
            
        }
    })
    output$NaiveModel <- renderPlot({
    us_gasoline %>% 
        model(
            "Naive" = NAIVE(Barrels)
        ) -> MODEL
        MODEL %>%
            forecast(h = 15) -> fit
        autoplot(us_gasoline, .vars = Barrels) + autolayer(fit)
    
    })
    output$SeasonNaive <- renderPlot({
        us_gasoline %>% 
            model(
                "Seasonal Naive" = SNAIVE(Barrels)
            ) -> MODEL
        MODEL %>%
        forecast(h = 15) -> fit
        autoplot(us_gasoline, .vars = Barrels) + autolayer(fit)
    })
    output$Mean <- renderPlot({
        us_gasoline %>%
            model(
                "Mean" = MEAN(Barrels)
            ) -> MODEL
        MODEL %>%
            forecast(h = 15) -> fit
        autoplot(us_gasoline, .vars = Barrels) + autolayer(fit)
    })
    output$Drift <- renderPlot({
        us_gasoline %>%
            model(
                "Drift" = NAIVE(Barrels~drift())
            ) -> MODEL
        MODEL %>%
            forecast(h = 15) -> fit
        autoplot(us_gasoline, .vars = Barrels) + autolayer(fit)
    })
    output$HoltsWinter <- renderPlot({
        us_gasoline %>%
            model(
                "Holts-Winter" = ETS(Barrels ~ error())
            ) -> MODEL
        MODEL %>%
            forecast(h = 15) -> fit
        autoplot(us_gasoline, .vars = Barrels) + autolayer(fit)
    })
    output$Holts <- renderPlot({
        us_gasoline %>%
            model(
                "Holts" = ETS(Barrels ~ trend())
            ) -> MODEL
        MODEL %>%
            forecast(h = 15) -> fit
        autoplot(us_gasoline, .vars = Barrels) + autolayer(fit)
    })
    output$ARIMA <- renderPlot({
        us_gasoline %>%
            model(
                "Auto" = ARIMA(Barrels, stepwise = FALSE)
            ) -> MODEL
        MODEL %>%
            forecast(h = 15) -> fit
        autoplot(us_gasoline, .vars = Barrels) + autolayer(fit)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)