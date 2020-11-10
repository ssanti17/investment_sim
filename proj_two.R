# ===============================================
# Fill in the following fields
# ===============================================
# Title: Investment Simulator
# Description: Shiny app for investment simuation
# Author: Sarah Santiago
# Date:


# ===============================================
# Required packages
# ===============================================
library(dplyr)
library(ggplot2)
library(reshape2)
# ...



# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  #Application Title
  titlePanel("Investment Simulator"),
  
  hr(),
  
  #description of shiny app
  fluidPage(
    "This app simulates investing in a Total Stock Market index fund. 
    Users are able to adjust simulations based on factors such as: the initial amount inputted, the number of years, the amount of periodic contributions,
    whether contributions are made at the end of the year or end of the month, average annual return, average annual volatility, number of simulations, and the random seed value.
    A summary of percentiles for the value at the end of the specified time period is also included."
  ),
  
  hr(),
  
  fluidRow(
    h4("Input Values"),
    # Inputs for initial amount, and number of years
    column(3,
           sliderInput('initial',
                       "Initial Amount", 
                       min = 0, 
                       max = 10000,
                       value = 1000,
                       step = 100),
           
           numericInput('years', 
                        "Number of Years",
                        value = 10)
    ),
    
    # Inputs for periodic contributions
    column(3,
           sliderInput('amt_contribution',
                       "Amount of Periodic Contribution", 
                       min = 0, 
                       max = 5000,
                       value = 360,
                       step = 5), 
           
           radioButtons('end_of_year',
                        "Type of Periodic Contribution", 
                        choices = list('End of year' = TRUE, "End of month" = FALSE),
                        selected = TRUE)
    ),
    
    # Inputs for Avg annual return, and avg annual volatility
    column(3,
           sliderInput('avg_return',
                       "Average Annual Rate of Return", 
                       min = 0, 
                       max = .30,
                       value = .1,
                       step = .01),
           
           sliderInput('avg_volatility',
                       "Average Annual Volatility", 
                       min = 0, 
                       max = .30,
                       value = .18,
                       step = .01)
    ),
    
    # Inputs for number of simulations, and random seed
    column(3,
           sliderInput('simulations',
                       "Number of Simulations", 
                       min = 1, 
                       max = 1000,
                       value = 50,
                       step = 1),
           
           numericInput('seed',
                        "Random Seed Value", 
                        value = 12345)
    )
  ),
  fluidRow(
    hr(),
    h4('Timeline'),
    plotOutput('plot'),
    
    hr(),
    h4('Summary Statistics'),
    "The table below displays the quantiles of the balance amounts at the end of the given investing period. By adjusting widgets above, you can see how this table shifts based on given inputs.",
    verbatimTextOutput('table')
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {

  #run single simulation
  simulation <- function(initial_amount = 1000, contribution = 360, years = 10 , avg_return = .1 , avg_volatility = .18, end_of_year = TRUE, seed = 12345) {
    
    
    simulations <- c(initial_amount)
    
    for (year in 1 : years) {
      
      #calculate based on contribution made at end of year
      if (end_of_year == TRUE){
        
        if (year == 1) {
          simulations <- c(simulations, initial_amount * (1 + rnorm(1, avg_return, avg_volatility)) + contribution)
        }
        
        else {
          simulations <- c(simulations, tail(simulations, n = 1) * (1 + rnorm(1, avg_return, avg_volatility)) + contribution)
        }
      }
      
      #calculate based on contribution made at end of month 
      else if (end_of_year == FALSE){
        if (year == 1) {
          simulations <- c(simulations, initial_amount * (1 + rnorm(1, avg_return, avg_volatility) / 12) + contribution)
        }
        
        else {
          simulations <- c(simulations, tail(simulations, n = 1) * (1 + rnorm(1, avg_return, avg_volatility) / 12) + contribution)
        }  
      }
    }              
    return (simulations)
  }
  
  #run multiple simulations and make them into singular dataframe
  
  run_simulations <- function(initial_amount = 1000, contribution = 360, 
                              years = 10 , avg_return = .1 , avg_volatility = .18, 
                              end_of_year = TRUE, num_simulations = 50, seed = 12345) {
    
    set.seed(seed)
    
    sims <- c()
    value <- c()
    
    for (sim in 1 : num_simulations) {
      
      sims <- c(sims, rep(paste0("sim", sim), years + 1))
      
      value <- c(value, simulation(initial_amount, contribution, years, avg_return, avg_volatility, end_of_year))
      
    }
    
    data <- data.frame(sims, value, years = rep(0 : years, num_simulations))
    
    return (data)
    
  }
  
  # code for graph
  output$plot <- renderPlot({
    
    #calculate the median values of each year
    median <- group_by(run_simulations(initial_amount = input$initial, 
                                       years = input$years, avg_return = input$avg_return,
                                       avg_volatility = input$avg_volatility,
                                       end_of_year = input$end_of_year, num_simulations = input$simulations,
                                       contribution = input$amt_contribution, seed = input$seed), years) %>%
              summarize(median = median(value))
    
    #calculate the 25th percentile of each year
    first <- run_simulations(initial_amount = input$initial, 
                             years = input$years, avg_return = input$avg_return,
                             avg_volatility = input$avg_volatility,
                             end_of_year = input$end_of_year, num_simulations = input$simulations,
                             contribution = input$amt_contribution, seed = input$seed) %>%
              group_by(years) %>%
              summarize(first_quartile = quantile(value, probs = .25))
    
    #calculate the 75th percentile of each year
    third <- run_simulations(initial_amount = input$initial, 
                             years = input$years, avg_return = input$avg_return,
                             avg_volatility = input$avg_volatility,
                             end_of_year = input$end_of_year, num_simulations = input$simulations,
                             contribution = input$amt_contribution, seed = input$seed) %>%
            group_by(years) %>%
            summarize(third_quartile = quantile(value, probs = .75))
    
    
    #plot function
    ggplot() + 
      geom_line(run_simulations(initial_amount = input$initial, 
                                years = input$years, avg_return = input$avg_return,
                                avg_volatility = input$avg_volatility,
                                end_of_year = input$end_of_year, num_simulations = input$simulations,
                                contribution = input$amt_contribution, seed = input$seed),
                mapping = aes(x = years, y = value, group = sims), show.legend = FALSE, alpha = .15, color = "grey3") + 
      
      geom_line(median, mapping = aes(x = years, y = median, color = "50th"), show.legend = TRUE, size = 1.4) + 
      
      geom_line(first, mapping = aes(x = years, y = first_quartile, color = "25th"), show.legend = TRUE, size = 1.4) + 
      
      geom_line(third, mapping = aes(x = years, y = third_quartile, color = "75th"), show.legend = TRUE, size = 1.4) + 
      
      labs(title = "Investment Simulations:", y = "Amount ($)", x = "Years",
           caption = "Plot of every simulation based on given inputs. Blue line represents the 75th percentile of \n simulations, green is the 50th percentile, and red is the 25%.",
           color = "Percentiles", subtitle = "With their average balance (green)")
  
  
    })
  
  # code for statistics
  output$table <- renderPrint({
    
    data.frame(end_amount = quantile(filter(run_simulations(initial_amount = input$initial, 
                                                        years = input$years, avg_return = input$avg_return,
                                                        avg_volatility = input$avg_volatility,
                                                        end_of_year = input$end_of_year, num_simulations = input$simulations,
                                                        contribution = input$amt_contribution), years == input$years)[['value']], 
                                                        seq(.1, by = .1, 1)))
 
  })
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

