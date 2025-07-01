library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinythemes)

# Load cleaned CPI data frames
load("../Data/cpi_data_cleaned.RData")

# Pre-calc choices (all categories except Date)
cpi_choices <- names(rural_df)[-1]

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("India CPI Explorer"),
  tabsetPanel(
    tabPanel("Level CPI",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput(
                   inputId   = "date_range",
                   label     = "Select Date Range:",
                   start     = min(combined_df$Date),
                   end       = max(combined_df$Date),
                   format    = "yyyy-mm",
                   separator = " to "
                 ),
                 selectizeInput(
                   inputId    = "metrics",
                   label      = "Pick categories to plot:",
                   choices    = cpi_choices,
                   multiple   = TRUE,
                   options    = list(placeholder = 'Start typing to search…')
                 )
               ),
               mainPanel(
                 plotOutput("plot_rural", height = "300px"),
                 plotOutput("plot_urban", height = "300px"),
                 plotOutput("plot_combined", height = "300px")
               )
             )
    ),
    tabPanel("YoY Change",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput(
                   inputId   = "date_range_yoy",
                   label     = "Select Date Range (for YoY %):",
                   start     = min(combined_df$Date),
                   end       = max(combined_df$Date),
                   format    = "yyyy-mm",
                   separator = " to "
                 ),
                 selectizeInput(
                   inputId    = "metrics_yoy",
                   label      = "Pick categories to plot YoY % change:",
                   choices    = cpi_choices,
                   multiple   = TRUE,
                   options    = list(placeholder = 'Start typing to search…')
                 )
               ),
               mainPanel(
                 plotOutput("plot_rural_yoy", height = "300px"),
                 plotOutput("plot_urban_yoy", height = "300px"),
                 plotOutput("plot_combined_yoy", height = "300px")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  # Reactive filters for level CPI
  filter_level <- reactive({
    list(
      rural    = rural_df %>% filter(Date >= input$date_range[1], Date <= input$date_range[2]),
      urban    = urban_df %>% filter(Date >= input$date_range[1], Date <= input$date_range[2]),
      combined = combined_df %>% filter(Date >= input$date_range[1], Date <= input$date_range[2])
    )
  })
  
  # Reactive filters for YoY
  filter_yoy <- reactive({
    list(
      rural    = rural_df %>% filter(Date >= input$date_range_yoy[1] - months(12), Date <= input$date_range_yoy[2]),
      urban    = urban_df %>% filter(Date >= input$date_range_yoy[1] - months(12), Date <= input$date_range_yoy[2]),
      combined = combined_df %>% filter(Date >= input$date_range_yoy[1] - months(12), Date <= input$date_range_yoy[2])
    )
  })
  
  # Pivot helper
  pivot_fn <- function(df, metrics) {
    req(metrics)
    df %>% select(Date, all_of(metrics)) %>%
      pivot_longer(cols = -Date, names_to = "Category", values_to = "Value")
  }
  
  # Calculate YoY % change (value / lag(12) - 1) * 100
  calc_yoy <- function(df_long) {
    df_long %>%
      group_by(Category) %>%
      arrange(Date) %>%
      mutate(Value = (Value / lag(Value, 12) - 1) * 100) %>%
      ungroup() %>%
      filter(!is.na(Value))
  }
  
  # Level CPI plots
  output$plot_rural <- renderPlot({
    data <- pivot_fn(filter_level()$rural, input$metrics)
    ggplot(data, aes(Date, Value, color = Category)) +
      geom_line(size = 1) + labs(title = "Rural CPI", y = "CPI") +
      theme_minimal() + theme(legend.position = "none")
  })
  
  output$plot_urban <- renderPlot({
    data <- pivot_fn(filter_level()$urban, input$metrics)
    ggplot(data, aes(Date, Value, color = Category)) +
      geom_line(size = 1) + labs(title = "Urban CPI", y = "CPI") +
      theme_minimal() + theme(legend.position = "none")
  })
  
  output$plot_combined <- renderPlot({
    data <- pivot_fn(filter_level()$combined, input$metrics)
    ggplot(data, aes(Date, Value, color = Category)) +
      geom_line(size = 1) + labs(title = "Combined CPI", x = "Date", y = "CPI") +
      theme_minimal() + theme(legend.position = "bottom")
  })
  
  # YoY plots
  output$plot_rural_yoy <- renderPlot({
    data_long <- pivot_fn(filter_yoy()$rural, input$metrics_yoy)
    data_yoy  <- calc_yoy(data_long)
    ggplot(data_yoy, aes(Date, Value, color = Category)) +
      geom_line(size = 1) + labs(title = "Rural CPI YoY % change", y = "% Change") +
      theme_minimal() + theme(legend.position = "none")
  })
  
  output$plot_urban_yoy <- renderPlot({
    data_long <- pivot_fn(filter_yoy()$urban, input$metrics_yoy)
    data_yoy  <- calc_yoy(data_long)
    ggplot(data_yoy, aes(Date, Value, color = Category)) +
      geom_line(size = 1) + labs(title = "Urban CPI YoY % change", y = "% Change") +
      theme_minimal() + theme(legend.position = "none")
  })
  
  output$plot_combined_yoy <- renderPlot({
    data_long <- pivot_fn(filter_yoy()$combined, input$metrics_yoy)
    data_yoy  <- calc_yoy(data_long)
    ggplot(data_yoy, aes(Date, Value, color = Category)) +
      geom_line(size = 1) + labs(title = "Combined CPI YoY % change", x = "Date", y = "% Change") +
      theme_minimal() + theme(legend.position = "bottom")
  })
}

shinyApp(ui = ui, server = server)
