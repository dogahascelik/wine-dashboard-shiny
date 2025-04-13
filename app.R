library(shiny)
library(shinydashboard)
library(tidyverse)

load("WineDataset_Cleaned.RData")

ui <- dashboardPage(
  skin = "red",
  
  dashboardHeader(title = span("Find Your Wine", style = "color:white; font-weight:bold")),
  
  dashboardSidebar(
    style = "background-color:#8B0000; color:white;",
    sidebarMenu(
      menuItem("Wine Explorer", tabName = "explore", icon = icon("search")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      
      selectInput("region", "Region:", choices = c("All", unique(wine_data$Region))),
      selectInput("grape", "Grape Type:", choices = c("All", unique(wine_data$Grape))),
      selectInput("country", "Country:", choices = c("All", unique(wine_data$Country))),
      selectInput("style", "Style:", choices = c("All", unique(wine_data$Style))),
      selectInput("vintage", "Vintage (Year):", choices = c("All", sort(unique(wine_data$Vintage)))),
      selectInput("capacity", "Capacity:", choices = c("All", unique(wine_data$Capacity))),
      selectInput("type", "Wine Type:", choices = c("All", unique(wine_data$Type))),
      
      sliderInput("price", "Price (£):",
                  min = floor(min(wine_data$Price, na.rm = TRUE)), 
                  max = ceiling(max(wine_data$Price, na.rm = TRUE)), 
                  value = c(5, 50)),
      
      sliderInput("abv", "ABV (%):",
                  min = floor(min(wine_data$ABV, na.rm = TRUE)), 
                  max = ceiling(max(wine_data$ABV, na.rm = TRUE)), 
                  value = c(8, 15))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body {
          background-color: #FDF5F5;
        }
        .box {
          border-top: 3px solid #B22222 !important;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "explore",
              fluidRow(
                box(title = "Wine Types", width = 6, plotOutput("typePlot")),
                box(title = "Top Grape Types", width = 6, plotOutput("grapePie"))
              ),
              fluidRow(
                box(title = "Matching Wines", width = 12, dataTableOutput("wineTable"))
              ),
              fluidRow(
                box(title = "🎯 Our Top Pick for You", width = 12, solidHeader = TRUE, status = "success",
                    uiOutput("recommendation"))
              )
      ),
      
      tabItem(tabName = "analysis",
              h3("Go further in the analysis:"),
              tags$ul(
                tags$li("What is the most expensive grape types"),
                tags$li("What is the region that makes most expensive wines"),
                tags$li("Is there a relation between vintage & price"),
                tags$li("Does closure affect the price of the wine?"),
                tags$li("What is the most common grape type")
              ),
              fluidRow(
                box(title = "Top 5 Most Expensive Grapes", width = 6, tableOutput("expensiveGrapes")),
                box(title = "Top 5 Most Expensive Regions", width = 6, tableOutput("expensiveRegions"))
              ),
              fluidRow(
                box(title = "Vintage vs Price", width = 6, plotOutput("vintagePlot")),
                box(title = "Closure Type vs Price", width = 6, plotOutput("closurePlot"))
              ),
              fluidRow(
                box(title = "Most Common Grape Type", width = 12, verbatimTextOutput("mostCommonGrape"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df <- wine_data
    
    if (input$region != "All") df <- df[df$Region == input$region, ]
    if (input$grape != "All") df <- df[df$Grape == input$grape, ]
    if (input$country != "All") df <- df[df$Country == input$country, ]
    if (input$style != "All") df <- df[df$Style == input$style, ]
    if (input$vintage != "All") df <- df[df$Vintage == input$vintage, ]
    if (input$capacity != "All") df <- df[df$Capacity == input$capacity, ]
    if (input$type != "All") df <- df[df$Type == input$type, ]
    
    df <- df[df$Price >= input$price[1] & df$Price <= input$price[2], ]
    df <- df[df$ABV >= input$abv[1] & df$ABV <= input$abv[2], ]
    
    df
  })
  
  output$typePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Type)) +
      geom_bar(fill = "#B22222") +
      labs(title = "Wine Types", x = "Type", y = "Count") +
      theme_minimal() +
      theme(plot.title = element_text(color = "#8B0000", face = "bold"))
  })
  
  output$grapePie <- renderPlot({
    df <- filtered_data() %>%
      group_by(Grape) %>%
      summarise(count = n()) %>%
      slice_max(order_by = count, n = 5)
    
    ggplot(df, aes(x = "", y = count, fill = Grape)) +
      geom_col(width = 1, color = "white") +
      coord_polar("y") +
      theme_void() +
      scale_fill_brewer(palette = "Reds") +
      labs(title = "Top Grape Types") +
      theme(plot.title = element_text(color = "#8B0000", face = "bold"))
  })
  
  output$wineTable <- renderDataTable({
    filtered_data() %>%
      select(Title, Grape, Region, Country, Price, ABV, Type)
  })
  
  output$recommendation <- renderUI({
    wine <- filtered_data() %>%
      arrange(Price) %>%
      slice(1)
    
    if (nrow(wine) == 0) {
      return(HTML("<strong>No wine matches your criteria.</strong>"))
    } else {
      HTML(paste0(
        "<div style='padding:10px; background-color:#FAEAEA; border-left:6px solid #B22222;'>",
        "<h4 style='color:#8B0000;'>", wine$Title, "</h4>",
        "<p><strong>Grape:</strong> ", wine$Grape, "<br>",
        "<strong>Region:</strong> ", wine$Region, "<br>",
        "<strong>Country:</strong> ", wine$Country, "<br>",
        "<strong>Type:</strong> ", wine$Type, "<br>",
        "<strong>ABV:</strong> ", wine$ABV, "%<br>",
        "<strong>Price:</strong> £", wine$Price, "</p>",
        "</div>"
      ))
    }
  })
  
  # Analysis Outputs
  output$expensiveGrapes <- renderTable({
    wine_data %>%
      group_by(Grape) %>%
      summarise(avg_price = mean(Price, na.rm = TRUE)) %>%
      arrange(desc(avg_price)) %>%
      slice_head(n = 5)
  })
  
  output$expensiveRegions <- renderTable({
    wine_data %>%
      group_by(Region) %>%
      summarise(avg_price = mean(Price, na.rm = TRUE)) %>%
      arrange(desc(avg_price)) %>%
      slice_head(n = 5)
  })
  
  output$vintagePlot <- renderPlot({
    ggplot(wine_data, aes(x = as.numeric(Vintage), y = Price)) +
      geom_point(color = "#8B0000", alpha = 0.5) +
      geom_smooth(method = "lm", color = "black") +
      labs(x = "Vintage", y = "Price (£)", title = "Vintage vs Price")
  })
  
  output$closurePlot <- renderPlot({
    ggplot(wine_data, aes(x = Closure, y = Price)) +
      geom_boxplot(fill = "#B22222", alpha = 0.7) +
      labs(x = "Closure Type", y = "Price (£)", title = "Closure vs Price") +
      theme_minimal()
  })
  
  output$mostCommonGrape <- renderText({
    most_common <- wine_data %>%
      count(Grape, sort = TRUE) %>%
      slice(1)
    paste("The most common grape type is:", most_common$Grape, "with", most_common$n, "wines.")
  })
}

shinyApp(ui, server)

