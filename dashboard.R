library(shiny)
library(shinydashboard)
library(tidyverse)
library(DBI)
library(RSQLite)
library(plotly)
library(DT)
library(lubridate)

# Database connection
db_path <- file.path("data", "hyperlocal_dw.sqlite")

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Hyperlocal Delivery Intelligence"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Delivery Analysis", tabName = "delivery", icon = icon("truck")),
      menuItem("Customer Sentiment", tabName = "reviews", icon = icon("comments")),
      menuItem("Raw Data", tabName = "data", icon = icon("table"))
    ),
    hr(),
    selectInput("city_filter", "Select City:", choices = NULL, multiple = TRUE),
    dateRangeInput("date_filter", "Date Range:", start = Sys.Date() - 30, end = Sys.Date()),
    hr(),
    h4("OLAP Controls"),
    radioButtons("time_level", "Time Hierarchy (Roll-up):",
      choices = c("Daily" = "order_date", "Monthly" = "year_month", "Yearly" = "year"),
      selected = "order_date"
    ),
    radioButtons("geo_level", "Geo Hierarchy (Drill-down):",
      choices = c("City Level" = "city", "Agent Level" = "agent_name"),
      selected = "city"
    )
  ),
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("total_orders"),
          valueBoxOutput("avg_rating"),
          valueBoxOutput("delay_rate")
        ),
        fluidRow(
          box(title = textOutput("timeline_title"), plotlyOutput("timeline_plot"), width = 12)
        )
      ),

      # Delivery Analysis Tab
      tabItem(
        tabName = "delivery",
        fluidRow(
          box(title = "Delivery Time by Traffic Level", plotlyOutput("traffic_plot"), width = 6),
          box(title = textOutput("sla_title"), plotlyOutput("sla_plot"), width = 6)
        ),
        fluidRow(
          box(title = textOutput("perf_title"), plotlyOutput("city_performance"), width = 12)
        )
      ),

      # Review Analytics Tab
      tabItem(
        tabName = "reviews",
        fluidRow(
          box(title = "Rating Distribution", plotlyOutput("rating_dist"), width = 6),
          box(title = "Top Review Keywords", plotlyOutput("keyword_plot"), width = 6)
        ),
        fluidRow(
          box(title = textOutput("agent_title"), plotlyOutput("agent_plot"), width = 12)
        )
      ),

      # Raw Data Tab
      tabItem(
        tabName = "data",
        fluidRow(
          box(title = "Recent Deliveries", DTOutput("delivery_table"), width = 12)
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Reactive connection to DB
  con <- dbConnect(RSQLite::SQLite(), dbname = db_path)
  onStop(function() {
    dbDisconnect(con)
  })

  # Load initial data
  deliveries <- dbGetQuery(con, "
    SELECT f.*, t.order_date, t.year, t.month, t.year_month, c.city, a.agent_name
    FROM fact_delivery f
    JOIN dim_time t ON f.time_id = t.time_id
    JOIN dim_city c ON f.city_id = c.city_id
    LEFT JOIN dim_agent a ON f.agent_id = a.agent_id
  ") %>% mutate(order_date = as.Date(order_date))

  reviews <- dbGetQuery(con, "
    SELECT r.*, t.order_date, t.year, t.month, t.year_month, c.city, a.agent_name
    FROM fact_review r
    JOIN dim_time t ON r.time_id = t.time_id
    JOIN dim_city c ON r.city_id = c.city_id
    JOIN dim_agent a ON r.agent_key = a.agent_key
  ") %>% mutate(order_date = as.Date(order_date))

  # Update UI filters
  updateSelectInput(session, "city_filter", choices = unique(deliveries$city), selected = unique(deliveries$city))

  # Dynamic Titles to confirm level changes
  output$timeline_title <- renderText({
    paste("Orders Trend (Level:", input$time_level, ")")
  })
  output$sla_title <- renderText({
    paste("SLA Compliance (Level:", input$geo_level, ")")
  })
  output$agent_title <- renderText({
    paste("Top 10 Performers (Level:", input$geo_level, ")")
  })
  output$perf_title <- renderText({
    paste("Performance Matrix (Level:", input$geo_level, ")")
  })

  # Reactive filtered data
  filtered_deliveries <- reactive({
    deliveries %>%
      filter(
        city %in% input$city_filter,
        order_date >= input$date_filter[1],
        order_date <= input$date_filter[2]
      )
  })

  filtered_reviews <- reactive({
    reviews %>%
      filter(
        city %in% input$city_filter,
        order_date >= input$date_filter[1],
        order_date <= input$date_filter[2]
      )
  })

  # Value Boxes
  output$total_orders <- renderValueBox({
    valueBox(nrow(filtered_deliveries()), "Total Deliveries", icon = icon("list"), color = "blue")
  })

  output$avg_rating <- renderValueBox({
    val <- round(mean(filtered_reviews()$rating, na.rm = TRUE), 1)
    valueBox(val, "Avg Rating", icon = icon("star"), color = "yellow")
  })

  output$delay_rate <- renderValueBox({
    rate <- round(mean(filtered_deliveries()$delayed_flag == 1) * 100, 1)
    valueBox(paste0(rate, "%"), "Delay Rate", icon = icon("clock"), color = "red")
  })

  # Plots
  output$timeline_plot <- renderPlotly({
    # Robust Time Roll-up
    df_timeline <- filtered_deliveries() %>%
      group_by(time_unit = .data[[input$time_level]]) %>%
      summarise(count = n(), .groups = "drop")

    p <- plot_ly(df_timeline,
      x = ~time_unit, y = ~count, type = "scatter", mode = "lines+markers",
      line = list(color = "steelblue"), marker = list(color = "steelblue")
    ) %>%
      layout(
        xaxis = list(title = "Time Period"), yaxis = list(title = "Orders"),
        hovermode = "closest"
      )
    p
  })

  output$traffic_plot <- renderPlotly({
    p <- filtered_deliveries() %>%
      ggplot(aes(x = traffic_level, y = delivery_time_min, fill = traffic_level)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Delivery Time by Traffic", y = "Time (min)")
    ggplotly(p)
  })

  output$sla_plot <- renderPlotly({
    # Robust Geo Drill-down
    df_plot <- filtered_deliveries() %>%
      group_by(geo_unit = .data[[input$geo_level]]) %>%
      summarise(compliance = mean(delayed_flag == 0) * 100, .groups = "drop")

    p <- df_plot %>%
      ggplot(aes(x = reorder(geo_unit, compliance), y = compliance, fill = geo_unit)) +
      geom_col() +
      coord_flip() +
      theme_minimal() +
      labs(x = "Level", y = "Compliance %") +
      theme(legend.position = "none")
    ggplotly(p)
  })

  output$rating_dist <- renderPlotly({
    p <- filtered_reviews() %>%
      ggplot(aes(x = rating)) +
      geom_histogram(bins = 5, fill = "orange", color = "white") +
      theme_minimal() +
      labs(title = "Rating Distribution")
    ggplotly(p)
  })

  output$agent_plot <- renderPlotly({
    # Robust Geo Drill-down for Top Performers
    df_plot <- filtered_reviews() %>%
      group_by(geo_unit = .data[[input$geo_level]]) %>%
      summarise(avg_rating = mean(rating, na.rm = TRUE), .groups = "drop") %>%
      top_n(10, avg_rating)

    p <- df_plot %>%
      ggplot(aes(x = reorder(geo_unit, avg_rating), y = avg_rating, fill = geo_unit)) +
      geom_col() +
      coord_flip() +
      theme_minimal() +
      labs(x = "Level", y = "Avg Rating") +
      theme(legend.position = "none")
    ggplotly(p)
  })

  output$city_performance <- renderPlotly({
    # Robust Combined Hierarchy Matrix
    df_plot <- filtered_deliveries() %>%
      group_by(geo_unit = .data[[input$geo_level]], traffic_level) %>%
      summarise(
        avg_time = mean(delivery_time_min),
        delay_rate = mean(delayed_flag) * 100,
        .groups = "drop"
      )

    p <- df_plot %>%
      ggplot(aes(x = avg_time, y = delay_rate, label = geo_unit, color = geo_unit)) +
      geom_point(size = 4, aes(shape = traffic_level)) +
      theme_minimal() +
      labs(x = "Avg Delivery Time (min)", y = "Delay Rate (%)")
    ggplotly(p)
  })

  output$keyword_plot <- renderPlotly({
    library(tidytext)

    stop_words_custom <- stop_words %>% filter(lexicon == "snowball")

    tokens <- filtered_reviews() %>%
      unnest_tokens(word, review_text) %>%
      anti_join(stop_words_custom, by = "word") %>%
      count(word, sort = TRUE) %>%
      head(15)

    if (nrow(tokens) == 0) {
      return(NULL)
    }

    p <- tokens %>%
      ggplot(aes(x = reorder(word, n), y = n, fill = n)) +
      geom_col() +
      coord_flip() +
      theme_minimal() +
      labs(title = "Top 15 Review Keywords", x = "Keyword", y = "Frequency")
    ggplotly(p)
  })

  output$delivery_table <- renderDT({
    datatable(filtered_deliveries() %>% select(delivery_id, order_date, city, delivery_time_min, sla_min, traffic_level),
      options = list(pageLength = 10)
    )
  })
}

# Run App
# shinyApp(ui, server)
# For running from Rscript:
runApp(list(ui = ui, server = server), launch.browser = TRUE, port = 3838)
