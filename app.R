library(shiny)
library(rsconnect)
library(tidyverse)
library(ggplot2)

# Load data
data <- read.csv("merged_vdem_income_freedom.csv")

# Define readable labels for V-Dem and Freedom variables
vdem_labels <- c(
  "v2x_polyarchy" = "Electoral",
  "v2x_freexp_altinf" = "Expression",
  "v2x_partip" = "Participation"
)

freedom_labels <- c(
  "freedom_status" = "Status",
  "civil_liberties_score" = "Liberties",
  "freedom_total_score" = "TotalScore"
)

# Custom vibrant color palette (at least 20)
vibrant_colors <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
  "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5",
  "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F",
  "#E5C494", "#B3B3B3", "#1B9E77", "#D95F02", "#7570B3"
)

# Define UI
ui <- fluidPage(
  tags$head(tags$style(HTML("body { background-color: #f8f9fa; } h2, label { color: #2c3e50; }
    .title-row { text-align: center; margin-bottom: 20px; } .btn-danger { margin-top: 10px; }"))),
  fluidRow(class = "title-row",
           column(12, h2("Inequalitics"), actionButton("reset", "Reset", class = "btn btn-danger"))),
  
  fluidRow(
    column(6,
           selectInput("income_country", "Select Country (Income):",
                       choices = unique(data$country_name[!is.na(data$income_top10_share)]),
                       selected = unique(data$country_name[!is.na(data$income_top10_share)])[1]),
           plotOutput("incomePlot")
    ),
    column(6,
           selectInput("vdem_var", "Select V-Dem Variable:",
                       choices = setNames(names(vdem_labels), vdem_labels),
                       selected = "v2x_polyarchy"),
           plotOutput("vdemPlot")
    )
  ),
  fluidRow(
    column(6,
           selectInput("freedom_var", "Select Freedom House Variable:",
                       choices = freedom_labels,
                       selected = "freedom_total_score"),
           plotOutput("freedomPlot")
    ),
    column(6,
           selectInput("overlay_country", "Select Countries for Overlay:",
                       choices = unique(data$country_name[!is.na(data$income_top10_share)]),
                       selected = unique(data$country_name[!is.na(data$income_top10_share)])[1:2],
                       multiple = TRUE),
           checkboxInput("overlay", "Overlay Graphs", value = TRUE),
           plotOutput("overlayPlot")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  observeEvent(input$reset, {
    updateSelectInput(session, "income_country", selected = unique(data$country_name[!is.na(data$income_top10_share)])[1])
    updateSelectInput(session, "vdem_var", selected = "v2x_polyarchy")
    updateSelectInput(session, "freedom_var", selected = "freedom_total_score")
    updateSelectInput(session, "overlay_country", selected = unique(data$country_name[!is.na(data$income_top10_share)])[1:2])
    updateCheckboxInput(session, "overlay", value = TRUE)
  })
  
  output$incomePlot <- renderPlot({
    req(input$income_country)
    data %>%
      filter(country_name == input$income_country, !is.na(income_top10_share)) %>%
      ggplot(aes(x = year, y = income_top10_share)) +
      geom_line(color = "#0072B2", size = 1.2) +
      labs(title = paste("Top 10% Income Share -", input$income_country), x = "Year", y = "% Share") +
      theme_minimal()
  })
  
  output$vdemPlot <- renderPlot({
    var <- input$vdem_var
    excluded_countries <- c("Germany", "Greece", "Ireland", "Israel", "Italy", "Paraguay", "Portugal", "Spain", "Sweden", "United Kingdom", "Uruguay")
    data %>%
      filter(!country_name %in% excluded_countries, !is.na(.data[[var]])) %>%
      ggplot(aes_string(x = "country_name", y = var, fill = "country_name")) +
      stat_summary(fun = mean, geom = "bar", show.legend = FALSE) +
      labs(title = paste("Average", vdem_labels[var]), x = "Country", y = "Score") +
      scale_fill_manual(values = vibrant_colors) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$freedomPlot <- renderPlot({
    var <- names(freedom_labels[freedom_labels == input$freedom_var])[1]
    filtered_data <- data %>% filter(!is.na(.data[[var]]))
    if (var == "freedom_status") {
      filtered_data %>%
        ggplot(aes(x = country_name, fill = freedom_status)) +
        geom_bar() +
        labs(title = "Freedom Status Count", x = "Country") +
        scale_fill_manual(values = vibrant_colors) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      filtered_data %>%
        ggplot(aes_string(x = "country_name", y = var, fill = "country_name")) +
        geom_boxplot(show.legend = FALSE) +
        labs(title = paste("Distribution of", input$freedom_var), x = "Country") +
        scale_fill_manual(values = vibrant_colors) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  output$overlayPlot <- renderPlot({
    req(input$overlay_country)
    df <- data %>% filter(country_name %in% input$overlay_country, !is.na(income_top10_share))
    p <- ggplot(df, aes(x = year, y = income_top10_share, group = country_name))
    
    if (input$overlay) {
      p + geom_area(aes(fill = country_name), alpha = 0.5, position = 'identity') +
        labs(title = "Overlay Income Comparison", y = "% Share", x = "Year") +
        scale_fill_manual(values = vibrant_colors) +
        theme_minimal()
    } else {
      p + geom_line(aes(color = country_name), size = 1.2) +
        labs(title = "Income Trends", y = "% Share", x = "Year") +
        scale_color_manual(values = vibrant_colors) +
        theme_minimal()
    }
  })
}

# Run the app
shinyApp(ui, server)
# Set your account info
rsconnect::setAccountInfo(
  name = 'jorda',
  token = '207EBDEAD5A698F58B179E67A6D3C1F9',
  secret = 'o3sX+zT0jQh14othmeul6tcyXf8dhIMKI35EL7qd'
)

# Deploy the app
rsconnect::deployApp(
  appDir = ".",          # current directory
  appName = "inequaltrics",
  appTitle = "Inequaltrics", # optional display title
  account = "jorda"
)