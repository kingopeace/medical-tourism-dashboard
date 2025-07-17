## Loading the libraries and the data

library(shiny)
library(tidyverse)
library(plotly)
library(DT) 

cleaned_data <- read_csv("cleaned_medical_data.csv")
author_counts <- read_csv("author_counts.csv")
keyword_counts <- read_csv("keyword_counts.csv")
journal_counts <- read_csv("journal_counts.csv")

## UI

ui <- fluidPage(
  # App title
  titlePanel("Medical Tourism: A Bibliometric Analysis Dashboard"),
  
  # A tabbed layout for the different analyses
  tabsetPanel(
    type = "tabs",
    
    # TAB 1: PUBLICATION TRENDS
    tabPanel("Publication Trends",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "year_slider",
                             label = "Select Year Range:",
                             min = min(cleaned_data$year),
                             max = max(cleaned_data$year),
                             value = c(min(cleaned_data$year), max(cleaned_data$year)),
                             sep = "") # No comma in year
               ),
               mainPanel(
                 h3("Publications Over Time"),
                 plotlyOutput(outputId = "trend_plot")
               )
             )
    ),
    
    # TAB 2: KEY CONTRIBUTORS
    tabPanel("Key Contributors",
             sidebarLayout(
               sidebarPanel(
                 numericInput(inputId = "top_n_contributors",
                              label = "Select Top 'N' Contributors:",
                              value = 10,
                              min = 5,
                              max = 20)
               ),
               mainPanel(
                 h3("Top Authors"),
                 plotlyOutput(outputId = "top_authors_plot"),
                 hr(), # Horizontal line
                 h3("Top Journals"),
                 plotlyOutput(outputId = "top_journals_plot")
               )
             )
    ),
    
    # TAB 3: RESEARCH THEMES
    tabPanel("Research Themes",
             sidebarLayout(
               sidebarPanel(
                 numericInput(inputId = "top_n_keywords",
                              label = "Select Top 'N' Keywords:",
                              value = 15,
                              min = 5,
                              max = 30)
               ),
               mainPanel(
                 h3("Most Frequent Keywords"),
                 plotlyOutput(outputId = "top_keywords_plot")
               )
             )
    ),
    
    # TAB 4: INFLUENTIAL PAPERS
    tabPanel("Influential Papers",
             h3("Most Cited Papers"),
             p("You can search the table and sort by clicking on the column headers."),
             DTOutput(outputId = "papers_table")
    )
  )
)

## Server

server <- function(input, output) {
  
  # TAB 1: TREND PLOT SERVER LOGIC
  output$trend_plot <- renderPlotly({
    plot_data <- cleaned_data %>%
      filter(year >= input$year_slider[1] & year <= input$year_slider[2]) %>%
      count(year)
    
    p <- ggplot(plot_data, aes(x = year, y = n)) +
      geom_line(color = "#0072B2", size = 1) +
      geom_point(color = "#0072B2", size = 2) +
      labs(title = "Annual Publication Volume", x = "Year", y = "Number of Publications") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # TAB 2: CONTRIBUTORS PLOTS SERVER LOGIC
  output$top_authors_plot <- renderPlotly({
    plot_data <- head(author_counts, input$top_n_contributors)
    
    p <- ggplot(plot_data, aes(x = reorder(authors, publication_count), y = publication_count)) +
      geom_col(fill = "#D55E00") +
      coord_flip() +
      labs(title = paste("Top", input$top_n_contributors, "Authors"), x = "", y = "Publication Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$top_journals_plot <- renderPlotly({
    plot_data <- head(journal_counts, input$top_n_contributors)
    
    p <- ggplot(plot_data, aes(x = reorder(source_title, publication_count), y = publication_count)) +
      geom_col(fill = "#009E73") +
      coord_flip() +
      labs(title = paste("Top", input$top_n_contributors, "Journals"), x = "", y = "Publication Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # TAB 3: KEYWORDS PLOT SERVER LOGIC
  output$top_keywords_plot <- renderPlotly({
    plot_data <- head(keyword_counts, input$top_n_keywords)
    
    p <- ggplot(plot_data, aes(x = reorder(author_keywords, frequency), y = frequency)) +
      geom_col(fill = "#56B4E9") +
      coord_flip() +
      labs(title = paste("Top", input$top_n_keywords, "Keywords"), x = "", y = "Frequency") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # TAB 4: PAPERS TABLE SERVER LOGIC
  output$papers_table <- renderDT({
    table_data <- cleaned_data %>%
      select(title, authors, year, source_title, cited_by) %>%
      arrange(desc(cited_by))
    
    datatable(table_data,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE,
              filter = 'top',
              class = 'cell-border stripe')
  })
}

## Running Shiny

shinyApp(ui = ui, server = server)
