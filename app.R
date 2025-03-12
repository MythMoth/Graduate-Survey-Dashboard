library(shiny)
library(shinydashboard)
library(tidyverse)

#List of field explanations
explanations <- list(
  Campus = "The Eduvos campus the graduate is from",
  StudyField = "The graduate's field of study (IT, Data Science, or Computer Science)",
  Branch = "Primary Type of work",
  Role = "Type of Developer role at work",
  EduLevel = "Highest level of education achieved",
  ProgLang = "Programming languages the graduate works with",
  Databases = "Databases the graduate works with",
  Platform = "The cloud platform the graduate works with",
  WebFramework = "The web framework the graduate uses in their work",
  Industry = "The industry in which the graduate works",
  AISearch = "AI search tool (e.g., ChatGPT, Bing AI) the graduate uses",
  AITool = "AI developer tool (e.g., GitHub Copilot) the graduate uses",
  Employment = "The employment status and type of the graduate"
)

# Defining the bar function
bar <- function(df, var) {
  var_sym <- sym(var)
  
  top <- df |>
    mutate(new_var = strsplit(as.character(!!var_sym), ";")) |>
    unnest_longer(new_var) |>
    count(new_var, sort = TRUE) |>
    slice_max(n, n = 5) |>
    pull(new_var)
  
  df |>
    filter(!!var_sym %in% top) |>
    ggplot(aes(x = fct_infreq(!!var_sym), fill = !!var_sym)) +
    geom_bar(width = 0.8) +
    labs(
      x = var,
      y = "Amount",
      fill = var,
      title = var
    ) +
    scale_x_discrete(labels = function(x) abbreviate(x, minlength = 10)) +
    theme_bw() +
    theme(
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), 
      text = element_text(size = 16),          
      axis.title.x = element_text(size = 16),  
      plot.title = element_text(size = 18),  
      legend.text = element_text(size = 14),   
      legend.title = element_text(size = 16)   
    )
}

# Define the multiple choice columns that will use the bar function
m_choice <- c("Employment", "AITool", "AISearch", "Industry", 
              "WebFramework", "Platform", "Databases", "ProgLang")

# Creating the UI
ui <- dashboardPage(
  dashboardHeader(title = "IT Graduate Survey Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(4,
                       box(
                         title = "Variable Summary", status = "primary",
                         solidHeader = TRUE, width = 12,  
                         selectInput("summaryVar", "Select Variable:", 
                                     choices = names(df),
                                     selected = "Campus"),
                         verbatimTextOutput("variableSummary")
                       ),
                       box(
                         title = "Summary Explanation", status = "info",
                         solidHeader = TRUE, width = 12,  
                         textOutput("summaryExplanation")
                       )
                ),
                column(8, 
                       box(
                         title = "Bar Plot", status = "primary",
                         solidHeader = TRUE, width = 12,
                         plotOutput("barPlot", height = 550)
                       )
                )
              ),
              fluidRow(
                box(
                  title = "Customize Bar Plot", status = "warning",
                  solidHeader = TRUE, width = 12,
                  selectInput("xvar", "X-axis Variable", 
                              choices = names(df),
                              selected = "Campus")
                )
              )
      )
    )
  )
)

# Defining Server Logic
server <- function(input, output, session) {
  
  # Render Explanation Text
  output$summaryExplanation <- renderText({
    selected <- input$summaryVar
    explanation <- explanations[[selected]]
    if (is.null(explanation)) {
      "No explanation available for this variable."
    } else {
      explanation
    }
  })
  
  # Render Bar Plot
  output$barPlot <- renderPlot({
    var <- as.character(input$xvar)  # Ensure input is a string
    
    if (var %in% m_choice) {
      bar(df, var)
    } else {
      df |>
        ggplot(aes(x = !!sym(var), fill = !!sym(var))) +  
        geom_bar(width = 0.8) +
        labs(
          x = var,
          y = "Amount",
          fill = var,
          title = var
        ) +
        scale_x_discrete(labels = function(x) abbreviate(x, minlength = 10)) +
        theme_bw() +
        theme(
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          text = element_text(size = 16),          
          axis.title.x = element_text(size = 16),  
          plot.title = element_text(size = 18),  
          legend.text = element_text(size = 14),   
          legend.title = element_text(size = 16)   
        )
    }
  })
}

#Run the app
shinyApp(ui, server)
