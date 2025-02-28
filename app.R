library(librarian)

# Load required packages
librarian::shelf(tidyverse, bslib, shiny, openxlsx, DT, ragg, readr)

library(ragg)
library(readr)
library(shiny)
library(tidyverse)
library(bslib)
library(DT)
library(openxlsx)

# Source the main.R script to use the assign_tasks function
source("main.R")

# Define UI for the Shiny app
ui <- fluidPage(
  # Set a default theme using bslib (Superhero for "Less dark")
  theme = bs_theme(version = 5, bootswatch = "superhero"),  # Default theme (Less dark)
  
  titlePanel("Task Assignment App"),
  
  sidebarLayout(
    sidebarPanel(
      # Add a theme selector with a default value
      selectInput(
        "theme", "Select Theme",
        choices = c(
          "Dark" = "darkly",
          "Less dark" = "superhero",
          "Not dark" = "cerulean"
        ),
        selected = "superhero"  # Default theme (Less dark)
      ),
      fileInput("file", "Upload Excel File", accept = c(".xlsx")),
      sliderInput(
        "coef", 
        label = "Score Adjustment Coefficient", 
        value = 0,  # Default coefficient set to 0
        min = 0, 
        max = 2, 
        step = 0.1
      ),
      helpText(
        "The coefficient adjusts worker scores based on their current SGSE usage.",
        "A value of 0 means no adjustment, while higher values reduce scores for workers with higher SGSE usage."
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Assignment Results", DTOutput("assignment_results")),  # Use DT for better tables
        tabPanel("Tasks per Worker", DTOutput("tasks_per_worker")),  # Use DT for modern tables
        tabPanel("Initial Scores", DTOutput("initial_scores")),  # New tab for initial scores
        tabPanel("Final Adjusted Scores", DTOutput("final_scores"))  # Use DT for better tables
      )
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output, session) {
  # Observe changes in the theme selector and update the app theme
  observe({
    # Update the theme based on the selected value
    theme <- bs_theme(version = 5, bootswatch = input$theme)
    session$setCurrentTheme(theme)  # Apply the selected theme
  })
  
  # Reactive expression to run the task assignment function
  assignment_results <- reactive({
    req(input$file)  # Ensure a file is uploaded
    
    # Read the uploaded file
    file_path <- input$file$datapath

    n_workers <- openxlsx::read.xlsx('manual_input.xlsx', sheet = "scores") %>% 
      select(starts_with("worker_")) %>% 
      ncol(.)
    
    # Run the task assignment function
    results <- assign_tasks(
      input_filepath = file_path,
      num_workers = n_workers,
      scores_sheet = "scores",
      max_sgse_sheet = "max_sgse",
      score_adjust_coef = input$coef
    )
    
    # Return the results
    results
  })
  
  # Render the assignment results table with better column names
  output$assignment_results <- renderDT({
    df_assign <- assignment_results()$df_assign
    df_assign <- df_assign %>%
      mutate(worker = gsub("worker_", "Worker ", worker)) %>%
      rename(
        "Task" = tasks,
        "SGSE" = sgse,
        "Assigned Worker" = worker,
        "Task Desirability" = task_desirability
      )
    datatable(df_assign, options = list(pageLength = 20, scrollX = TRUE))
  })
  
  # Render the tasks per worker in a modern way using DT
  output$tasks_per_worker <- renderDT({
    results <- assignment_results()
    df_assign <- results$df_assign
    worker_sgse <- results$worker_sgse
    
    # Join df_assign with worker_sgse to get max_sgse
    df_assign <- df_assign %>%
      left_join(worker_sgse, by = "worker")
    
    # Create a summary table for tasks per worker
    tasks_per_worker <- df_assign %>%
      group_by(worker) %>%
      summarize(
        Tasks = paste(sort(tasks), collapse = ", "),  # Sort tasks alphabetically
        Total_SGSE = sum(sgse, na.rm = TRUE),
        Max_SGSE = first(max_sgse),  # Now max_sgse is available after the join
        Percentage_Used = round(100 * Total_SGSE / Max_SGSE, 1)
      ) %>%
      rename(
        "Worker" = worker,
        "Tasks Assigned" = Tasks,
        "Total SGSE" = Total_SGSE,
        "Max SGSE" = Max_SGSE,
        "Percentage Used" = Percentage_Used
      )
    
    datatable(tasks_per_worker, options = list(pageLength = 20, scrollX = TRUE))
  })
  
  # Render the initial scores table with cell coloring
  output$initial_scores <- renderDT({
    req(input$file)  # Ensure a file is uploaded
    
    # Read the initial scores from the uploaded file
    df_scores <- openxlsx::read.xlsx(input$file$datapath, sheet = "scores")
    
    # Rename columns for better readability
    df_scores <- df_scores %>%
      rename("Task" = tasks) %>% 
      select(-sgse) %>% 
      #rename all columns starting with "worker_"
      rename_with(~ str_replace(., "^worker_", "Worker "), .cols = starts_with("worker_"))
    
    # Define cuts and values for cell coloring
    cuts <- seq(0, 10, length.out = 10)  # 10 cuts for 11 values
    colors <- colorRampPalette(c("grey80", "hotpink"))(length(cuts) + 1)  # 11 colors
    
    # Render the table with cell coloring based on values
    datatable(df_scores, options = list(pageLength = 20, scrollX = TRUE)) %>%
      formatStyle(
        columns = colnames(df_scores)[-1],  # Apply coloring to all columns except "Task"
        backgroundColor = styleInterval(
          cuts = cuts,  # Define cuts for the color gradient
          values = colors  # Define colors for the gradient
        )
      )
  })
  
  # Render the final adjusted scores table with better column names and rounded scores
  output$final_scores <- renderDT({
    results <- assignment_results()
    df_scores <- list(
      scores = openxlsx::read.xlsx(input$file$datapath, sheet = "scores"),
      max_sgse = openxlsx::read.xlsx(input$file$datapath, sheet = "max_sgse")
    )
    
    worker_sgse <- results$worker_sgse
    
    final_scores <- adjust_scores(
      df_scores$scores,
      worker_sgse,
      input$coef
    ) %>%
      pivot_wider(names_from = "worker", values_from = "adjusted_score") %>%
      rename("Task" = tasks) %>% 
        #rename all columns starting with "worker_"
        rename_with(~ str_replace(., "^worker_", "Worker "), .cols = starts_with("worker_"))
    
    # Round all numeric columns to 2 decimal places
    final_scores <- final_scores %>%
      mutate(across(where(is.numeric), ~ round(., 2)))
    
    # Define cuts and values for cell coloring
    cuts <- seq(0, 10, length.out = 10)  # 10 cuts for 11 values
    colors <- colorRampPalette(c("grey80", "hotpink"))(length(cuts) + 1)  # 11 colors
    
    # Render the table with cell coloring based on values
    datatable(final_scores, options = list(pageLength = 20, scrollX = TRUE)) %>%
      formatStyle(
        columns = colnames(final_scores)[-1],  # Apply coloring to all columns except "Task"
        backgroundColor = styleInterval(
          cuts = cuts,  # Define cuts for the color gradient
          values = colors  # Define colors for the gradient
        )
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)