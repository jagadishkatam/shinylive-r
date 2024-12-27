library(shiny)
library(cards)
library(gtsummary)
library(dplyr)
library(gt)
library(shinylive)
library(httpuv)

create_summary <- function(data, group = NULL, cols = NULL, ...) {
  
  data <- data |> filter(...)
  
  if (nrow(data)==0){
    stop("dataset is empty.")
  }
  
  # Check if cols is provided
  if (is.null(cols)) {
    stop("Please provide column names in the 'cols' argument.")
  }
  
  # Check if all specified columns exist in the data
  missing_cols <- setdiff(cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop(paste("The following columns do not exist in the data:", paste(missing_cols, collapse = ", ")))
  }
  
  # Separate columns into numeric and character
  numeric_cols <- cols[sapply(data[cols], is.numeric)]
  character_cols <- cols[sapply(data[cols], is.factor)|sapply(data[cols], is.character)]
  
  # Display categorized columns
  message("Numeric columns: ", paste(numeric_cols, collapse = ", "))
  message("Character columns: ", paste(character_cols, collapse = ", "))
  
  # Build the ARD with the needed summary statistics using {cards}
  ard <-
    ard_stack(
      data,
      ard_continuous(variables = all_of(numeric_cols)),
      ard_categorical(variables = all_of(character_cols)),
      .by = {{group}},
      .attributes = TRUE
    )
  
  # Define dynamic types and statistics for the columns
  type_list <- lapply(numeric_cols, function(col) {
    if (is.numeric(data[[col]])) {
      as.formula(paste0(col, " ~ 'continuous2'"))
    }
  })
  
  statistic_list <- lapply(numeric_cols, function(col) {
    if (is.numeric(data[[col]])) {
      as.formula(paste0(col, " ~ c('{N}', '{mean} ({sd})', '{median} ({p25}, {p75})', '{min}, {max}')"))
    }
  })
  
  # Use the ARD to create a demographics table using {gtsummary}
  
  if (length(numeric_cols)==0){
    tbl_ard_summary(
      cards = ard,
      by = all_of(group),
      include = all_of(cols),
      # type = as.formula(paste(numeric_cols[1], "~ 'continuous2'")),
      # statistic = as.formula(paste(numeric_cols[1], "~ c('{N}', '{mean} ({sd})', '{median} ({p25}, {p75})', '{min}, {max}')"))
    ) |> 
      bold_labels() |> 
      modify_header(all_stat_cols() ~ "**{level}**  \nN = {n}") |> 
      modify_footnote(everything() ~ NA)  |>  
      modify_caption("**Summary of Demography**") |> 
      as_gt()
  } else {
    tbl_ard_summary(
      cards = ard,
      by = all_of(group),
      include = all_of(cols),
      type = type_list,
      statistic = statistic_list
    ) |> 
      bold_labels() |> 
      modify_header(all_stat_cols() ~ "**{level}**  \nN = {n}") |> 
      modify_footnote(everything() ~ NA)  |>
      modify_caption("**Summary of Demography**") |> 
      as_gt()
  }
  
}
# debugonce(create_summary)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("\n      .navbar-default { background-color: #2c3e50; border-color: #2c3e50; } \n      .navbar-default .navbar-brand { color: #ecf0f1; } \n      .sidebar { background-color: #ecf0f1; padding: 15px; border-right: 2px solid #bdc3c7; } \n      .mainpanel { background-color: #ffffff; padding: 20px; border-left: 2px solid #bdc3c7; } \n      .shiny-download-link { background-color: #3498db; color: white; padding: 10px 15px; border: none; border-radius: 4px; } \n    "))
  ),
  titlePanel("Demographic Summary"),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      fileInput("file", "Upload File", accept = c(".csv", ".Rdata", ".rds")),
      selectInput("group_col", "Select Grouping Column(s)", choices = NULL, multiple = TRUE),
      selectInput("summary_col", "Select Summary Column(s)", choices = NULL, multiple = TRUE),
      textInput("filter_val", "Enter Filter Expression (e.g., Age > 30 & Gender == 'M')")
    ),
    mainPanel(
      class = "mainpanel",
      gt_output("summary_table"),
      downloadButton("download_data", "Download Data")
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (ext == "csv") {
      read.csv(input$file$datapath)
    } else if (ext == "Rdata") {
      load(input$file$datapath)
      get(ls()[1])
    } else if (ext == "rds") {
      readRDS(input$file$datapath)
    } else {
      stop("Unsupported file type")
    }
  })
  observe({
    req(data())
    cols <- colnames(data())
    updateSelectInput(session, "group_col", choices = cols)
    updateSelectInput(session, "summary_col", choices = cols)
  })
  filtered_data <- reactive({
    req(data(), input$filter_val)
    d <- data()
    d <- d |> filter(eval(parse(text = input$filter_val)))
    d
  })
  summary_table <- reactive({
    req(filtered_data(), input$group_col, input$summary_col)
    create_summary(data = filtered_data(), group = input$group_col, cols = input$summary_col)
  })
  output$summary_table <- render_gt({
    summary_table()
  })
  output$download_data <- downloadHandler(
    filename = function() { "summary_data.csv" },
    content = function(file) {
      write.csv(summary_table(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)



