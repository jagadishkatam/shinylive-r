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