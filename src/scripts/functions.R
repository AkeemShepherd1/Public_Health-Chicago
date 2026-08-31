likert_index <- function(data, questions, na.rm = TRUE) {
  
  # Convert Likert responses to numeric values
  likert_values <- data[questions] |>
    lapply(function(x) {
      dplyr::case_when(
        #strongly disagree,..., strongly agree scale
        x == "Strongly disagree" ~ 1,
        x == "Disagree" ~ 2,
        x == "Neither agree nor disagree" ~ 3,
        x == "Agree" ~ 4,
        x == "Strongly agree" ~ 5,
        
        # Yes/No scale 
        x == "No" ~ 0, 
        x == "Yes" ~ 1,
        TRUE ~ NA_real_
      )
    }) |>
    as.data.frame()
  
  return(index)
}
