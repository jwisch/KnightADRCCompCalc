

#' Takes in dataframe of cognitive data
#' Returns dataframe of baseline scans for cognitively normal participants for norming
#' @param df dataframe containing participant id, visit date, cdr score, and relavant cognitive tests
#' @return df corresponding dataframe with only baseline visits
#' @export
#' @examples
#' df <- data.frame("ID" = c(1, 1, 2, 3), "cdr" = c(0, 0, 0,5, 0), 
#'                   "TESTDATE" = c("1989-04-11", "1991-03-26", "2011-05-31", "2021-06-23"),
#'                   "ANIMALS" = c(28, 27, 20, 11))
#' get_baselineCohort(df, "ID", "TESTDATE", "cdr")

get_baselineCohort <- function(df, id_col_name = "ID", visit_date_col_name = "TESTDATE",
                   cdr_score_name = "cdr"){
  
  df <- df[df[[cdr_score_name]] == 0,]
  df <- df[!is.na(df),]
  df[, visit_date_col_name] <- as.Date(df[, visit_date_col_name], format = "%Y-%m-%d")
  df <- data.table::setDT(df)[order(get(visit_date_col_name)), head(.SD, 1L), by = eval(id_col_name)]
  df <- data.frame(df)
  df <- df[!is.na(df[, id_col_name]),]
  
  return(df)
  
}


#' Takes in vector of baseline scores and full list of scores (cross-walking to equivalents should have already happened)
#' Returns normed scores
#' @param vector_baseline vector containing baseline tests (could be generated with get_baselineCohort)
#' @param vector_full vector containing all scores for the cognitive test of interest (be sure you've already converted relevant scores using crosswalks if necessary)
#' @return normed_scores vector containing z scores based on baseline norms
#' @export
#' @examples
#' get_normedScore(c(300, 300, 290, 270, NA, NA), c(300, 300, 290, 292, 295, 100, NA, 199))
get_normedScore <- function(vector_baseline, vector_full){
  mean_score <- mean(as.numeric(as.character(vector_baseline)), na.rm = TRUE)
  sd_score <- sd(as.numeric(as.character(vector_baseline)), na.rm = TRUE)
  normed_scores <- ifelse(is.na(vector_full), NA, (vector_full - mean_score) / sd_score)
  return(normed_scores)
  
}