create_games_df <- function(games_list) {
  
  first_test <- games_list
  # there are some elements of the list that are blank (""), want to remove these first
  # create a vector with the column names. The moves column doesn't have a title in the code so create one called "Moves"
  # the moves are always the last element so need to pull that out manually
  tab_names <- c(gsub( "\\s.*", "", first_test[substr(first_test,1,1)=="["]) %>% gsub("\\[", "", .), "Moves")
  # then extract the values for each key above. Manually grab the moves value also and append to vector
  tab_values <- c(gsub(".*[\"]([^\"]+)[\"].*", "\\1", first_test[substr(first_test,1,1)=="["]), first_test[length(first_test)])
  #create the df of values
  df <- rbind(tab_values) %>% data.frame(stringsAsFactors = F)
  # then the header for table
  colnames(df) <- tab_names
  # remove the row names
  rownames(df) <- c()
  # because of a wierd handling of moves with the Evals, need to drop these suprious columns, that are indicated by a number
  # in the column name
  errorneous_columns <- which(grepl("[0-9]", colnames(df)))
  df[, errorneous_columns] <- NULL
  # remove the "+" sign and convert RatingDiff columns to numeric
  column_names <- colnames(df) %>% paste0(collapse = ",")
  if(grepl("WhiteRatingDiff", column_names)) {
    df$WhiteRatingDiff <- gsub("\\+", "", df$WhiteRatingDiff)
  }
  
  if(grepl("WhiteRatingDiff", column_names)) {
    df$BlackRatingDiff <- gsub("\\+", "", df$BlackRatingDiff)
  }
  return(df)
}