get_file <- function(player_name) {
  
  # cat("Extracting ", player_name, " games. Please wait\n")
  
  # download the tmp file
  tmp <- tempfile()
  curl::curl_download("https://lichess.org/api/tournament/jul22bta/games?evals=true&clocks=true&opening=true", tmp)
  # read in the file
  read_in_file <- readLines(tmp)
  # cleaning steps of the file
  collapsed_strings <- paste(read_in_file, collapse = "\n") %>% strsplit(., "\n\n\n") %>% unlist()
  games_list <- strsplit(collapsed_strings, "\n")
  
  return(games_list)
}
#lichess.org/api/tournament/YVSZChhB/games?clocks=true
get_file(x)