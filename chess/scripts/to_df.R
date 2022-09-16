#install.packages("bigchess")
library("bigchess")
library("chessR")
library("stringr")
setwd("C:/Users/barry/Documents/chess")
pgn_file = "data in/lichess_tournament_2021.04.29_YVSZChhB_lichess-quarantane-bundesliga2.pgn"
df = read.pgn(pgn_file,nlines=1)
View(df)
lichess_clock_move_time(pgn_file[1])
lichess_game_data <- get_raw_lichess("LordyLeroy")


#################
read_in_file <- readLines(pgn_file)
# cleaning steps of the file
collapsed_strings <- paste(read_in_file, collapse = "\n") %>% strsplit(., "\n\n\n") %>% unlist()
games_list <- strsplit(collapsed_strings, "\n")
View(games_list[[1]])
df = sapply(games_list,create_games_df)

df1 = create_games_df(games_list)
df = bind_rows(df)

mysplitter = function(game) {
  X=strsplit(moves,"[%clk ",fixed=TRUE)[[1]][c(2:3)]
  X=strsplit(X,"]",fixed=TRUE)
  X=sapply(X, `[[`, 1,USE.NAMES = FALSE)
  return(X)
}
toSeconds <- function(x){
  if (!is.character(x)) stop("x must be a character string of the form H:M:S")
  if (length(x)<=0)return(x)
  
  unlist(
    lapply(x,
           function(i){
             i <- as.numeric(strsplit(i,':',fixed=TRUE)[[1]])
             if (length(i) == 3) 
               i[1]*3600 + i[2]*60 + i[3]
             else if (length(i) == 2) 
               i[1]*60 + i[2]
             else if (length(i) == 1) 
               i[1]
           }  
    )  
  )  
} 

addtimes <- function(df) {
  df_1 = sapply(df$Moves,start=1,stop=55,substr)
  xxx=sapply(df_1,mysplitter)
  colnames(xxx) = NULL
  xxx=t(xxx)
  colnames(xxx) = c('white_starttime','black_starttime')
  xxx[,'white_starttime']=toSeconds(xxx[,'white_starttime'])
  xxx[,'black_starttime']=toSeconds(xxx[,'black_starttime'])
  df = cbind(df,xxx)
  return(df)
  
}

df = addtimes(df)

#time control no increment
tc = str_split_fixed(df$TimeControl, "\\+", 2)
colnames(tc) = c('Initial_Regulation','Increment')
df = cbind(df,tc)

df['white_berzerk'] = df$Initial_Regulation!=df$white_starttime
df['black_berzerk'] = df$Initial_Regulation!=df$black_starttime

df$Initial_Regulation[1]
df$white_starttime[1]

xxx = str_split_fixed(df$Moves[1:5], "\\[\\%clk ", 4)[,2:3]
xxx = str_split_fixed(xxx,"\\]",4)
xxx
