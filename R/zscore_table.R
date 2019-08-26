#' @title Z-Score Table
#'
#' @description creates z-score table
#'
#' @param position QB,RB,WR,TE
#' @param scoring ppr,halfppr,standard,sixpttd
#' @param fwd a player database
#'
#' @return dataframe
#'
#' @examples zscore_df(position,scoring)
#'
#' @export

zscore_df <- function(position,scoring,fwd){
  pos <- position
  scoring <- tolower(scoring)

  df <- fwd %>% select(year,player_id,scoring) %>%
    group_by(year,player_id) %>% summarise_all(sum)
  colnames(df)[3] <- "FP"

  pfrplayers2 <- pfrplayers %>% select(player_id,player2,player,position,dob)
  pfrplayers2$dob_year <- substr(pfrplayers2$dob,1,4)
  df <- merge(df,pfrplayers2, by = c("player_id"))
  df$dob_year <- as.numeric(df$dob_year)
  df$age <- df$year - df$dob_year
  df <- df %>% group_by(age,position) %>% mutate(percentile = round(cume_dist(FP),4))
  df$zscore <- round(qnorm(df$percentile),3)
  df$zscore <- gsub(Inf,3,df$zscore)
  df$zscore <- as.numeric(df$zscore)
  df$percentile <- df$percentile*100
  df$age <- as.integer(df$age)
  df$FP <- round(df$FP,1)

  posdf <- df %>% filter(position == pos, !is.na(zscore)) %>% arrange(-year,-percentile) %>%
    select(year,player,player2,position,age,FP,zscore,percentile)
  return(posdf)
}
