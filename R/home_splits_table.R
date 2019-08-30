#' @title Home Splits Table
#'
#' @description creates home splits table
#'
#' @param players player name
#' @param years year vector
#' @param scoring ppr,halfppr,standard,sixpttd
#' @param fwd a player database
#' @param pfrplayers PFR database
#'
#' @return datatable
#'
#' @examples home_splits_table(players,years,scoring,fwd,pfrplayers)
#'
#' @export

home_splits_table <- function(players,years,scoring,fwd,pfrplayers){
  players <- sort(players)
  playerids <- c()
  for(i in 1:length(players)){
    id <- pfrplayers$player_id[pfrplayers$player2 == players[i]]
    playerids <- c(playerids,id)
  }
  years <- c(years)
  scoring <- tolower(scoring)

  df <- fwd %>% filter(year %in% years, player_id %in% playerids) %>%
    select(player_id,player,scoring,home,games) %>%
    group_by(player_id,player,home) %>%
    summarise_all(sum) %>% arrange(player)
  df$average <- round(df$ppr/df$games,2)
  colnames(df)[4] <- "FP"
  df$home <- gsub(0,"Away",df$home)
  df$home <- gsub(1,"Home",df$home)

  dfhome <- matrix(nrow = length(players),ncol = 6)
  x <- 1:nrow(df)
  for(i in x[lapply(x, "%%", 2) != 0]){
    dfloop1 <- df[i,]
    dfloop2 <- df[i+1,]
    v <- c(dfloop2$games,dfloop2$FP,dfloop2$average,dfloop1$games,dfloop1$FP,dfloop1$average)
    dfhome[((i+1)/2),] <- v
  }
  dfhome <- as.data.frame(dfhome)
  rownames(dfhome) <- players
  colnames(dfhome) <- c("HomeGames","HomeFPTotal","HomeAverage","AwayGames","AwayFPTotal","AwayAverage")
  dfhome$AverageDiff <- dfhome$HomeAverage - dfhome$AwayAverage

  return(dfhome)
}
