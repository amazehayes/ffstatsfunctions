#' @title Yearly Table
#'
#' @description generates yearly table
#'
#' @param players
#' @param years
#' @param con
#'
#' @return NULL
#'
#' @examples yearly_comp_player_df(players,years,con)
#'
#' @export

yearly_player_df <- function(players,years,con){
  years <- years
  years2 <- paste(years, collapse = ",")
  v <- paste0("select year,player,week,position,player_id,ppr,halfppr,standard,sixpttd from finalweeklydata where year in (", years2,")")
  df <- dbGetQuery(con, v)
  players <- c(players)
  playerids <- c()
  for(i in 1:length(players)){
    id <- pfrplayers$player_id[pfrplayers$player2 == players[i]]
    playerids <- c(playerids,id)
  }

  df2 <- df %>% select(year,player_id,ppr,halfppr,standard,sixpttd) %>%
    group_by(player_id,year) %>% summarise_all(sum)
  positions <- unique(df %>% arrange(-year,-week) %>% select(year,player_id,player,position))
  df2 <- merge(df2, positions, by = c("year","player_id"))
  df2 <- df2 %>% group_by(year,position) %>%
    mutate(ppr_rank = rank(-ppr, ties.method = "first"),
           halfppr_rank = rank(-halfppr, ties.method = "first"),
           standard_rank = rank(-standard, ties.method = "first"),
           sixpttd_rank = rank(-sixpttd, ties.method = "first"))

  df3 <- df2 %>% filter(player_id %in% playerids) %>%
    select(year,player,position,ppr,ppr_rank,halfppr,halfppr_rank,
           standard,standard_rank,sixpttd,sixpttd_rank) %>%
    arrange(-year)

  return(df3)
}
