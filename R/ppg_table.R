#' @title PPG Table
#'
#' @description creates ppg table
#'
#' @param players a vector of players
#' @param years a vector of years
#' @param scoring ppr,halfppr,standard,sixpttd
#' @param con DB connection
#' @param pfrplayers pfr DB
#'
#' @return dataframe
#'
#' @examples ppg_table(players,years,scoring,con,pfrplayers)
#'
#' @export

ppg_table <- function(players,years,scoring,con,pfrplayers){
  years <- years
  years2 <- paste(years, collapse = ",")
  scoring <- tolower(scoring)
  v <- paste0("select year,player,week,position,player_id,",paste0(scoring,","),paste0(scoring,"_pr"),"
              from finalweeklydata where year in (", years2,")")
  df <- dbGetQuery(con, v)
  colnames(df)[6] <- "FP"
  colnames(df)[7] <- "PR"
  players <- c(players)
  playerids <- c()
  for(i in 1:length(players)){
    id <- pfrplayers$player_id[pfrplayers$player2 == players[i]]
    playerids <- c(playerids,id)
  }

  positions <- unique(df %>% select(player_id,player,position))
  ppgdf <- df %>% filter(player_id %in% playerids) %>% group_by(player_id, year) %>%
    summarise(avg = round(mean(FP),2), sd = round(sd(FP),2))
  ppgdf <- merge(ppgdf,positions, by = "player_id")
  ppgdf$Lower <- ppgdf$avg-ppgdf$sd
  ppgdf$Upper <- ppgdf$avg+ppgdf$sd
  ppgdf[ppgdf<0] <- 0
  ppgdf$cv <- ifelse(is.infinite(round(ppgdf$sd/ppgdf$avg,2)),0,round(ppgdf$sd/ppgdf$avg,2))
  ppgdf$cor <- round(ppgdf$Upper/ppgdf$cv,2)

  loopdf <- matrix(ncol = 9,nrow = nrow(ppgdf))
  for(i in 1:nrow(loopdf)){
    p <- ppgdf[i,1]
    y <- ppgdf[i,2]

    t1 <- nrow(df %>% filter(player_id == p, year == y, PR == 1))
    t4 <- nrow(df %>% filter(player_id == p, year == y, PR < 4))
    t6 <- nrow(df %>% filter(player_id == p, year == y, PR < 6))
    t13 <- nrow(df %>% filter(player_id == p, year == y, PR < 13))
    t25 <- nrow(df %>% filter(player_id == p, year == y, PR < 25, PR > 12))
    t37 <- nrow(df %>% filter(player_id == p, year == y, PR < 37, PR > 24))
    trest <- nrow(df %>% filter(player_id == p, year == y, PR > 36))
    v <- c(t1,t4,t6,t13,t25,t37,trest)

    loopdf[i,] <- c(p,y,v)
  }
  loopdf <- as.data.frame(loopdf)
  colnames(loopdf) <- c("player_id","year","top-1","top-3","#Top-5","#Top-12","#13-24","#25-36","#Rest")

  ppgdf <- merge(ppgdf,loopdf, by = c("player_id","year"))
  ppgdf <- ppgdf %>% select(year,player,position,avg,sd,Lower,Upper,cv,cor,11:17)
  colnames(ppgdf) <- c("year","player","position","average","stddev","floor","ceiling","cv","cor","#1","#top-3","#top-5","#top-12","#13-24","#25-36","#rest")
  return(ppgdf)
}
