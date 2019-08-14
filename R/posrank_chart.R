#' @title Position Rank Chart
#'
#' @description creates posrank chart
#'
#' @param players a vector of players
#' @param years a vector of years
#' @param scoring ppr,halfppr,standard,sixpttd
#' @param con DB connection
#' @param pfrplayers pfr DB
#'
#' @return plotly object
#'
#' @examples posrank_chart(players,years,scoring,con,pfrplayers)
#'
#' @export

posrank_chart <- function(players,years,scoring,con,pfrplayers){
  years <- years
  years2 <- paste(years, collapse = ",")
  scoring <- tolower(scoring)
  v <- paste0("select year,player,week,position,player_id,",paste0(scoring,"_pr"),"
              from finalweeklydata where year in (", years2,")")
  df <- dbGetQuery(con, v)
  colnames(df)[6] <- "PR"
  players <- c(players)
  playerids <- c()
  for(i in 1:length(players)){
    id <- pfrplayers$player_id[pfrplayers$player2 == players[i]]
    playerids <- c(playerids,id)
  }

  df <- df %>% filter(player_id %in% playerids)
  df$Type <- 0
  df$Type[df$PR > 36] <- 5
  df$Type[df$PR < 37] <- 4
  df$Type[df$PR < 25] <- 3
  df$Type[df$PR < 13] <- 2
  df$Type[df$PR < 6] <- 1
  df$type2 <- 0
  df$type2[df$PR < 13] <- 1
  finishes <- df %>% group_by(player_id,Type) %>% tally() %>% group_by(player_id) %>% mutate(t = sum(n), p = round(n/t,4)*100)
  positions <- unique(df %>% select(player_id,player,position))
  finishes <- merge(finishes, positions, by = c("player_id"))
  finishes$Bin <- 0
  finishes$Bin[finishes$Type == 5] <- "Rest"
  finishes$Bin[finishes$Type == 4] <- "25-36"
  finishes$Bin[finishes$Type == 3] <- "13-24"
  finishes$Bin[finishes$Type == 2] <- "Top-12"
  finishes$Bin[finishes$Type == 1] <- "Top-5"
  finishes$Bin <- as.factor(finishes$Bin)
  finishes$Bin <- factor(finishes$Bin,levels = c("Top-5","Top-12","13-24","25-36","Rest"))
  finishes2 <- df %>% group_by(player_id,type2) %>% tally() %>% group_by(player_id) %>% mutate(t = sum(n), p = round(n/t,4)*100)
  finishes2 <- finishes2[finishes2$type2 != 0,]

  for(i in 1:nrow(finishes2)){
    loopv <- finishes2[finishes2$player_id == playerids[i],3:5]
    finishes[finishes$player_id == playerids[i] & finishes$Type == 2,3:5] <- loopv
  }

  colors = RColorBrewer::brewer.pal(max(length(players),3),"Set1")
  if(length(colors) > length(players)){
    colors <- colors[1:length(players)]
  }
  ggthemr::ggthemr("dust")
  p <- ggplot(finishes, aes(x = Bin, y = n, fill = player)) + geom_bar(stat = "identity", position=position_dodge()) +
    labs(title = paste0("Weekly Position Finishes")) + xlab("Position Rank Bin") + ylab("Number of Weeks") +
    scale_color_manual(values = c(colors), labels = unique(finishes$player)) +
    ylim(0,mround(max(finishes$n),5)+5)
  g <- plotly_build(p)
  for(i in 1:length(players)){
    dfloop <- finishes %>% filter(player == players[i])
    g$x$data[[i]]$text <- paste0("Player: ", dfloop$player, "<br>",
                                 "Bin: ", dfloop$Bin, "<br>",
                                 "No. Games: ", dfloop$n, "<br>",
                                 "Percentage: ", dfloop$p)
  }
  for(i in 1:length(players)){
    dfloop <- finishes %>% filter(player == players[i])
    g$x$data[[i]]$name <- paste0(dfloop$player)
  }
  final_prod <- g %>% layout(images = list(source =  "https://raw.githubusercontent.com/amazehayes/ffstats_navbar/master/www/FFStats_BlackLogo_3.5x3.5.png",
                                           xref = "paper",
                                           yref = "paper",
                                           x = 0.01,
                                           y = 1,
                                           sizex = 0.2,
                                           sizey = 0.2,
                                           opacity = 0.1,
                                           layer = "below"),
                             legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25)) %>%
    config(displayModeBar = FALSE) %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))

  return(final_prod)
}
