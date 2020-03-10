#' @title Z-Score Chart
#'
#' @description creates z-score chart
#'
#' @param players player name
#' @param position QB,RB,WR,TE
#' @param scoring ppr,halfppr,standard,sixpttd
#' @param fwd a player database
#' @param pfrplayers PFR database
#'
#' @return plotly object
#'
#' @examples zscore_chart(players,position,scoring)
#'
#' @export

zscore_chart <- function(players,position,scoring,fwd,pfrplayers){
  all_players <- players
  pos <- position
  scoring <- tolower(scoring)

  df <- zscore_df(pos,scoring,fwd,pfrplayers)

  playerdf <- df %>% filter(player2 %in% all_players)
  playerdf2 <- playerdf %>% select(player,zscore,age,position)
  playerdf3 <- melt(playerdf2,c("age","zscore","position"))
  posdf <- df %>% filter(position == pos, !is.na(zscore))
  s <- ggplot(data = playerdf3,aes(x=age,y=zscore,col=value)) + geom_line(size=1.5) +
    geom_point(data = posdf,aes(x=age,y=zscore),color = "black") +
    labs(y='Z-Score',title = paste0("Z-Scores by Age for ",pos),x="Age") +
    theme(title = element_text(size=14), axis.title=element_text(size=20), legend.title=element_blank())
  g <- plotly_build(s)
  for(i in 1:length(all_players)+1){
    g$x$data[[i]]$text <- paste("Player Name:", posdf$player, "<br>",
                                "Age:", posdf$age, "<br>",
                                "FanPts:", posdf$FP, "<br>",
                                "Z-Score:", posdf$zscore, "<br>",
                                "Percentile:", posdf$percentile)
  }
  final_prod <- g %>% layout(images = list(source =  "https://raw.githubusercontent.com/amazehayes/ffstats_navbar/master/www/DLF_Logo-2-black-80.png",
                                           xref = "paper",
                                           yref = "paper",
                                           x = 0,
                                           y = 1,
                                           sizex = 1,
                                           sizey = 1,
                                           opacity = 0.1,
                                           layer = "below"),
                             legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25)) %>%
    config(displayModeBar = FALSE) %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  return(final_prod)
}
