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
  all_players <- sort(players)
  pos <- position
  scoring <- tolower(scoring)

  df <- zscore_df(pos,scoring,fwd,pfrplayers)

  playerdf <- df %>% filter(player2 %in% all_players)
  playerdf2 <- playerdf %>% select(player,zscore,age,position)
  playerdf3 <- melt(playerdf2,c("age","zscore","position"))
  posdf <- df %>% filter(position == pos, !is.na(zscore))

  getPalette = colorRampPalette(brewer.pal(length(all_players), "Dark2"))

  s <- ggplot(data = playerdf3, aes(x = age, y = zscore, col = value)) + geom_line(size = 2) +
    scale_color_manual(values = getPalette(length(all_players)), name = "") +
    geom_point(data = posdf, aes(x = age, y = zscore), color = "black") + theme_classic() +
    labs(y='Z-Score', title = paste0("Z-Scores by Age for ",pos), x="Age") + scale_y_continuous(breaks = c(-3,-2,-1,0,1,2,3)) +
    scale_x_continuous(breaks = unique(sort(posdf$age)))
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
                                           x = 0.9,
                                           y = 1.08,
                                           sizex = 0.1,
                                           sizey = 0.1,
                                           opacity = 0.1,
                                           layer = "below"),
                             legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25)) %>%
    config(displayModeBar = FALSE) %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  return(final_prod)
}
