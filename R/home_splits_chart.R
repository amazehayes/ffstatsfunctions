#' @title Home Splits Chart
#'
#' @description creates home splits chart
#'
#' @param players player name
#' @param years year vector
#' @param scoring ppr,halfppr,standard,sixpttd
#' @param fwd a player database
#' @param pfrplayers PFR database
#'
#' @return plotly object
#'
#' @examples home_splits_chart(players,years,scoring,fwd,pfrplayers)
#'
#' @export

home_splits_chart <- function(players,years,scoring,fwd,pfrplayers){
  filter <- dplyr::filter

  select <- dplyr::select

  summarize <-dplyr::summarize

  count <- dplyr::count

  gather <- tidyr::gather

  spread <- tidyr::spread

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

  colorCount = 2
  getPalette = colorRampPalette(brewer.pal(colorCount, "Dark2"))

  s <- ggplot(df,aes(x = player, y = average, fill = home)) +
    geom_bar(position = "dodge", stat = "identity") + theme_classic() +
    labs(title = paste0("Home/Road Splits")) + xlab("Players") + ylab("Average") +
    ylim(0,mround(max(df$average),5)+5) + scale_fill_manual(values=getPalette(colorCount), labels = c("Away","Home"))
  g <- plotly_build(s)
  for(i in 0:1){
    df$home <- gsub("Away",0,df$home)
    df$home <- gsub("Home",1,df$home)
    dfloop <- df %>% filter(home == i)
    g$x$data[[i+1]]$text <- paste("Player Name:", dfloop$player, "<br>",
                                  "Home/Away: ", ifelse(i==0,"Away","Home"), "<br>",
                                  "Games:", dfloop$games, "<br>",
                                  "Points:", dfloop$FP, "<br>",
                                  "Average:", dfloop$average, "<br>")
  }
  final_prod <- g %>% layout(images = list(source =  "https://raw.githubusercontent.com/amazehayes/ffstats_navbar/master/www/DLF_Logo-2-black-80.png",
                                           xref = "paper",
                                           yref = "paper",
                                           x = 0.83,
                                           y = 1.08,
                                           sizex = 0.2,
                                           sizey = 0.2,
                                           opacity = 0.1,
                                           layer = "below"),
                             legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25)) %>%
    config(displayModeBar = FALSE) %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  return(final_prod)
}
