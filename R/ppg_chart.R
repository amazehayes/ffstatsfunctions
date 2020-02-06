#' @title PPG Chart
#'
#' @description creates ppg chart
#'
#' @param players a vector of players
#' @param years a vector of years
#' @param scoring ppr,halfppr,standard,sixpttd
#' @param con DB connection
#' @param pfrplayers pfr DB
#'
#' @return plotly object
#'
#' @examples ppg_chart(players,years,scoring,con,pfrplayers)
#'
#' @export

ppg_chart <- function(players,years,scoring,con,pfrplayers){

  years <- years
  years2 <- paste(years, collapse = ",")
  scoring <- tolower(scoring)
  v <- paste0("select year,player,week,position,player_id,",paste0(scoring),"
            from finalweeklydata where year in (", years2,")")
  df <- dbGetQuery(con, v)
  colnames(df)[6] <- "FP"
  players <- sort(c(players))
  playerids <- c()
  for(i in 1:length(players)){
    id <- pfrplayers$player_id[pfrplayers$player2 == players[i]]
    playerids <- c(playerids,id)
  }

  positions <- unique(df %>% select(player_id,player,position))
  df <- df %>% filter(player_id %in% playerids) %>% group_by(player_id, year) %>%
    summarise(avg = round(mean(FP),2), sd = round(sd(FP),2))
  df <- merge(df,positions, by = "player_id")
  df$Lower <- df$avg-df$sd
  df$Upper <- df$avg+df$sd
  df[df<0] <- 0
  df$year <- as.factor(as.character(df$year))
  df$year <- factor(df$year,levels = c(years))

  colorCount = length(unique(players))
  getPalette = colorRampPalette(brewer.pal(colorCount, "Dark2"))

  p <- ggplot(df, aes(x = year, y = avg, fill = player)) + geom_bar(stat = "identity", position=position_dodge()) +
    labs(title = paste0("Average Fantasy Points Per Game")) + xlab("Year") + ylab("Fantasy Points")  +
    geom_errorbar(aes(ymin=Lower, ymax=Upper), col = "black", width=.2, position=position_dodge(.9)) +
    scale_fill_manual(values=getPalette(colorCount), labels = unique(df$player)) + theme_classic() +
    ylim(0,mround(max(df$Upper),5)+5)
  g <- plotly_build(p)
  for(i in 1:length(players)){
    dfloop <- df %>% filter(player == players[i])
    g$x$data[[i]]$text <- paste0("Player: ", dfloop$player, "<br>",
                                 "Average: ", dfloop$avg, "<br>",
                                 "Std Dev: ", dfloop$sd, "<br>",
                                 "Upper: ", dfloop$Upper, "<br>",
                                 "Lower: ", dfloop$Lower)
  }
  for(i in 1:length(players)){
    g$x$data[[length(players)+i]]$text <- paste0()
  }
  for(i in 1:length(players)){
    dfloop <- df %>% filter(player == players[i])
    g$x$data[[i]]$name <- paste0(dfloop$player)
  }
  final_prod <- g %>% layout(images = list(source = "https://raw.githubusercontent.com/dlfootball/dlf-tools/master/www/DLF_Logo-2-black-80.png?token=AHI2LZE7YUTUT4CT3XICYQK6IXGJY",
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

  final_prod

  return(final_prod)

}
