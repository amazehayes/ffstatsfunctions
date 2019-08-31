#' @title PPO Receiving Plot
#'
#' @description creates receiving ppo plot
#'
#' @param players a vector of players
#' @param years a vector of years
#' @param scoring ppr,halfppr,standard,sixpttd
#' @param con DB connection
#' @param pfrplayers pfr DB
#'
#' @return plotly object
#'
#' @examples ppo_receiving_chart(players,years,scoring,con,pfrplayers)
#'
#' @export

ppo_receiving_chart <- function(players,years,scoring,con,pfrplayers){
  players <- sort(c(players))
  scoring <- tolower(scoring)
  years <- years
  years2 <- paste(years,collapse = ",")
  playerids <- c()
  for(i in 1:length(players)){
    id <- pfrplayers$player_id[pfrplayers$player2 == players[i]]
    playerids <- c(playerids,id)
  }
  v <- paste0("select year,player_id,player,position,rushatt,rushyards,rushtd,targets,receptions,recyards,rectd,ppr,halfppr,standard,sixpttd from finalweeklydata where position in ('RB','WR','TE') and year in (", years2,")")
  fwd <- dbGetQuery(con,v)

  positions <- unique(fwd %>% arrange(-year) %>% select(year,player_id,player,position))

  fwd <- fwd %>% select(year,player_id,player,rushatt,rushyards,rushtd,targets,receptions,recyards,rectd) %>%
    group_by(year,player_id,player) %>% summarise_all(sum)
  fwd$rushFP <- round((fwd$rushyards*0.1)+(6*fwd$rushtd),2)
  if(scoring == "ppr"){
    fwd$recFP <- round((fwd$recyards*0.1)+(6*fwd$rectd),2) + fwd$receptions
  }
  if(scoring == "halfppr"){
    fwd$recFP <- round((fwd$recyards*0.1)+(6*fwd$rectd),2) + (0.5*fwd$receptions)
  }
  if(scoring == "standard"){
    fwd$recFP <- round((fwd$recyards*0.1)+(6*fwd$rectd),2)
  }
  fwd$rushPPO <- round(fwd$rushFP/fwd$rushatt,2)
  fwd$recPPO <- round(fwd$recFP/fwd$targets,2)
  fwd$rushPPO <- gsub(NaN,0,fwd$rushPPO)
  fwd$recPPO <- gsub(NaN,0,fwd$recPPO)
  fwd$rushPPO <- gsub(Inf,0,fwd$rushPPO)
  fwd$recPPO <- gsub(Inf,0,fwd$recPPO)
  fwd$rushPPO <- as.numeric(fwd$rushPPO)
  fwd$recPPO <- as.numeric(fwd$recPPO)
  fwd <- merge(fwd,positions, by = c("year","player","player_id"))
  fwd <- fwd[,c(1:3,15,4:14)]

  df <- fwd %>% filter(player_id %in% playerids) %>% select(year,player_id,player,position,rushatt,targets,rushFP,recFP,rushPPO,recPPO)

  p <- ggplot(df,aes(x = year, y = recPPO, color = player)) + geom_line(stat = "identity") + geom_point() +
    ylab("Receiving Points Per Opportunity") + xlab("Year") +
    labs(title = paste0("Receiving Points Per Opportunity"))
  g <- ggplotly(p)
  for(i in 1:length(players)){
    dfloop <- df %>% filter(player == players[i])
    g$x$data[[i]]$text <- paste0("Player: ", dfloop$player, "<br>",
                                 "Year: ", dfloop$year, "<br>",
                                 "FanPts: ", dfloop$recFP, "<br>",
                                 "Targets: ", dfloop$targets, "<br>",
                                 "PPOpp: ", dfloop$recPPO, "<br>")
  }
  final_prod <- g %>% layout(images = list(source =  "https://raw.githubusercontent.com/amazehayes/ffstats_navbar/master/www/FFStats_BlackLogo_3.5x3.5.png",
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
