#' @title Yearly Plot
#'
#' @description creates yearly plot
#'
#' @param players a vector of players
#' @param years a vector of years
#' @param scoring ppr,halfppr,standard,sixpttd
#' @param con DB connection
#' @param pfrplayers pfr DB
#'
#' @return plotly object
#'
#' @examples yearly_player_chart(players,years,scoring,con,pfrplayers)
#'
#' @export

yearly_player_chart <- function(players,years,scoring,con,pfrplayers){
  players <- sort(c(players))
  years <- years
  scoring <- scoring
  tt_p = paste("Yearly Finishes for", paste(players, collapse = " & "))

  df <- yearly_player_df(players,years,con,pfrplayers)
  df <- df %>% select(year,player,position,scoring,paste0(scoring,"_rank"))
  mround <- function(x,base){
    base*round(x/base)
  }
  pmax <- mround(max(df[,5],na.rm = TRUE),5) + 5

  colorCount = length(unique(players))
  getPalette = colorRampPalette(brewer.pal(colorCount, "Dark2"))

  s = ggplot(df,aes(x=year,y=get(names(df)[5]),color=player)) +
    geom_point(size = 3) +
    geom_line(size = 1.5) +
    theme(title = element_text(size=14),
          axis.title=element_text(size=20),
          legend.title=element_blank()) +
    scale_y_reverse(limits=c(pmax,0)) + theme_classic() +
    scale_color_manual(values=getPalette(colorCount)) +
    scale_x_discrete(limits = years) +
    labs(y='Position Finish',title = tt_p,x="Year")

  g <- plotly_build(s)

  for(i in 1:length(players)){
    p_name <- g$x$data[[i]]$name
    if(!(p_name %in% df$player)){
      next
    }
    dat <- df %>% filter(player == p_name)
    g$x$data[[i]]$text <- paste("Player Name:", dat$player, "<br>",
                                "Rank:", as.vector(unlist(dat[,5])), "<br>",
                                "Year:", dat$year, "<br>",
                                "Fantasy Points Total:", as.vector(unlist(dat[,4])))
  }

  final_prod <- g %>% layout(images = list(source = "https://raw.githubusercontent.com/dlfootball/dlf-tools/master/www/DLF_Logo-2-black-80.png?token=AHI2LZH5KMZITAUIGBIMDCK6HRIIG",
                                           xref = "paper",
                                           yref = "paper",
                                           x = 0.9,
                                           y = 0.15,
                                           sizex = 0.15,
                                           sizey = 0.15,
                                           opacity = 0.05,
                                           layer = "below"),
                             legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25)) %>%
    config(displayModeBar = FALSE) %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  return(final_prod)
}
