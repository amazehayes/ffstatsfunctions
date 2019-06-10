#' @title Yearly Plot ggplot
#'
#' @description creates yearly plot
#'
#' @param pA
#' @param pB
#' @param pC
#' @param years
#' @param scoring
#'
#' @return NULL
#'
#' @examples yearly_comp_player_ggplot2(pA=NULL,pB=NULL,pC=NULL,years,scoring)
#'
#' @export

yearly_comp_player_ggplot2 <- function(pA=NULL,pB=NULL,pC=NULL,years,scoring){
  all_players <- c(pA,pB,pC)
  tt_p = create_title(pA,pB,pC)

  colors = RColorBrewer::brewer.pal(3,"Set1")
  colors = colors[1:length(all_players)]
  # requires "ggthemer
  total_df <- yearly_comp_player_df(pA,pB,pC,years,scoring)
  ggthemr::ggthemr("dust")
  s = ggplot(total_df,aes(x=Year,y=Rank,color=Player2,label=points_total)) +
    geom_point() +
    geom_line() +
    theme(title = element_text(size=14),
          axis.title=element_text(size=12),
          legend.title=element_blank(),legend.position='bottom') +
    scale_y_reverse(limits=c(25,0),breaks=seq(25,0,-3)) +
    scale_color_manual(values = colors) +
    labs(y='Position Finish',title = tt_p) +
    annotate(
      geom = "text",
      x = min(total_df$Year) + 1,
      y = 23.5,
      label = 'FFSTATISTICS',
      color = 'red',
      fontface = 'bold',
      size = 5,
      alpha = 0.5,
      family = 'Arial'
    )

  g <- plotly_build(s)

  len <- length(all_players)
  for(i in 1:len){
    p_name <- g$x$data[[i]]$name
    if(!(p_name %in% total_df$Player2)){
      next
    }
    dat <- total_df %>% filter(Player2 == p_name)
    g$x$data[[i]]$text <- paste("Player Name:", dat$Player2, "<br>",
                                "Rank:", dat$Rank, "<br>",
                                "Year:", dat$Year, "<br>",
                                "Fantasy Points Total:", dat$points_total)
  }

  final_prod <- g %>% layout(legend = list(orientation = "h",
                                           xanchor = 'center', # show entries horizontally
                                           x=0.5, y=0.15))
  return(final_prod)
}
