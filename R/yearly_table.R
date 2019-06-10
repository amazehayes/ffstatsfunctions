#' @title Yearly Table
#'
#' @description generates yearly table
#'
#' @param pA
#' @param pB
#' @param pC
#' @param years
#' @param scoring
#'
#' @return NULL
#'
#' @examples yearly_comp_player_df(pA=NULL,pB=NULL,pC=NULL,years,scoring)
#'
#' @export

yearly_comp_player_df <- function(pA=NULL,pB=NULL,pC=NULL,years,scoring){
  all_players <- c(pA,pB,pC)
  # find proper columns
  scoring_str <- paste0('^',scoring,'$')
  temp_df <- finalWeeklyData %>% select(Year,Player2,Player,Position,Team,matches(scoring_str))
  colnames(temp_df)[6] <- "points"

  year_df <- temp_df %>%
    group_by(Year,Player2) %>%
    summarise(points_total = sum(points))

  positions <- unique(finalWeeklyData %>% select(Year, Player2, Position))
  year_df <- merge(year_df, positions, by = c("Year","Player2"))

  year_df <- year_df %>%
    group_by(Year,Position) %>%
    mutate(Rank = rank(-points_total, ties.method = "first"))

  total_df <- year_df %>% filter(Player2 %in% all_players & Year %in% years)
  return(total_df)
}
