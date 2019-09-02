#' @title Stats by Week Range Table
#'
#' @description creates stats by week range table
#'
#' @param stat a football stat
#' @param years a vector of years
#' @param weeks vector of weeks
#' @param con DB connection
#'
#' @return datatable
#'
#' @examples week_range(stat,years,weeks,con)
#'
#' @export

week_range <- function(stat,years,weeks,con){

  filter <- dplyr::filter
  select <- dplyr::select
  summarize <- dplyr::summarize
  summarise_all <- dplyr::summarise_all
  count <-dplyr::count
  gather <- tidyr::gather

  stat <- stat
  years <- years
  weeks <- weeks
  if(stat == "snaps"){
    stat <- "scoffnum"
  }
  v <- paste0("select year,player_id,player,position,games,week,",paste0(stat),"
              from finalweeklydata where year in (", years,")")
  fwd <- dbGetQuery(con, v)
  weeks_all <- 1:17
  weeks_other <- setdiff(weeks_all,weeks)

  if(length(weeks_other) == 0){
    dfweeks <- fwd %>% filter(year %in% years, week %in% weeks) %>% select(year,player_id,player,position,games,stat) %>%
      group_by(year,player_id,player,position) %>% summarise_all(sum)
    colnames(dfweeks)[6] <- "stat"
    dfweeks <- dfweeks %>% group_by(position) %>% mutate(posrank = rank(-stat, ties.method = "first"))
    dfweeks$games_total <- dfweeks$games
    dfweeks$stat_total <- dfweeks$stat
    dfweeks <- dfweeks %>% group_by(position) %>% mutate(posrank_total = rank(-stat_total, ties.method = "first"))
    dfweeks <- dfweeks %>% arrange(-stat)

    if(stat == "scoffnum"){
      stat <- "snaps"
    }

    df <- dfweeks[,c(1,3:10)]
    colnames(df)[5] <- paste0(stat)
    colnames(df)[8] <- paste0(stat,"_total")
  }

  if(length(weeks_other) > 0){
    dfweeks <- fwd %>% filter(year %in% years, week %in% weeks) %>% select(year,player_id,player,position,games,stat) %>%
      group_by(year,player_id,player,position) %>% summarise_all(sum)
    colnames(dfweeks)[6] <- "stat"
    dfweeks <- dfweeks %>% group_by(position) %>% mutate(posrank = rank(-stat, ties.method = "first"))

    dfweeks_other <- fwd %>% filter(year %in% years, week %in% weeks_other) %>% select(year,player_id,player,position,games,stat) %>%
      group_by(year,player_id,player,position) %>% summarise_all(sum)
    colnames(dfweeks_other)[6] <- "stat"
    dfweeks_other <- dfweeks_other %>% group_by(position) %>% mutate(posrank = rank(-stat, ties.method = "first"))

    dfweeks <- merge(dfweeks,dfweeks_other,by = c("year","player_id","player","position"))
    colnames(dfweeks)[5:10] <- c("games_in","stat_in","posrank_in","games_out","stat_out","posrank_out")
    dfweeks$games_total <- dfweeks$games_in + dfweeks$games_out
    dfweeks$stat_total <- dfweeks$stat_in + dfweeks$stat_out
    dfweeks <- dfweeks %>% group_by(position) %>% mutate(posrank_total = rank(-stat_total, ties.method = "first"))
    dfweeks$stat_in_percentage <- round(dfweeks$stat_in/dfweeks$stat_total*100,2)
    dfweeks$stat_in_percentage <- gsub(NaN,0,dfweeks$stat_in_percentage)
    dfweeks$stat_in_percentage <- gsub(Inf,0,dfweeks$stat_in_percentage)
    dfweeks$stat_in_percentage <- as.numeric(dfweeks$stat_in_percentage)

    dfweeks <- dfweeks %>% arrange(-stat_in)

    if(stat == "scoffnum"){
      stat <- "snaps"
    }

    df <- dfweeks[,c(1,3:7,14,8:13)]
    colnames(df)[5] <- paste0(stat,"_in")
    colnames(df)[9] <- paste0(stat,"_out")
    colnames(df)[12] <- paste0(stat,"_total")
  }

  return(df)

}
