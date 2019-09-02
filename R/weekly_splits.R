#' @title Weekly Splits Table
#'
#' @description creates splits table
#'
#' @param fwd player database
#' @param player_pos_name player to check splits
#' @param measure_vars variables to check splits
#' @param year_filter vector of years
#' @param week_split vector of weeks
#' @param player_pos_split player to split on
#' @param team_split team to split on
#' @param opponent_split opponent to split on
#' @param gameday_split game day to split on
#' @param gamestart_time_split game time to split on
#' @param win_split win/loss to split on
#' @param posrank_year year to get posrank for projection
#' @param df_output dataframe to return
#' @param transpose_output default to TRUE
#' @param pfrplayers PFR player database
#'
#' @return dataframe
#'
#' @examples weekly_split(fwd,player_pos_name, measure_vars, year_filter, week_split, player_pos_split, home_away_split, team_split,opponent_split, gameday_split, gamestart_time_split, win_split,posrank_year = 2018, df_output, transpose_output = TRUE,pfrplayers)
#'
#' @export

weekly_split <- function(fwd,player_pos_name, measure_vars, year_filter,
                         week_split, player_pos_split, home_away_split, team_split,
                         opponent_split, gameday_split, gamestart_time_split, win_split,
                         posrank_year = 2018, df_output, transpose_output = TRUE,pfrplayers){
  filter <- dplyr::filter
  select <- dplyr::select
  summarize <-dplyr::summarize
  count <- dplyr::count
  gather <- tidyr::gather
  spread <- tidyr::spread

  measure_vars <- tolower(measure_vars)

  df <- clean_names(fwd) %>%
    mutate(split_col = paste0(year, week, team, home, opp, day, time, win)) %>%
    select(year, week, team, home, opp, day, time, win, position,
           measure_vars, player_id, player, split_col)

  if(any(tolower(player_pos_split) %in% c(NA, 'any'))){
    player_pos_split <- player_pos_name
  }

  if(any(tolower(team_split) %in% c(NA, 'all'))){
    team_split <- unique(df$team)
  }

  if(any(tolower(home_away_split) %in% c('either','both', NA))){
    home_away_split <- c(0:1)
  }

  if(any(tolower(win_split) %in% c('either','both', NA))){
    win_split <- c(0:1)
  }

  if(any(opponent_split %in% c(NA, 'any'))){
    opponent_split <- unique(df$opp)
  }

  if(any(gameday_split %in% c(NA, 'any'))){
    gameday_split <- sort(unique(df$day))
  }

  if(any(gamestart_time_split %in% c(NA, 'any'))){
    gamestart_time_split <- sort(unique(df$time))
  }

  playerid_name <- c()
  for(i in 1:length(player_pos_name)){
    id <- pfrplayers$player_id[pfrplayers$player2 == player_pos_name[i]]
    playerid_name <- c(playerid_name,id)
  }
  playerid_split <- c()
  for(i in 1:length(player_pos_split)){
    id <- pfrplayers$player_id[pfrplayers$player2 == player_pos_split[i]]
    playerid_split <- c(playerid_split,id)
  }

  split_groups <- df %>%
    filter(player_id == playerid_name,
           year %in% year_filter,
           week %in% week_split,
           team %in% team_split,
           home %in% home_away_split,
           opp %in% opponent_split,
           day %in% gameday_split,
           time %in% gamestart_time_split,
           win %in% win_split) %>%
    distinct(split_col)

  df_split <- df %>%
    filter(split_col %in% split_groups$split_col, player_id %in% playerid_split, year %in% year_filter) %>%
    mutate(in_split = 1) %>% select(split_col, in_split)

  df_player <- df %>% filter(player_id == playerid_name, year %in% year_filter)

  df_player <- merge(df_player, df_split, by = c('split_col'), all.x = TRUE)

  df_player$in_split <- na.is.zero(df_player$in_split)

  if(length(measure_vars) > 1){
    res <- ## create initial result - take means and totals, project ppr points
      df_player %>%
      group_by(in_split) %>%
      summarize_at(.vars = measure_vars,
                   .funs = c(mean="mean")) %>% #, total="sum")) %>%
      mutate_at(.funs = funs(projection = .*16),
                .vars = vars(paste0(measure_vars, "_mean"))) %>%
      mutate(player = player_pos_name)
  } else {
    res <- ## create initial result - take means and totals, project ppr points
      df_player %>%
      group_by(in_split) %>%
      summarize_at(.vars = measure_vars,
                   .funs = c(mean="mean")) %>% #, total="sum")) %>%
      mutate_at(.funs = funs(projection = .*16),
                .vars = vars("mean")) %>%
      mutate(player = player_pos_name)
    res[, paste0(measure_vars, "_mean")] <- res$mean
    res[, paste0(measure_vars, '_mean_projection')] <- res$projection
    #res[, paste0(measure_vars, "_total")] <- res$sum
    res$mean <- NULL
    res$projection <- NULL
  }

  ## need to account for variables that don't need to be projected ie rams and tms
  for(i in c('msrush' ,'mstarget')){
    if(i %in% measure_vars){
      res[, paste0(i, '_mean_projection')] <- res[, paste0(i, '_mean')]
    }
  }

  df_count <- df_player %>% group_by(in_split) %>% summarize(games = n())

  res <- merge(df_count, res, by = 'in_split', all.x = TRUE)


  ## fill in gaps for missing data
  if(nrow(res) == 0){
    res <- na.is.zero(res %>% bind_rows(data.frame(in_split = c(0,1))))
  }

  if(!(1 %in% unique(res$in_split))){
    temp <- res
    temp$in_split <- 1
    temp[, setdiff(names(temp), 'in_split')] <- 0
    res <- rbind(res, temp)
  }

  ## fill in gaps for missing data

  if(!(0 %in% unique(res$in_split))){
    temp <- res
    temp$in_split <- 0
    temp[, setdiff(names(temp), 'in_split')] <- 0
    res <- rbind(res, temp)
  }

  posrankvars <- c('ppr', 'halfppr', 'standard', 'fourpttd', 'sixpttd')

  for(i in intersect(posrankvars, measure_vars)){

    pos <- unique(as.character(pfrplayers$position[pfrplayers$player_id == playerid_name]))
    i_mean_proj <- paste0(i,"_mean_projection")
    df$points <- df[,i]

    df_posrank <- df %>% filter(year == posrank_year, position == pos, week %in% c(1:17),
                                !(player_id %in% c(playerid_name))) %>%
      group_by(player_id) %>%
      summarize(total_in_split_1 = sum(points),
                total_in_split_0 = sum(points)) %>% ## bind player projected pts
      bind_rows(
        data.frame(player_id = playerid_name,
                   total_in_split_1 = na.is.zero(res[res$in_split==1, i_mean_proj]),
                   total_in_split_0 = na.is.zero(res[res$in_split==0, i_mean_proj])
        )
      ) %>% ## add in rank columns
      mutate(
        pos_rank_in_split_1 = dense_rank(-total_in_split_1),
        pos_rank_in_split_0 = dense_rank(-total_in_split_0)) %>%
      filter(player_id==playerid_name) %>% ## filter down to player and select columns
      select(pos_rank_in_split_1, pos_rank_in_split_0)

    df_posrank <- data.frame(t(df_posrank))

    names(df_posrank) <- paste0(i, '_posrank_projection')

    df_posrank$in_split <- ifelse(row.names(df_posrank) == 'pos_rank_in_split_1', 1, 0)

    res <- merge(res, df_posrank, by = 'in_split', all.x = TRUE)

  }

  ## Round it all !
  res <- res %>% mutate_if(is.numeric, round, 2)

  ## transpose data frame for easy display if TRUE
  if(transpose_output){
    res <- data.frame(res %>% arrange(-in_split) %>% t(.))
    names(res) <- c('in_split', 'out_of_split')
    res$metric <- row.names(res)
    row.names(res) <- NULL

    res <- res %>% filter(metric != 'player') %>%
      mutate(player = player_pos_name) %>% select(player, metric, in_split, out_of_split)
  }

  df_player <- df_player %>% select(player,position,year,week,home,opp,day,time,win,measure_vars,in_split)

  df_player_in <- df_player %>% filter(in_split == 1)
  df_player_out <- df_player %>% filter(in_split == 0)

  cmean_in <- round(colMeans(df_player_in[,measure_vars]),2)
  cnumber_in <- match(measure_vars,names(df_player_in))
  avg_in <- c("Average",rep(NA,ncol(df_player_in)-1))
  avg_in[cnumber_in] <- cmean_in
  df_player_in[nrow(df_player_in)+1,] <- avg_in

  cmean_out <- round(colMeans(df_player_out[,measure_vars]),2)
  cnumber_out <- match(measure_vars,names(df_player_out))
  avg_out <- c("Average",rep(NA,ncol(df_player_out)-1))
  avg_out[cnumber_out] <- cmean_out
  df_player_out[nrow(df_player_out)+1,] <- avg_out

  df_player_in <- df_player %>% arrange(year,week)
  df_player_out <- df_player %>% arrange(year,week)

  if(df_output == "split"){
    return(res)
  }
  if(df_output == "in_split_log"){
    return(df_player_in)
  }
  if(df_output == "out_split_log"){
    return(df_player_out)
  }

}
