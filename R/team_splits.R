#' @title Team Splits Table
#'
#' @description creates team splits table
#'
#' @param player a player name
#' @param years year vector
#' @param fwd player database
#' @param pfrplayers PFR database
#'
#' @return datatable
#'
#' @examples team_splits(player,years,fwd,pfrplayers)
#'
#' @export

team_splits <- function(player,years,fwd,pfrplayers){
  player_pos_name <- player
  year_filter <- c(years)
  playerids <- pfrplayers$player_id[pfrplayers$player2 == player_pos_name]

  df <- clean_names(fwd) %>% mutate(split_col = paste0(week, year, team)) %>% filter(year %in% year_filter)
  split_groups <- df %>% filter(player_id == playerids) %>% distinct(split_col, team)
  df_split <- df %>% filter(team %in% unique(split_groups$team)) %>%
    mutate(in_split = ifelse(split_col %in% split_groups$split_col, 1,0))
  win_split <- df_split %>% distinct(split_col, win, teampts, in_split) %>%
    group_by(in_split) %>%
    summarize(win_perc = mean(win),
              average_points = mean(teampts),
              games = n()
    )
  team_split <- df_split %>% group_by(in_split) %>%
    summarize(
      run_pass_ratio = sum(rushatt)/sum(passatt),
      pass_perc  = sum(passatt)/(sum(passatt) + sum(rushatt)),
      run_perc  = sum(rushatt)/(sum(passatt) + sum(rushatt)),
      pass_tds = sum(passtd),
      rush_tds = sum(rushtd),
      pass_tds_perc = sum(passtd)/(sum(passtd) + sum(rushtd)),
      rush_tds_perc = sum(rushtd)/(sum(passtd) + sum(rushtd)),
      total_targets = sum(targets),
      total_touches = sum(rushatt) + sum(receptions),
      ypa = sum(recyards)/sum(passatt),
      ypc = sum(rushyards)/sum(rushatt),
      pass_yds_avg = mean(passyards),
      rush_yds_avg = mean(rushyards)
    )

  ## positional metrics
  positional_split_targets <- df_split %>% group_by(in_split, position) %>%
    summarize(targets = sum(targets)) %>% spread(position, targets)
  positional_split_touches <- df_split %>% group_by(in_split, position) %>%
    summarize(touches = sum(receptions) + sum(rushatt)) %>% spread(position, touches)
  names(positional_split_targets) <- gsub('in_split_targets', 'in_split',
                                          tolower(paste0(names(positional_split_targets), '_targets')))
  names(positional_split_touches) <- gsub('in_split_touches', 'in_split',
                                          tolower(paste0(names(positional_split_touches), '_touches')))

  ## merge everythign together and create ms metrics
  positional_split <- merge(positional_split_targets, positional_split_touches, by = 'in_split', all = TRUE)

  res <- merge(win_split, team_split, by = 'in_split', all =TRUE) %>%
    merge(., positional_split, by = 'in_split', all = TRUE) %>%
    mutate(rb_target_share = rb_targets/total_targets,
           wr_target_share = wr_targets/total_targets,
           te_target_share = te_targets/total_targets,
           rb_touch_share = rb_targets/total_touches,
           wr_touch_share = wr_targets/total_touches,
           te_touch_share = te_targets/total_touches) %>%
    select(-qb_targets, -rb_targets, -wr_targets, -te_targets, -total_targets,
           -qb_touches, -rb_touches, -wr_touches, -te_touches, -total_touches) %>%
    mutate_if(is.numeric, round, 2)

  if(1 %nin% unique(res$in_split)){
    temp <- res
    temp$in_split <- 1
    temp[, setdiff(names(temp), 'in_split')] <- 0
    res <-rbind(res, temp)
  }
  if(0 %nin% unique(res$in_split)){
    temp <- res
    temp$in_split <- 0
    temp[, setdiff(names(temp), 'in_split')] <- 0
    res <- rbind(res, temp)
  }

  res <- data.frame(res %>% arrange(-in_split) %>% t(.))
  names(res) <- c('in_split', 'out_of_split')
  res$metric <- row.names(res)
  row.names(res) <- NULL
  res <- res %>% mutate(player = player_pos_name) %>% select(player, metric, in_split, out_of_split)
  return(res)
}
