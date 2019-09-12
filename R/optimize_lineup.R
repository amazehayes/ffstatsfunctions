#' @title Lineup Optimizer
#'
#' @description optimizes DFS lineup
#'
#' @param dat DFS datatable
#' @param budget_in budget price
#' @param qb_fixed QB to optimize around
#' @param rb_fixed RB to optimize around
#' @param wr_fixed WR to optimize around
#' @param te_fixed TE to optimize around
#' @param dst_fixed DST to optimize around
#'
#' @return datatable
#'
#' @examples optimize_lineup(dat, budget_in, qb_fixed, rb_fixed, wr_fixed, te_fixed, dst_fixed)
#'
#' @export

optimize_lineup <- function(dat, budget_in, qb_fixed, rb_fixed, wr_fixed, te_fixed, dst_fixed){

  names_to_check <- c("position", "player", "cost", "projected_points", "keep")
  missing_names <- setdiff(names_to_check, names(dat))

  if(length(missing_names)> 0){
    stop(paste0("You're' missing these columns in the input dataframe: " ,
                paste(missing_names, collapse = ', ')))
  }

  dat <- dat %>% filter(keep == 1) %>% mutate(proj_pts = projected_points, pos = position)
  pos_count <- list(QB=1,RB=2,WR=3,TE=1,DST=1)
  pos_count_max <- list(QB=1,RB=3,WR=4,TE=2,DST=1)
  constr_str <- list(QB='==',RB='==',WR='==',TE='==',DST='==')

  if(length(rb_fixed) == 0){
    rb_fixed <- NA
  }
  if(length(wr_fixed) == 0){
    wr_fixed <- NA
  }

  fixed_players <- list(
    #QB = 'Andrew Luck',
    QB = ifelse(qb_fixed == "", NA, qb_fixed),
    RB = rb_fixed,
    WR = wr_fixed,
    TE = ifelse(te_fixed == "", NA, te_fixed),
    DST = ifelse(dst_fixed == "", NA, dst_fixed)
    #DST = NULL
  )

  fixed_player_dat <- NULL
  fixed_player_cost <- 0

  for(i in names(fixed_players)){
    if(sum(is.na(fixed_players[[i]])) == 0){

      fixed_player_inds <-
        (dat$player %in% fixed_players[[i]]) & (dat$pos == i)


      fixed_player_cost <-
        fixed_player_cost + sum(dat$cost[fixed_player_inds])


      fixed_player_dat <-
        rbind(dat[fixed_player_inds, ], fixed_player_dat)

      dat <-
        dat[!fixed_player_inds, ]

      if(pos_count_max[[i]] == 1){
        dat <- dat[dat$pos %nin% i, ]
      }
      pos_count[[i]] <- pos_count[[i]] - length(fixed_players[[i]])
    }
  }


  budget <- budget_in - fixed_player_cost
  obj_fun <- matrix(dat$proj_pts , ncol = nrow(dat))
  constr <- matrix(dat$cost, ncol = nrow(dat))

  for(i in names(pos_count)){
    if(pos_count[[i]] > 0 | pos_count_max[[i]] > 1){
      constr <- rbind(constr, matrix(dat$pos == i, ncol = nrow(dat)))
    }
    else{
      constr_str[[i]] <- NULL
    }
  }

  models <- list()

  for(i in c('RB', 'WR', 'TE')){
    constr_val <- c(budget)
    for(j in names(constr_str)){
      if(i == j){
        constr_val <- c(constr_val, pos_count[[j]]+1)
      }
      else
        constr_val <- c(constr_val, pos_count[[j]])
    }
    constr_dir <- c('<=', unlist(constr_str))
    prod.sol <- lp("max", obj_fun, constr, constr_dir, constr_val, all.bin = TRUE)
    models[[i]] <- prod.sol
  }

  res <- list()
  max_val <- 0
  max_model <- 'No max, fam'

  for(i in names(models)){
    if(sum(!is.na(unlist(fixed_players))) > 0 ){
      res[[i]] <- rbind(dat[models[[i]][['solution']] > 0,],fixed_player_dat)
      if(max_val < sum(res[[i]]$proj_pts)){
        max_val <- sum(res[[i]]$proj_pts)
        max_model <- i
      }
    }
    else{
      res[[i]] <- dat[models[[i]][['solution']] > 0,]
      if(max_val < sum(res[[i]]$proj_pts)){
        max_val <- sum(res[[i]]$proj_pts)
        max_model <- i
      }
    }
  }
  res <- res[[max_model]] %>% arrange(pos) %>% select(-keep, -proj_pts, -pos)
  total_res <- res %>% select(cost, projected_points) %>% summarize_all(.funs = sum) %>%
    as.data.frame(.) %>% mutate(position = '',player = 'Total')
  res <- res %>% bind_rows(total_res)
  return(res)
}
