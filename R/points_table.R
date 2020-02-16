#' @title Points Table
#'
#' @description generates points table
#'
#' @param position position
#' @param years a vector of years
#' @param con DB connection
#' @param pfrplayers pfr DB
#' @param minpassatt number
#' @param minrushattQB number
#' @param minrushattRB number
#' @param mintargetRB number
#' @param mintargetWR number
#' @param mintargetTE number
#'
#' @return dataframe
#'
#' @examples points_table(position,years,con,pfrplayers,minpassatt,minrushattQB,minrushattRB,mintargetRB,mintargetWR,mintargetTE)
#'
#' @export

points_table <- function(position,years,con,pfrplayers,minpassatt,minrushattQB,minrushattRB,mintargetRB,mintargetWR,mintargetTE){
  years <- years
  years2 <- paste(years,collapse = ",")
  p <- position

  if(p == "QB"){
    v <- paste0("select player_id,player,passatt,passyards,passtd,rushatt,rushyards,rushtd,ppr
                from finalweeklydata where year in (",years2,") and position = 'QB'")
    fwd <- dbGetQuery(con,v)
    fwd$fpnew <- (0.04*fwd$passyards) + (4*fwd$passtd) + (0.1*fwd$rushyards) + (6*fwd$rushtd)
    minRA <- minrushattQB
    minPA <- minpassatt

    df <- fwd %>% group_by(player_id,player) %>% summarise_all(sum) %>% filter(rushatt >= minRA, passatt >= minPA) %>% ungroup() %>%
      mutate(py_perc = round((passyards*0.04)/fpnew*100,2),
             ptd_perc = round((passtd*4)/fpnew*100,2),
             ry_perc = round((rushyards*0.1)/fpnew*100,2),
             rtd_perc = round((rushtd*6)/fpnew*100,2),
             py_rank = rank(-py_perc, ties.method = "first"),
             ptd_rank = rank(-ptd_perc, ties.method = "first"),
             ry_rank = rank(-ry_perc, ties.method = "first"),
             rtd_rank = rank(-rtd_perc, ties.method = "first")) %>% arrange(ptd_rank)
    df <- df[,c(2:9,11:ncol(df))]
    colnames(df) <- c("Player","PassAtt","PassYards","PassTD","RushAtt","RushYards","RushTD","FanPts",
                      "PassYards%","PassTD%","RushYard%","RushTD%",
                      "PassYards%_Rank","PassTD%_Rank","RushYard%_Rank","RushTD%_Rank")
  }

  if(p == "RB"){
    v <- paste0("select player_id,player,rushatt,rushyards,rushtd,targets,receptions,recyards,rectd,ppr
                from finalweeklydata where year in (",years2,") and position = 'RB'")
    fwd <- dbGetQuery(con,v)
    fwd$fpnew <- (0.1*fwd$rushyards) + (6*fwd$rushtd) + (0.1*fwd$recyards) + (6*fwd$rectd) + fwd$receptions
    minRA <- minrushattRB
    minT <- mintargetRB

    df <- fwd %>% group_by(player_id,player) %>% summarise_all(sum) %>% filter(rushatt >= minRA, targets >= minT) %>% ungroup() %>%
      mutate(ry_perc = round((rushyards*0.1)/ppr*100,2),
             rtd_perc = round((rushtd*6)/ppr*100,2),
             rec_perc = round((receptions)/ppr*100,2),
             recy_perc = round((recyards*0.1)/ppr*100,2),
             rectd_perc = round((rectd*6)/ppr*100,2),
             ry_rank = rank(-ry_perc, ties.method = "first"),
             rtd_rank = rank(-rtd_perc, ties.method = "first"),
             rec_rank = rank(-rec_perc, ties.method = "first"),
             recy_rank = rank(-recy_perc, ties.method = "first"),
             rectd_rank = rank(-rectd_perc, ties.method = "first")) %>% arrange(rtd_rank)
    df <- df[,c(2:10,12:ncol(df))]
    colnames(df) <- c("Player","RushAtt","RushYards","RushTD","Targets","Receptions","RecYards","RecTD","FanPts",
                      "RushYard%","RushTD%","Recs%","RecYards%","RecTD%",
                      "RushYard%_Rank","RushTD%_Rank","Recs%_Rank","RecYards%_Rank","RecTD%_Rank")
  }

  if(p == "WR"){
    v <- paste0("select player_id,player,targets,receptions,recyards,rectd,ppr
                from finalweeklydata where year in (",years2,") and position = 'WR'")
    fwd <- dbGetQuery(con,v)
    fwd$fpnew <- (0.1*fwd$recyards) + (6*fwd$rectd) + fwd$receptions
    minT <- mintargetWR

    df <- fwd %>% group_by(player_id,player) %>% summarise_all(sum) %>% filter(targets >= minT) %>% ungroup() %>%
      mutate(rec_perc = round((receptions)/ppr*100,2),
             recy_perc = round((recyards*0.1)/ppr*100,2),
             rectd_perc = round((rectd*6)/ppr*100,2),
             rec_rank = rank(-rec_perc, ties.method = "first"),
             recy_rank = rank(-recy_perc, ties.method = "first"),
             rectd_rank = rank(-rectd_perc, ties.method = "first")) %>% arrange(rectd_rank)
    df <- df[,c(2:7,9:ncol(df))]
    colnames(df) <- c("Player","Targets","Receptions","RecYards","RecTD","FanPts",
                      "Recs%","RecYards%","RecTD%","Recs%_Rank","RecYards%_Rank","RecTD%_Rank")
  }

  if(p == "TE"){
    v <- paste0("select player_id,player,targets,receptions,recyards,rectd,ppr
                from finalweeklydata where year in (",years2,") and position = 'TE'")
    fwd <- dbGetQuery(con,v)
    fwd$fpnew <- (0.1*fwd$recyards) + (6*fwd$rectd) + fwd$receptions
    minT <- mintargetTE

    df <- fwd %>% group_by(player_id,player) %>% summarise_all(sum) %>% filter(targets >= minT) %>% ungroup() %>%
      mutate(rec_perc = round((receptions)/ppr*100,2),
             recy_perc = round((recyards*0.1)/ppr*100,2),
             rectd_perc = round((rectd*6)/ppr*100,2),
             rec_rank = rank(-rec_perc, ties.method = "first"),
             recy_rank = rank(-recy_perc, ties.method = "first"),
             rectd_rank = rank(-rectd_perc, ties.method = "first")) %>% arrange(rectd_rank)
    df <- df[,c(2:7,9:ncol(df))]
    colnames(df) <- c("Player","Targets","Receptions","RecYards","RecTD","FanPts",
                      "Recs%","RecYards%","RecTD%","Recs%_Rank","RecYards%_Rank","RecTD%_Rank")
  }

  return(df)
}
