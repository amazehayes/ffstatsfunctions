#' @title PPO Raw Table
#'
#' @description creates ppo raw table
#'
#' @param years a vector of years
#' @param scoring ppr,halfppr,standard,sixpttd
#' @param con DB connection
#' @param pfrplayers pfr DB
#'
#' @return datatable
#'
#' @examples ppo_raw_table(years,scoring,con,pfrplayers)
#'
#' @export

ppo_raw_table <- function(years,scoring,con,pfrplayers){
  scoring <- tolower(scoring)
  years <- years
  years2 <- paste(years,collapse = ",")
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

  df <- fwd %>% select(year,player,position,rushatt,rushFP,rushPPO,targets,recFP,recPPO) %>%
    arrange(-year,-targets)
  return(df)
}
