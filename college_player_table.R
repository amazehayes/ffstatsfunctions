#' @title College Player Stats
#'
#' @description generates college stats table
#'
#' @param player player names
#' @param con DB connection
#' @param datatype gamelogs or year
#' @param combinedf table of metrics
#'
#' @return dataframe
#'
#' @examples get_player_stats(player, con, datatype, combinedf)
#'
#' @export

get_player_stats <- function(player, con, datatype, combinedf){
  players <- player
  pos <- combinedf$Position[combinedf$Player2 == players]
  pid <- combinedf %>% filter(Player2 == players) %>% select(College_ID)
  pid <- pid$College_ID

  if(pos == "QB"){
    columns <- c("player","id","gameno","year","date","school","home","opponent","win","passcomp","passatt","compperc","passtd",
                 "int","passerrating","rushatt","rushyards","ypc","rushtd")
    yearcolumns <- c("passcomp","passatt","passtd","int","rushatt","rushyards","rushtd")
    statcolumns <- c("rushatt","rushyards")
    teamcolumns <- c("rushatt_tt","rushyards_tt")
  }
  if(pos == "RB"){
    columns <- c("player","id","gameno","year","date","school","home","opponent","win","rushatt","rushyards","ypc","rushtd",
                 "receptions","recyards","ypr","rectd")
    yearcolumns <- c("rushatt","rushyards","rushtd","receptions","recyards","rectd")
    statcolumns <- c("rushatt","rushyards","receptions","recyards")
    teamcolumns <- c("rushatt_tt","rushyards_tt","receptions_tt","recyards_tt")
  }
  if(pos == "WR" | pos == "TE"){
    columns <- c("player","id","gameno","year","date","school","home","opponent","win","receptions","recyards","ypr","rectd")
    yearcolumns <- c("receptions","recyards","rectd")
    statcolumns <- c("receptions","recyards")
    teamcolumns <- c("receptions_tt","recyards_tt")
  }
  if(pos != "QB" & pos != "RB" & pos != "WR" & pos != "TE"){
    columns <- c("player","id","gameno","year","date","school","home","opponent","win","tackles","assisttackles","totaltackles",
                 "tfl","sacks","defint","intyards","inttd","passdefensed","fumblerec","fumbleyards","fumbletd","forcedfumble")
    yearcolumns <- c("tackles","assisttackles","totaltackles","tfl","sacks","defint","intyards","inttd",
                     "passdefensed","fumblerec","fumbleyards","fumbletd","forcedfumble")
    statcolumns <- c("tackles","sacks","int","forcedfumble")
    teamcolumns <- c("tackles_tt","sacks_tt","int_tt","forcedfumble_tt")
  }

  v <- paste0("select ",paste0(columns, collapse = ",")," from cfbgamelogs where id = '", pid,"'")
  playercfblogs <- dbGetQuery(con, v)

  player_school <- unique(playercfblogs$school)
  nc <- 3 + length(statcolumns)
  schooldf <- matrix(ncol = nc)
  colnames(schooldf) <- c("year","date","school",statcolumns)
  for(i in 1:length(player_school)){
    y <- playercfblogs %>% filter(school == player_school[i]) %>% select(year)
    y <- unique(y$year)
    y <- paste0("(",paste0(y, collapse = ","),")")
    v <- paste0("select year,date,school,",paste0(statcolumns, collapse = ",")," from cfbgamelogs where school = '",player_school[i],"' and year in ",y)
    dfloop <- dbGetQuery(con, v)
    schooldf <- rbind(schooldf,dfloop)
  }
  schooldf <- schooldf[2:nrow(schooldf),]

  totalstats <- schooldf %>% group_by(year,date,school) %>% summarise_all(sum) %>% arrange(-year)
  totalstats <- totalstats[,c(2,4:ncol(totalstats))]
  colnames(totalstats) <- c("date",teamcolumns)
  playercfblogs <- merge(playercfblogs,totalstats, by = "date")


  if(pos == "QB"){
    yearstats <- playercfblogs %>% select(year,player,school,yearcolumns,teamcolumns) %>% group_by(year,player,school) %>%
      summarise_all(sum) %>% arrange(-year) %>% mutate(ms.rushatt = round(rushatt/rushatt_tt*100,2),
                                                       ms.rushyards = round(rushyards/rushyards_tt*100,2)) %>%
      ungroup() %>%
      select(year,school,yearcolumns,ms.rushatt,ms.rushyards)
  }
  if(pos == "RB"){
    yearstats <- playercfblogs %>% select(year,player,school,yearcolumns,teamcolumns) %>% group_by(year,player,school) %>%
      summarise_all(sum) %>% arrange(-year) %>% mutate(ms.rushatt = round(rushatt/rushatt_tt*100,2),
                                                       ms.rushyards = round(rushyards/rushyards_tt*100,2),
                                                       ms.receptions = round(receptions/receptions_tt*100,2),
                                                       ms.recyards = round(recyards/recyards_tt*100,2)) %>%
      ungroup() %>%
      select(year,school,yearcolumns,ms.rushatt,ms.rushyards,ms.receptions,ms.recyards)
  }
  if(pos == "WR" | pos == "TE"){
    yearstats <- playercfblogs %>% select(year,player,school,yearcolumns,teamcolumns) %>% group_by(year,player,school) %>%
      summarise_all(sum) %>% arrange(-year) %>% mutate(ms.receptions = round(receptions/receptions_tt*100,2),
                                                       ms.recyards = round(recyards/recyards_tt*100,2)) %>%
      ungroup() %>%
      select(year,school,yearcolumns,ms.receptions,ms.recyards)
  }
  if(pos != "QB" & pos != "RB" & pos != "WR" & pos != "TE"){
    yearstats <- playercfblogs %>% select(year,player,school,yearcolumns,teamcolumns) %>% group_by(year,player,school) %>%
      summarise_all(sum) %>% arrange(-year) %>% mutate(ms.tackles = round(tackles/tackles_tt*100,2),
                                                       ms.sacks = round(sacks/sacks_tt*100,2),
                                                       ms.int = round(int/int_tt*100,2),
                                                       ms.forcedfumble = round(forcedfumble/forcedfumble_tt*100,2)) %>%
      ungroup() %>%
      select(year,school,yearcolumns,ms.tackles,ms.sacks,ms.int,ms.forcedfumble)
  }

  yearstats$age <- round(age_calc(dob, as.Date(paste0(yearstats$year,"-08-01")), units = "years"),1)
  yearstats <- yearstats[,c(1,2,ncol(yearstats),3:(ncol(yearstats)-1))]

  if(datatype == "gamelogs"){
    return(playercfblogs)
  }
  if(datatype == "year"){
    return(yearstats)
  }
}
