#' @title Coaching Plot
#'
#' @description creates coach table or plot
#'
#' @param coach coach
#' @param position position
#' @param stat_name stat name
#' @param fwd database
#' @param output_type table or chart
#'
#' @return plotly object or datatable
#'
#' @examples get_coach_positional_chart(coach,position,stat_name,fwd,output_type)
#'
#' @export

get_coach_positional_chart <- function(coach,position,stat_name,fwd,output_type){
  coachA <- coach
  pos <- position
  s <- stat_name
  s1 <- ifelse(s == "Fantasy Rank", "Fantasy Points", s)

  sinput <- c("Fantasy Rank","Pass Attempts","Passing Yards","Passing TD","Rush Attempts","Rushing Yards","Rushing TD",
              "Targets","Receptions","Receiving Yards","Receiving TD")
  sconvert <- c("ppr","passatt","passyards","passtd","rushatt","rushyards","rushtd","targets","receptions","recyards","rectd")
  si <- match(s, sinput)
  coachdffinal <- coach_main_table(coachA,fwd)
  names(coachdffinal) <- gsub('[.]', '', tolower(names(coachdffinal)))
  coachyears <- max(coachdffinal$year):min(coachdffinal$year)

  if(pos == "QB"){
    coachdfvalues <- matrix(nrow = length(coachyears), ncol = 3)
    rownames(coachdfvalues) <- coachyears
    colnames(coachdfvalues) <- c("QB1","RQB1","PQB1")
    posv <- c("QB1")

    for(i in 1:nrow(coachdfvalues)){

      week1 <- coachdffinal[i,4]
      week2 <- coachdffinal[i,5]
      teamA <- coachdffinal[i,3]

      if(is.na(teamA)){
        coachdfvalues[i,] <- NA
      }
      if(!is.na(teamA)){
        df <- fwd %>% filter(year == coachyears[i], week >= week1, week <= week2, team == teamA) %>%
          select(player,player_id,passatt,games) %>% group_by(player_id,player) %>% summarise_all(sum)
        positions <- unique(fwd %>% filter(year == coachyears[i], week >= week1, week <= week2, team == teamA) %>%
                              select(player_id,position))
        df <- merge(df, positions, by = "player_id")
        df <- df %>% group_by(position) %>% mutate(Weeks_Pos_Rank = rank(-passatt, ties.method = "first"))
        df$PositionTier <- paste0(df$position,df$Weeks_Pos_Rank)
        colnames(df)[3] <- "sort"

        df2 <- fwd %>% filter(year == coachyears[i], week >= week1, week <= week2) %>%
          select(player,player_id,sconvert[si],games) %>% group_by(player_id,player) %>% summarise_all(sum)
        positions2 <- unique(fwd %>% filter(year == coachyears[i], week >= week1, week <= week2) %>%
                               select(player_id,position))
        df2 <- merge(df2, positions2, by = "player_id")
        df2 <- df2 %>% group_by(position) %>% mutate(Weeks_Pos_Rank = rank(-get(sconvert[si]), ties.method = "first"))

        df <- merge(df,df2, by = "player_id")
        df <- df %>% filter(grepl("QB",PositionTier)) %>% select(player_id,player.x,PositionTier,sconvert[si],Weeks_Pos_Rank.y)

        for(j in 1:length(posv)){

          p <- paste0("P",posv[j])
          r <- paste0("R",posv[j])
          p1 <- df$player_id[df$PositionTier == posv[j]]

          coachdfvalues[i,posv[j]] <- df$Weeks_Pos_Rank.y[df$player_id == p1]
          coachdfvalues[i,p] <- df$player.x[df$player_id == p1]
          coachdfvalues[i,r] <- df[df$player_id == p1,4]

        }
      }

    }
    coachdfvalues <- as.data.frame(coachdfvalues)
    coachdffinal <- cbind(coachdffinal, coachdfvalues)
    coachdffinal[,4:8] <- sapply(coachdffinal[,4:8], as.numeric)

    plotdf <- coachdffinal %>% arrange(year)
    getPalette = colorRampPalette(brewer.pal(3, "Dark2"))

    if(s == "Fantasy Rank"){
      p <- ggplot(data = plotdf, aes(x = year, y = QB1)) + geom_line(size = 1.5, color = getPalette(2)[1]) +
        geom_point(size = 3, color = getPalette(2)[1]) + scale_x_discrete(limits = plotdf$year) +
        scale_y_reverse() + ylab(s) + xlab("Year") + labs(title = paste(coachA,"QB1",s)) + theme_classic() +
        geom_hline(yintercept = mean(plotdf$QB1), color = "black", linetype = "dashed", size = 1)
    }
    if(s != "Fantasy Rank"){
      p <- ggplot(data = plotdf, aes(x = year, y = RQB1)) + geom_line(size = 1.5, color = getPalette(2)[1]) +
        geom_point(size = 3, color = getPalette(2)[1]) + scale_x_discrete(limits = plotdf$year) +
        ylab(s) + xlab("Year") + labs(title = paste(coachA,"QB1",s)) + theme_classic() +
        geom_hline(yintercept = mean(plotdf$RQB1), color = "black", linetype = "dashed", size = 1)
    }
    g <- ggplotly(p)
    g$x$data[[1]]$text <- paste0("Coach: ", coachA, "<br>",
                                 "Year: ", plotdf$year, "<br>",
                                 "Position: QB1", "<br>",
                                 "Player: ", plotdf$PQB1, "<br>",
                                 s1,": ", plotdf$RQB1, "<br>",
                                 s1," Rank: ", plotdf$QB1, "<br>")
    g$x$data[[2]]$text <- paste0("Coach: ", coachA, "<br>",
                                 "Position: QB1", "<br>",
                                 "Stat: ", s1, "<br>",
                                 "Average: ",round(mean(plotdf$RQB1),2), "<br>")
    final_prod <- g %>% layout(images = list(source =  "https://raw.githubusercontent.com/dlfootball/dlf-tools/master/www/DLF_Logo-2-black-80.png?token=AHI2LZE7YUTUT4CT3XICYQK6IXGJY",
                                             xref = "paper",
                                             yref = "paper",
                                             x = 0.87,
                                             y = 0.15,
                                             sizex = 0.15,
                                             sizey = 0.15,
                                             opacity = 0.1,
                                             layer = "below"),
                               legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25)) %>%
      config(displayModeBar = FALSE) %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  }
  if(pos == "RB"){
    coachdfvalues <- matrix(nrow = length(coachyears), ncol = 6)
    rownames(coachdfvalues) <- coachyears
    colnames(coachdfvalues) <- c("RB1","RB2","RRB1","RRB2","PRB1","PRB2")
    posv <- c("RB1","RB2")

    for(i in 1:nrow(coachdfvalues)){

      week1 <- coachdffinal[i,4]
      week2 <- coachdffinal[i,5]
      teamA <- coachdffinal[i,3]

      if(is.na(teamA)){
        coachdfvalues[i,] <- NA
      }
      if(!is.na(teamA)){
        df <- fwd %>% filter(year == coachyears[i], week >= week1, week <= week2, team == teamA) %>%
          select(player,player_id,rushatt,games) %>% group_by(player_id,player) %>% summarise_all(sum)
        positions <- unique(fwd %>% filter(year == coachyears[i], week >= week1, week <= week2, team == teamA) %>%
                              select(player_id,position))
        df <- merge(df, positions, by = "player_id")
        df <- df %>% group_by(position) %>% mutate(Weeks_Pos_Rank = rank(-rushatt, ties.method = "first"))
        df$PositionTier <- paste0(df$position,df$Weeks_Pos_Rank)
        colnames(df)[3] <- "sort"

        df2 <- fwd %>% filter(year == coachyears[i], week >= week1, week <= week2) %>%
          select(player,player_id,sconvert[si],games) %>% group_by(player_id,player) %>% summarise_all(sum)
        positions2 <- unique(fwd %>% filter(year == coachyears[i], week >= week1, week <= week2) %>%
                               select(player_id,position))
        df2 <- merge(df2, positions2, by = "player_id")
        df2 <- df2 %>% group_by(position) %>% mutate(Weeks_Pos_Rank = rank(-get(sconvert[si]), ties.method = "first"))

        df <- merge(df,df2, by = "player_id")
        df <- df %>% filter(grepl("RB",PositionTier)) %>% select(player_id,player.x,PositionTier,sconvert[si],Weeks_Pos_Rank.y)

        for(j in 1:length(posv)){

          p <- paste0("P",posv[j])
          r <- paste0("R",posv[j])
          p1 <- df$player_id[df$PositionTier == posv[j]]

          coachdfvalues[i,posv[j]] <- df$Weeks_Pos_Rank.y[df$player_id == p1]
          coachdfvalues[i,p] <- df$player.x[df$player_id == p1]
          coachdfvalues[i,r] <- df[df$player_id == p1,4]

        }
      }

    }
    coachdfvalues <- as.data.frame(coachdfvalues)
    coachdffinal <- cbind(coachdffinal, coachdfvalues)
    coachdffinal[,4:10] <- sapply(coachdffinal[,4:10], as.numeric)

    plotdf <- coachdffinal %>% arrange(year)
    getPalette = colorRampPalette(brewer.pal(2, "Dark2"))

    if(s == "Fantasy Rank"){
      p <- ggplot(data = plotdf, aes(x = year, y = RB1)) + geom_line(size = 1.5, color = getPalette(2)[1]) +
        geom_point(size = 3, color = getPalette(2)[1]) +
        ylab(s) + xlab("Year") + labs(title = paste(coachA,"RB1 & RB2",s)) + theme_classic() +
        geom_hline(yintercept = mean(plotdf$RB1), color = "black", linetype = "dashed", size = 1) +
        geom_line(aes(x = year, y = RB2), size = 1.5, color = getPalette(2)[2]) +
        geom_point(aes(x = year, y = RB2), size = 3, color = getPalette(2)[2]) +
        geom_hline(yintercept = mean(plotdf$RB2), color = "black", linetype = "dashed", size = 1) + scale_y_reverse() +
        scale_x_discrete(limits = plotdf$year)
    }
    if(s != "Fantasy Rank"){
      p <- ggplot(data = plotdf, aes(x = year, y = RRB1)) + geom_line(size = 1.5, color = getPalette(2)[1]) +
        geom_point(size = 3, color = getPalette(2)[1]) +
        ylab(s) + xlab("Year") + labs(title = paste(coachA,"RB1 & RB2",s)) + theme_classic() +
        geom_hline(yintercept = mean(plotdf$RRB1), color = "black", linetype = "dashed", size = 1) +
        geom_line(aes(x = year, y = RRB2), size = 1.5, color = getPalette(2)[2]) +
        geom_point(aes(x = year, y = RRB2), size = 3, color = getPalette(2)[2]) +
        geom_hline(yintercept = mean(plotdf$RRB2), color = "black", linetype = "dashed", size = 1) +
        scale_x_discrete(limits = plotdf$year)
    }
    g <- ggplotly(p)
    g$x$data[[1]]$text <- paste0("Coach: ", coachA, "<br>",
                                 "Year: ", plotdf$year, "<br>",
                                 "Position: RB1", "<br>",
                                 "Player: ", plotdf$PRB1, "<br>",
                                 s1,": ", plotdf$RRB1, "<br>",
                                 s1," Rank: ", plotdf$RB1, "<br>")
    g$x$data[[3]]$text <- paste0("Coach: ", coachA, "<br>",
                                 "Year: ", plotdf$year, "<br>",
                                 "Position: RB2", "<br>",
                                 "Player: ", plotdf$PRB2, "<br>",
                                 s1,": ", plotdf$RRB2, "<br>",
                                 s1," Rank: ", plotdf$RB2, "<br>")
    g$x$data[[2]]$text <- paste0("Coach: ", coachA, "<br>",
                                 "Position: RB1", "<br>",
                                 "Stat: ", s1, "<br>",
                                 "Average: ",round(mean(plotdf$RRB1),2), "<br>")
    g$x$data[[4]]$text <- paste0("Coach: ", coachA, "<br>",
                                 "Position: RB2", "<br>",
                                 "Stat: ", s1, "<br>",
                                 "Average: ",round(mean(plotdf$RRB2),2), "<br>")
    final_prod <- g %>% layout(images = list(source =  "https://raw.githubusercontent.com/dlfootball/dlf-tools/master/www/DLF_Logo-2-black-80.png?token=AHI2LZE7YUTUT4CT3XICYQK6IXGJY",
                                             xref = "paper",
                                             yref = "paper",
                                             x = 0.87,
                                             y = 0.15,
                                             sizex = 0.15,
                                             sizey = 0.15,
                                             opacity = 0.1,
                                             layer = "below"),
                               legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25)) %>%
      config(displayModeBar = FALSE) %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  }
  if(pos == "WR"){
    coachdfvalues <- matrix(nrow = length(coachyears), ncol = 9)
    rownames(coachdfvalues) <- coachyears
    colnames(coachdfvalues) <- c("WR1","WR2","WR3","RWR1","RWR2","RWR3","PWR1","PWR2","PWR3")
    posv <- c("WR1","WR2","WR3")

    for(i in 1:nrow(coachdfvalues)){

      week1 <- coachdffinal[i,4]
      week2 <- coachdffinal[i,5]
      teamA <- coachdffinal[i,3]

      if(is.na(teamA)){
        coachdfvalues[i,] <- NA
      }
      if(!is.na(teamA)){
        df <- fwd %>% filter(year == coachyears[i], week >= week1, week <= week2, team == teamA) %>%
          select(player,player_id,targets,games) %>% group_by(player_id,player) %>% summarise_all(sum)
        positions <- unique(fwd %>% filter(year == coachyears[i], week >= week1, week <= week2, team == teamA) %>%
                              select(player_id,position))
        df <- merge(df, positions, by = "player_id")
        df <- df %>% group_by(position) %>% mutate(Weeks_Pos_Rank = rank(-targets, ties.method = "first"))
        df$PositionTier <- paste0(df$position,df$Weeks_Pos_Rank)
        colnames(df)[3] <- "sort"

        df2 <- fwd %>% filter(year == coachyears[i], week >= week1, week <= week2) %>%
          select(player,player_id,sconvert[si],games) %>% group_by(player_id,player) %>% summarise_all(sum)
        positions2 <- unique(fwd %>% filter(year == coachyears[i], week >= week1, week <= week2) %>%
                               select(player_id,position))
        df2 <- merge(df2, positions2, by = "player_id")
        df2 <- df2 %>% group_by(position) %>% mutate(Weeks_Pos_Rank = rank(-get(sconvert[si]), ties.method = "first"))

        df <- merge(df,df2, by = "player_id")
        df <- df %>% filter(grepl("WR",PositionTier)) %>% select(player_id,player.x,PositionTier,sconvert[si],Weeks_Pos_Rank.y)

        for(j in 1:length(posv)){

          p <- paste0("P",posv[j])
          r <- paste0("R",posv[j])
          p1 <- df$player_id[df$PositionTier == posv[j]]

          coachdfvalues[i,posv[j]] <- df$Weeks_Pos_Rank.y[df$player_id == p1]
          coachdfvalues[i,p] <- df$player.x[df$player_id == p1]
          coachdfvalues[i,r] <- df[df$player_id == p1,4]

        }
      }

    }
    coachdfvalues <- as.data.frame(coachdfvalues)
    coachdffinal <- cbind(coachdffinal, coachdfvalues)
    coachdffinal[,4:12] <- sapply(coachdffinal[,4:12], as.numeric)

    plotdf <- coachdffinal %>% arrange(year)
    getPalette = colorRampPalette(brewer.pal(3, "Dark2"))

    if(s == "Fantasy Rank"){
      p <- ggplot(data = plotdf, aes(x = year, y = WR1)) + geom_line(size = 1.5, color = getPalette(2)[1]) +
        geom_point(size = 3, color = getPalette(2)[1]) +
        ylab(s) + xlab("Year") + labs(title = paste(coachA,"WR1 & WR2 & WR3",s)) + theme_classic() +
        geom_hline(yintercept = mean(plotdf$WR1), color = "black", linetype = "dashed", size = 1) +
        geom_line(aes(x = year, y = WR2), size = 1.5, color = getPalette(3)[2]) +
        geom_point(aes(x = year, y = WR2), size = 3, color = getPalette(3)[2]) +
        geom_hline(yintercept = mean(plotdf$WR2), color = "black", linetype = "dashed", size = 1) +
        geom_line(aes(x = year, y = WR3), size = 1.5, color = getPalette(3)[3]) +
        geom_point(aes(x = year, y = WR3), size = 3, color = getPalette(3)[3]) +
        geom_hline(yintercept = mean(plotdf$WR3), color = "black", linetype = "dashed", size = 1) + scale_y_reverse() +
        scale_x_discrete(limits = plotdf$year)
    }
    if(s != "Fantasy Rank"){
      p <- ggplot(data = plotdf, aes(x = year, y = RWR1)) + geom_line(size = 1.5, color = getPalette(3)[1]) +
        geom_point(size = 3, color = getPalette(3)[1]) +
        ylab(s) + xlab("Year") + labs(title = paste(coachA,"WR1 & WR2 & WR3",s)) + theme_classic() +
        geom_hline(yintercept = mean(plotdf$RWR1), color = "black", linetype = "dashed", size = 1) +
        geom_line(aes(x = year, y = RWR2), size = 1.5, color = getPalette(3)[2]) +
        geom_point(aes(x = year, y = RWR2), size = 3, color = getPalette(3)[2]) +
        geom_hline(yintercept = mean(plotdf$RWR2), color = "black", linetype = "dashed", size = 1) +
        geom_line(aes(x = year, y = RWR3), size = 1.5, color = getPalette(3)[3]) +
        geom_point(aes(x = year, y = RWR3), size = 3, color = getPalette(3)[3]) +
        geom_hline(yintercept = mean(plotdf$RWR3), color = "black", linetype = "dashed", size = 1) +
        scale_x_discrete(limits = plotdf$year)
    }
    g <- ggplotly(p)
    g$x$data[[1]]$text <- paste0("Coach: ", coachA, "<br>",
                                 "Year: ", plotdf$year, "<br>",
                                 "Position: WR1", "<br>",
                                 "Player: ", plotdf$PWR1, "<br>",
                                 s1,": ", plotdf$RWR1, "<br>",
                                 s1," Rank: ", plotdf$WR1, "<br>")
    g$x$data[[3]]$text <- paste0("Coach: ", coachA, "<br>",
                                 "Year: ", plotdf$year, "<br>",
                                 "Position: WR2", "<br>",
                                 "Player: ", plotdf$PWR2, "<br>",
                                 s1,": ", plotdf$RWR2, "<br>",
                                 s1," Rank: ", plotdf$WR2, "<br>")
    g$x$data[[5]]$text <- paste0("Coach: ", coachA, "<br>",
                                 "Year: ", plotdf$year, "<br>",
                                 "Position: WR3", "<br>",
                                 "Player: ", plotdf$PWR3, "<br>",
                                 s1,": ", plotdf$RWR3, "<br>",
                                 s1," Rank: ", plotdf$WR3, "<br>")
    g$x$data[[2]]$text <- paste0("Coach: ", coachA, "<br>",
                                 "Position: WR1", "<br>",
                                 "Stat: ", s1, "<br>",
                                 "Average: ",round(mean(plotdf$RWR1),2), "<br>")
    g$x$data[[4]]$text <- paste0("Coach: ", coachA, "<br>",
                                 "Position: WR2", "<br>",
                                 "Stat: ", s1, "<br>",
                                 "Average: ",round(mean(plotdf$RWR2),2), "<br>")
    g$x$data[[6]]$text <- paste0("Coach: ", coachA, "<br>",
                                 "Position: WR3", "<br>",
                                 "Stat: ", s, "<br>",
                                 "Average: ",round(mean(plotdf$RWR3),2), "<br>")
    final_prod <- g %>% layout(images = list(source =  "https://raw.githubusercontent.com/dlfootball/dlf-tools/master/www/DLF_Logo-2-black-80.png?token=AHI2LZGUMTTCLNWX5ZK727S6KHA26",
                                             xref = "paper",
                                             yref = "paper",
                                             x = 0.87,
                                             y = 0.15,
                                             sizex = 0.15,
                                             sizey = 0.15,
                                             opacity = 0.1,
                                             layer = "below"),
                               legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25)) %>%
      config(displayModeBar = FALSE) %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  }
  if(pos == "TE"){
    coachdfvalues <- matrix(nrow = length(coachyears), ncol = 6)
    rownames(coachdfvalues) <- coachyears
    colnames(coachdfvalues) <- c("TE1","TE2","RTE1","RTE2","PTE1","PTE2")
    posv <- c("TE1","TE2")

    for(i in 1:nrow(coachdfvalues)){

      week1 <- coachdffinal[i,4]
      week2 <- coachdffinal[i,5]
      teamA <- coachdffinal[i,3]

      if(is.na(teamA)){
        coachdfvalues[i,] <- NA
      }
      if(!is.na(teamA)){
        df <- fwd %>% filter(year == coachyears[i], week >= week1, week <= week2, team == teamA) %>%
          select(player,player_id,targets,games) %>% group_by(player_id,player) %>% summarise_all(sum)
        positions <- unique(fwd %>% filter(year == coachyears[i], week >= week1, week <= week2, team == teamA) %>%
                              select(player_id,position))
        df <- merge(df, positions, by = "player_id")
        df <- df %>% group_by(position) %>% mutate(Weeks_Pos_Rank = rank(-targets, ties.method = "first"))
        df$PositionTier <- paste0(df$position,df$Weeks_Pos_Rank)
        colnames(df)[3] <- "sort"

        df2 <- fwd %>% filter(year == coachyears[i], week >= week1, week <= week2) %>%
          select(player,player_id,sconvert[si],games) %>% group_by(player_id,player) %>% summarise_all(sum)
        positions2 <- unique(fwd %>% filter(year == coachyears[i], week >= week1, week <= week2) %>%
                               select(player_id,position))
        df2 <- merge(df2, positions2, by = "player_id")
        df2 <- df2 %>% group_by(position) %>% mutate(Weeks_Pos_Rank = rank(-get(sconvert[si]), ties.method = "first"))

        df <- merge(df,df2, by = "player_id")
        df <- df %>% filter(grepl("TE",PositionTier)) %>% select(player_id,player.x,PositionTier,sconvert[si],Weeks_Pos_Rank.y)

        for(j in 1:length(posv)){

          p <- paste0("P",posv[j])
          r <- paste0("R",posv[j])
          p1 <- df$player_id[df$PositionTier == posv[j]]

          coachdfvalues[i,posv[j]] <- df$Weeks_Pos_Rank.y[df$player_id == p1]
          coachdfvalues[i,p] <- df$player.x[df$player_id == p1]
          coachdfvalues[i,r] <- df[df$player_id == p1,4]

        }
      }

    }
    coachdfvalues <- as.data.frame(coachdfvalues)
    coachdffinal <- cbind(coachdffinal, coachdfvalues)
    coachdffinal[,4:10] <- sapply(coachdffinal[,4:10], as.numeric)
    plotdf <- coachdffinal %>% arrange(year)
    getPalette = colorRampPalette(brewer.pal(2, "Dark2"))

    if(s == "Fantasy Rank"){
      p <- ggplot(data = plotdf, aes(x = year, y = TE1)) + geom_line(size = 1.5, color = getPalette(2)[1]) +
        geom_point(size = 3, color = getPalette(2)[1]) +
        ylab(s) + xlab("Year") + labs(title = paste(coachA,"TE1 & TE2",s)) + theme_classic() +
        geom_hline(yintercept = mean(plotdf$TE1), color = "black", linetype = "dashed", size = 1) +
        geom_line(aes(x = year, y = TE2), size = 1.5, color = getPalette(2)[2]) +
        geom_point(aes(x = year, y = TE2), size = 3, color = getPalette(2)[2]) +
        geom_hline(yintercept = mean(plotdf$TE2), color = "black", linetype = "dashed", size = 1) + scale_y_reverse() +
        scale_x_discrete(limits = plotdf$year)
    }
    if(s != "Fantasy Rank"){
      p <- ggplot(data = plotdf, aes(x = year, y = RTE1)) + geom_line(size = 1.5, color = getPalette(2)[1]) +
        geom_point(size = 3, color = getPalette(2)[1]) +
        ylab(s) + xlab("Year") + labs(title = paste(coachA,"TE1 & TE2",s)) + theme_classic() +
        geom_hline(yintercept = mean(plotdf$RTE1), color = "black", linetype = "dashed", size = 1) +
        geom_line(aes(x = year, y = RTE2), size = 1.5, color = getPalette(2)[2]) +
        geom_point(aes(x = year, y = RTE2), size = 3, color = getPalette(2)[2]) +
        geom_hline(yintercept = mean(plotdf$RTE2), color = "black", linetype = "dashed", size = 1) +
        scale_x_discrete(limits = plotdf$year)
    }
    g <- ggplotly(p)
    g$x$data[[1]]$text <- paste0("Coach: ", coachA, "<br>",
                                 "Year: ", plotdf$year, "<br>",
                                 "Position: TE1", "<br>",
                                 "Player: ", plotdf$PTE1, "<br>",
                                 s1,": ", plotdf$RTE1, "<br>",
                                 s1," Rank: ", plotdf$TE1, "<br>")
    g$x$data[[3]]$text <- paste0("Coach: ", coachA, "<br>",
                                 "Year: ", plotdf$year, "<br>",
                                 "Position: TE2", "<br>",
                                 "Player: ", plotdf$PTE2, "<br>",
                                 s1,": ", plotdf$RTE2, "<br>",
                                 s1," Rank: ", plotdf$TE2, "<br>")
    g$x$data[[2]]$text <- paste0("Coach: ", coachA, "<br>",
                                 "Position: TE1", "<br>",
                                 "Stat: ", s1, "<br>",
                                 "Average: ",round(mean(plotdf$RTE1),2), "<br>")
    g$x$data[[4]]$text <- paste0("Coach: ", coachA, "<br>",
                                 "Position: TE2", "<br>",
                                 "Stat: ", s1, "<br>",
                                 "Average: ",round(mean(plotdf$RTE2),2), "<br>")
    final_prod <- g %>% layout(images = list(source =  "https://raw.githubusercontent.com/dlfootball/dlf-tools/master/www/DLF_Logo-2-black-80.png?token=AHI2LZE7YUTUT4CT3XICYQK6IXGJY",
                                             xref = "paper",
                                             yref = "paper",
                                             x = 0.87,
                                             y = 0.15,
                                             sizex = 0.15,
                                             sizey = 0.15,
                                             opacity = 0.1,
                                             layer = "below"),
                               legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25)) %>%
      config(displayModeBar = FALSE) %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  }

  if(output_type == "table"){
    return(coachdffinal)
  }
  if(output_type == "chart"){
    return(final_prod)
  }
}
