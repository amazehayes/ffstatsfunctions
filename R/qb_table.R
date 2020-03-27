#' @title QB Plot
#'
#' @description creates qb plot
#'
#' @param qb qb
#' @param position position
#' @param stat_name stat name
#' @param fwd database
#' @param pfrplayers database
#'
#' @return plotly object
#'
#' @examples get_qb_positional_chart(qb,position,stat_name,fwd,pfrplayers)
#'
#' @export

get_qb_positional_chart <- function(qb,position,stat_name,fwd, pfrplayers){
  qbA <- qb
  id <- as.character(pfrplayers %>% filter(position == "QB" & player == qbA) %>% select(player_id))
  pos <- position
  s <- stat_name
  s1 <- ifelse(s == "Fantasy Rank", "Fantasy Points", s)

  sinput <- c("Fantasy Rank","Pass Attempts","Passing Yards","Passing TD","Rush Attempts","Rushing Yards","Rushing TD",
              "Targets","Receptions","Receiving Yards","Receiving TD")
  sconvert <- c("ppr","passatt","passyards","passtd","rushatt","rushyards","rushtd","targets","receptions","recyards","rectd")
  si <- match(s, sinput)
  qbdfA <- fwd %>% filter(player_id == id)
  qbdffinal <- matrix(nrow = length(unique(fwd$year)), ncol = 3)
  rownames(qbdffinal) <- unique(fwd$year)
  colnames(qbdffinal) <- c("Name","Team","Games")
  for(i in 1:nrow(qbdffinal)){

    weeks <- unique(qbdfA$week[qbdfA$year == unique(fwd$year)[i]])
    qbdffinal[i,1] <- qbA

    c1 <- qbdfA %>% filter(year == unique(fwd$year)[i], player_id == id)

    if(nrow(c1) == 0){
      qbdffinal[i,2] <- NA
      qbdffinal[i,3] <- length(weeks)
    }
    if(nrow(c1) != 0){
      qbdffinal[i,2] <- unique(as.character(c1$team))
      qbdffinal[i,3] <- length(weeks)
    }
  }
  qbdffinal <- as.data.frame(qbdffinal)
  qbdffinal$Games <- as.numeric(qbdffinal$Games)
  qbdffinal$Year <- as.numeric(rownames(qbdffinal))
  qbyears <- max(qbdffinal$Year):min(qbdffinal$Year)

  if(pos == "RB"){
    qbdfvalues <- matrix(nrow = length(qbyears), ncol = 6)
    rownames(qbdfvalues) <- qbyears
    colnames(qbdfvalues) <- c("RB1","RB2","RRB1","RRB2","PRB1","PRB2")
    posv <- c("RB1","RB2")

    for(i in 1:nrow(qbdfvalues)){

      weeks <- unique(qbdfA$week[qbdfA$year == unique(fwd$year)[i]])
      teamA <- qbdffinal[i,2]

      if(is.na(teamA)){
        qbdfvalues[i,] <- NA
      }
      if(!is.na(teamA)){
        df <- fwd %>% filter(year == qbyears[i], week %in% weeks, team == teamA) %>%
          select(player,player_id,rushatt,games) %>% group_by(player_id,player) %>% summarise_all(sum)
        positions <- unique(fwd %>% filter(year == qbyears[i], week %in% weeks, team == teamA) %>%
                              select(player_id,position))
        df <- merge(df, positions, by = "player_id")
        df <- df %>% group_by(position) %>% mutate(Weeks_Pos_Rank = rank(-rushatt, ties.method = "first"))
        df$PositionTier <- paste0(df$position,df$Weeks_Pos_Rank)
        colnames(df)[3] <- "sort"

        df2 <- fwd %>% filter(year == qbyears[i], week %in% weeks) %>%
          select(player,player_id,sconvert[si],games) %>% group_by(player_id,player) %>% summarise_all(sum)
        positions2 <- unique(fwd %>% filter(year == qbyears[i], week %in% weeks) %>%
                               select(player_id,position))
        df2 <- merge(df2, positions2, by = "player_id")
        df2 <- df2 %>% group_by(position) %>% mutate(Weeks_Pos_Rank = rank(-get(sconvert[si]), ties.method = "first"))

        df <- merge(df,df2, by = "player_id")
        df <- df %>% filter(grepl("RB",PositionTier)) %>% select(player_id,player.x,PositionTier,sconvert[si],Weeks_Pos_Rank.y)

        for(j in 1:length(posv)){

          p <- paste0("P",posv[j])
          r <- paste0("R",posv[j])
          p1 <- df$player_id[df$PositionTier == posv[j]]

          qbdfvalues[i,posv[j]] <- df$Weeks_Pos_Rank.y[df$player_id == p1]
          qbdfvalues[i,p] <- df$player.x[df$player_id == p1]
          qbdfvalues[i,r] <- df[df$player_id == p1,4]

        }
      }

    }
    qbdfvalues <- as.data.frame(qbdfvalues)
    qbdffinal <- cbind(qbdffinal, qbdfvalues)
    qbdffinal[,4:8] <- sapply(qbdffinal[,4:8], as.numeric)

    plotdf <- qbdffinal %>% arrange(Year)
    getPalette = colorRampPalette(brewer.pal(2, "Dark2"))

    if(s == "Fantasy Rank"){
      p <- ggplot(data = plotdf, aes(x = Year, y = RB1)) + geom_line(size = 1.5, aes(color = "RB1")) +
        geom_point(size = 3, color = getPalette(2)[1]) +
        ylab(s) + xlab("Year") + theme_classic() +
        geom_hline(yintercept = mean(plotdf$RB1, na.rm = TRUE), color = "black", linetype = "dashed", size = 1) +
        geom_line(aes(x = Year, y = RB2, color = "RB2"), size = 1.5) +
        geom_point(aes(x = Year, y = RB2), size = 3, color = getPalette(2)[2]) +
        geom_hline(yintercept = mean(plotdf$RB2, na.rm = TRUE), color = "black", linetype = "dashed", size = 1) + scale_y_reverse() +
        scale_x_continuous(breaks = plotdf$Year) + labs(title = "") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_manual(name = "Legend", values = c(RB1 = getPalette(2)[1], RB2 = getPalette(2)[2]), labels = c("RB1","RB2"))
    }
    if(s != "Fantasy Rank"){
      p <- ggplot(data = plotdf, aes(x = Year, y = RRB1)) + geom_line(size = 1.5, aes(color = "RB1")) +
        geom_point(size = 3, color = getPalette(2)[1]) +
        ylab(s) + xlab("Year") + theme_classic() + labs(title = "") +
        geom_hline(yintercept = mean(plotdf$RRB1, na.rm = TRUE), color = "black", linetype = "dashed", size = 1) +
        geom_line(aes(x = Year, y = RRB2, color = "RB2"), size = 1.5) +
        geom_point(aes(x = Year, y = RRB2), size = 3, color = getPalette(2)[2]) +
        geom_hline(yintercept = mean(plotdf$RRB2, na.rm = TRUE), color = "black", linetype = "dashed", size = 1) +
        scale_x_continuous(breaks = plotdf$Year) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_manual(name = "Legend", values = c(RB1 = getPalette(2)[1], RB2 = getPalette(2)[2]), labels = c("RB1","RB2"))
    }
    g <- ggplotly(p)
    g$x$data[[2]]$text <- paste0("QB: ", qbA, "<br>",
                                 "Year: ", plotdf$Year, "<br>",
                                 "Position: RB1", "<br>",
                                 "Player: ", plotdf$PRB1, "<br>",
                                 s1,": ", plotdf$RRB1, "<br>",
                                 s1," Rank: ", plotdf$RB1, "<br>")
    g$x$data[[5]]$text <- paste0("QB: ", qbA, "<br>",
                                 "Year: ", plotdf$Year, "<br>",
                                 "Position: RB2", "<br>",
                                 "Player: ", plotdf$PRB2, "<br>",
                                 s1,": ", plotdf$RRB2, "<br>",
                                 s1," Rank: ", plotdf$RB2, "<br>")
    g$x$data[[3]]$text <- paste0("QB: ", qbA, "<br>",
                                 "Position: RB1", "<br>",
                                 "Stat: ", s1, "<br>",
                                 "Average: ",round(mean(plotdf$RRB1, na.rm = TRUE),2), "<br>")
    g$x$data[[6]]$text <- paste0("QB: ", qbA, "<br>",
                                 "Position: RB2", "<br>",
                                 "Stat: ", s1, "<br>",
                                 "Average: ",round(mean(plotdf$RRB2, na.rm = TRUE),2), "<br>")
    final_prod <- g %>% layout(images = list(source =  "https://raw.githubusercontent.com/amazehayes/ffstats_navbar/master/www/DLF_Logo-2-black-80.png",
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
    qbdfvalues <- matrix(nrow = length(qbyears), ncol = 9)
    rownames(qbdfvalues) <- qbyears
    colnames(qbdfvalues) <- c("WR1","WR2","WR3","RWR1","RWR2","RWR3","PWR1","PWR2","PWR3")
    posv <- c("WR1","WR2","WR3")

    for(i in 1:nrow(qbdfvalues)){

      weeks <- unique(qbdfA$week[qbdfA$year == unique(fwd$year)[i]])
      teamA <- qbdffinal[i,2]

      if(is.na(teamA)){
        qbdfvalues[i,] <- NA
      }
      if(!is.na(teamA)){
        df <- fwd %>% filter(year == qbyears[i], week %in% weeks, team == teamA) %>%
          select(player,player_id,targets,games) %>% group_by(player_id,player) %>% summarise_all(sum)
        positions <- unique(fwd %>% filter(year == qbyears[i], week %in% weeks, team == teamA) %>%
                              select(player_id,position))
        df <- merge(df, positions, by = "player_id")
        df <- df %>% group_by(position) %>% mutate(Weeks_Pos_Rank = rank(-targets, ties.method = "first"))
        df$PositionTier <- paste0(df$position,df$Weeks_Pos_Rank)
        colnames(df)[3] <- "sort"

        df2 <- fwd %>% filter(year == qbyears[i], week %in% weeks) %>%
          select(player,player_id,sconvert[si],games) %>% group_by(player_id,player) %>% summarise_all(sum)
        positions2 <- unique(fwd %>% filter(year == qbyears[i], week %in% weeks) %>%
                               select(player_id,position))
        df2 <- merge(df2, positions2, by = "player_id")
        df2 <- df2 %>% group_by(position) %>% mutate(Weeks_Pos_Rank = rank(-get(sconvert[si]), ties.method = "first"))

        df <- merge(df,df2, by = "player_id")
        df <- df %>% filter(grepl("WR",PositionTier)) %>% select(player_id,player.x,PositionTier,sconvert[si],Weeks_Pos_Rank.y)

        for(j in 1:length(posv)){

          p <- paste0("P",posv[j])
          r <- paste0("R",posv[j])
          p1 <- df$player_id[df$PositionTier == posv[j]]

          qbdfvalues[i,posv[j]] <- df$Weeks_Pos_Rank.y[df$player_id == p1]
          qbdfvalues[i,p] <- df$player.x[df$player_id == p1]
          qbdfvalues[i,r] <- df[df$player_id == p1,4]

        }
      }

    }
    qbdfvalues <- as.data.frame(qbdfvalues)
    qbdffinal <- cbind(qbdffinal, qbdfvalues)
    qbdffinal[,4:10] <- sapply(qbdffinal[,4:10], as.numeric)

    plotdf <- qbdffinal %>% arrange(Year)
    getPalette = colorRampPalette(brewer.pal(3, "Dark2"))

    if(s == "Fantasy Rank"){
      p <- ggplot(data = plotdf, aes(x = Year, y = WR1)) + geom_line(size = 1.5, aes(color = "WR1")) +
        geom_point(size = 3, color = getPalette(3)[1]) +
        ylab(s) + xlab("Year") + theme_classic() + labs(title = "") +
        geom_hline(yintercept = mean(plotdf$WR1, na.rm = TRUE), color = "black", linetype = "dashed", size = 1) +
        geom_line(aes(x = Year, y = WR2, color = "WR2"), size = 1.5) +
        geom_point(aes(x = Year, y = WR2), size = 3, color = getPalette(3)[2]) +
        geom_hline(yintercept = mean(plotdf$WR2, na.rm = TRUE), color = "black", linetype = "dashed", size = 1) +
        geom_line(aes(x = Year, y = WR3, color = "WR3"), size = 1.5) +
        geom_point(aes(x = Year, y = WR3), size = 3, color = getPalette(3)[3]) +
        geom_hline(yintercept = mean(plotdf$WR3, na.rm = TRUE), color = "black", linetype = "dashed", size = 1) + scale_y_reverse() +
        scale_x_continuous(breaks = plotdf$Year) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_manual(name = "Legend", values = c(WR1 = getPalette(3)[1], WR2 = getPalette(3)[2], WR3 = getPalette(3)[3]),
                           labels = c("WR1","WR2","WR3"))
    }
    if(s != "Fantasy Rank"){
      p <- ggplot(data = plotdf, aes(x = Year, y = RWR1)) + geom_line(size = 1.5, aes(color = "WR1")) +
        geom_point(size = 3, color = getPalette(3)[1]) +
        ylab(s) + xlab("Year") + theme_classic() + labs(title = "") +
        geom_hline(yintercept = mean(plotdf$RWR1, na.rm = TRUE), color = "black", linetype = "dashed", size = 1) +
        geom_line(aes(x = Year, y = RWR2, color = "WR2"), size = 1.5) +
        geom_point(aes(x = Year, y = RWR2), size = 3, color = getPalette(3)[2]) +
        geom_hline(yintercept = mean(plotdf$RWR2, na.rm = TRUE), color = "black", linetype = "dashed", size = 1) +
        geom_line(aes(x = Year, y = RWR3, color = "WR3"), size = 1.5) +
        geom_point(aes(x = Year, y = RWR3), size = 3, color = getPalette(3)[3]) +
        geom_hline(yintercept = mean(plotdf$RWR3, na.rm = TRUE), color = "black", linetype = "dashed", size = 1) +
        scale_x_continuous(breaks = plotdf$Year) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_manual(name = "Legend", values = c(WR1 = getPalette(3)[1], WR2 = getPalette(3)[2], WR3 = getPalette(3)[3]),
                           labels = c("WR1","WR2","WR3"))
    }
    g <- ggplotly(p)
    g$x$data[[2]]$text <- paste0("QB: ", qbA, "<br>",
                                 "Year: ", plotdf$Year, "<br>",
                                 "Position: WR1", "<br>",
                                 "Player: ", plotdf$PWR1, "<br>",
                                 s1,": ", plotdf$RWR1, "<br>",
                                 s1," Rank: ", plotdf$WR1, "<br>")
    g$x$data[[5]]$text <- paste0("QB: ", qbA, "<br>",
                                 "Year: ", plotdf$Year, "<br>",
                                 "Position: WR2", "<br>",
                                 "Player: ", plotdf$PWR2, "<br>",
                                 s1,": ", plotdf$RWR2, "<br>",
                                 s1," Rank: ", plotdf$WR2, "<br>")
    g$x$data[[8]]$text <- paste0("QB: ", qbA, "<br>",
                                 "Year: ", plotdf$Year, "<br>",
                                 "Position: WR3", "<br>",
                                 "Player: ", plotdf$PWR3, "<br>",
                                 s1,": ", plotdf$RWR3, "<br>",
                                 s1," Rank: ", plotdf$WR3, "<br>")
    g$x$data[[3]]$text <- paste0("QB: ", qbA, "<br>",
                                 "Position: WR1", "<br>",
                                 "Stat: ", s1, "<br>",
                                 "Average: ",round(mean(plotdf$RWR1, na.rm = TRUE),2), "<br>")
    g$x$data[[6]]$text <- paste0("QB: ", qbA, "<br>",
                                 "Position: WR2", "<br>",
                                 "Stat: ", s1, "<br>",
                                 "Average: ",round(mean(plotdf$RWR2, na.rm = TRUE),2), "<br>")
    g$x$data[[9]]$text <- paste0("QB: ", qbA, "<br>",
                                 "Position: WR3", "<br>",
                                 "Stat: ", s, "<br>",
                                 "Average: ",round(mean(plotdf$RWR3, na.rm = TRUE),2), "<br>")
    final_prod <- g %>% layout(images = list(source =  "https://raw.githubusercontent.com/amazehayes/ffstats_navbar/master/www/DLF_Logo-2-black-80.png",
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
    qbdfvalues <- matrix(nrow = length(qbyears), ncol = 6)
    rownames(qbdfvalues) <- qbyears
    colnames(qbdfvalues) <- c("TE1","TE2","RTE1","RTE2","PTE1","PTE2")
    posv <- c("TE1","TE2")

    for(i in 1:nrow(qbdfvalues)){

      weeks <- unique(qbdfA$week[qbdfA$year == unique(fwd$year)[i]])
      teamA <- qbdffinal[i,2]

      if(is.na(teamA)){
        qbdfvalues[i,] <- NA
      }
      if(!is.na(teamA)){
        df <- fwd %>% filter(year == qbyears[i], week %in% weeks, team == teamA) %>%
          select(player,player_id,targets,games) %>% group_by(player_id,player) %>% summarise_all(sum)
        positions <- unique(fwd %>% filter(year == qbyears[i], week %in% weeks, team == teamA) %>%
                              select(player_id,position))
        df <- merge(df, positions, by = "player_id")
        df <- df %>% group_by(position) %>% mutate(Weeks_Pos_Rank = rank(-targets, ties.method = "first"))
        df$PositionTier <- paste0(df$position,df$Weeks_Pos_Rank)
        colnames(df)[3] <- "sort"

        df2 <- fwd %>% filter(year == qbyears[i], week %in% weeks) %>%
          select(player,player_id,sconvert[si],games) %>% group_by(player_id,player) %>% summarise_all(sum)
        positions2 <- unique(fwd %>% filter(year == qbyears[i], week %in% weeks) %>%
                               select(player_id,position))
        df2 <- merge(df2, positions2, by = "player_id")
        df2 <- df2 %>% group_by(position) %>% mutate(Weeks_Pos_Rank = rank(-get(sconvert[si]), ties.method = "first"))

        df <- merge(df,df2, by = "player_id")
        df <- df %>% filter(grepl("TE",PositionTier)) %>% select(player_id,player.x,PositionTier,sconvert[si],Weeks_Pos_Rank.y)

        for(j in 1:length(posv)){

          p <- paste0("P",posv[j])
          r <- paste0("R",posv[j])
          p1 <- df$player_id[df$PositionTier == posv[j]]

          qbdfvalues[i,posv[j]] <- df$Weeks_Pos_Rank.y[df$player_id == p1]
          qbdfvalues[i,p] <- df$player.x[df$player_id == p1]
          qbdfvalues[i,r] <- df[df$player_id == p1,4]

        }
      }

    }
    qbdfvalues <- as.data.frame(qbdfvalues)
    qbdffinal <- cbind(qbdffinal, qbdfvalues)
    qbdffinal[,4:8] <- sapply(qbdffinal[,4:8], as.numeric)
    plotdf <- qbdffinal %>% arrange(Year)
    getPalette = colorRampPalette(brewer.pal(2, "Dark2"))

    if(s == "Fantasy Rank"){
      p <- ggplot(data = plotdf, aes(x = Year, y = TE1)) + geom_line(size = 1.5, aes(color = "TE1")) +
        geom_point(size = 3, color = getPalette(2)[1]) +
        ylab(s) + xlab("Year") + labs(title = "") + theme_classic() +
        geom_hline(yintercept = mean(plotdf$TE1, na.rm = TRUE), color = "black", linetype = "dashed", size = 1) +
        geom_line(aes(x = Year, y = TE2, color = "TE2"), size = 1.5) +
        geom_point(aes(x = Year, y = TE2), size = 3, color = getPalette(2)[2]) +
        geom_hline(yintercept = mean(plotdf$TE2, na.rm = TRUE), color = "black", linetype = "dashed", size = 1) + scale_y_reverse() +
        scale_x_continuous(breaks = plotdf$Year) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_manual(name = "Legend", values = c(TE1 = getPalette(2)[1], TE2 = getPalette(2)[2]), labels = c("TE1","TE2"))
    }
    if(s != "Fantasy Rank"){
      p <- ggplot(data = plotdf, aes(x = Year, y = RTE1)) + geom_line(size = 1.5, aes(color = "TE1")) +
        geom_point(size = 3, color = getPalette(2)[1]) +
        ylab(s) + xlab("Year") + labs(title = "") + theme_classic() +
        geom_hline(yintercept = mean(plotdf$RTE1, na.rm = TRUE), color = "black", linetype = "dashed", size = 1) +
        geom_line(aes(x = Year, y = RTE2, color = "TE2"), size = 1.5) +
        geom_point(aes(x = Year, y = RTE2), size = 3, color = getPalette(2)[2]) +
        geom_hline(yintercept = mean(plotdf$RTE2, na.rm = TRUE), color = "black", linetype = "dashed", size = 1) +
        scale_x_continuous(breaks = plotdf$Year) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_manual(name = "Legend", values = c(TE1 = getPalette(2)[1], TE2 = getPalette(2)[2]), labels = c("TE1","TE2"))
    }
    g <- ggplotly(p)
    g$x$data[[2]]$text <- paste0("QB: ", qbA, "<br>",
                                 "Year: ", plotdf$Year, "<br>",
                                 "Position: TE1", "<br>",
                                 "Player: ", plotdf$PTE1, "<br>",
                                 s1,": ", plotdf$RTE1, "<br>",
                                 s1," Rank: ", plotdf$TE1, "<br>")
    g$x$data[[5]]$text <- paste0("QB: ", qbA, "<br>",
                                 "Year: ", plotdf$Year, "<br>",
                                 "Position: TE2", "<br>",
                                 "Player: ", plotdf$PTE2, "<br>",
                                 s1,": ", plotdf$RTE2, "<br>",
                                 s1," Rank: ", plotdf$TE2, "<br>")
    g$x$data[[3]]$text <- paste0("QB: ", qbA, "<br>",
                                 "Position: TE1", "<br>",
                                 "Stat: ", s1, "<br>",
                                 "Average: ",round(mean(plotdf$RTE1, na.rm = TRUE),2), "<br>")
    g$x$data[[6]]$text <- paste0("QB: ", qbA, "<br>",
                                 "Position: TE2", "<br>",
                                 "Stat: ", s1, "<br>",
                                 "Average: ",round(mean(plotdf$RTE2, na.rm = TRUE),2), "<br>")
    final_prod <- g %>% layout(images = list(source =  "https://raw.githubusercontent.com/amazehayes/ffstats_navbar/master/www/DLF_Logo-2-black-80.png",
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
  return(final_prod)
}
