#' @title Coaching Table
#'
#' @description generates coaching table
#'
#' @param coach
#' @param s
#'
#' @return NULL
#'
#' @examples coaching_table(coach,s)
#'
#' @export

coaching_table <- function(coach,s){

  coachA <- coach
  s <- s
  coachdfA <- fwd %>% filter(HC == coachA | OC == coachA)

  if(s == "Fantasy Rank"){

    coachdffinal <- matrix(nrow = length(unique(fwd$Year)), ncol = 5)
    rownames(coachdffinal) <- unique(fwd$Year)
    colnames(coachdffinal) <- c("Name","Coach","Team","WeekFirst","WeekLast")
    for(i in 1:nrow(coachdffinal)){

      coachdffinal[i,1] <- coachA

      c1 <- coachdfA %>% filter(Year == unique(fwd$Year)[i], HC == coachA)
      c2 <- coachdfA %>% filter(Year == unique(fwd$Year)[i], OC == coachA)

      if(nrow(c1) == 0 & nrow(c2) == 0){
        coachdffinal[i,2] <- NA
        coachdffinal[i,3] <- NA
        coachdffinal[i,4] <- NA
        coachdffinal[i,5] <- NA
      }
      if(nrow(c2) != 0){
        coachdffinal[i,2] <- "OC"
        coachdffinal[i,3] <- unique(as.character(c2$Team))
        coachdffinal[i,4] <- min(c2$Week)
        coachdffinal[i,5] <- max(c2$Week)
      }
      if(nrow(c1) != 0){
        coachdffinal[i,2] <- "HC"
        coachdffinal[i,3] <- unique(as.character(c1$Team))
        coachdffinal[i,4] <- min(c1$Week)
        coachdffinal[i,5] <- max(c1$Week)
      }
    }
    coachdffinal <- as.data.frame(coachdffinal)
    coachdffinal$WeekFirst <- as.numeric(coachdffinal$WeekFirst)
    coachdffinal$WeekLast <- as.numeric(coachdffinal$WeekLast)


    posv <- c("QB1","RB1","RB2","WR1","WR2","WR3","TE1","TE2")
    coachdfvalues <- matrix(nrow = length(unique(fwd$Year)), ncol = 8)
    rownames(coachdfvalues) <- unique(fwd$Year)
    colnames(coachdfvalues) <- c("QB1","RB1","RB2","WR1","WR2","WR3","TE1","TE2")
    for(i in 1:nrow(coachdfvalues)){

      week1 <- coachdffinal[i,4]
      week2 <- coachdffinal[i,5]
      team <- coachdffinal[i,3]

      if(is.na(team)){
        coachdfvalues[i,] <- NA
      }
      if(!is.na(team)){
        df <- fwd %>%
          filter(Year == unique(fwd$Year)[i], Week >= week1, Week <= week2, Team == team) %>%
          select(Player,Position,PPR) %>% group_by(Player) %>%
          summarise(Weeks_Total = round(sum(PPR),1))
        positions <- unique(fwd %>% filter(Year == unique(fwd$Year)[i], Week >= week1, Week <= week2, Team == team) %>%
                              select(Player,Position))
        df <- merge(df, positions, by = "Player")
        df <- df %>% group_by(Position) %>%
          mutate(Weeks_Pos_Rank = rank(-Weeks_Total, ties.method = "first"))
        df$PositionTier <- paste0(df$Position,df$Weeks_Pos_Rank)
        df$Player2 <- paste0(df$Player,", ",df$Position)

        df2 <- fwd %>%
          filter(Year == unique(fwd$Year)[i], Week >= week1, Week <= week2) %>%
          select(Player2,Position,PPR) %>% group_by(Player2) %>%
          summarise(Weeks_Total = round(sum(PPR),1))
        positions2 <- unique(fwd %>% filter(Year == unique(fwd$Year)[i], Week >= week1, Week <= week2) %>%
                               select(Player2,Position))
        df2 <- merge(df2, positions2, by = "Player2")
        df2 <- df2 %>% group_by(Position) %>%
          mutate(Weeks_Pos_Rank = rank(-Weeks_Total, ties.method = "first"))


        for(j in 1:length(posv)){

          p1 <- df$Player2[df$PositionTier == posv[j]]

          coachdfvalues[i,posv[j]] <- df2$Weeks_Pos_Rank[df2$Player2 == p1]

        }
      }

    }
    coachdfvalues <- as.data.frame(coachdfvalues)

    coachdffinal <- cbind(coachdffinal, coachdfvalues)
    Average <- c(NA,NA,NA,NA,NA,round(colMeans(coachdffinal[,6:13], na.rm = TRUE),0))
    coachdffinal <- rbind(coachdffinal,Average)
    rownames(coachdffinal)[nrow(coachdffinal)] <- "Average"

  }

  if(s != "Fantasy Rank" & s != "Players"){

    coachdffinal <- matrix(nrow = length(unique(fwd$Year)), ncol = 5)
    rownames(coachdffinal) <- unique(fwd$Year)
    colnames(coachdffinal) <- c("Name","Coach","Team","WeekFirst","WeekLast")
    for(i in 1:nrow(coachdffinal)){

      coachdffinal[i,1] <- coachA

      c1 <- coachdfA %>% filter(Year == unique(fwd$Year)[i], HC == coachA)
      c2 <- coachdfA %>% filter(Year == unique(fwd$Year)[i], OC == coachA)

      if(nrow(c1) == 0 & nrow(c2) == 0){
        coachdffinal[i,2] <- NA
        coachdffinal[i,3] <- NA
        coachdffinal[i,4] <- NA
        coachdffinal[i,5] <- NA
      }
      if(nrow(c2) != 0){
        coachdffinal[i,2] <- "OC"
        coachdffinal[i,3] <- unique(as.character(c2$Team))
        coachdffinal[i,4] <- min(c2$Week)
        coachdffinal[i,5] <- max(c2$Week)
      }
      if(nrow(c1) != 0){
        coachdffinal[i,2] <- "HC"
        coachdffinal[i,3] <- unique(as.character(c1$Team))
        coachdffinal[i,4] <- min(c1$Week)
        coachdffinal[i,5] <- max(c1$Week)
      }
    }
    coachdffinal <- as.data.frame(coachdffinal)
    coachdffinal$WeekFirst <- as.numeric(coachdffinal$WeekFirst)
    coachdffinal$WeekLast <- as.numeric(coachdffinal$WeekLast)

    posv <- c("QB1","RB1","RB2","WR1","WR2","WR3","TE1","TE2")
    coachdfvalues <- matrix(nrow = length(unique(fwd$Year)), ncol = 8)
    rownames(coachdfvalues) <- unique(fwd$Year)
    colnames(coachdfvalues) <- c("QB1","RB1","RB2","WR1","WR2","WR3","TE1","TE2")
    for(i in 1:nrow(coachdfvalues)){

      week1 <- coachdffinal[i,4]
      week2 <- coachdffinal[i,5]
      team <- coachdffinal[i,3]

      if(is.na(team)){
        coachdfvalues[i,] <- NA
      }
      if(!is.na(team)){
        df <- fwd %>%
          filter(Year == unique(fwd$Year)[i], Week >= week1, Week <= week2, Team == team) %>%
          select(Player,Position,PPR) %>% group_by(Player) %>%
          summarise(Weeks_Total = round(sum(PPR),1))
        positions <- unique(fwd %>% filter(Year == unique(fwd$Year)[i], Week >= week1, Week <= week2, Team == team) %>%
                              select(Player,Position))
        df <- merge(df, positions, by = "Player")
        df <- df %>% group_by(Position) %>%
          mutate(Weeks_Pos_Rank = rank(-Weeks_Total, ties.method = "first"))
        df$PositionTier <- paste0(df$Position,df$Weeks_Pos_Rank)
        df$Player2 <- paste0(df$Player,", ",df$Position)

        df2 <- coachdfA %>%
          filter(Year == unique(fwd$Year)[i], Week >= week1, Week <= week2) %>%
          select(Player2,s) %>% group_by(Player2) %>%
          summarise_at(s, sum)
        positions2 <- unique(fwd %>% filter(Year == unique(fwd$Year)[i], Week >= week1, Week <= week2) %>%
                               select(Player2,Position))
        df2 <- merge(df2, positions2, by = "Player2")

        for(j in 1:length(posv)){

          p1 <- df$Player2[df$PositionTier == posv[j]]

          coachdfvalues[i,posv[j]] <- df2[df2$Player2 == p1,2]

        }
      }



    }
    coachdfvalues <- as.data.frame(coachdfvalues)

    coachdffinal <- cbind(coachdffinal, coachdfvalues)
    Average <- c(NA,NA,NA,NA,NA,round(colMeans(coachdffinal[,6:13], na.rm = TRUE),0))
    coachdffinal <- rbind(coachdffinal,Average)
    rownames(coachdffinal)[nrow(coachdffinal)] <- "Average"

  }

  if(s == "Players"){

    coachdffinal <- matrix(nrow = length(unique(fwd$Year)), ncol = 5)
    rownames(coachdffinal) <- unique(fwd$Year)
    colnames(coachdffinal) <- c("Name","Coach","Team","WeekFirst","WeekLast")
    for(i in 1:nrow(coachdffinal)){

      coachdffinal[i,1] <- coachA

      c1 <- coachdfA %>% filter(Year == unique(fwd$Year)[i], HC == coachA)
      c2 <- coachdfA %>% filter(Year == unique(fwd$Year)[i], OC == coachA)

      if(nrow(c1) == 0 & nrow(c2) == 0){
        coachdffinal[i,2] <- NA
        coachdffinal[i,3] <- NA
        coachdffinal[i,4] <- NA
        coachdffinal[i,5] <- NA
      }
      if(nrow(c2) != 0){
        coachdffinal[i,2] <- "OC"
        coachdffinal[i,3] <- unique(as.character(c2$Team))
        coachdffinal[i,4] <- min(c2$Week)
        coachdffinal[i,5] <- max(c2$Week)
      }
      if(nrow(c1) != 0){
        coachdffinal[i,2] <- "HC"
        coachdffinal[i,3] <- unique(as.character(c1$Team))
        coachdffinal[i,4] <- min(c1$Week)
        coachdffinal[i,5] <- max(c1$Week)
      }
    }
    coachdffinal <- as.data.frame(coachdffinal)
    coachdffinal$WeekFirst <- as.numeric(coachdffinal$WeekFirst)
    coachdffinal$WeekLast <- as.numeric(coachdffinal$WeekLast)

    posv <- c("QB1","RB1","RB2","WR1","WR2","WR3","TE1","TE2")
    coachdfvalues <- matrix(nrow = length(unique(fwd$Year)), ncol = 8)
    rownames(coachdfvalues) <- unique(fwd$Year)
    colnames(coachdfvalues) <- c("QB1","RB1","RB2","WR1","WR2","WR3","TE1","TE2")
    for(i in 1:nrow(coachdfvalues)){

      week1 <- coachdffinal[i,4]
      week2 <- coachdffinal[i,5]
      team <- coachdffinal[i,3]

      if(is.na(team)){
        coachdfvalues[i,] <- NA
      }
      if(!is.na(team)){
        df <- fwd %>%
          filter(Year == unique(fwd$Year)[i], Week >= week1, Week <= week2, Team == team) %>%
          select(Player,Position,PPR) %>% group_by(Player) %>%
          summarise(Weeks_Total = round(sum(PPR),1))
        positions <- unique(fwd %>% filter(Year == unique(fwd$Year)[i], Week >= week1, Week <= week2, Team == team) %>%
                              select(Player,Position))
        df <- merge(df, positions, by = "Player")
        df <- df %>% group_by(Position) %>%
          mutate(Weeks_Pos_Rank = rank(-Weeks_Total, ties.method = "first"))
        df$PositionTier <- paste0(df$Position,df$Weeks_Pos_Rank)
        df$Player2 <- paste0(df$Player,", ",df$Position)

        for(j in 1:length(posv)){

          p1 <- df$Player[df$PositionTier == posv[j]]

          coachdfvalues[i,posv[j]] <- p1

        }
      }

    }
    coachdfvalues <- as.data.frame(coachdfvalues)
    coachdffinal <- cbind(coachdffinal, coachdfvalues)

  }

  return(coachdffinal)


}
