#' @title Coaching Table
#'
#' @description generates coaching table
#'
#' @param coach coach
#' @param fwd database
#'
#' @return datatable
#'
#' @examples coach_main_table(coach,fwd)
#'
#' @export

coach_main_table <- function(coach,fwd){
  coachA <- coach
  coachdf <- fwd %>% filter(hc == coachA | oc == coachA) %>% arrange(-year,-week)
  coachyears <- max(coachdf$year):min(coachdf$year)

  coachdffinal <- matrix(nrow = length(coachyears), ncol = 5)
  rownames(coachdffinal) <- coachyears
  colnames(coachdffinal) <- c("Name","Coach","Team","WeekFirst","WeekLast")
  for(i in 1:nrow(coachdffinal)){

    coachdffinal[i,1] <- coachA

    c1 <- coachdf %>% filter(year == coachyears[i], hc == coachA)
    c2 <- coachdf %>% filter(year == coachyears[i], oc == coachA)

    if(nrow(c1) == 0 & nrow(c2) == 0){
      coachdffinal[i,2] <- NA
      coachdffinal[i,3] <- NA
      coachdffinal[i,4] <- NA
      coachdffinal[i,5] <- NA
    }
    if(nrow(c2) != 0){
      coachdffinal[i,2] <- "OC"
      coachdffinal[i,3] <- unique(as.character(c2$team))
      coachdffinal[i,4] <- min(c2$week)
      coachdffinal[i,5] <- max(c2$week)
    }
    if(nrow(c1) != 0){
      coachdffinal[i,2] <- "HC"
      coachdffinal[i,3] <- unique(as.character(c1$team))
      coachdffinal[i,4] <- min(c1$week)
      coachdffinal[i,5] <- max(c1$week)
    }
  }
  coachdffinal <- as.data.frame(coachdffinal)
  coachdffinal$WeekFirst <- as.numeric(coachdffinal$WeekFirst)
  coachdffinal$WeekLast <- as.numeric(coachdffinal$WeekLast)
  coachdffinal$Year <- coachyears

  return(coachdffinal)
}
