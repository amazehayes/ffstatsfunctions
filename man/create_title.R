#' @title Create Title
#'
#' @description creates plot title
#'
#' @param pA
#' @param pB
#' @param pC
#'
#' @return NULL
#'
#' @examples create_title(pA=NULL,pB=NULL,pC=NULL)
#'
#' @export

create_title <- function(pA=NULL,pB=NULL,pC=NULL){
  all_players <- c(pA,pB,pC)
  #if(length(all_players)==1){
  #  return(paste("Yearly Finishes for", all_players))
  #}
  #if(length(all_players)==2){
  #  return(paste("Yearly Finishes for", all_players[1],"&",all_players[2]))
  #}
  #if(length(all_players)==3){
  #  return(paste("Yearly Finishes for", all_players[1],"&",all_players[2],"&",all_players[3]))
  #}
  return(paste("Yearly Finishes for", paste(all_players, collapse = " & ")))
}
