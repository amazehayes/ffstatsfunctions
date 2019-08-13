#' @title mround
#'
#' @description rounds to nearest X
#'
#' @param x
#' @param base
#'
#' @return NULL
#'
#' @examples mround(x,base)
#'
#' @export

mround <- function(x,base){
  base*round(x/base)
}
