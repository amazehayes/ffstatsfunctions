#' @title NA.Is.Zero
#'
#' @description na to zero
#'
#' @param X
#' @param value set to 0
#'
#' @return
#'
#' @examples na.is.zero(X,value=)
#'
#' @export

na.is.zero <- function(X, value = 0) {
    X1 <- X
    X1[is.na(X1)] <- value
    return(X1)
  }
