#' @title Clean Names
#'
#' @description clean names
#'
#' @param dat
#'
#' @return
#'
#' @examples clean_names(dat)
#'
#' @export

clean_names <- function(dat){
  old_names <- names(dat)
  new_names <- old_names %>%
    gsub("%", "percent", .) %>%
    make.names(.) %>%
    gsub("[.]+", "_", .) %>%
    tolower(.) %>%
    gsub("_$", "", .)
  setNames(dat, tolower(new_names))
}
