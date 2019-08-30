#' @title Lineup Optimizer Get Input FIle
#'
#' @description gets input file for optimizer
#'
#' @param input
#'
#' @return datatable
#'
#' @examples optimize_lineup_get_input_file(input)
#'
#' @export

optimize_lineup_get_input_file <- function(input){
  filter <- dplyr::filter
  select <- dplyr::select
  summarize <- dplyr::summarize
  summarise_all <- dplyr::summarize
  count <-dplyr::count
  gather <- tidyr::gather

  req(input$file1)

  tryCatch({df <- clean_names(read.csv(input$file1$datapath))},
           error = function(e) {
             stop(safeError(e))
           }
  )
  names_to_check <- c("position", "player", "cost", "projected_points", "keep")
  missing_names <- setdiff(names_to_check, names(df))
  if(length(missing_names)> 0){
    stop(paste0("You're' missing these columns in the uploaded file: " ,
                paste(missing_names, collapse = ', ')))
  }
  df <- df %>% filter(keep == 1)
  return(df)
}
