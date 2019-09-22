#' Removes observations with NAs
#'
#' remove_na is a function that removes observations if any specified column is 
#' NA
#'
#' @param DT a data.table
#' @param cols columns to look for NA observations
#'
#' @return The data.table without NAs in any of the specified columns
#'
#' @examples 
#' # 2013 nyc flights data
#' DT <- as.data.table(nycflights13::flights)
#' 
#' DT_clean <- remove_na(DT = DT, cols = c("arr_delay", "carrier"))
#' @export
remove_na <- function(DT, cols, track = F) {
  if (track == F) {
    DT[complete.cases(DT[, cols, with = F])]
  }
}





