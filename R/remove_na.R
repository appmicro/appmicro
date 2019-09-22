#' Removes observations with NAs
#'
#' remove_na() is a function that removes observations that have an NA in any 
#'  specified column
#'
#' @param DT a data.table
#' @param cols columns to look for NA observations
#' @param track logical, if TRUE prints the number of observations dropped 
#'  because of each variable
#'
#' @return The data.table without NAs in any of the specified columns
#'
#' @examples
#' # 2013 nyc flights data
#' DT <- as.data.table(nycflights13::flights)
#' DT_clean <- remove_na(DT = DT, cols = c("arr_delay", "carrier"))
#' @export
remove_na <- function(DT, cols, track = T) {
  if (track == TRUE) {
    na_obs <- DT[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = cols]
    na_obs <- suppressWarnings(melt(na_obs, variable.name = "Variable", 
                                    value.name = "NA Observations"))
    print(na_obs)
  } 
  DT[complete.cases(DT[, cols, with = F])]
}