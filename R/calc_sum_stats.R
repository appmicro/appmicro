#' Calculate sample summary statistics
#'
#' calc_sum_stats() is a funtion that calculates summary statistics (mean,
#' standard deviation, minimum, and maximum) for the either the entire sample,
#' by group, or both
#'
#' @param DT A data.table.
#' @param sum_vars A vector of string column names in DT to calculate summary
#' statistics of.
#' @param by_var A string of the column name in DT to group summary statistics
#' by
#'
#' @return data.table of summary statistics
#'
#' @examples
#' # mtcars data set
#' DT <- as.data.table(mtcars)
#' # entire sample
#' calc_sum_stats(DT, sum_vars = c("mpg", "hp"))
#' 
#' @export
calc_sum_stats <- function(DT, sum_vars, by_var = NULL) {
  mean_sd_min_max <- function(x) {
    c(mean = mean(x, na.rm = T), sd = sd(x, na.rm = T),
      min = min(x, na.rm = T), max = max(x, na.rm = T))
  }
  if (is.null(by_var)) {
    DT %>%
      .[, lapply(.SD, mean_sd_min_max), .SDcols = sum_vars] %>% 
      .[, sample := "all"] %>% 
      melt(id.var = "sample") %>% 
    .[, stat := rep(c("mean", "sd", "min", "max"), nrow(.)/4)] %>%
      dcast(variable ~ stat, value.var = "value") %>% 
      .[, .(variable, mean, sd, min, max)]
  } else {
      DT %>%
        .[, lapply(.SD, mean_sd_min_max), by = by_var, .SDcols = sum_vars] %>%
        melt(id.var = by_var) %>%
        .[, stat := rep(c("mean", "sd", "min", "max"), nrow(.)/4)] %>%
        setnames(by_var, "by_var") %>%
        dcast(by_var + variable ~ stat, value.var = "value") %>%
        merge(DT[, .(obs = .N), by = by_var], by.x = "by_var", by.y = by_var) %>%
        .[order(variable, by_var), ] %>%
        .[, .(variable, by_var, mean, sd, min, max)] %>% 
        setnames("by_var", by_var)
  }
}