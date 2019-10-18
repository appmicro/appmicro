#' Calculate conditional sample mean/SEs
#'
#' calc_cmean() is a function that calculates sample means (plus
#'  standard errors and confidence intervals) by group.
#'
#' @param DT a data.table
#' @param y a character vector of column names of DT to calculate the group mean
#'  of
#' @param x a character vector of columns in DT to group by
#' @param se logical, if TRUE output will calculate standard error and confidence
#'  intervals of mean alng with  number of observations of conditional sample
#' mean
#' @param alpha a real number between 0 and 1, to use as the alpha level in
#'  confidence interval construction (default is .05)
#'
#' @examples
#' DT <- data.table(x1 = sample(1:5, 1000, replace = T),
#'                  x2 = sample(1:2, 1000, replace = T)) %>%
#'       .[, y := 2*x1 + 4*x2 + rnorm(mean = 10, sd = 4, n = 1000)]
#'
#' calc_cmean(DT, y = c("y"), x = c("x1", "x2"))
#'
#' @return The data.table of conditional sample means
#'
#' @export
calc_cmean <- function(DT, y, x, se = FALSE, alpha = .05) {
  mean_se <- function(x) {
    c(mean = mean(x, na.rm = T),
      se = sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
  }
  if (se == T) {
    ci_mult <- qnorm(1 - alpha / 2)
    DT_cmean <- DT[, lapply(.SD, mean_se),  by = x, .SDcols = y]
    DT_cmean <- DT_cmean[, measure := rep(c("mean", "se"), nrow(DT_cmean) / 2)]
    DT_cmean <- melt(DT_cmean, id.var = c(x, "measure"))
    DT_cmean <- dcast(DT_cmean, as.formula(paste(paste(x, collapse  = " + "),
                                                 "+ variable ~ measure")),
                      value.var = "value")
    DT_cmean[, `:=`(lb = mean - ci_mult * se, ub = mean + ci_mult * se)]
  } else {
   DT_cmean <- DT[, lapply(.SD, mean), by = x, .SDcols = y]
   melt(DT_cmean, id.var = x, value.name = "mean")
  }
}
