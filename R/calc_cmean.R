#' Calculate conditional sample mean/SEs
#'
#' calc_cmean() is a function that calculates sample means by group, their
#' standard errors, and confidence intervals
#'
#' @param DT a data.table
#' @param y a character vector of column names of DT to calculate the group mean
#' of
#' @param x a character vector of columns in DT to group by
#' @param se if TRUE output will include standard error of conditional sample
#' mean
#' @param alpha a real number between 0 and 1, to use as the alpha level in
#' confidence interval construction (default is .05)
#' 
#' @examples
#' DT <- data.table(x1 = sample(1:5, 1000, replace = T), 
#'                  x2 = sample(1:2, 1000, replace = T)) %>% 
#'       .[, y := 2*x1 + 4*x2 + rnorm(mean = 10, sd = 4, n = 1000)]
#'
#' calc_cmean(DT, y = c("y1"), x = c("x1", "x2"))
#'
#' @return The data.table of conditional sample means
#'
#' @export
calc_cmean <- function(DT, y, x, se = F, alpha = .05) {
  
  mean_se <- function(x) {
    c(mean = mean(x, na.rm = T), se = sd(x, na.rm = T)/sqrt(sum(!is.na(x))))
  }

  if (se == T) {
    ci_mult <- qnorm(1 - alpha/2)
    DT[, lapply(.SD, mean_se),  by = x, .SDcols = y] %>%
      .[, measure := rep(c("mean", "se"), nrow(.)/2)] %>%
      melt(id.var = c(x, "measure")) %>%
      dcast(as.formula(paste(paste(x, collapse  = " + "),
                             "+ variable ~ measure")),
            value.var = "value") %>%
      # 95% CI interval bounds
      .[, `:=`(lb = mean - ci_mult*se, ub = mean + ci_mult*se)]
  } else {
    DT[, lapply(.SD, mean), by = x, .SDcols = y] %>%
      melt(id.var = x, value.name = "mean")
  }
}