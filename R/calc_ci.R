#' Calculate confidence intervals
#' 
#' calc_ci() is a function that calculates a confidence interval using the 
#' normal distribution given a point estimate, SE, and alpha value
#' 
#' @param point point estimate 
#' @param se standard error of point estimate
#' @param alpha alpha value for confidence interval (defaul to .05)
#'
#' @return list with two elements, $lb and $ub, which are the lower and 
#' upper bounds of the confidence interval respectively
#' 
#' @examples
#' calc_ci
#'
#' @export
calc_ci <- function(point, se, alpha = .05) {
  # calculates critical value based on standard normal distibution
  crit <- qnorm(1 - alpha/2)
  # list with lower and upper bounds of confidence interval
  list(lb = point - crit*se, ub = point + crit*se)
}

library(broom)


DT <- mtcars


fit <- lm(mpg ~ cyl:factor(am):factor(gear) + factor(am)*factor(gear), data = DT)

p_var <- "cyl"
s_vars <- c("am", "gear")



p_var <- c("cyl")

library(stringr)


clean_slope_by_interact <- function(fit, p_var, s_vars) {  
  dtp <- tidy(fit) %>% 
    as.data.table() %>% 
    .[grep(p_var, term), ] %>% 
    .[, term := gsub(paste0(p_var, ":"), "", term)]
  for (i in s_vars) {
    dtp[, term := gsub(paste0("factor\\(", i, "\\)"), "", term)]
  }
  for (i in 1:length(s_vars)) {
    dtp[, s_vars[i] := str_split_fixed(term, ":", length(s_vars))[, i]]
  }
 dtp %<>% 
   .[, var := p_var] %>% 
   .[, c("var", s_vars, "estimate", "std.error", "statistic", "p.value"), with = F] %>% 
   .[, `:=`(lb = calc_ci(estimate, std.error)$lb, 
            ub = calc_ci(estimate, std.error)$ub)]
}

form <- fit$call %>% 
  as.character() %>% 
  .[2] %>% 
  str_split_fixed(" ~ ", 2) %>% 
  .[, 2]

form

hi <- clean_slope_by_interact(fit, "cyl", c("am", "gear"))


hi <- clean_reg(fit, "cyl", c("am", "gear"))  
  
  
  
  gsub(paste0("factor\\(", s_vars[1], "\\)"), "", dtp$term)
