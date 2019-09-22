#' Subset to analytic sample
#'
#' subset_sample() is a function that subsets data to create a smaller analytic 
#'  dataset. It prints how many observations remain after each sequential 
#'  subset, as well as how many observations meet the keep criteria overall.
#'
#' @param DT A data.table.
#' @param subset_vars A vector of string column names in DT. Each column should 
#'  be a dummy variable, with 1 (or TRUE) set as the keep condition and 0 (or 
#'   FALSE) as the drop condition.
#'
#' @return Subsetted DT
#'
#' @examples
#' # 2013 nyc flights data
#' DT <- as.data.table(nycflights13::flights)
#' # define keep criteria (1 for keep, 0 for drop)
#' # afternoon flights
#' DT[, `:=`(keep_sched_dep_time = ifelse(sched_dep_time >= 1200, 1, 0),
#'           # departing from Newark 
#'           keep_origin = ifelse(origin == "EWR", 1, 0))]
#' # assign subsetted data and print observations at each step 
#' DT_sub <- subset_sample(DT, subset_vars = c("keep_sched_dep_time", 
#'                                             "keep_origin"))
#'
#' @export
subset_sample <- function(DT, subset_vars) {
  DT_sub <- DT
  obs <- data.frame(subset = "all", obs_seq = nrow(DT_sub))
  for (var in subset_vars) {
    DT_sub  <- DT_sub[get(var) == 1, ]
    obs1 <- data.table(subset = var, obs_seq = nrow(DT_sub))
    obs <- rbind(obs, obs1)
  }
  obs2 <- DT[, lapply(.SD, sum), .SDcols = subset_vars]
  obs2 <- suppressWarnings(melt(obs2, variable.name = "subset",
                                value.name = "obs_cum"))
 merge(obs, obs2, by = "subset", all.x = T) %>%
    .[, subset := gsub("keep_", "", subset)] %>% 
    .[subset == "all", obs_cum := obs_seq] %>% 
   print()
 invisible(DT_sub)
}