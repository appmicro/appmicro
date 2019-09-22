library(appmicro)
library(data.table)

test_that("removes NAs",{
  DT <- data.table(y = c(NA, 2, 3, 4), 
                   x = c(1, NA, 8, 9))
  expect_equal(remove_na(DT, c("y", "x")), DT[3:4, ])
})
