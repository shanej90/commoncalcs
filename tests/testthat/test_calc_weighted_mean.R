test_that("weighted mean column is numeric", {
expect_type(calc_weighted_mean(mtcars, mpg, wt, 1, carb)$weighted_mean, "double")
  }
)

test_that("setting a non-numeric multiplier produces an error", {
expect_error(calc_weighted_mean(mtcars, mpg, wt, "not a number", carb), "multiplier must be numeric")
}
)
