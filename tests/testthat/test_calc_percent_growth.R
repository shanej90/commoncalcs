test_that("a non-accepted calculation value gives an error", {
  expect_error(calc_percent_growth(mtcars, cyl, 4, 8, 2, qsec, "test", gear), "calculation must be either 'abs' (absolute) or 'avg' (average per period).", fixed = T)
  }
  )

test_that("non-numeric periods gives an error", {
expect_error(calc_percent_growth(mtcars, cyl, 4, 8, "x", qsec, "abs", gear), "Number of periods specified must be numeric.", fixed = T)
  }
)

test_that("non-numeric value col throws up an error", {
  expect_error(calc_percent_growth(iris, Petal.Width, .1, .4, 2, Species, "abs", Petal.Length), "value_col must be numeric.", fixed = T)
  }
)






