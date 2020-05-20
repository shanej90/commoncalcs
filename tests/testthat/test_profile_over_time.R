#end col must be in date format (assume same holds for begin_col)-------------------
testthat::test_that(
  "non-date begin col gives an error",
  expect_error(
    profile_over_time(
      commoncalcs::commoncalcs_df,
      actual_start_date,
      hesa_category,
      "2020-01-01",
      "2020-02-01",
      "month",
      award_value,
      college
      ),
    "end_col must be in date format",
    fixed = T
  )
)

#periodstart in wrong format returns error (assume same holds for period_end)-------------
testthat::test_that(
  "test that putting in wrong period start/end format returns error",
  expect_error(
    profile_over_time(
      commoncalcs::commoncalcs_df,
      actual_start_date,
      actual_end_date,
      "2020-xy-01",
      "2020-02-01",
      "month",
      award_value,
      college,
      hesa_category
    ),
    "period_start must be in 'yyyy-mm-dd' format",
    fixed = T
  )
)

#make sure non-allowed period breaks value causes error-------------
testthat::test_that(
  "test that using a non-allowed period_breaks value returns an error",
  expect_error(
    profile_over_time(
      commoncalcs::commoncalcs_df,
      actual_start_date,
      actual_end_date,
      "2020-01-01",
      "2020-02-01",
      "covid19",
      award_value,
      college,
      hesa_category
    ),
    "period_breaks must be one of: 'day', 'week', 'month', 'quarter', 'year'",
    fixed = T
  )
)

#make sure returning too many period breaks returns an error-------------
testthat::test_that(
  "test that using period_breaks length > 1 returns error",
  expect_error(
    profile_over_time(
      commoncalcs::commoncalcs_df,
      actual_start_date,
      actual_end_date,
      "2020-01-01",
      "2020-02-01",
      c("month", "year"),
      award_value,
      college,
      hesa_category
    ),
    "period_breaks must be one of: 'day', 'week', 'month', 'quarter', 'year'",
    fixed = T
  )
)

#make sure a non-numeric value col returns an error-------------
testthat::test_that(
  "test that using a non-numeric value_col returns an error",
  expect_error(
    profile_over_time(
      df = commoncalcs::commoncalcs_df,
      begin_col = actual_start_date,
      end_col = actual_end_date,
      period_start = "2020-01-01",
      period_end = "2020-02-01",
      period_breaks = "month",
      value_col = hesa_category,
      ... = college
    ),
    "value_col must be numeric",
    fixed = T
  )
)


# function(df, begin_col, end_col, period_start, period_end, period_breaks, value_col, ...)

