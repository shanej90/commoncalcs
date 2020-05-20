#' Create a 'straight-line' profile of a value over its associated lifetime, split up into user-defined periods.
#'
#' Can take a value associated with an item and spreads the value over the item's lifetime. One example would be profiling income/expenditure using the total value of a five year contract.
#'
#' @param df Dataframe holding the data you would like to perform the calculation for.
#' @param begin_col Column containing information about the begin date for the item you're interested in. Must be in date format. Will be renamed 'start_date' during processing.
#' @param end_col Column ontaining the information about the end date of the item you're interested in. Must be in date format. Will be renamed 'end_date' during processing.
#' @param period_start The beginning of the period you would like to profile over. Enter as "yyyy-mm-dd".
#' @param period_end The end of the period you would like to profile over. Enter as "yyyy-mm-dd".
#' @param period_breaks Defines the length of each period. Can be "day", "week", "month", "quarter" or "year".
#' @param value_col The value you'd like to profile. Must be numeric.
#' @param ... Columns containing information about the item the value is attached to.
#' @return A dataframe containing the data profiled over the specified periods.
#' @export

profile_over_time <- function(df, begin_col, end_col, period_start, period_end, period_breaks, value_col, ...){

  #error checking-------------------------------------------------------

  #make begin_col and end_col both dates
  if(inherits(df[[deparse(substitute(begin_col))]], "Date") == F) stop("begin_col must be in date format")
  if(inherits(df[[deparse(substitute(end_col))]], "Date") == F) stop("end_col must be in date format")

  #make sure period_start/end in correct format
  if(stringr::str_detect(period_start, "[0-9][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]") == F) stop("period_start must be in 'yyyy-mm-dd' format")
  if(stringr::str_detect(period_end, "[0-9][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]") == F) stop("period_end must be in 'yyyy-mm-dd' format")

  #make sure period_breaks set to one of pre-defined strings
  if(!period_breaks %in% c("day", "week", "month", "quarter", "year") | length(period_breaks) != 1) stop("period_breaks must be one of: 'day', 'week', 'month', 'quarter', 'year'")

  #make sure value col is numeric
  if(!is.numeric(df %>% dplyr::pull({{value_col}}))) stop("value_col must be numeric")


  #data type fixing-------------------------------------------------

  #ui message
  usethis::ui_todo("Converting {ui_field('period_start')} and {ui_field('period_end')} to date format.")

  #set period_start and period_end to date
  period_start <- as.Date(period_start)
  period_end <- as.Date(period_end)

  #completed step message
  usethis::ui_done("{ui_field('period_start')} and {ui_field('period_end')} converted to date format.")


  #pre-procesing-----------------------------------------------

  #todo ui message
  usethis::ui_info("Calculating minimum and maximum dates for {ui_field('df')} within specified period.")

  #work out min/max dates
  MinDate <- df %>%
    dplyr::filter({{begin_col}} >= period_start)  %>%
    dplyr::pull({{begin_col}}) %>%
    min(., na.rm = T)

  MaxDate <- df %>%
    dplyr::filter({{end_col}} <= period_end) %>%
    dplyr::pull({{end_col}})  %>%
    max(., na.rm = T)

  #done message
  usethis::ui_done("Minimum and maximum dates found")

  #working out dates message
  usethis::ui_info("Getting all relevant date data.")

  #create datelist
  DateList <- seq.Date(MinDate, MaxDate, by = period_breaks) %>% format(.)

  #data frame of pertinent date data
  DateDf <- data.frame(
    year = lubridate::year(DateList),
    quarter = lubridate::quarter(DateList),
    month = lubridate::month(DateList),
    month_name = month.abb[lubridate::month(DateList)],
    week = lubridate::week(DateList),
    day = lubridate::day(DateList),
    day_name = weekdays(as.Date(as.character(DateList))),
    date = as.Date(as.character(DateList), origin = "1899-12-30")
    ) %>%
    #add start dates for various periods
    dplyr::mutate(
      year_start = lubridate::floor_date(date, unit = "year"),
      year_end = lubridate::ceiling_date(date, unit = "year") - 1,
      quarter_start = lubridate::floor_date(date, unit = "quarter"),
      quarter_end = lubridate::ceiling_date(date, unit = "quarter") - 1,
      month_start = lubridate::floor_date(date, unit = "month"),
      month_end = lubridate::ceiling_date(date, unit = "month") - 1,
      week_start = lubridate::floor_date(date, unit = "week"),
      week_end = lubridate::ceiling_date(date, unit = "week") - 1
    )

  #ui message done
  usethis::ui_done("All date data gathered.")

  #message about working out profile periods
  usethis::ui_info("Working out profile breaks for each item.")

  #rename source data frame columns to enable sql script
  df_fn <- df %>%
    dplyr::rename(start_date = {{begin_col}}, end_date = {{end_col}})

  #get by period breakdown----------------------------------------------

  if(period_breaks == "day"){
  sql_df <-  sqldf::sqldf("SELECT *
  FROM DateDf
  LEFT JOIN df_fn
  ON (df_fn.start_date < DateDf.date)
        AND (df_fn.end_date > DateDf.date) "
    )
  } else   if(period_breaks == "week"){
  sql_df <- sqldf::sqldf("SELECT *
  FROM DateDf
  LEFT JOIN df_fn
  ON (df_fn.start_date < DateDf.week_end)
        AND (df_fn.end_date > DateDf.week_start) "
    )
   } else if(period_breaks == "month"){
  sql_df <- sqldf::sqldf("SELECT *
  FROM DateDf
  LEFT JOIN df_fn
  ON (df_fn.start_date < DateDf.month_end)
        AND (df_fn.end_date > DateDf.month_start) "
  )
  } else  if(period_breaks == "quarter"){
    sql_df <- sqldf::sqldf("SELECT *
  FROM DateDf
  LEFT JOIN df_fn
  ON (df_fn.start_date < DateDf.quarter_end)
        AND (df_fn.end_date > DateDf.quarter_start) "
    )
} else   if(period_breaks == "year"){
    sql_df <- sqldf::sqldf("SELECT *
  FROM DateDf
  LEFT JOIN df_fn
  ON (df_fn.start_date < DateDf.year_end)
        AND (df_fn.end_date > DateDf.year_start) "
    )
  }

  #ui message stating completed
  usethis::ui_done("Profiled breaks calculated.")
  #return---------------------------------------

  #ui message
  usethis::ui_info("Calculating profiled values.")

  if(period_breaks == "day"){
    sql_df %>%
    dplyr::mutate(
      duration = as.numeric(end_date - start_date) + 1,
      duration = ifelse(duration == 0, 1, duration),
      profiled_value = {{value_col}} / duration,
      interval = period_breaks
    ) %>%
    dplyr::select(day, day_name, date, ..., start_date, end_date, {{value_col}}, duration, profiled_value, interval)
} else  if(period_breaks == "week"){
    sql_df %>%
    dplyr::mutate(
      duration = (as.numeric(end_date - start_date) + 1) / 7,
      duration = ifelse(duration == 0, 1, duration),
      profiled_value = {{value_col}} / duration,
      interval = period_breaks
    ) %>%
    dplyr::select(year, week, week_start, week_end, ..., start_date, end_date, {{value_col}}, duration, profiled_value, interval)
} else  if(period_breaks == "month"){
    sql_df %>%
    dplyr::mutate(
      duration = lubridate::interval(start_date, end_date) %/% months(1),
      duration = ifelse(duration == 0, 1, duration),
      profiled_value = {{value_col}} / duration,
      interval = period_breaks
    ) %>%
    dplyr::select(year, month, month_start, month_end, ..., start_date, end_date, {{value_col}}, duration, profiled_value, interval)
} else  if(period_breaks == "quarter"){
    sql_df %>%
    dplyr::mutate(
      duration = lubridate::interval(start_date, end_date) %/% months(3),
      duration = ifelse(duration == 0, 1, duration),
      profiled_value = {{value_col}} / duration,
      interval = period_breaks
    ) %>%
    dplyr::select(year, quarter, quarter_start, quarter_end, ..., start_date, end_date, {{value_col}}, duration, profiled_value, interval)
  } else  if(period_breaks == "year"){
    sql_df %>%
    dplyr::mutate(
      duration = lubridate::interval(start_date, end_date) %/% months(12),
      duration = ifelse(duration == 0, 1, duration),
      profiled_value = {{value_col}} / duration,
      interval = period_breaks
    ) %>%
    dplyr::select(year, year_start, year_end, ..., start_date, end_date, {{value_col}}, duration, profiled_value, interval)
}

}
