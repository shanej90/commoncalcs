#' Calculates percentage growth on an absolute or per period average basis
#'
#' Allows the calculation of percentage change in a variable over user-defined periods of time, on either an absolute basis, or averaged over the course of the period.
#'
#' @param data Dataframe holding the data you would like to perform the calculation for.
#' @param period_col The column containing data for the period the value you are calculating pertains to.
#' @param start_period The first period you want to use for your calculations. Must be present in the \code{period_col}.
#' @param end_period The last period you want to use for your calculations. Must be present in the \code{period_col}.
#' @param periods Number of periods covered in the calculation. First period is period 0.
#' @param value_col The column containing the value you want to calculate percentage growth for.
#' @param calculation Specify whether you would like to calculate growth on an absolute or average per period basis.
#' @param ... Grouping variables if the data needs to be aggregated.
#' @return A dataframe containing the calculated percentages for the specified grouping variables and periods considered. It will be listed for the start period to the end period.
#' @export

calc_percent_growth <- function(
  data,
  period_col,
  start_period,
  end_period,
  periods,
  value_col,
  calculation,
  ...
) {

  #make sure calculation values are in the allowed set
  if(!calculation %in% c("abs", "avg")) stop("calculation must be either 'abs' (absolute) or 'avg' (average per period).")

  #make sure specified number of periods is numeric
  if(!is.numeric(periods)) stop("Number of periods specified must be numeric.")

  #make sure value col is numeric
  if(!is.numeric(data[[deparse(substitute(value_col))]])) stop("value_col must be numeric.")

  #calculate on absolute basis
  if(calculation == "abs"){

    data %>%
      #filter
      dplyr::filter({{period_col}} %in% c(start_period, end_period)) %>%
      #make sure levels are set correctly
      dplyr::mutate({{period_col}} := factor({{period_col}}, levels = c(start_period, end_period))) %>%
      #arrange correctly
      dplyr::arrange({{period_col}}) %>%
      #group
      dplyr::group_by(...) %>%
      #work out absolute percentage change
      dplyr::mutate(percent_change = 100 * (dplyr::last({{value_col}}) - dplyr::first({{value_col}})) / dplyr::first({{value_col}})) %>%
      dplyr::ungroup() %>%
      #only include data for start_yr
      dplyr::filter({{period_col}} == start_period)

  } else {

    data %>%
      #filter
      dplyr::filter({{period_col}} %in% c(start_period, end_period)) %>%
      #make sure year factors are right
      dplyr::mutate({{period_col}} := factor({{period_col}}, levels = c(start_period, end_period))) %>%
      #arrange
      dplyr::arrange({{period_col}}) %>%
      #sum figures
      dplyr::group_by({{period_col}}, ...) %>%
      dplyr::summarise({{value_col}} := sum({{value_col}}, na.rm = T)) %>%
      dplyr::ungroup() %>%
      #group for working out first/last period
      dplyr::group_by(...) %>%
      #work out absolute percentage change
      dplyr::mutate(
        last_over_first = dplyr::last({{value_col}}) / dplyr::first({{value_col}}),
        percent_change = 100 * ((last_over_first ^ (1 / periods)) - 1)
      )  %>%
      dplyr::ungroup() %>%
      #only include data for start_yr
      dplyr::filter({{period_col}} == start_period) %>%
      #remove intermiedate calculation column
      dplyr::select(..., percent_change)

  }


}
