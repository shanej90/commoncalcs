#' Calculates a weighted mean based on grouping criteria you set
#'
#' Takes a dataframe and calculates a weighted mean based on a chosen denominator and numerator column.
#'
#' @param data The data frame/table holding the information you'd like to calculate a weighted mean for
#' @param numerator_col Value field that when aggregated will be the numerator in the calculation to work out weighted mean
#' @param denominator_col Value field that when aggregated will be the denominator in the calculation to work out a weighted mean
#' @param multiplier A figure to divide your end result by if your prior processing involves counting your denominator multiple times
#' @param ... Any columns you would like to group the data by when aggregating
#' @return A data frame containing the weighted mean for each group of data calculated
#' @importFrom rlang :=
#' @export

calc_weighted_mean <- function(
  data,
  numerator_col,
  denominator_col,
  multiplier = 1,
  ...
) {

  #check to make sure multiplier is a number
  if (!is.numeric(multiplier)) stop("multiplier must be numeric")

  #actual output
  data %>%
    #first calculate your weightings
    dplyr::mutate(weighted = {{numerator_col}} * {{denominator_col}}) %>%
    #group by the desired variables
    dplyr::group_by(...) %>%
    #summarise your weighted and denominator value, including any multiplier effect
    dplyr::summarise(weighted = sum(weighted, na.rm = T), {{denominator_col}} := sum({{denominator_col}} / multiplier, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(weighted_mean = weighted / {{denominator_col}}) %>%
    dplyr::select(..., weighted_mean)


}
