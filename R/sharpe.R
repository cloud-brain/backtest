#' Sharpe ratio
#'
#' calculate the sharpe ratio
#'
#' @param x yield vector
#' @param rf free interest rate
#' @param adjust adjust for the stardard error date
#'
#' @examples
#' data(m_index_acount)
#' sharpe(cal_yield(m_index_acount$acount))
#'
#' @export
#'
sharpe <- function(x, rf = 0, adjust = 1)
{
  (prod(1 + x) - rf - 1) / sd(x) / asjust
}
