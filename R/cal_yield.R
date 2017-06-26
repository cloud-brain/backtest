#' calculate the yield
#'
#' calculate the yield from net value
#'
#' @param x vector of net value
#' @param fisrt, the first day yield
#'
#' @examples
#' data(m_index_acount)
#' cal_yield(m_index_acount$acount)
#'
#' @export
#'
cal_yield <- function(x, first = 0)
{
  return(c(first, (x[-1] / x[-length(x)]) - 1))
}
