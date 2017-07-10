#' Max back
#'
#' calculate the max back
#'
#' @param x yield vector
#' @param type char, yield input or net value input
#'
#' @examples
#' data(m_index_acount)
#' max_back(m_index_acount$acount)
#'
#' @export
#'
max_back <- function(x, type = c('yield', 'acount'))
{
  type <- type[1]
  if(type == 'yield')
  {
    return(max_back(cumprod(c(1,x)), type = 'acount'))
  }else{
    return(-max((cummax(x) - x) / cummax(x)))
  }
}
