#' find next business day
#'
#' find the next business day
#'
#' @param con, rdf connection
#' @param x, integer, date list
#' @param including, logic, if include the today
#'
#' @examples
#' x <- rep(20170101+ 1:2, 5)
#' next_buz_day(con, x)
#'
#' @export
#'
next_buz_day <- function(con, x, including = F)
{
  buz_day <- get_buz_day(con, ymd(min(x)), ymd(max(x)) %m+% years(1))
  temp <- unique(x)
  if(including)
  {
    output <- mapply(function(temp) min(buz_day[buz_day >= temp]), temp)
  }else{
    output <- mapply(function(temp) min(buz_day[buz_day > temp]), temp)
  }
  output[match(x, temp)]
}
