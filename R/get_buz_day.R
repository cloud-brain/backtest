#' Get business day
#'
#' Get business day of stock market bewteen two days
#'
#' @param con a connection
#' @param beg_dt the begin date, can be integer or character format like \%Y\%m\%d
#' @param end_dt the end date, can be integer or character format like \%Y\%m\%d
#' @details
#' The begin date and end date should not be the business day.
#' The return vector will begin at the first business day after begin date.
#'
#' @return
#' a vector of date
#'
#' @examples
#' \dontrun{
#' get_buz_day(con, 20100101, 20110101)
#' }
#'
#' @import lubridate
#' @importFrom DBI dbGetQuery
#' @export
#'
get_buz_day <- function(con, beg_dt, end_dt, ...)
{
  UseMethod('get_buz_day')
}

#' @rdname get_buz_day
get_buz_day.default <- function(...)
{
  return('unknown type')
}

#' @rdname get_buz_day
get_buz_day.tiny <- function(con, beg_dt, end_dt)
{
  beg_dt <- dt_to_char(beg_dt)
  end_dt <- dt_to_char(end_dt)
  
  if(beg_dt > end_dt)
    stop('begin date must less than end date')
  
  return(sqlQuery(con, sprintf("return get_buz_day(%s,%s);",
                               beg_dt, end_dt)))
}

#' @rdname get_buz_day
get_buz_day.rdf <- function(con, beg_dt, end_dt)
{
  beg_dt <- dt_to_char(beg_dt[1])
  end_dt <- dt_to_char(end_dt[1])
  
  if(beg_dt > end_dt)
    stop('begin date must less than end date')
  
  sql_char <- sprintf("SELECT trade_dt FROM calendar_data where trade_dt between %s and %s order by trade_dt", 
                      beg_dt, end_dt)
  result <- dbGetQuery(con$con, sql_char)
  return(result$trade_dt)
}
