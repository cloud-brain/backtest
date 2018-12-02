#' Get future price for one day
#'
#' Get future price for one day
#'
#' @param con a tiny connection
#' @param wind_code vecter of future code list in wind_code format
#' @param buz_day format can be detect by lubridate
#' @param type choose price type as close, vwap
#'
#' @return
#' a data.frame with stock code and price, trade_dt
#'
#' @examples
#' \dontrun{
#' con <- odbcConnect('tiny')
#' get_price(con, c('SH600000','SZ000001'), 20110104)
#' }
#'
#' @importFrom RODBC sqlQuery
#' @importFrom DBI dbGetQuery
#'
#' @export
#'
get_price_future <- function(...)
{
  UseMethod('get_price')
}

#' @rdname get_price_future
get_price_future.default <- function(...)
{
  stop('unknown con type')
}

#' @rdname get_price_future
get_price_future.tiny <- function(con, wind_code, buz_day,...)
{
  buz_day <- ymd(buz_day)
  return(sqlQuery(con, sprintf("return get_price(array(%s),%s,'vwap');",
                               paste0("'",wind_code,"'", collapse = ','), format(buz_day,'%Y%m%d'))))
}

#' @rdname get_price
get_price_future.rdf <- function(con, wind_code, buz_day, type = 'close')
{
  buz_day <- ymd(buz_day)
  send_query <- sprintf("SELECT wind_code as code, %s as price FROM price_data where trade_dt = %s and wind_code in (%s) order by field(code,%s)",
                        switch(type, close = 's_dq_settle', vwap = 's_dq_avgprice'),
                        format(buz_day,'%Y%m%d'),
                        paste0("'",wind_code,"'", collapse = ','),
                        paste0("'",wind_code,"'", collapse = ','))
  if(class(con$con) == 'MySQLConnection')
  {
    result <- dbGetQuery(con$con, send_query)
  }else{
    result <- sqlQuery(con$con, send_query)
  }
  if(nrow(result) != length(wind_code))
  {
    stop(sprintf('%s is not in database on %s', paste0(setdiff(wind_code, result$code), collapse = ','), format(buz_day, '%Y%m%d')))
  }
  return(result)
}
