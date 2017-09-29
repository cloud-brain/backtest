#' Get stock price for one day
#'
#' Get stock price for one day
#'
#' @param con a tiny connection
#' @param stocks vecter of stock code list in tiny format
#' @param buz_day integer or character format like \%Y\%m\%d, test for this day
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
get_price <- function(...)
{
  UseMethod('get_price')
}

#' @rdname get_price
#' @export
get_price.default <- function(...)
{
  stop('unknown con type')
}

#' @rdname get_price
#' @export
get_price.tiny <- function(con, stocks, buz_day,...)
{
  buz_day <- ymd(buz_day)
  return(sqlQuery(con, sprintf("return get_price(array(%s),%s,'vwap');",
                               paste0("'",stocks,"'", collapse = ','), format(buz_day,'%Y%m%d'))))
}

#' @rdname get_price
#' @export
get_price.rdf <- function(con, stocks, buz_day, type = 'close')
{
  buz_day <- ymd(buz_day)
  send_query <- sprintf("SELECT wind_code as code, %s as price FROM price_data where trade_dt = %s and wind_code in (%s) order by field(code,%s)",
                        switch(type, close = 's_adj_close', vwap = 's_adj_avgprice'),
                        format(buz_day,'%Y%m%d'),
                        paste0("'",stocks,"'", collapse = ','),
                        paste0("'",stocks,"'", collapse = ','))
  if(class(con$con) == 'MySQLConnection')
  {
    result <- dbGetQuery(con$con, send_query)
  }else{
    result <- sqlQuery(con$con, send_query)
  }
  if(nrow(result) != length(stocks))
  {
    stop(sprintf('%s is not in database on %s', paste0(setdiff(stocks, result$code), collapse = ','), format(buz_day, '%Y%m%d')))
  }
  return(result)
}
