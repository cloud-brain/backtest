#' Get future price for one day
#'
#' Get future price for one day
#'
#' @param con a tiny connection
#' @param wind_code vecter of future code list in wind_code format
#' @param beg_dt int or or char, format like \%Y\%m\%d, test for this day
#' @param end_dt int or or char, format like \%Y\%m\%d, test for this day, default null
#' @param type choose price type as close, vwap
#' @param balance logi, if a balance panel or not
#'
#' @return
#' if end_date is null
#' a df with stock code and price
#' else
#' a df with stock code and price, trade_dt
#'
#' @examples
#' \dontrun{
#' con <- odbcConnect('tiny')
#' get_price_future(con, 'IF1603.CFE', 20110104, type = 'close')
#' }
#'
#' @importFrom DBI dbGetQuery
#'
#' @export
#'
get_price_future <- function(con, wind_code, beg_dt, end_dt, type, balance,...)
{
  UseMethod('get_price_future')
}

#' @export
get_price_future.default <- function(con, wind_code, beg_dt) stop('unknown con type')

get_price_future.tiny <- function(con, wind_code, beg_dt, end_dt) {}

#' @export
get_price_future.rdf <- function(con, wind_code, beg_dt, end_dt = NULL, type = 'close', balance = T)
{
  beg_dt <- dt_to_char(beg_dt[1])
  end_dt <- dt_to_char(end_dt[1])
  
  if(is.null(end_dt))
  {
    send_query <- sprintf("SELECT wind_code as code, %s as price FROM price_base_future where trade_dt = %s and wind_code in (%s) order by field(code,%s)",
                          switch(type, close = 's_dq_settle', vwap = 's_dq_avgprice'),
                          beg_dt, comb_char(wind_code), comb_char(wind_code))
  }else{
    send_query <- sprintf("SELECT wind_code as code, trade_dt, s_dq_settle as price FROM price_base_future where trade_dt between %s and %s and wind_code in (%s)",
                          beg_dt, end_dt, comb_char(wind_code))
  }
  
  result <- dbGetQuery(con$con, send_query)
  if(!balance)
    return(result)
  
  if(is.null(end_dt))
  {
    if(nrow(result) != length(wind_code))
    {
      stop(sprintf('%s is not in database on %s', paste0(setdiff(wind_code, result$code), collapse = ','), format(buz_day, '%Y%m%d')))
    }
  }else{
    error_test <- result %>% count(code) %>% filter(n < max(n))
    if(nrow(error_test) > 0)
    {
      stop(sprintf('%s is not in database', paste0(error_test$code, collapse = ',')))
    }
  }
  
  return(result)
}
