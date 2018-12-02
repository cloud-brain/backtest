#' Get stock price for one day
#'
#' Get stock price for one day
#'
#' @param con a data connection
#' @param wind_code vecter of future code list in wind_code format
#' @param beg_dt int or or char, format like \%Y\%m\%d, test for this day
#' @param end_dt int or or char, format like \%Y\%m\%d, test for this day, default null
#' @param type choose price type as close, vwap
#'
#' @return
#' if end_date is null
#' a df with stock code and price
#' else
#' a df with stock code and price, trade_dt
#'
#' @examples
#' \dontrun{
#' get_price(con, c('600000.SH','000001.SH'), 20110104, type = 'close')
#' }
#'
#' @importFrom DBI dbGetQuery
#'
#' @export
#'
get_price <- function(con, wind_code, beg_dt, end_dt = NULL)
{
  UseMethod('get_price')
}

#' @rdname get_price
get_price.default <- function()
{
  stop('unknown con type')
}

#' @rdname get_price
get_price.tiny <- function()
{
  
}

#' @rdname get_price
get_price.rdf <- function(con, stocks, beg_dt, end_dt = NULL, type = 'close')
{
  beg_dt <- dt_to_char(beg_dt[1])
  end_dt <- dt_to_char(end_dt[1])
  
  if(is.null(end_dt))
  {
    send_query <- sprintf("SELECT wind_code as code, %s as price FROM price_data where trade_dt = %s and wind_code in (%s) order by field(code,%s)",
                          switch(type, close = 's_adj_close', vwap = 's_adj_avgprice'),
                          beg_dt, comb_char(stocks), comb_char(stocks))
  }else{
    send_query <- sprintf("SELECT wind_code as code, trade_dt, s_adj_close as price FROM price_data where trade_dt between %s and %s and wind_code in (%s)",
                          beg_dt, end_dt, comb_char(stocks))
  }

  result <- dbGetQuery(con$con, send_query)
  
  if(is.null(end_dt))
  {
    if(nrow(result) != length(stocks))
    {
      stop(sprintf('%s is not in database on %s', paste0(setdiff(stocks, result$code), collapse = ','), format(buz_day, '%Y%m%d')))
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
