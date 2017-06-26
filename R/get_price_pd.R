#' Get stock price for a period
#'
#' Get stock price for a period
#'
#' @param con a tiny connection
#' @param stocks vecter of stock code list in tiny format
#' @param beg_date integer or character format like \%Y\%m\%d, test for this day
#' @param end_date integer or character format like \%Y\%m\%d, test for this day
#'
#' @return
#' a data.frame with stock code and price
#'
#' @examples
#' \dontrun{
#' con <- odbcConnect('tiny')
#' is_st(con, c('SH600000','SZ000001'), 20110104)
#' }
#'
#' @importFrom RODBC sqlQuery
#' @importFrom DBI dbGetQuery
#'
#' @export
#'
get_price_pd <- function(...)
{
  UseMethod('get_price_pd')
}

#' @rdname get_price
#' @export
get_price_pd.default <- function(...)
{
  stop('unknown con type')
}


#' @rdname get_price
#' @export
get_price_pd.rdf <- function(con, stocks, begin_date, end_date)
{
  send_query <- sprintf("SELECT wind_code as code, trade_dt, s_adj_close as price FROM price_data where trade_dt between %s and %s and wind_code in (%s)",
                        format(begin_date,'%Y%m%d'),
                        format(end_date,'%Y%m%d'),
                        paste0("'",stocks,"'", collapse = ','))
  if(class(con$con) == 'MySQLConnection')
  {
    result <- dbGetQuery(con$con, send_query)
  }else{
    result <- sqlQuery(con$con, send_query)
  }
  error_test <- result %>% count(code) %>% filter(n < max(n))
  if(nrow(error_test) > 0)
  {
    stop(sprintf('%s is not in database', paste0(error_test$code, collapse = ',')))
  }
  return(result)
}
