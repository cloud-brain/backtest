#' Test if the stock can buy
#'
#' Test if the stock can buy or not at business day
#'
#' @param con a tiny connection
#' @param stocks vecter of stock code list in tiny format
#' @param buz_day int or char, format like \%Y\%m\%d, test for this day
#'
#' @details
#' if the amount equal to 0 or high price equal to low price, the stock regards can't buy that day.
#'
#' @return
#' a data.frame with stock code and flag that 1 means purchasable and 0 not
#'
#' @examples
#' \dontrun{
#' con <- odbcConnect('tiny')
#' if_can_buy(con, c('SH600000','SZ000001'), 20110104)
#' }
#' @importFrom RODBC sqlQuery
#' @importFrom DBI dbGetQuery
#' @export
#'
if_can_buy <- function(...) UseMethod('if_can_buy')

#' @rdname if_can_buy
#' @export
if_can_buy.default <- function(...)
{
  return('unknown type')
}

#' @rdname if_can_buy
#' @export
if_can_buy.tiny <- function(con, stocks, buz_day)
{
  stopifnot(!is.null(buz_day))
  return(sqlQuery(con$con, sprintf("return buy_or_not(array(%s),%s);",
                               paste0("'",stocks,"'", collapse = ','),format(buz_day,'%Y%m%d'))))
}

#' @rdname if_can_buy
#' @export
if_can_buy.rdf <- function(con, stocks, buz_day, full = F)
{
  stopifnot(!is.null(buz_day))

  if(full)
  {
    send_query <- sprintf("SELECT wind_code as code, canbuy FROM price_data where trade_dt = %s and wind_code in (%s) order by field(code,%s)",
                          format(buz_day,'%Y%m%d'),
                          paste0("'",stocks,"'", collapse = ','),
                          paste0("'",stocks,"'", collapse = ','))
  }else{
    send_query <- sprintf("SELECT wind_code as code FROM price_data where trade_dt = %s and canbuy = 1 and wind_code in (%s)",
                          format(buz_day,'%Y%m%d'), paste0("'",stocks,"'", collapse = ','))
  }
  if(class(con$con) == 'MySQLConnection')
  {
    result <- dbGetQuery(con$con, send_query)
  }else{
    result <- sqlQuery(con$con, send_query)
  }
  if(full)
  {
    if(nrow(result) != length(stocks))
      stop(sprintf('%s is not in database on %s', paste0(setdiff(stocks, result$code), collapse = ','), format(buz_day,'%Y%m%d')))
    return(result)
  }else{
    return(result$code)
  }
}
