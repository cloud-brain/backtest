#' Test if the stock can buy
#'
#' Test if the stock can buy or not at business day
#'
#' @param con a tiny connection
#' @param wind_code vecter of future code list in wind_code format
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
#' if_can_buy(con, c('600000.SH','000001.SH'), 20110104)
#' }
#' 
#' @importFrom DBI dbGetQuery
#' @export
#'
if_can_buy <- function(con, wind_code, buz_day, ...) UseMethod('if_can_buy')

#' @export
if_can_buy.default <- function() return('unknown type')

if_can_buy.tiny <- function(con, wind_code, buz_day) {}

#' @export
if_can_buy.rdf <- function(con, wind_code, buz_day, full = F)
{
  stopifnot(!is.null(buz_day))
  buz_day <- dt_to_char(buz_day[1])
  if(full)
  {
    send_query <- sprintf("SELECT wind_code as code, canbuy FROM price_data where trade_dt = %s and wind_code in (%s) order by field(code,%s)",
                          buz_day, comb_char(wind_code), comb_char(wind_code))
  }else{
    send_query <- sprintf("SELECT wind_code as code FROM price_data where trade_dt = %s and canbuy = 1 and wind_code in (%s)",
                          buz_day, comb_char(wind_code))
  }
  
  result <- dbGetQuery(con$con, send_query)
  
  if(nrow(result) != length(wind_code))
    stop(sprintf('%s is not in database on %s', paste0(setdiff(wind_code, result$code), collapse = ','), format(buz_day,'%Y%m%d')))
  
  if(full)
  {
    return(result)
  }else{
    return(result$code)
  }
}
