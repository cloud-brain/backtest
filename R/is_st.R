#' Test if the stock is st or not
#'
#' Test if the stock is st or not at business day
#'
#' @param con a tiny connection
#' @param wind_code vecter of future code list in wind_code format
#' @param buz_day integer or character format like \%Y\%m\%d, test for this day
#'
#' @return
#' a data.frame with stock code and flag that 1 means st and 0 not
#'
#' @examples
#' \dontrun{
#' is_st(con, c('600000.SH','000001.SH'), 20110104)
#' }
#'
#' @importFrom DBI dbGetQuery
#'
#' @export
#'
is_st <- function(con, wind_code, buz_day, ...) UseMethod('is_st')

#' @export
is_st.default <- function() return('unknown con type')

is_st.tiny <- function(con, wind_code, buz_day) {}

#' @export
is_st.rdf <- function(con, wind_code, buz_day)
{
  buz_day <- dt_to_char(buz_day)
  if(full)
  {
    send_query <- sprintf("SELECT wind_code as code, isst FROM price_data where trade_dt = %s and wind_code in (%s) order by field(code,%s)",
                          buz_day, comb_char(wind_code), comb_char(wind_code))
  }else{
    send_query <- sprintf("SELECT wind_code as code FROM price_data where trade_dt = %s and isst = 0 and wind_code in (%s)",
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
