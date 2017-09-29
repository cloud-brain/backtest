#' Test if the stock is st or not
#'
#' Test if the stock is st or not at business day
#'
#' @param con a tiny connection
#' @param stocks vecter of stock code list in tiny format
#' @param buz_day integer or character format like \%Y\%m\%d, test for this day
#'
#' @return
#' a data.frame with stock code and flag that 1 means st and 0 not
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
is_st <- function(...)
{
  UseMethod('is_st')
}

#' @rdname is_st
#' @export
is_st.default <- function(...)
{
  return('unknown con type')
}

#' @rdname is_st
#' @export
is_st.tiny <- function(con, stocks, buz_day)
{
  buz_day <- ymd(buz_day)
  return(sqlQuery(con, sprintf("return is_st(array(%s),%s);",
                               paste0("'",stocks,"'", collapse = ','),format(buz_day,'%Y%m%d'))))
}

#' @rdname is_st
#' @export
is_st.rdf <- function(con, stocks, buz_day)
{
  buz_day <- ymd(buz_day)
  if(full)
  {
    send_query <- sprintf("SELECT wind_code as code, isst FROM price_data where trade_dt = %s and wind_code in (%s) order by field(code,%s)",
                          format(buz_day,'%Y%m%d'),
                          paste0("'",stocks,"'", collapse = ','),
                          paste0("'",stocks,"'", collapse = ','))
  }else{
    send_query <- sprintf("SELECT wind_code as code FROM price_data where trade_dt = %s and isst = 0 and wind_code in (%s)",
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
    return(result)
  }else{
    return(result$code)
  }
}
