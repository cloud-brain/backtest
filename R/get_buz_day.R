#' Get business day
#'
#' Get business day of stock market bewteen tow days
#'
#' @param con a tiny connection
#' @param begin_date the begin date, can be integer or character format like \%Y\%m\%d
#' @param end_date the end date, can be integer or character format like \%Y\%m\%d
#' @details
#' The connenction only support tiny. The begin date and end date should not be the business day.
#' The return vector will begin at the first business day after begin date.
#'
#' @return
#' a vector of date
#'
#' @examples
#' \dontrun{
#' con <- odbcConnect('tiny')
#' get_buz_day(con, 20100101, 20110101)
#' }
#'
#' @import dplyr
#' @importFrom RODBC sqlQuery
#' @importFrom DBI dbGetQuery
#' @export
#'
get_buz_day <- function(...)
{
  UseMethod('get_buz_day')
}

#' @rdname get_buz_day
#' @export
#'
get_buz_day.default <- function(...)
{
  return('unknown type')
}

#' @rdname get_buz_day
#' @export
get_buz_day.tiny <- function(con, begin_date, end_date)
{
  if(begin_date > end_date)
    stop('begin date must less than end date')
  begin_date <- ifelse(is.character(begin_date), begin_date %>% as.integer, begin_date)
  end_date <- ifelse(is.character(end_date), end_date %>% as.integer, end_date)
  return(sqlQuery(con, sprintf("return get_buz_day(%d,%d);",begin_date,end_date)))
}

#' @rdname get_buz_day
#' @export
get_buz_day.rdf <- function(con, begin_date, end_date)
{
  if(begin_date > end_date)
    stop('begin date must less than end date')
  begin_date <- ifelse(is.character(begin_date), begin_date %>% as.integer, begin_date)
  end_date <- ifelse(is.character(end_date), end_date %>% as.integer, end_date)
  if(class(con$con) == 'MySQLConnection')
  {
    result <- dbGetQuery(con$con, paste0("SELECT trade_dt FROM calendar_data where trade_dt between ",begin_date, " and ", end_date, " order by trade_dt"))
  }else{
    result <- sqlQuery(con$con, paste0("SELECT trade_dt FROM calendar_data where trade_dt between ",begin_date, " and ", end_date, " order by trade_dt"))
  }
  return(result$trade_dt)
}