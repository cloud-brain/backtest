#' trade day offset
#'
#' find the offset of trade day
#'
#' @param con, rdf connection
#' @param buz_day, integer or date
#' @param offset, integer
#'
#' @examples
#' tday_offset(con, 20170101, -10)
#'
#' @export
#'
tday_offset <- function(con, buz_day, offset = 0)
{
  buz_day <- ymd(buz_day)
  
  if(offset == 0)
  {
    return(buz_day %>% format('%Y%m%d') %>% as.integer)
  }else{
    if(offset > 0)
    {
      result <- dbGetQuery(con$con, sprintf("SELECT trade_dt FROM calendar_data where trade_dt > %s order by trade_dt limit %d", format(buz_day, '%Y%m%d'), offset))
    }else{
      result <- dbGetQuery(con$con, sprintf("SELECT trade_dt FROM calendar_data where trade_dt < %s order by trade_dt desc limit %d", format(buz_day, '%Y%m%d'), abs(offset)))
    }
  }
  return(result$trade_dt %>% tail(1))
}
