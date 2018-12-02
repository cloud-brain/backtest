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
  buz_day <- dt_to_char(buz_day)
  
  if(offset == 0)
  {
    return(as.integer(buz_day))
  }else{
    if(offset > 0)
    {
      result <- dbGetQuery(con$con, sprintf("SELECT trade_dt FROM calendar_data where trade_dt > %s order by trade_dt limit %d", buz_day, offset))
    }else{
      result <- dbGetQuery(con$con, sprintf("SELECT trade_dt FROM calendar_data where trade_dt < %s order by trade_dt desc limit %d", buz_day, abs(offset)))
    }
  }
  return(result$trade_dt %>% tail(1))
}
