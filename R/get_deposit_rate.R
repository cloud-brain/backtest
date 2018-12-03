#' Get future deposit rate
#'
#' Get future deposit rate
#'
#' @param con a data connection
#' @param wind_code vecter of future code list in wind_code format
#' 
#' @return
#' data.frame with wind_code, deposit_rate
#' 
#' @examples
#' \dontrun{
#' get_price(con, c('600000.SH','000001.SH'), 20110104, type = 'close')
#' }
#'
#' @importFrom DBI dbGetQuery
#' @import tidyverse
#'
#' @export
#'
get_deposit_rate <- function(con, wind_code)
{
  output <- tibble(wind_code) %>% mutate(future_type = substr(wind_code, 1, 2))
  send_query <- sprintf("SELECT * FROM future_deposit_rate where future_type in (%s)",
                        comb_char(output$future_type %>% unique))
  deposit_rate <- dbGetQuery(con$con, send_query)
  output %>% left_join(deposit_rate, by = 'future_type') %>% select(-future_type)
}