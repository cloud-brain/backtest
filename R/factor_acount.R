#' factor_acount
#'
#' create a R6 object of acount to analyse factor. can calculate the factor/number rank stock easily.
#' 
#' @usage 
#' \preformatted{
#' p <- factor_acount$new(con, buy_stock = NULL)
#' p$factor_update(buy_num, begin_date, end_date, wait_sell = F, refresh = T, show_trace = T)
#' p$show_wait_sell()
#' }
#'
#' @param con connection for data
#' @param buy_stock data.frame, column code, buy_date(date), num(int)
#' @param begin_date int, the first date to backtest
#' @param end_date, int, the end date to backtest
#' @param wait_sell, logic. if True, sell the stock wait for sell every day
#' @param refresh, logic, clean acount history
#' @param show_trace, logic, print history day by day
#' @param ... the paramater inherit from stock_acount
#'
#' @details
#' An \code{\link{R6Class}} generator object inherit from stock_acount
#'
#' @docType class
#'
#' @format
#' \describe{
#' \item{\code{stock_acount$new(con, buy_stock)}}{to create new object}
#' \item{\code{factor_update(buy_num, begin_date, end_date, wait_sell = F, refresh = T, show_trace = T)}}{factor buy top buy_num stock every month}
#' \item{\code{show_wait_sell()}}{to show stock can't sell}
#' }
#'
#'
#' @examples
#' \dontrun{
#' con <- odbcConnect('tiny')
#' my_acount <- factor_acount$new(con = con, 
#'                                buy_stock = data_all %>% 
#'                                   transmute(code = wind_code, buy_date = ymd(trade_dt)) %>%
#'                                   group_by(buy_date) %>% mutate(num = 1:n()))
#' my_acount$factor_update(20, 20170101, 20170531, wait_sell = T)
#' }
#'
#' @keywords data
#' @export
#' @importFrom R6 R6Class
#' @import dplyr tibble
#' @importFrom RODBC sqlQuery
#' @importFrom DBI dbGetQuery

factor_acount <-
  R6Class("factor_acount",
          inherit = stock_acount,
          public = list(
            buy_stock = NULL,
            initialize = function(buy_stock, ...)
            {
              self$buy_stock <- buy_stock
              private$buy_date <- unique(buy_stock$buy_date)
              super$initialize(...)
            },
            factor_update = function(buy_num, begin_date, end_date, wait_sell = F, refresh = T, show_trace = T)
            {
              if(refresh)
              {
                ##重置账户
                private$acount_f = 1000000
                private$stock_holder = tibble(code = character(0), num = integer(0))
                private$total_acount = data.frame()
                private$trade_history = tibble()
                private$wait_for_sell = data.frame()
              }

              ##确定交易日
              buz_day <- get_buz_day(private$con, begin_date, end_date) %>% ymd

              ##需要每日卖出时必须每日遍历
              if(wait_sell)
              {
                for(i in buz_day)
                {
                  i <- as.Date(i, origin = ymd(19700101))

                  ##持续挂单卖出需交易的股票
                  if(nrow(private$wait_for_sell) > 0)
                  {
                    sell_list <- subset(private$wait_for_sell, code %in% if_can_buy(private$con, private$wait_for_sell$code, i))
                    if(nrow(sell_list) > 0)
                    {
                      private$wait_for_sell <- subset(private$wait_for_sell, !(code %in% sell_list$code))
                      self$order_sell(i, sell_list$code, sell_list$num)
                    }
                  }

                  ##买到指定仓位
                  if(i %in% private$buy_date)
                  {
                    buy_list <- filter(self$buy_stock, buy_date == i)
                    buy_list <- buy_list %>% filter(code %in% if_can_buy(private$con, buy_list$code, i)) %>%
                      top_n(buy_num, -num)
                    private$wait_for_sell <- self$order_to(i, buy_list$code, weight = rep(1/nrow(buy_list), nrow(buy_list)))
                  }
                  self$acount_update(i)
                  if(show_trace)
                    self$show_total_acount(1) %>% print
                }
              }else{
                ##不需要时以买入日获得
                first_d <- buz_day[1]
                buy_date_t <- private$buy_date[between(private$buy_date, ymd(begin_date), ymd(end_date))] %>% sort
                for(i in buy_date_t)
                {
                  i <- as.Date(i, origin = ymd(19700101))
                  update_date <- buz_day[buz_day >= first_d & buz_day < i]
                  if(length(update_date) > 0)
                  {
                    self$acount_update(update_date)
                    if(show_trace)
                      self$show_total_acount(length(update_date)) %>% print
                  }
                  first_d <- i
                  buy_list <- subset(self$buy_stock, buy_date == i)
                  buy_list <- buy_list %>% subset(code %in% if_can_buy(private$con, buy_list$code, i)) %>%
                    top_n(buy_num, -num)
                  self$order_to(i, buy_list$code, weight = rep(1/nrow(buy_list), nrow(buy_list)))
                }
                update_date <- buz_day[buz_day >= first_d]
                self$acount_update(update_date)
                if(show_trace)
                  self$show_total_acount(length(update_date)) %>% print
              }
            },
            show_wait_sell = function()
            {
              return(private$wait_for_sell)
            }
          ),
          private = list(
            buy_date = NULL,
            wait_for_sell = data.frame()
          )
  )
