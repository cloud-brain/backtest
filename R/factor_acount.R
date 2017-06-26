#' factor_acount
#'
#' create a R6 object of acount to analyse factor. can calculate the factor/number rank stock easily.
#' \code{factor_update} update the history date
#'
#' @param con a tiny connection
#' @param fee float, the fee you sold stock
#' @param begin_date, the first date to buy stock list
#' @param end_date, the end date to hold stock list
#' @param wait_sell, logical. if True, sell the stock sell every day
#' @param ... the paramater inherit from stock_acount
#'
#' @details
#' it inherit from stock_acount
#'
#'
#' @docType class
#'
#' @format An \code{\link{R6Class}} generator object
#' \preformatted{
#'  stock_acount$new(con, buy_stock, fee = 0.005)
#'  factor_update(buy_num, begin_date, end_date, wait_sell = F, refresh = T, show_trace = T)
#' }
#'
#'
#' @examples
#' \dontrun{
#' con <- odbcConnect('tiny')
#' my_acount <- stock_acount$new(con = con, acount_f = 1000000)
#' my_acount$order_buy(as.Date('2015-01-04'), stock = 'SH600000',num = 1000)
#' my_acount$acount_update(as.Date('2015-01-04'))
#' my_acount$order_sell(as.Date('2015-01-05'), stock = 'SH600000', num = 1000)
#' my_acount$acount_update(as.Date('2015-01-05'))
#' my_acount$show_acount_f()
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
            }
          ),
          private = list(
            buy_date = NULL,
            wait_for_sell = data.frame()
          )
  )
