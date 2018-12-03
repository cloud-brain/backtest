#' future_acount
#'
#' create a R6 object of future_acount.
#' 
#' @usage 
#' \preformatted{
#' p <- future_acount$new(con, fee = 0.005, acount_only = F)
#' p$set_connection(con)
#' p$show_connection()
#' p$order_buy(date, stock, num = NULL, amount = NULL)
#' p$order_sell(date, stock, num = NULL)
#' p$order_to(date, stock, weight, amount = 'all')
#' p$acount_update(date)
#' p$show_stock()
#' p$show_acount_f()
#' p$show_trade_history()
#' p$show_total_acount()
#' p$show_holder_history(date)
#' }
#'
#' @param con connection for data
#' @param fee float, the fee you sold stock
#' @param acount_f int, initial acount value, default 1m
#' @param date date, the date you alter you stock acount
#' @param stock vector, the stock you buy or sell
#' @param num vector, the stock vol you buy or sell
#' @param amount vector, the stock amount you buy
#' @param acount_only logic, if save the trade_history and other details
#' 
#' @details 
#' An \code{\link{R6Class}} generator object
#'
#' @format
#' \describe{
#' \item{\code{stock_acount$new(con, fee, acount_only)}}{to create new object, this fee is charged when sell stock;}
#' \item{\code{set_connection(con)}}{to set the connection;}
#' \item{\code{show_connection(con)}}{to return the connection;}
#' \item{\code{order_buy(date, stock, num, amount)}}{to buy the stock list, one of num and amount is needed.
#' if it have both, the num will be used. amount is preferd because adjust price is easily wrong.
#' if lenght amount is one, it will be rep for every stock.}
#' \item{\code{order_sell(date, stock, num)}}{to sell the stock with fix number}
#' \item{\code{order_to(date, stock, weight, amount = 'all')}}{buy the stock to the target. for unknown amount , it use weight instead}
#' \item{\code{acount_update(date)}}{to update the acount by close price, date can be a vector, so that can update several days for once}
#' \item{\code{show_stock()}}{to show the stock hold now}
#' \item{\code{show_acount_f()}}{to show the stock free money}
#' \item{\code{show_trade_history()}}{to show trade_history}
#' \item{\code{show_total_acount()}}{to show total acount history}
#' \item{\code{show_holder_history(date)}}{to show holder stock in history, date can be vector}
#' }
#'
#'
#' @docType class
#'
#' @examples
#' \dontrun{
#' my_acount <- stock_acount$new(con = con)
#' my_acount$order_buy(as.Date('2015-01-04'), stock = '600000.SH',num = 1000)
#' my_acount$acount_update(as.Date('2015-01-04'))
#' my_acount$order_sell(as.Date('2015-01-05'), stock = '600000.SH', num = 500)
#' my_acount$acount_update(as.Date('2015-01-05'))
#' my_acount$show_acount_f()
#' }
#'
#' @keywords data
#' @export
#' @importFrom R6 R6Class
#' @import tidyverse
#' @importFrom DBI dbGetQuery


#' @export
#' 
future_acount <-
  R6Class("future_acount",
          public = list(
            initialize = function(con, fee = 0.005, acount_f = 1000000, acount_only = F)
            {
              private$con <- con
              private$acount_f <- acount_f
              private$fee <- fee
              private$acount_only <- acount_only
            },
            
            ##设定接口
            set_connection = function(con)
            {
              private$con <- con
            },
            
            ##展示接口
            show_connection = function(con)
            {
              return(private$con)
            },
            
            ##交易函数（仅支持数量）
            ##num +开仓 -平仓
            ##type 1多头，0空头
            order_trade = function(date, stock, num = NULL, type)
            {
              ##获取股票当日成交均价
              buy_price <- get_price_future(private$con, stock, date, type = 'vwap')
              ##至少数量
              if(is.null(num))
                stop("need num")
              ##获取保证金比例
              deposit_rate <- get_deposit_rate(private$con, stock)
              
              if(length(type) == 1)
              {
                type <- rep(type, length(stock))
              }else{
                if(length(type) != length(stock))
                  stop('direction length is not equal to stock')
              }
              
              stock_buy <- cbind(tibble(code = stock, num = num, type = type),
                                 price = buy_price$price,
                                 deposit_rate = deposit_rate$deposit_rate)
              
              ##更新持仓
              private$future_holder <- future_combine(private$future_holder, stock_buy)
              ##更新保证金
              private$maintain <- with(private$future_holder, sum(init_value * deposit_rate))
              ##本次交易保证金超过现金时提示错误
              if(private$maintain > private$acount_f) stop('maintain exceed acount_value')
              
              if(!private$acount_only)
              {
                ##更新交易历史
                private$trade_history <- rbind(private$trade_history, cbind(stock_buy, date = date))
              }
              return(NULL)
            },
            
            ##交易到指定的持仓（仅数量）
            ##num 为数量，因此仅为正数
            order_to = function(date, stock, num = NULL, type)
            {
              if(any(num < 0))
                stop('num only positive')
              
              ##如果没有持仓，直接购买
              if(nrow(private$future_holder) == 0)
              {
                self$order_trade(date, stock, num = NULL, type)
                return(NULL)
              }
              target_pf <- tibble(code = stock, num = num, type = type)
              stock_buy <- stock_combine(target_pf, private$future_holder %>% mutate(num = -1 * num))
              self$order_trade(date, stock_buy$code, num = stock_buy$num, stock_buy$type)
            },
            
            ##更新组合净值
            acount_update = function(date)
            {
              if(length(date) == 1)
              {
                future_cg <- 0
                ##当持仓时计算持仓市值
                if(nrow(private$future_holder)>0)
                {
                  close_price <- get_price_future(private$con, private$future_holder$code, date, type = 'close')
                  future_cg <- with(private$future_holder, sum(type * (num * close_price$price - init_value)))
                  private$future_holder$init_value <- private$future_holder$num * close_price$price
                  private$maintain <- with(private$future_holder, sum(init_value * deposit_rate))
                }
                ##更新现金变化
                private$acount_f <- private$acount_f + future_cg
                ##本次交易保证金超过现金时提示错误
                if(private$maintain > private$acount_f) stop('maintain exceed acount_value')
                
                private$total_acount <- rbind(private$total_acount,
                                              data.frame(trade_dt = date, acount = private$acount_f, maintain = private$maintain))
              }else{
                future_cg <- 0
                ##当持仓时计算持仓市值
                if(nrow(private$future_holder)>0)
                {
                  close_price <- get_price_future(private$con, private$future_holder$code, min(date), max(date))
                  asset_price <- left_join(close_price, private$future_holder, by = 'code') %>%
                    mutate(asset_v = price * num) 
                  future_cg <- asset_price %>% mutate(future_cg = asset_v - init_value) %>% 
                    group_by(trade_dt) %>% summarise(maintain = sum(asset_v * deposit_rate),
                                                     acount = private$acount_f + sum(future_cg * type))
                  
                  ##更新现金变化
                  private$acount_f <- future_cg$acount[future_cg$trade_dt == max(future_cg$trade_dt)]
                  
                  ##更新市值变化
                  private$total_acount <- rbind(private$total_acount, future_cg %>% arrange(trade_dt))
                }else{
                  ##更新市值变化
                  private$total_acount <- rbind(private$total_acount,
                                                data.frame(trade_dt = date, acount = private$acount_f))
                }
              }
            },
            
            show_stock = function() return(private$future_holder),
            
            show_acount_f = function() return(private$acount_f),
            
            show_trade_history = function() return(private$trade_history),
            
            ##返回账户净值
            show_total_acount = function(num = NULL)
            {
              if(is.null(num))
              {
                return(private$total_acount)
              }else{
                return(tail(private$total_acount, num))
              }
            },
            
            ##返回当日持仓
            show_holder_history = function(show_date)
            {
              if(!lubridate::is.Date(show_date))
                stop("input should be Date")
              if(private$acount_only)
                return(data.frame())
              mapply(function(x) cbind(date = x, private$trade_history %>%
                                         filter(date < x) %>%
                                         group_by(code) %>%
                                         summarise(num = sum(num * type)) %>%
                                         filter(num >0)), show_date, SIMPLIFY = F) %>% do.call('rbind', .)
            }
          ),
          private = list(
            con = NULL,
            acount_f = 0,
            fee = 0.005,
            acount_only = F,
            maintain = 0,
            future_holder = NULL,
            total_acount = data.frame(),
            ##用于储存交易日志
            trade_history = tibble()
          )
  )

##用于组合数据表
##future列表的组合为code,num
future_combine <- function(stock_old, stock_chg)
{
  if(is.null(stock_old))
    return(temp %>% mutate(init_value = num * price))
  temp <- full_join(stock_old, stock_chg %>% rename(num_d = num), by = c('code', 'type'), all = T)
  temp <- temp %>% mutate(num = fill_na(num) + fill_na(num_d),
                          init_value = fill_na(init_value) + fill_na(price * num_d),
                          deposit_rate = ifelse(is.na(deposit_rate.x), deposit_rate.y, deposit_rate.x))
  
  return(temp %>% select(code, num, type, init_value, deposit_rate))
}

stock_old <- private$future_holder
stock_chg <- stock_buy
