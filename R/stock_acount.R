#' stock_acount
#'
#' create a R6 object of stock_acount.
#' 
#' @usage 
#' \preformatted{
#' p <- stock_acount$new(con, fee = 0.005, acount_only = F)
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
#' con <- odbcConnect('tiny')
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
#' @import dplyr tibble
#' @importFrom RODBC sqlQuery
#' @importFrom DBI dbGetQuery


#' @export
#' 
stock_acount <-
  R6Class("stock_acount",
          public = list(
            initialize = function(con, fee = 0.005, acount_only = F)
            {
              private$con <- con
              private$acount_f <- 1000000
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

            ##买入函数（支持数量，金额）
            ##不推荐使用数量，反之由于前复权价格上有问题
            ##买入函数不剔除无法买入股票
            order_buy = function(date, stock, num = NULL, amount = NULL)
            {
              ##获取股票当日成交均价
              buy_price <- get_price(private$con, stock, date, type = 'vwap')
              ##至少需要金额或数量之一
              if(is.null(amount) & is.null(num))
                stop("need num or amount")

              ##优先考虑金额
              ##按金额买入的转化称为数量
              if(!is.null(amount))
              {
                ##仅一个输入值是认为等金额
                if(length(amount) == 1)
                {
                  amount <- rep(amount, length(stock))
                }else{
                  if(length(amount)!=length(stock))
                    stop('stock and amount had different length')
                }
                private$acount_f <- private$acount_f - sum(amount)
                stock_buy <- cbind(tibble(code = stock, num = amount),
                                   price = buy_price$price) %>%
                  mutate(num = amount / price)
              }else{
                warning('make sure use the backward adjust price')
                stock_buy <- cbind(tibble(code = stock, num = num),
                                   price = buy_price$price)
                private$acount_f <- private$acount_f - with(stock_buy, sum(num * price))
              }

              ##提示余额错误
              if(private$acount_f< -1) stop("account is not enough")

              ##更新持仓
              private$stock_holder <- stock_combine(private$stock_holder, stock_buy, type = 'add')
              
              if(!private$acount_only)
              {
                ##更新交易历史
                private$trade_history <- rbind(private$trade_history, cbind(stock_buy, date = date, type = 1))
              }
            },

            ##卖出函数（仅支持数量）
            ##卖出函数不提出无法卖出股票
            order_sell = function(date, stock, num = NULL)
            {

              ##获取股票当日成交均价
              sell_price <- get_price(private$con, stock, date, type = 'vwap')

              ##构建卖出组合
              sell_stock <- cbind(tibble(code = stock, num = num),
                                  price = sell_price$price)

              ##更新账户净值
              private$acount_f <- with(sell_stock, sum(price * num)) * (1 - private$fee) + private$acount_f

              ##修正持仓数据
              private$stock_holder <- stock_combine(private$stock_holder, sell_stock, type = 'sub') %>% filter(num != 0)

              if(any(private$stock_holder$num <0))
                stop("can't short stock")

              
              if(!private$acount_only)
              {
                ##更新交易历史
                private$trade_history <- rbind(private$trade_history, cbind(sell_stock, date = date, type = -1))
              }
            },

            ##卖出到指定的支持，涉及卖出，因此仅接受权重
            ##amount为all是全部卖出
            order_to = function(date, stock, weight, amount = 'all')
            {
              ##如果没有持仓，直接购买
              if(nrow(private$stock_holder) == 0)
              {
                if(amount == 'all')
                {
                  self$order_buy(date, stock, amount = private$acount_f * weight)
                }else{
                  self$order_buy(date, stock, amount = amount * weight)
                }
                return(tibble())
              }
              
              ##目标股票池
              stock_target <- tibble(code = stock, weight) %>%
                mutate(price = get_price(private$con, code, date, type = 'vwap')$price)
              
              ##当前股票池
              stock_now <- mutate(private$stock_holder,
                                  canbuy = if_can_buy(private$con, code, date, full = T)$canbuy)
              wait_sell <- subset(stock_now, canbuy == 0) %>% select(-canbuy)
              stock_now <- subset(stock_now, canbuy == 1) %>% select(-canbuy) %>% 
                mutate(price = get_price(private$con, code, date, type = 'vwap')$price)
              
              if(amount == 'all')
              {
                
                ##第一次确定可用金额
                acount_total <- private$acount_f +
                  (1 - private$fee) * with(stock_now, sum(num * price))
                
                #确定目标持仓
                stock_target <- stock_target %>% mutate(num = acount_total * weight / price)
                
                ##第一次确定卖出量
                stock_change <- stock_combine(stock_target, stock_now, type = 'sub', add_price = T)
                stock_sell <- stock_change %>% subset(num < 0)
                
                ##第二次确定可用金额
                acount_total <- private$acount_f + with(stock_now, sum(num * price)) +
                  0.005 * with(stock_sell, sum(num * price))
                
                ##修正目标持仓和卖出量
                stock_target <- stock_target %>% mutate(num = acount_total * weight / price)
                stock_change <- stock_combine(stock_target, stock_now, type = 'sub', add_price = T)
                stock_sell <- stock_change %>% subset(num < 0)
                stock_buy <- stock_change %>% subset(num > 0)
                
                ##修正剩余资金
                private$acount_f <- private$acount_f +
                  0.995 * with(stock_sell, sum(-num * price)) - with(stock_buy, sum(num * price))
                
              }else{
                #确定目标持仓
                stock_target <- stock_target %>% mutate(num = amount * weight / price)

                ##确定卖出量
                stock_change <- stock_combine(stock_target, stock_now, type = 'sub', add_price = T)
                stock_sell <- stock_change %>% filter(num < 0) %>% mutate(num = abs(num))
                stock_buy <- stock_change %>% filter(num > 0)

                private$acount_f <- private$acount_f + with(stock_sell, sum(price * num)) * (1 - private$fee) -
                  with(stock_buy, sum(price * num))

                if(private$acount_f< -1) stop("account is not enough")
              }
              ##记录操作
              ###无法出售+目标持仓等于当前持仓
              private$stock_holder <- stock_combine(stock_target, wait_sell, type = 'add') %>%
                select(code, num) %>% filter(num > 0)
              
              if(!private$acount_only)
              {
                private$trade_history <- rbind(private$trade_history,
                                               cbind(stock_sell, date = date, type = -1),
                                               cbind(stock_buy, date = date, type = 1))
              }
              return(wait_sell)
            },

            ##更新股票净值
            acount_update = function(date)
            {
              if(length(date) == 1)
              {
                asset_price <- 0
                ##当持仓时计算持仓市值
                if(nrow(private$stock_holder)>0)
                {
                  close_price <- get_price(private$con, private$stock_holder$code, date, type = 'close')
                  asset_price <- sum(private$stock_holder$num * close_price$price)
                }
                ##更新市值变化
                private$total_acount <- rbind(private$total_acount,
                                              data.frame(date = date, acount = private$acount_f + asset_price))
              }else{
                asset_price <- 0
                ##当持仓时计算持仓市值
                if(nrow(private$stock_holder)>0)
                {
                  close_price <- get_price_pd(private$con, private$stock_holder$code, min(date), max(date))
                  asset_price <- left_join(close_price, private$stock_holder, by = 'code') %>%
                    mutate(asset_v = price * num) %>% group_by(date = ymd(trade_dt)) %>%
                    summarise(acount = sum(asset_v) + private$acount_f)
                  ##更新市值变化
                  private$total_acount <- rbind(private$total_acount, asset_price)
                }else{
                  ##更新市值变化
                  private$total_acount <- rbind(private$total_acount,
                                                data.frame(date = date, acount = private$acount_f))
                }
              }
            },

            show_stock = function() return(private$stock_holder),

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
            stock_holder = tibble(code = character(0), num = integer(0)),
            total_acount = data.frame(),
            ##用于储存交易日志
            trade_history = tibble()
          )
  )

##用于组合数据表
##stock列表的组合为code,num
stock_combine <- function(stock_old, stock_chg, type = c('add','sub'), add_price = F)
{
  type <- switch(type[1], add = 1, sub = -1)
  temp <- full_join(stock_old, stock_chg %>% rename(num_d = num), by = 'code', all = T)
  temp <- temp %>% mutate(num = ifelse(is.na(num), 0, num) + type * ifelse(is.na(num_d), 0, num_d))
  if(add_price)
  {
    return(temp %>% mutate(price = ifelse(is.na(price.x), price.y, price.x)) %>% select(code, num, price))
  }else{
    return(temp %>% select(code, num))
  }
}

