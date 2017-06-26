library(backtest)
library(RMySQL)

##stock_acount测试--------------------

load("C:/Users/hfjj/Desktop/量化策略/数据库/因子数据/bench_mark.RData")
bench_mark <- bench_mark %>% filter(!(code == 'SZ000618' & month == '200601'))
bench_mark <- bench_mark %>% filter(!(code == 'SZ000406' & month == '200603'))
bench_mark <- bench_mark %>% filter(!(code == 'SZ000866' & month == '200603'))
bench_mark <- bench_mark %>% filter(!(code == 'SZ000956' & month == '200603'))
bench_mark <- bench_mark %>% filter(!(code == 'SH600002' & month == '200603'))


multi_factor <- bench_mark %>%
  subset(isst==0 & firstday>90 & tradeday>5) %>%
  select(code, month) %>%
  mutate(month = as.character(month), code = paste0(substr(code,3,8),'.', substr(code,1,2)))

##根据月份调整买入卖出日
con <- dbConnect(MySQL(), dbname = "quant", username="cloud_brain",
                 password="laokaijun",client.flag=CLIENT_MULTI_STATEMENTS)
# con <- odbcConnect('quant')
con <- as.rdf(con)
begin_date <- min(multi_factor$month) %>% paste0('01'); end_date <- Sys.Date() %>% format('%Y%m%d')
buz_day <- get_buz_day(con, begin_date ,end_date) %>% ymd

##修正多因子的交易日
temp <- tibble(month = unique(multi_factor$month)) %>%
  mutate(buy_date = ymd(paste0(month,'01')) + months(1)) %>%
  mutate(buy_date = mapply(function(x) min(buz_day[buz_day>=x]), buy_date) %>% as.Date(origin = as.Date('1970-01-01')))

multi_factor <- merge(temp, multi_factor, by = 'month')
multi_factor <- multi_factor %>% na.omit

buy_date <- temp$buy_date


##回测时间
my_acount <- stock_acount$new(con = con, acount_f = 1000000, fee = 0)
wait_for_sell <- tibble()

begin_date <- 20050201; end_date <- 20060101
buz_day <- get_buz_day(con, begin_date,end_date) %>% ymd


for(i in buz_day)
{
  i <- as.Date(i, origin = as.Date('1970-01-01'))

  ##持续挂单卖出需交易的股票
  if(nrow(wait_for_sell) > 0)
  {
    sell_list <- subset(wait_for_sell, code %in% if_can_buy(con, wait_for_sell$code, i))
    if(nrow(sell_list) > 0)
    {
      wait_for_sell <- subset(wait_for_sell, !(code %in% sell_list$code))
      my_acount$order_sell(i, sell_list$code, sell_list$num)
    }
  }

  ##买到指定仓位
  if(i %in% buy_date)
  {
    buy_list <- subset(multi_factor, buy_date == i)
    buy_list <- subset(buy_list, code %in% if_can_buy(con, buy_list$code, i))
    wait_for_sell <- my_acount$order_to(i, buy_list$code, weight = rep(1/nrow(buy_list), nrow(buy_list)))
  }
  my_acount$acount_update(i)
  my_acount$show_total_acount(1) %>% print
}

dbDisconnect(con$con)
# odbcClose(con$con)

##根据月份调整买入卖出日
stock_holder <- my_acount$show_stock()
asset_price <- 0
close_price <- get_price(con, buy_list$code, i, type = 'close')
full_join(buy_list, close_price, by = 'code') %>% filter(is.na(price))


##效率测算-----------

load("C:/Users/cloud/Desktop/量化策略/数据库/因子数据/bench_mark.RData")
bench_mark <- bench_mark %>% filter(month == '201702')
bench_mark <- bench_mark %>% mutate(code = paste0(substr(code,3,8), '.',substr(code,1,2)))

library(RODBC)
con <- odbcConnect('quant')

stocks <- sample(bench_mark$code,300)
system.time({
send_query <- sprintf("SELECT wind_code as code, canbuy FROM price_data where trade_dt = %s and wind_code in (%s)",
                      '20170301', paste0("'",stocks,"'", collapse = ','))
result <- sqlQuery(con, send_query)
result <- result[match(result$code, stocks),]
})

system.time({
send_query <- sprintf("SELECT wind_code as code, canbuy FROM price_data where trade_dt = %s and wind_code in (%s)
                      order by field(code,%s)",
                      '20170321', paste0("'",stocks,"'", collapse = ','),  paste0("'",stocks,"'", collapse = ','))
result <- sqlQuery(con, send_query)
})

odbcClose(con)

##stock_acount测试------------------
date <- i
stock <- buy_list$code
weight <- rep(1/nrow(buy_list), nrow(buy_list))
amount <- 'all'
stock_holder <- my_acount$show_stock()

##目标股票池
stock_target <- tibble(code = stock, weight)

##当前股票池
stock_now <- mutate(stock_holder,
                    canbuy = if_can_buy(con, code, date, full = T)$canbuy)
wait_sell <- subset(stock_now, canbuy == 0) %>% select(-canbuy)
stock_now <- subset(stock_now, canbuy == 1) %>% select(-canbuy)

##获取价格信息
price_target <- get_price(con, stock_target$code, date, type = 'vwap')
price_now <- get_price(self$con, stock_now$code, date, type = 'vwap')

if(amount == 'all')
{
  ##第一次确定可用金额

  acount_total <- private$acount_f +
    (1 - private$fee) * sum(stock_now$num * price_now$price)

  #确定目标持仓
  stock_target <- stock_target %>% mutate(num = acount_total * weight / price_target$price)

  ##第一次确定卖出量
  stock_change <- stock_combine(stock_target, stock_now, type = 'sub')
  stock_sell <- stock_change %>% subset(num < 0)

  ##确定增加的资金量
  acount_add <- private$fee * (sum(stock_now$num * price_now$price) -
                                 sum(stock_sell$num * price_now$price[match(stock_sell$code, price_now$code)]))

  ##修正目标持仓和卖出量
  stock_target <- stock_target %>% mutate(num = num + acount_add * weight / price_target$price)
  stock_change <- stock_combine(stock_target, stock_now, type = 'sub')

  ##第二次确定买入量和卖出量
  stock_sell <- stock_change %>% filter(num < 0) %>%
    mutate(num = abs(num), price = price_now$price[match(code, price_now$code)])
  stock_buy <- stock_change %>% filter(num > 0) %>%
    mutate(price = price_target$price[match(code, price_target$code)])

  ##修正剩余资金
  private$acount_f <- 0
}else{
  #确定目标持仓
  price_target <- get_price(con, stock_target$code, date, type = 'vwap')
  stock_target <- stock_target %>% mutate(num = amount * weight / price_target$price)

  ##确定卖出量
  stock_change <- stock_combine(stock_target, stock_now, type = 'sub')
  stock_sell <- stock_change %>% filter(num < 0) %>%
    mutate(num = abs(num), price = price_now$price[match(code, price_now$code)])
  stock_buy <- stock_change %>% filter(num > 0) %>%
    mutate(price = price_target$price[match(code, price_target$code)])

  private$acount_f <- private$acount_f + with(stock_sell, sum(price * num)) * (1 - private$fee) -
    with(stock_buy, sum(price * num))

  if(private$acount_f< -1) stop("account is not enough")
}
##记录操作
###无法出售+目标持仓等于当前持仓
private$stock_holder <- stock_combine(stock_target, wait_sell, type = 'add') %>%
  select(code, num) %>% filter(num > 0)
private$trade_history <- rbind(private$trade_history,
                               cbind(stock_sell, date = date, type = -1),
                               cbind(stock_buy, date = date, type = 1))

load_all()


##factor_acount测试--------------------
con <- dbConnect(MySQL(), dbname = "quant", username="cloud_brain",
                 password="laokaijun",client.flag=CLIENT_MULTI_STATEMENTS)
data_all <- dbGetQuery(con, 'select trade_dt, wind_code, total_value from factor_data_m
                       where firstday>90 and tradeday > 5 and isst = 0')
dbDisconnect(con)

multi_factor <- data_all %>%
  group_by(trade_dt) %>% mutate(num = rank(total_value)) %>% filter(num < 100)

con <- dbConnect(MySQL(), dbname = "quant", username="cloud_brain",
                 password="laokaijun",client.flag=CLIENT_MULTI_STATEMENTS) %>% as.rdf

buz_day <- get_buz_day(con, min(multi_factor$trade_dt), max(multi_factor$trade_dt) + 10000)

##修正多因子的交易日
temp <- tibble(trade_dt = unique(multi_factor$trade_dt)) %>%
  mutate(buy_date = mapply(function(x) min(buz_day[buz_day>x]), trade_dt))

multi_factor <- left_join(multi_factor, temp, by = 'trade_dt') %>%
  mutate(buy_date = ymd(buy_date))

my_acount <- factor_acount$new(con = con, fee = 0, buy_stock = multi_factor %>% rename(code = wind_code))
my_acount$factor_update(20, 20060101, 20170418)

dbDisconnect(con$con)

total_acount <- my_acount$show_total_acount()
##分解测试
buy_stock <- multi_factor %>% rename(code = wind_code)
my_acount <- stock_acount$new(con = con, fee = 0)
buy_stock <- multi_factor %>% rename(code = wind_code)
begin_date <- 20060101
end_date <- 20170418
##确定交易日
buz_day <- get_buz_day(con, begin_date, end_date) %>% ymd
buy_date <- buy_stock$buy_date %>% unique %>% sort

##不需要时以买入日获得
first_d <- buz_day[1]
buy_date_t <- buy_date[buy_date >= ymd(begin_date)] %>% sort
for(i in buy_date_t)
{
  i <- as.Date(i, origin = as.Date('1970-01-01'))
  update_date <- buz_day[buz_day >= first_d & buz_day < i]
  if(length(update_date) > 0)
  {
    my_acount$acount_update(update_date)
    my_acount$show_total_acount(length(update_date)) %>% print
  }
  first_d <- i
  buy_list <- filter(buy_stock, buy_date == i)
  buy_list <- buy_list %>% filter(code %in% if_can_buy(con, buy_list$code, i)) %>%
    top_n(buy_num, -num)
  my_acount$order_to(i, buy_list$code, weight = rep(1/nrow(buy_list), nrow(buy_list)))
}
update_date <- buz_day[buz_day >= first_d]
my_acount$acount_update(update_date)
my_acount$show_total_acount(length(update_date)) %>% print

##profile analyst-------------------------------
library(RMySQL)
library(backtest)

con <- dbConnect(MySQL(), dbname = "quant", username="cloud_brain",
                 password="laokaijun",client.flag=CLIENT_MULTI_STATEMENTS)
data_all <- dbGetQuery(con, 'select wind_code, zf_m, trade_dt, total_value, amount_10
                       from factor_data_m where firstday>90 and tradeday > 5 and isst = 0')
dbDisconnect(con)

multi_factor <- data_all %>% na.omit %>% group_by(trade_dt) %>%
  mutate(num = rank(rank(amount_10) + rank(total_value), ties.method = "first")) %>%
  subset(num < 150)

con <- dbConnect(MySQL(), dbname = "quant", username="root",
                 password="laokaijun",client.flag=CLIENT_MULTI_STATEMENTS) %>% as.rdf
begin_date <- min(multi_factor$trade_dt); end_date <- Sys.Date() %>% format('%Y%m%d')
buz_day <- get_buz_day(con, begin_date ,end_date)

##修正多因子的交易日
temp <- tibble(trade_dt = unique(multi_factor$trade_dt)) %>%
  mutate(buy_date = mapply(function(x) min(buz_day[buz_day>x]), trade_dt) %>% ymd)

multi_factor <- left_join(multi_factor, temp, by = 'trade_dt')

multi_factor <- multi_factor %>% rename(code = wind_code)

##回测框架

my_acount <- stock_acount$new(con = con, fee = 0)
wait_for_sell <- tibble()

begin_date <- 20060101; end_date <- 20100101
buz_day <- get_buz_day(con, begin_date, end_date) %>% ymd


library(profvis)

p <- profvis({
first_d <- buz_day[1]
buy_date_t <- buy_date[buy_date >= ymd(begin_date) & buy_date < ymd(end_date)] %>% sort
for(i in buy_date_t)
{
  i <- as.Date(i, origin = ymd(19700101))
  update_date <- buz_day[buz_day >= first_d & buz_day < i]
  if(length(update_date) > 0)
  {
    my_acount$acount_update(update_date)

  }
  first_d <- i
  buy_list <- subset(multi_factor, buy_date == i)
  buy_list <- buy_list %>% filter(code %in% if_can_buy(con, buy_list$code, i)) %>%
    top_n(20, -num)
  my_acount$order_to(i, buy_list$code, weight = rep(1/nrow(buy_list), nrow(buy_list)))
}
update_date <- buz_day[buz_day >= first_d]
my_acount$acount_update(update_date)
})

dbDisconnect(con$con)
htmlwidgets::saveWidget(p, "profile.html")

# Can open in browser from R
browseURL("profile.html")
