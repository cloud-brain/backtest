library(devtools)
library(RMySQL)
load_all()

##无停牌测试，无交叉--------------------
data_all <- readr::read_csv('test/stock_clean.csv')

##根据月份调整买入卖出日
con <- dbConnect(MySQL(), dbname = "quant", username="cloud_brain", host = "172.20.11.38", post = 3306,
                 password="laokaijun",client.flag=CLIENT_MULTI_STATEMENTS) %>% as.rdf
buy_date <- data_all$trade_dt %>% unique

begin_date <- 20170101; end_date <- 20170531
buz_day <- get_buz_day(con, begin_date,end_date)
dbDisconnect(con$con)


##基础测试
my_acount <- stock_acount$new(con = con)
for(i in buz_day)
{
  ##买到指定仓位
  if(i %in% buy_date)
  {
    buy_list <- subset(data_all, trade_dt == i)
    sell_list <- my_acount$show_stock()
    if(nrow(sell_list) != 0)
    {
      my_acount$order_sell(ymd(i), sell_list$code, sell_list$num)
    }
    my_acount$order_buy(ymd(i), buy_list$wind_code, amount = my_acount$show_acount_f() / nrow(buy_list))
  }
  my_acount$acount_update(ymd(i))
  my_acount$show_total_acount(1) %>% print
}
dbDisconnect(con$con)

##order_to测试
con <- dbConnect(MySQL(), dbname = "quant", username="cloud_brain", host = "172.20.11.38", post = 3306,
                 password="laokaijun",client.flag=CLIENT_MULTI_STATEMENTS) %>% as.rdf
my_acount2 <- stock_acount$new(con = con)
for(i in buz_day)
{
  ##买到指定仓位
  if(i %in% buy_date)
  {
    buy_list <- subset(data_all, trade_dt == i)
    my_acount2$order_to(ymd(i), buy_list$wind_code, weight = rep(1 / nrow(buy_list), nrow(buy_list)))
  }
  my_acount2$acount_update(ymd(i))
  my_acount2$show_total_acount(1) %>% print
}
dbDisconnect(con$con)


##factor_acount测试
con <- dbConnect(MySQL(), dbname = "quant", username="cloud_brain", host = "172.20.11.38", post = 3306,
                 password="laokaijun",client.flag=CLIENT_MULTI_STATEMENTS) %>% as.rdf
my_acount3 <- factor_acount$new(con = con, 
                                buy_stock = data_all %>% 
                                  transmute(code = wind_code, buy_date = ymd(trade_dt)) %>%
                                  group_by(buy_date) %>% mutate(num = 1:n()))
my_acount3$factor_update(20, 20170101, 20170531, wait_sell = T)
dbDisconnect(con$con)

##有交叉项测试--------------------


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
