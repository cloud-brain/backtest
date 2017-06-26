#' Summary account with sign
#'
#' summary the acount with sign
#'
#' @param date the date of the acount
#' @param yield the yield each day
#' @param sign the sign of buy or not.
#' @param fee the fee when sell the stock
#' @param plot plot or not
#'
#' @details
#' the sign can be regard as stock position. make sure that the sign can get at that day.
#'
#' @return
#' a data.frame with yield, win_loss, win_rate, max back by year. and these indicate for whole period
#'
#' @import dplyr
#' @export

summary_sign <- function(date, yield, sign, fee = 0.005, plot = T, show_result = T)
{
  require(ggplot2)
  ##sign_yield 表示累加信号的收益
  ##origin_yield 表示原收益
  ##设每次卖出时收取手续费
  sign_data <- data.frame(date, yield, sign) %>% na.omit %>%
    mutate(date = date,
           real_yield = 1 + yield * sign,
           fee = c(ifelse(diff(sign)==-1, (1 - fee), 1), 1),
           sign_yield = cumprod(real_yield * fee) / real_yield[1],
           origin_yield = cumprod(1 + yield) / (1 + yield[1]))

  ##plot控制是否作图
  if(plot)
  {
    print(
      sign_data %>% select(date, sign_yield, origin_yield) %>%
        reshape2::melt(id = 'date') %>%
        ggplot(aes(x = date, y = value, color = variable)) +
        geom_line(size = 1) + coord_trans(y = "log10"))
  }

  ##第一期数据不满时提示日期
  if(lubridate::month(min(sign_data$date)) >1) message(paste0('the first date is ', format(min(sign_data$date), '%Y%m%d')))

  ##构建次数
  temp <- sign_data %>% ungroup %>%
    mutate(times = cumsum(ifelse(diff(c(0, sign))==1,1,0))) %>%
    filter(sign == 1) %>% select(-fee)

  ##求取叠加信号的收益，盈亏比，胜率和整体的最大回撤
  output <- full_join(
    temp %>%
      group_by(year = format(date, '%Y'), times) %>%
      summarise(yield = prod(real_yield) * (1 - fee)) %>%
      group_by(year) %>%
      summarise(num = n(),
                yield_m = prod(yield) - 1,
                win_loss = mean(yield[yield>1] - 1) / mean(1 - yield[yield<1]),
                win_rate = sum(yield>1)/n()),
    sign_data %>% group_by(year = format(date, '%Y')) %>%
      summarise(max_back = max_back(real_yield),
                sharpratio = (prod(real_yield) - 1)/sd(real_yield)/sqrt(n())),
    by = 'year')
  output <- rbind(output,
                  ##提供整个样本期的对应指标
                  cbind(year = 'avg', temp %>%
                          group_by(times) %>%
                          summarise(yield = prod(real_yield) * (1 - fee) - 1) %>%
                          summarise(num = n(),
                                    yield_m = prod(yield),
                                    win_loss = mean(yield[yield>1] - 1)/mean(1 - yield[yield<1]),
                                    win_rate = sum(yield>1)/n()),
                        max_back = max_back(temp$real_yield),
                        sharpratio = (mean(temp$real_yield - 1) *250)/sd(temp$real_yield)/sqrt(250)))
  return(output)
}
