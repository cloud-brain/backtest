#' Summary account with sign
#'
#' summary the acount with sign
#'
#' @param date the date of the acount
#' @param yield the yield each day
#' @param sign the sign of buy or not.
#' @param fee the fee when sell the stock
#' @param plot plot or not
#' @param relative if output relative yield or not
#'
#' @details
#' the sign can be regard as stock position. make sure that the sign can get at that day.
#'
#' @return
#' a data.frame with yield, win_loss, win_rate, max back by year. and these indicate for whole period
#'
#' @import dplyr grid
#' @export

summary_sign <- function(date, yield, sign, fee = 0.005, plot = T, relative = T)
{
  require(ggplot2)
  require(grid)
  ##sign_yield 表示累加信号的收益
  ##origin_yield 表示原收益
  ##设每次卖出时收取手续费
  sign[1:which(sign == 0)[1]] <- 0
  
  ##当信号为1时假设第一天已经持仓
  sign_data <- data.frame(date, yield, sign) %>% na.omit %>% arrange(date) %>%
    mutate(date = date,
           real_yield = 1 + yield * sign,
           fee_line = c(ifelse(diff(sign)==-1, (1 - fee), 1), 1),
           sign_yield = cumprod(real_yield * fee_line),
           origin_yield = cumprod(1 + yield))
  
  ##第一期数据不满时提示日期
  if(lubridate::month(min(sign_data$date)) >1) message(paste0('the first date is ', format(min(sign_data$date), '%Y%m%d')))
  
  ##构建次数
  temp <- sign_data %>% ungroup %>%
    mutate(times = cumsum(ifelse(diff(c(0, sign))==1,1,0))) %>%
    filter(sign == 1) 
  
  ##求取叠加信号的收益，盈亏比，胜率和整体的最大回撤
  output <- full_join(
    temp %>%
      group_by(year = format(date, '%Y'), times) %>%
      summarise(yield = prod(real_yield * fee_line)) %>%
      group_by(year) %>%
      summarise(num = n(),
                yield_m = prod(yield) - 1,
                win_loss = mean(yield[yield>1] - 1) / mean(1 - yield[yield<1]),
                win_rate = sum(yield>1)/n()),
    sign_data %>% group_by(year = format(date, '%Y')) %>%
      summarise(max_back = max_back(real_yield * fee_line),
                sharpratio = (prod(real_yield * fee_line) - 1)/sd(real_yield * fee_line)/sqrt(n())),
    by = 'year') %>% mutate(num = ifelse(is.na(num), 0, num))
  
  if(relative)
  {
    output <- output %>% left_join(sign_data %>% 
                                     group_by(year = format(date, '%Y')) %>%
                                     summarise(yield_r = prod(1 + yield) - 1,
                                               sharp = (prod(1 + yield) - 1) / sd(yield) / sqrt(n()),
                                               max_back_r = max_back(1 + yield)), 
                                   by = 'year') %>% 
      mutate(yield_rel = yield_m - yield_r,
             sharp_rel = sharpratio - sharp,
             max_back_rel = max_back - max_back_r) %>% 
      select(year:num, yield_m, yield_rel, win_loss, win_rate, max_back_rel, sharp_rel)
  }
  
  avg_year <- cbind(
    year = 'avg',
    temp %>%
      group_by(times) %>%
      summarise(yield = prod(real_yield * fee_line) - 1) %>%
      summarise(
        num = n(),
        yield_m = prod(1 + yield) - 1,
        win_loss = - mean(yield[yield > 0]) / mean(yield[yield < 0]),
        win_rate = sum(yield > 0) / n()
      ),
    max_back = max_back(temp$real_yield * temp$fee_line),
    sharpratio = mean(temp$real_yield* temp$fee_line - 1) / sd(temp$real_yield* temp$fee_line) * sqrt(250)
  )
  
  if(relative)
  {
    avg_year <- cbind(
      avg_year,
      sign_data %>% 
        summarise(yield_r = prod(1 + yield) - 1, 
                  max_back_r = max_back(1 + yield),
                  sharp = mean(yield) / sd(real_yield) * sqrt(250))
    ) %>% mutate(yield_rel = yield_m - yield_r,
                 sharp_rel = sharpratio - sharp,
                 max_back_rel = max_back - max_back_r) %>% 
      select(year:num, yield_m, yield_rel, win_loss, win_rate, max_back_rel, sharp_rel)
  }
  
  output <- rbind(output %>% arrange(year), avg_year)
  
  if(plot)
  {
    line_plot <-
      ggplot() +
      geom_line(data = sign_data %>% select(date, sign_yield, origin_yield) %>%
                  reshape2::melt(id = 'date'), 
                aes(x = date, y = value, color = variable), size = 1) + 
      geom_rect(
        data = temp %>% group_by(times) %>%
          summarise(xmin = min(date),
                    xmax = max(date)),
        aes(
          xmin = xmin - 1,
          xmax = xmax,
          ymin = min(sign_data$sign_yield, sign_data$origin_yield),
          ymax = max(sign_data$sign_yield, sign_data$origin_yield)
        ), alpha = 0.2
      ) +coord_trans(y = "log10") +
      # scale_y_continuous(name = "return") +
      theme(
        legend.position = c(0, 1),
        legend.justification = c("left", "top"),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 1),
        axis.text.x = element_blank(),
        axis.ticks = element_blank()
        # axis.title.y = element_text(size = rel(0.8))
      ) + scale_x_date(limits = c(min(sign_data$date), max(sign_data$date)))
    
    max_back_plot <- sign_data %>% arrange(date) %>%
      mutate(
        sign_max = sign_yield / cummax(sign_yield) - 1,
        origin_max = origin_yield / cummax(origin_yield) - 1
      ) %>%
      select(date, sign_yield = sign_max, origin_yield = origin_max) %>%
      reshape2::melt(id = 'date') %>%
      ggplot(aes(x = date, y = value, color = variable)) +
      geom_line(size = 1) +
      # scale_y_continuous(name = "drawdowm") +
      theme(
        legend.position = 'none',
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 1)
        # axis.title.y = element_text(size = rel(0.8))
      ) + scale_x_date(limits = c(min(sign_data$date), max(sign_data$date)))
    
    
    rect_plot <-
      temp %>% group_by(times) %>% summarise(
        xmin = min(date),
        xmax = max(date),
        yield = prod(1 + yield) - 1
      ) %>%
      mutate(ymin = ifelse(yield >= 0, 0, yield),
             ymax = ifelse(yield >= 0, yield, 0)) %>%
      ggplot(aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      )) +
      geom_rect() +
      # scale_y_continuous(name = "signbar") +
      theme(
        axis.text.y = element_text(angle = 90, hjust = 1),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
        # axis.title.y = element_text(size = rel(0.8))
      ) + scale_x_date(limits = c(min(sign_data$date), max(sign_data$date)))
    
    
    
    grid.newpage()
    vp.line <- viewport(x=0, y=0.66, width=1, height=0.33, just=c("left", "bottom"))
    vp.rect <- viewport(x=0, y=0.33, width=1, height=0.33, just=c("left", "bottom"))
    vp.maxback <- viewport(x=0, y=0, width=1, height=0.33, just=c("left", "bottom"))
    # vp.scatter <- viewport(x=0, y=0, width=0.66, height=0.66, just=c("left", "bottom"))
    print(line_plot, vp = vp.line)
    print(rect_plot, vp = vp.rect)
    print(max_back_plot, vp = vp.maxback)
  }
  
  return(output)
}





