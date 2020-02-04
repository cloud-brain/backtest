#' extreme scale
#'
#' rescale the extreme data
#'
#' @param x vector
#' @param max_num, int, max adjust number, default is 4
#'
#' @examples
#' extreme_scale(x)
#'
#' @export
#'
extreme_scale <- function(x, max_num = 4)
{
  fun <- function(x)
  {
    m_value <- median(x, na.rm = T)
    mad <- median(abs(x - m_value), na.rm = T)
    upper <- m_value + 5 * 1.483 * mad
    lower <- m_value - 5 * 1.483 * mad
    which(x > upper | x < lower)
  }
  
  rank_p <- function(x)
  {
    if(length(x) == 1)
    {
      return(0)
    }else{
      return(rank(x, na.last = 'keep') / (length(x) + 1))
    }
  }
  
  temp_value <- data.frame(pos = 0:100, value = quantile(x, 0:100/100, na.rm = T)) %>% 
    mutate(dif_value = c(NA,diff(value)))
  max_adj <- min(max_num, length(fun(temp_value$dif_value)))
  if(length(fun(temp_value$dif_value)) != 0)
  {
    temp <- temp_value %>% arrange(desc(dif_value)) %>% head(max_adj) %>% arrange(pos)
    if(any(temp$pos < 50))
    {
      temp_high <- max((temp %>% subset(pos < 50))$pos)
      x_min <- temp_value$value[temp_value$pos == temp_high]
      x[x < x_min & !is.na(x)] <- 
        x_min + 
        (rank_p(x[x < x_min & !is.na(x)]) - 1) * 
        temp_high * 
        temp_value$dif_value[temp_value$pos == temp_high + 1]
    }
    if(any(temp$pos > 50))
    {
      temp_low <- min((temp %>% subset(pos > 50))$pos)
      x_max <- temp_value$value[temp_value$pos == temp_low - 1]
      x[x > x_max & !is.na(x)] <- 
        x_max + 
        rank_p(x[x > x_max & !is.na(x)]) * 
        (101 - temp_low) * 
        temp_value$dif_value[temp_value$pos == temp_low - 1]
    }
  }
  x
}

