#' Summary the acount performance
#'
#' Plot the acount performance and return perfomance indicator
#'
#' @param total_acount dataframe, with column date, acount, if benchmark mod there must be a variable named benchmark. All acount should stardardize for 1 million initial amount
#' @param benchmark logical, if relative mod or not
#' @param plot logical, plot or not
#' @param ... see details in \code{\link{charts.PerformanceSummary}}
#'
#' @return
#' A plot show the cumulative return and drawdown
#' If there's no benchmark ,a dataframe includes that by year
#' \itemize{
#'  \item{\code{yield}: last value this year / last value last year}
#'  \item{\code{max_back}: max back}
#'  \item{\code{back_ratio}: yield / max_back}
#'  \item{\code{sharpratio}: mean(yield)/(sd(yield) * sqrt(the business day this year))}
#'  \item{\code{capital_loss}: min(0,min(net value)/1000000 - 1)}
#' }
#' If there's benchmark mode,a dataframe includes more by year that
#' \itemize{
#'  \item{\code{yield_r}: cumprod(yield - benchmark yield)}
#'  \item{\code{max_back_r}: max back for yield_r}
#'  \item{\code{back_ratio_r}: yield_r / max_back_r}
#'  \item{\code{sharpratio}: mean(yield_r)/(sd(yield_r) * sqrt(the business day this year))}
#' }
#'
#' @details
#' when it has benchmark, input data must have a variable named benchmark, and it can only have two acount
#' when it benchmark is FALSE, input data can have a variable names benchmark but it will be ignore.
#'
#' @examples
#' data(m_index_acount)
#' summary_acount(m_index_acount, benchmark = FALSE)
#'
#' @import dplyr PerformanceAnalytics
#' @importFrom xts as.xts
#' @importFrom stargazer stargazer
#'
#' @export
#'

summary_acount <- function(total_acount, benchmark = F, plot = T, show_result = T, ...)
{
  
  # ## change the net value to yield, with first acount value adjust
  fun <- function(x, begin_acount = 1000000) c(x[1]/begin_acount, x[-1]/x[-length(x)]) -1
  ## if benchmark = T, the get if relative mod
  if(benchmark)
  {
    stopifnot(c("acount","benchmark") %in% names(total_acount))
    
    ##修正为收益序列
    yield <- total_acount %>% mutate_at(vars(acount, benchmark), fun)
    total_acount <- total_acount %>% mutate_at(vars(acount, benchmark), funs(./1000000))
    if(plot) 
    {
      line_plot <-
        ggplot() +
        geom_line(data = total_acount %>% 
                    transmute(date, acount, benchmark) %>%
                    reshape2::melt(id = 'date'), 
                  aes(x = date, y = value, color = variable), size = 1) + 
        coord_trans(y = "log10") +
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
        ) 
      
      relative_plot <- yield %>% 
        mutate(acount = cumprod(1+acount- benchmark)) %>% 
        ggplot(aes(x = date, y = acount)) +
        geom_line(size = 1) +
        # scale_y_continuous(name = "drawdowm") +
        theme(
          axis.text.y = element_text(angle = 90, hjust = 1),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()
          # axis.title.y = element_text(size = rel(0.8))
        )
      
      max_back_plot <-
        yield %>% mutate(acount = 1 + cumprod(1+acount- benchmark)) %>% 
        mutate(max_back = 100 * (acount - cummax(acount))/cummax(acount)) %>%
        ggplot(aes(
          x = date, y = max_back
        )) +
        geom_line() +
        # scale_y_continuous(name = "signbar") +
        theme(
          legend.position = 'none',
          axis.title = element_blank(),
          axis.text.y = element_text(angle = 90, hjust = 1)
          # axis.title.y = element_text(size = rel(0.8))
        ) 
      
      grid.newpage()
      vp.line <- viewport(x=0, y=0.66, width=1, height=0.33, just=c("left", "bottom"))
      vp.rel <- viewport(x=0, y=0.33, width=1, height=0.33, just=c("left", "bottom"))
      vp.maxback <- viewport(x=0, y=0, width=1, height=0.33, just=c("left", "bottom"))
      # vp.scatter <- viewport(x=0, y=0, width=0.66, height=0.66, just=c("left", "bottom"))
      print(line_plot, vp = vp.line)
      print(relative_plot, vp = vp.rel)
      print(max_back_plot, vp = vp.maxback)
    }
    output <- yield %>% 
      group_by(year = format(date, '%Y')) %>%
      summarise(yield = prod(1 + acount) - 1,
                yield_r = prod(1 + acount - benchmark) - 1,
                max_back = max_back(1 + acount),
                max_back_r = max_back(1 + acount - benchmark),
                back_ratio_r = yield_r/ -max_back_r,
                sharpratio_r = (prod(1 + acount - benchmark) - 1)/sd(acount - benchmark)/sqrt(n()))
  }else{
    yield <- total_acount %>% mutate(acount = fun(acount))
    total_acount <- total_acount %>% mutate(acount = acount / 1000000)
    
    if(plot)
    {
      line_plot <-
        total_acount %>% ggplot(aes(x = date, y = acount)) +
        geom_line(size = 1) + 
        coord_trans(y = "log10") +
        # scale_y_continuous(name = "return") +
        theme(
          axis.title = element_blank(),
          axis.text.y = element_text(angle = 90, hjust = 1),
          axis.text.x = element_blank(),
          axis.ticks = element_blank()
          # axis.title.y = element_text(size = rel(0.8))
        ) 
      
      relative_plot <- yield %>% 
        ggplot(aes(x = date, y = acount)) +
        geom_bar(stat = 'identity') + 
        # scale_y_continuous(name = "drawdowm") +
        theme(
          axis.text.y = element_text(angle = 90, hjust = 1),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()
          # axis.title.y = element_text(size = rel(0.8))
        )
      
      max_back_plot <-
        total_acount %>%
        mutate(max_back = 100 * (acount - cummax(acount))/cummax(acount)) %>%
        ggplot(aes(
          x = date, y = max_back
        )) +
        geom_line() +
        # scale_y_continuous(name = "signbar") +
        theme(
          legend.position = 'none',
          axis.title = element_blank(),
          axis.text.y = element_text(angle = 90, hjust = 1)
          # axis.title.y = element_text(size = rel(0.8))
        ) 
      
      grid.newpage()
      vp.line <- viewport(x=0, y=0.66, width=1, height=0.33, just=c("left", "bottom"))
      vp.rel <- viewport(x=0, y=0.33, width=1, height=0.33, just=c("left", "bottom"))
      vp.maxback <- viewport(x=0, y=0, width=1, height=0.33, just=c("left", "bottom"))
      # vp.scatter <- viewport(x=0, y=0, width=0.66, height=0.66, just=c("left", "bottom"))
      print(line_plot, vp = vp.line)
      print(relative_plot, vp = vp.rel)
      print(max_back_plot, vp = vp.maxback)
    }
    output <- yield %>% 
      group_by(year = format(date, '%Y')) %>%
      summarise(yield = prod(1 + acount) - 1,
                max_back = max_back(1 + acount),
                back_ratio = yield/ -max_back,
                sharpratio = (prod(1 + acount) - 1)/sd(acount)/sqrt(n()),
                capital_loss = min(0, min(cumprod(1 + acount)) - 1))
  }
  if(show_result)
    output %>% data.frame %>% stargazer::stargazer(type= 'text', summary = F, rownames = F)
  return(invisible(output))
}
