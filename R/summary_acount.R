#' Summary the acount performance
#'
#' Plot the acount performance and return perfomance indicator
#'
#' @param total_acount dataframe with daily net value. if benchmark mod there must be a variable named benchmark. All acount should stardardize for 1 million initial amount
#' @param benchmark logical, if relative mod or not
#' @param ylog logical, set the y-axis to logarithmic scale, default FALSE
#' @param plot logical, plot or not
#' @param ... see details in \code{\link{charts.PerformanceSummary}}
#'
#' @return
#' A plot show the cumulative return and drawdown
#' If there's no benchmark ,a dataframe includes that by year
#' \itemize{
#'  \item{\code{yeild}: last value this year / last value last year}
#'  \item{\code{max_back}: max back}
#'  \item{\code{back_ratio}: yeild / max_back}
#'  \item{\code{sharpratio}: mean(yeild)/(sd(yeild) * sqrt(the business day this year))}
#'  \item{\code{capital_loss}: min(0,min(net value)/1000000 - 1)}
#' }
#' If there's benchmark mode,a dataframe includes more by year that
#' \itemize{
#'  \item{\code{yeild_r}: cumprod(yeild - benchmark yeild)}
#'  \item{\code{max_back_r}: max back for yeild_r}
#'  \item{\code{back_ratio_r}: yeild_r / max_back_r}
#'  \item{\code{sharpratio}: mean(yeild_r)/(sd(yeild_r) * sqrt(the business day this year))}
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


summary_acount <- function(total_acount, benchmark = F, ylog = FALSE, plot = T, show_result = T, ...)
{

  ## change the net value to yeild, with first acount value adjust
  fun <- function(x, begin_acount = 1000000) c(x[1]/begin_acount, x[-1]/x[-length(x)]) -1

  ## if benchmark = T, the get if relative mod
  if(benchmark)
  {
    stopifnot(c("acount","benchmark") %in% names(total_acount))
    PerformanceSummary_rel <- function (R, ylog = FALSE, ...)
    {
      gap = 12; p = 0.95; geometric = TRUE; methods = "none"; Rf = 0; width = 0
      legend.loc = "topleft"
      begin = "first"
      x = checkData(R)
      colnames = colnames(x)
      ncols = ncol(x)
      length.column.one = length(x[, 1])
      start.row = 1
      start.index = 0
      while (is.na(x[start.row, 1])) {
        start.row = start.row + 1
      }
      x = x[start.row:length.column.one, ]
      if (ncols > 1)
        legend.loc = legend.loc
      else legend.loc = NULL
      wealth.index = ylog
      op <- par(no.readonly = TRUE)
      layout(matrix(c(1, 2, 3)), heights = c(2, 1, 1.3), widths = 1)
      par(mar = c(1, 4, 4, 2))
      chart.CumReturns(x, main = paste(colnames[1], "Performance", sep = " "), xaxis = FALSE, legend.loc = legend.loc,
                       event.labels = NULL, ylog = ylog, wealth.index = wealth.index,
                       begin = begin, geometric = geometric, ylab = "Cumulative Return",
                       ...)
      par(mar = c(1, 4, 0, 2))
      chart.CumReturns(x[,1] - x[,2], main = "", xaxis = FALSE, legend.loc = NULL,
                       event.labels = NULL, ylog = ylog, wealth.index = wealth.index,
                       begin = begin, geometric = geometric, ylab = "rel cum Return",
                       ...)
      par(mar = c(5, 4, 0, 2))
      chart.Drawdown(x[,1] - x[,2], geometric = geometric, main = "", ylab = "Drawdown",
                     event.labels = NULL, ylog = FALSE, ...)
      par(op)
    }

    ##修正为收益序列
    yeild <- total_acount %>% mutate_at(vars(-date), fun)

    if(plot) PerformanceSummary_rel(as.xts(yeild %>% select(-date), order.by = yeild$date), ylog = ylog, ...)
    yeild$year <- yeild$date %>% format('%Y')
    output <- yeild %>% data.frame %>%
      group_by(year) %>%
      summarise(yeild = prod(1 + acount) - 1,
                yeild_r = prod(1 + acount - benchmark) - 1,
                max_back = max_back(1 + acount),
                max_back_r = max_back(1 + acount - benchmark),
                back_ratio_r = yeild_r/ -max_back_r,
                sharpratio_r = (prod(1 + acount - benchmark) - 1)/sd(acount - benchmark)/sqrt(n()))
  }else{
    yeild <- total_acount %>% mutate_at(vars(-date), fun)
    if(plot) charts.PerformanceSummary(as.xts(yeild %>% select(-date), order.by = yeild$date), ylog = ylog, ...)
    yeild$year <- yeild$date %>% format('%Y')
    output <- yeild %>% data.frame %>%
      group_by(year) %>%
      summarise(yeild = prod(1 + acount) - 1,
                max_back = max_back(1 + acount),
                back_ratio = yeild/ -max_back,
                sharpratio = (prod(1 + acount) - 1)/sd(acount)/sqrt(n()),
                capital_loss = min(0, min(cumprod(1 + acount)) - 1))
  }
  if(show_result)
    output %>% data.frame %>% stargazer::stargazer(type= 'text', summary = F, rownames = F)
  return(invisible(output))
}
