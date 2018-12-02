#' @import lubridate
##date translate to char
dt_to_char <- function(x)
{
  if(is.null(x))
  {
    return(x)
  }
  return(format(ymd(x),'%Y%m%d'))
}

##combine char list
comb_char <- function(x)
{
  paste0("'",x,"'", collapse = ',')
}

