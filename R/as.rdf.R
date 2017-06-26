#' Change it as rdf connenction
#'
#' Change the connection type into rdf database
#'
#' @param con a tiny connection
#'
#' @return
#' a con for rdf
#'
#' @examples
#' \dontrun{
#' con <- odbcConnect('tiny')
#' }
#' @export
#'
as.rdf <- function(con)
{
  result <- list(con = con)
  class(result) <- c('rdf',class(result))
  result
}
