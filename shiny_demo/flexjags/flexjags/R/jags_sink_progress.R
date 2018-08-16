#' Title
#'
#' @param fn 
#'
#' @return
#' @export
#'
#' @examples
jags_sink_progress = function(fn){
  x = readLines(fn)
  s = regexec("\\d+%", x, perl = TRUE)
  m = regmatches(x,s)
  m = m[sapply(m, length)>0]
  if(length(m) == 0){
    progress = "--"
  }else{
    progress = m[[length(m)]]
  }
  return(progress)
}
