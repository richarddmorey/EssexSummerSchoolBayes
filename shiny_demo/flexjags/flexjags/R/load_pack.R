#' Title
#'
#' @param pack 
#'
#' @return
#' @export
#'
#' @examples
load_pack <- function( pack )
{
  jags_txt = readLines( file.path(pack, "model.jags") )
  jags_txt = paste( jags_txt, collapse="\n" )
  data_txt = readLines( file.path(pack, "data.R") )
  data_txt = paste( data_txt, collapse="\n" )
  
  if( file.exists( file.path( pack, "init.txt") ) ){
    init_txt = readLines( file.path(pack, "init.txt") )
    init_txt = paste( init_txt, collapse = "\n")
  }else{
    init_txt = c()
  }
  
  if( file.exists( file.path( pack, "default_variables.txt") ) ){
    default_variables = readLines( file.path(pack, "default_variables.txt") )
  }else{
    default_variables = c()
  }

  return(list(
    jags_txt = jags_txt,
    data_txt = data_txt,
    init_txt = init_txt,
    default_variables = default_variables
  ))
}
