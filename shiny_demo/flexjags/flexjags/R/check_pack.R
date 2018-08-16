#' Title
#'
#' @param pack 
#' @param stop_on_error 
#'
#' @return
#' @export
#'
#' @examples
check_pack <- function( pack, stop_on_error = FALSE )
{
  desc_exists = file.exists( file.path(pack, "desc.md") )
  if( !desc_exists & stop_on_error ) 
    stop( "No desc.md file found in ", pack )
  jags_exists = file.exists( file.path(pack, "model.jags") )
  if( !jags_exists & stop_on_error ) 
    stop( "No model.jags file found in ", pack )
  data_exists = file.exists( file.path(pack, "data.R") )
  if( !data_exists & stop_on_error ) 
    stop( "No data.R file found in ", pack )
  if( jags_exists & data_exists ){
    return( pack )
  }else{
    return( NULL )
  }
}
