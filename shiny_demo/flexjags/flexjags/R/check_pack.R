#' Title
#'
#' @param pack 
#' @param stop_on_error 
#'
#' @return
#' @export
#'
#' @examples
check_pack <- function( pack  )
{
  desc_exists = file.exists( file.path(pack, "desc.md") )
  if( !desc_exists ) 
    stop( "No desc.md file found in ", pack )
  
  jags_exists = file.exists( file.path(pack, "model.jags") )
  stan_exists = file.exists( file.path(pack, "model.stan") )
  if( !jags_exists & !stan_exists ) 
    stop( "No model file (model.jags or model.stan) found in ", pack )
  
  data_exists = file.exists( file.path(pack, "data.R") )
  data_jags_exists = file.exists( file.path(pack, "data_jags.R") )
  data_stan_exists = file.exists( file.path(pack, "data_stan.R") )
  
  if( !data_exists ){
    if(jags_exists & !data_jags_exists){
      stop("No data found for jags (data.R or data_jags.R) in", pack )
    }
    if(stan_exists & !data_stan_exists){
      stop("No data found for stan (data.R or data_stan.R) in", pack )
    }
  }
  return( pack )
}
