#' Title
#'
#' @param check 
#' @param stop_on_error 
#' @param pack_dir 
#'
#' @return
#' @export
#'
#' @examples
get_packs <- function( check = TRUE, stop_on_error = FALSE, pack_dir = system.file("Rmd", "example_packs", package="flexjags")  )
{
  dir_list = list.dirs(path = pack_dir, recursive = FALSE)
  if( length(dir_list) < 1 ){
    warning( "No example packs found in ", pack_dir ) 
    return( c() )
  }
  
  if( check ){
    checked = c()
    for( d in dir_list ){
        result = check_pack( pack = d, stop_on_error = stop_on_error)
        checked = c( checked, result )
    }
  }else{
    checked = dir_list
  }
  return(checked)
}
