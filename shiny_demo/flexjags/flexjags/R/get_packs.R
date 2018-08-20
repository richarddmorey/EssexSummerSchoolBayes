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
get_packs <- function( check = TRUE, pack_dir = system.file("Rmd", "example_packs", package="flexjags")  )
{
  dir_list = list.dirs(path = pack_dir, recursive = FALSE)
  if( length(dir_list) < 1 ){
    warning( "No example packs found in ", pack_dir ) 
    return( c() )
  }
  
  if( check ){
    checked = c()
    for( d in dir_list ){
        result = check_pack( pack = d )
        checked = c( checked, result )
    }
  }else{
    checked = dir_list
  }
  
  titles = sapply( checked,  function(el) load_pack( el )$title )
  sort_priority = sapply( checked,  function(el) load_pack( el )$sort_priority )
  
  has_priority = !sapply( sort_priority, is.null)
  sort_priority[ has_priority ] = rank( unlist( sort_priority[ has_priority ] ) )
  sort_priority[ ! has_priority ] = rank( unlist( titles[ !has_priority ] ) ) + 
    max( unlist( sort_priority[ has_priority ] ) ) + 1
  
  names( checked ) = titles
  
  checked = checked[ order( unlist(sort_priority) ) ]
  
  return( checked )
}
