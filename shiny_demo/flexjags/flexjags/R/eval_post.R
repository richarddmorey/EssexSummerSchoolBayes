#' Title
#'
#' @param pack 
#' @param post_txt 
#' @param samples 
#' @param envir 
#' @param render 
#'
#' @return
#' @export
#'
#' @examples
eval_post <- function( pack, post_txt, samples, envir = NULL, render = TRUE)
{
  
  if(render){
    td = tempdir()
    on.exit( { unlink(td) } )
  
    if( is.null(envir) ){
      env_envir = new.env()
    }
    if( is.null(post_txt) ){
      post_txt = load_pack( pack )[["post_txt"]]
    }
  
    envir$samples = samples
  
    all_files = list.files( pack, full.names = TRUE )
    file.copy(all_files, to = td)
    post_path = file.path( td, "post.R" )
    cat(post_txt, file = post_path)
  
    tf = tempfile(fileext = ".html")
    rmarkdown::render(post_path, 
                      output_format = "html_fragment",
                      output_file = tf,
                      envir = envir, quiet = TRUE)
    html_out = paste( readLines( tf ), collapse = "\n")
  }else{
    html_out = "<b>Rendered output disabled.</b>"
  }
  
  return(list(
    envir = envir,
    html_out = html_out
    ))
}
