#' Title
#'
#' @param pack 
#' @param data_txt 
#' @param envir 
#' @param render 
#'
#' @return
#' @export
#'
#' @examples
eval_data <- function( pack, data_txt = NULL, envir = NULL, render = TRUE)
{
  td = tempdir()
  on.exit( { unlink(td) } )
  
  if( is.null(data_txt) ){
    data_txt = load_pack( pack )[["data_txt"]]
  }
  if( is.null(envir) ){
    envir = new.env()
  }
  
  all_files = list.files( pack, full.names = TRUE )
  file.copy(all_files, to = td)
  data_path = file.path( td, "data.R" )
  cat(data_txt, file = data_path)
  
  if(render){
    tf = tempfile(fileext = ".html")
    rmarkdown::render(data_path, 
                      output_format = "html_fragment",
                      output_file = tf,
                      envir = envir, quiet = TRUE)
    html_out = paste( readLines( tf ), collapse = "\n")
  }else{
    expr = substitute(source(file = dp, chdir = TRUE, local = TRUE), list(dp = data_path))
    evaluate::evaluate(expr, envir = envir)
    html_out = "<b>Rendered output disabled.</b>"
  }
  
  return(list(
    envir = envir,
    html_out = html_out
    ))
}
