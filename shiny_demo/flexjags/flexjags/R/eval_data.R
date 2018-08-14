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
eval_data <- function( pack, data_txt = NULL, data_envir = NULL, init_txt = NULL, init_envir = NULL, render = TRUE)
{
  td = tempdir()
  on.exit( { unlink(td) } )
  
  if( is.null(data_envir) ){
    data_envir = new.env()
  }
  if( is.null(data_txt) ){
    data_txt = load_pack( pack )[["data_txt"]]
  }
  
  if( is.null(init_envir) ){
    init_envir = new.env()
  }  
  if( is.null(init_txt) ){
    init_txt = load_pack( pack )[["init_txt"]]
  }

  
  all_files = list.files( pack, full.names = TRUE )
  file.copy(all_files, to = td)
  data_path = file.path( td, "data.R" )
  cat(data_txt, file = data_path)
  
  init_path = file.path( td, "init.R" )
  cat(init_txt, file = init_path)
  expr = substitute(source(file = ip, chdir = TRUE, local = TRUE), list(ip = init_path))
  evaluate::evaluate(expr, envir = init_envir)
  
  
  if(render){
    tf = tempfile(fileext = ".html")
    rmarkdown::render(data_path, 
                      output_format = "html_fragment",
                      output_file = tf,
                      envir = data_envir, quiet = TRUE)
    html_out = paste( readLines( tf ), collapse = "\n")
  }else{
    expr = substitute(source(file = dp, chdir = TRUE, local = TRUE), list(dp = data_path))
    evaluate::evaluate(expr, envir = data_envir)
    html_out = "<b>Rendered output disabled.</b>"
  }
  
  return(list(
    data_envir = data_envir,
    init_envir = init_envir,
    html_out = html_out
    ))
}
