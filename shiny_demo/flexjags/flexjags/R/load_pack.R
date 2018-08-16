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
  ## Markdown to produce description
  desc_md = readLines( file.path(pack, "desc.md") )
  desc_md = paste( desc_md, collapse="\n" )
  ## Produce HTML
  tf_in = tempfile( fileext = ".md" )
  tf_out = tempfile( fileext = ".html" )
  file.copy( file.path(pack, "desc.md"), tf_in)
  rmarkdown::render(tf_in, output_format = "html_fragment",
                    output_file = tf_out)
  desc_html = readLines( tf_out )
  
  ## Model code
  jags_txt = readLines( file.path(pack, "model.jags") )
  jags_txt = paste( jags_txt, collapse="\n" )
  
  ## Code to produce data for JAGS
  data_txt = readLines( file.path(pack, "data.R") )
  data_txt = paste( data_txt, collapse="\n" )

  ## Code to produce initialization for JAGS
  if( file.exists( file.path( pack, "init.R") ) ){
    init_txt = readLines( file.path(pack, "init.R") )
    init_txt = paste( init_txt, collapse = "\n")
  }else{
    init_txt = ""
  }
  
  ## Code to produce post-sampling info from samples
  if( file.exists( file.path( pack, "post.R") ) ){
    post_txt = readLines( file.path(pack, "post.R") )
    post_txt = paste( post_txt, collapse = "\n")
  }else{
    post_txt = ""
  }
  
  ## Default variables (not used yet)
  if( file.exists( file.path( pack, "default_variables.txt") ) ){
    default_variables = readLines( file.path(pack, "default_variables.txt") )
  }else{
    default_variables = c()
  }

  return(list(
    jags_txt = jags_txt,
    data_txt = data_txt,
    init_txt = init_txt,
    post_txt = post_txt,
    desc_md = desc_md,
    desc_html = desc_html,
    default_variables = default_variables
  ))
}
