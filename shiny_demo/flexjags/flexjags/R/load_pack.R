#' Title
#'
#' @param pack 
#'
#' @return
#' @export
#'
#' @examples
load_pack <- function( pack, render = TRUE )
{
  title = basename( pack )
  
  ## Default config 
  config = config::get( file = system.file("Rmd/config.yml", package = "flexjags") )
  
  ## Markdown to produce description
  desc_md = readLines( file.path(pack, "desc.md") )
  desc_md = paste( desc_md, collapse="\n" )
  ## Produce HTML
  tf_in = tempfile( fileext = ".md" )
  tf_out = tempfile( fileext = ".html" )
  file.copy( file.path(pack, "desc.md"), tf_in)
  if( render ){
    rmarkdown::render(tf_in, output_format = "html_fragment",
                      output_file = tf_out)
    desc_html = readLines( tf_out )
  }else{
    desc_html = ""
  }
  
  ## Model code
  if(file.exists(file.path(pack, "model.jags"))){
    jags_txt = readLines( file.path(pack, "model.jags") )
    jags_txt = paste( jags_txt, collapse="\n" )
  }else{
    jags_txt = NA
  }
  if(file.exists(file.path(pack, "model.stan"))){
    stan_txt = readLines( file.path(pack, "model.stan") )
    stan_txt = paste( stan_txt, collapse="\n" )
  }else{
    stan_txt = NA
  }
  
  ## Code to produce data
  if(file.exists(file.path(pack, "data.R"))){
    data_txt = readLines( file.path(pack, "data.R") )
    data_txt = paste( data_txt, collapse="\n" )
  }else{
    data_txt = NA
  }
  if(file.exists(file.path(pack, "data_jags.R"))){
    data_jags_txt = readLines( file.path(pack, "data_jags.R") )
    data_jags_txt = paste( data_jags_txt, collapse="\n" )
  }else{
    data_jags_txt = data_txt
  }  
  if(file.exists(file.path(pack, "data_stan.R"))){
    data_stan_txt = readLines( file.path(pack, "data_stan.R") )
    data_stan_txt = paste( data_stan_txt, collapse="\n" )
  }else{
    data_stan_txt = data_txt
  }
  
  ## Code to produce initialization for JAGS
  if(file.exists(file.path(pack, "init.R"))){
    init_txt = readLines( file.path(pack, "init.R") )
    init_txt = paste( init_txt, collapse="\n" )
  }else{
    init_txt = NA
  }
  if(file.exists(file.path(pack, "init_jags.R"))){
    init_jags_txt = readLines( file.path(pack, "init_jags.R") )
    init_jags_txt = paste( init_jags_txt, collapse="\n" )
  }else{
    init_jags_txt = init_txt
  }  
  if(file.exists(file.path(pack, "init_stan.R"))){
    init_stan_txt = readLines( file.path(pack, "init_stan.R") )
    init_stan_txt = paste( init_stan_txt, collapse="\n" )
  }else{
    init_stan_txt = init_txt
  }
  
  ## Code to produce post-sampling info from samples
  if(file.exists(file.path(pack, "post.R"))){
    post_txt = readLines( file.path(pack, "post.R") )
    post_txt = paste( post_txt, collapse="\n" )
  }else{
    post_txt = NA
  }
  if(file.exists(file.path(pack, "post_jags.R"))){
    post_jags_txt = readLines( file.path(pack, "post_jags.R") )
    post_jags_txt = paste( post_jags_txt, collapse="\n" )
  }else{
    post_jags_txt = post_txt
  }  
  if(file.exists(file.path(pack, "post_stan.R"))){
    post_stan_txt = readLines( file.path(pack, "post_stan.R") )
    post_stan_txt = paste( post_stan_txt, collapse="\n" )
  }else{
    post_stan_txt = post_txt
  }
  
  ## config
  if( file.exists( file.path( pack, "config.yml") ) ){
    pack_config = config::get( file = file.path( pack, "config.yml") )
    for(n in names(pack_config)){
      config[[n]] = pack_config[[n]]
    }
    if(is.null(config[["title"]]))
      config[["title"]] = title
  }else{
    config[["title"]] = title
  }
  
  if(!is.na(jags_txt) & !is.na(stan_txt)){
    if( is.null( config[["default_engine"]] ) ){
      default_engine = "JAGS"
    }else{
      default_engine = config[["default_engine"]]
    }
  }else if(!is.na(jags_txt)){
    default_engine = "JAGS"
  }else if(!is.na(stan_txt)){
    default_engine = "stan"
  }else{
    stop("No valid engine found: model code file is missing.")
  }
  
  return(
    c(
      list(
        jags_txt = jags_txt,
        stan_txt = stan_txt,
        data_jags_txt = data_jags_txt,
        data_stan_txt = data_stan_txt,
        init_jags_txt = init_jags_txt,
        init_stan_txt = init_stan_txt,
        post_jags_txt = post_jags_txt,
        post_stan_txt = post_stan_txt,
        desc_md = desc_md,
        desc_html = desc_html,
        default_engine = default_engine
      ), 
    config)
    )
}
