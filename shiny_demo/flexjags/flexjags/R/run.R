#' Title
#'
#' @return
#' @export
#'
#' @examples
run_flexjags <- function( pack_dir = system.file("Rmd/example_packs", package="flexjags") ){
  
  rmd_file = system.file("Rmd", "flexjags.Rmd", package="flexjags") 
  envir = new.env()
  envir$pack_dir = pack_dir

  rmarkdown::run( rmd_file, render_args = list(envir = envir) )
}