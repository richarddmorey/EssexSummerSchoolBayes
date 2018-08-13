#' Title
#'
#' @return
#' @export
#'
#' @examples
run_flexjags <- function(){
  rmd_file = system.file("Rmd", "flexjags.Rmd", package="flexjags") 
  rmarkdown::run(rmd_file)
}