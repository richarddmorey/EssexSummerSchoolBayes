#' Title
#'
#' @param model 
#' @param variable.names 
#' @param iterations 
#' @param progress_iterations 
#' @param pack 
#' @param post_txt 
#' @param post_envir 
#' @param render 
#' @param data_html 
#' @param cur_trace 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
get_samples_and_post <- function(model, 
                                 variable.names,
                                 iterations,
                                 progress_iterations,
                                 pack,
                                 post_txt,
                                 post_envir,
                                 render,
                                 data_html,
                                 cur_trace,
                                 session = session
                                 ){
  
  
  iter_chunks = ceiling(iterations / progress_iterations)
  iterations_ceiling = iter_chunks * progress_iterations
  
  shiny::withProgress(message = "Sampling...", value = 0, {
    incProgress(1/iter_chunks, 
                detail = paste("Sampling ", "0-",progress_iterations, sep=""))
    samples = rjags::coda.samples(model, variable.names = variable.names, n.iter = progress_iterations, progress.bar = "none")[[1]]
    
    for(i in 2:iter_chunks){
      incProgress(1/iter_chunks, 
                  detail = paste("Sampling ", (i-1)*progress_iterations,"-",i*progress_iterations, sep=""))
      samples = rbind(samples, 
                      rjags::coda.samples(model, variable.names = variable.names, n.iter = progress_iterations, progress.bar = "none")[[1]]
      )
    }
  })
  samples = coda::mcmc(samples)
  
  variable_col = apply(samples, 2, function(cl) !all(cl[1] == cl) ) 
  
  sample_cols = colnames(samples)[ variable_col ]
  
  post_result = eval_post( pack, 
                           post_txt = post_txt,
                           samples = samples[, variable_col ],
                           envir = post_envir,
                           render = render)
  
  data_html$post_html_out = post_result$html_out
  
  if(!all(cur_trace %in% sample_cols))
    cur_trace = sample_cols[1]
  
  if( length(sample_cols) > 0 )
    shiny::updateSelectInput(session, "plotvar", label = NULL,
                      choices = sample_cols, 
                      selected = cur_trace)
  return(samples)
}