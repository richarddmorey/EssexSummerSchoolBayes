
#' Title
#'
#' @param pack 
#' @param model 
#' @param data_html 
#' @param post_txt 
#' @param result 
#' @param iterations 
#' @param progress_iterations 
#' @param n_chain 
#' @param cur_trace 
#' @param eliminate_data 
#' @param render 
#' @param session 
#'
#' @return
#' @export
#' @importFrom Rcpp evalCpp
#'
#' @examples
stan_samples <- function(pack, model, data_html, post_txt, result, iterations, progress_iterations, n_chain, cur_trace, eliminate_data, render, session){

  iter_chunks = ceiling(iterations / progress_iterations)
  iterations_ceiling = iter_chunks * progress_iterations

  incProgress(0, detail = "Sampling from stan model...")
  
  fit_obj = rstan::stan(model_code = model, chains = n_chain, iter = iterations, data = result$data_envir, 
                        init = result$init_envir$inits)
  
  samples = rstan::As.mcmc.list(fit_obj)[[1]]
  
  incProgress(0, detail = "Evaluating post-sampling code")
  post_result = eval_post( pack, 
                           post_txt = post_txt,
                           samples = samples,
                           envir = result$data_envir,
                           render = render)
  
  data_html$post_html_out = post_result$html_out
  
  sample_cols = colnames(samples)
  
  if(!all(cur_trace %in% sample_cols))
    cur_trace = sample_cols[1]
  
  if( length(sample_cols) > 0 )
    shiny::updateSelectInput(session, "plotvar", label = NULL,
                             choices = sample_cols, 
                             selected = cur_trace)
  
  return(samples)
}