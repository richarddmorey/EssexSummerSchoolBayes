
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
#'
#' @examples
jags_samples = function(pack, model, data_html, post_txt, result, iterations, progress_iterations, n_chain, cur_trace, eliminate_data, render, session){
  
  incProgress(0, detail = "Compiling model")
  mod = rjags::jags.model(textConnection(model),
                          data = result$data_envir, 
                          inits = result$init_envir$inits,
                          n.chains = n_chain,
                          quiet = TRUE)

  all_trace = variable.names(mod) 

  
  if(eliminate_data)
    all_trace = all_trace[ !( all_trace %in% ls( envir = result$data_envir ) ) ]

  samples = get_samples_and_post(model = mod,
                                 variable.names = all_trace,
                                 iterations = as.integer(iterations),
                                 progress_iterations = as.integer(progress_iterations),
                                 pack = pack,
                                 post_txt = post_txt,
                                 post_envir = result$data_envir,
                                 render = render,
                                 data_html = data_html,
                                 cur_trace = cur_trace,
                                 session = session)
  return(samples)
  
}  