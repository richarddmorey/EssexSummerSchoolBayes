get_samples_and_post <- function(model, 
                                 variable.names,
                                 iterations, 
                                 pack,
                                 post_txt,
                                 post_envir,
                                 render,
                                 data_html,
                                 cur_trace,
                                 session = session,
                                 sink_file){
  sink(file = sink_file)
  samples = rjags::coda.samples(model = model,variable.names = variable.names, n.iter = iterations)[[1]]
  sink()
  
  
  
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