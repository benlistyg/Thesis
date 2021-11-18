final_model_fitting = function(n_dim,
                               n_item, 
                               n_response_options, 
                               n_people, 
                               model_string,
                               item_type,
                               misspecification,
                               ...){
  
  if(item_type == 'graded'){
    out = fit_grm_model(n_dim,
                        n_item, 
                        n_response_options,
                        n_people, 
                        model_string)
  }
  
  if(item_type == 'gpcm'){
    out = fit_gpcm_model(n_dim,
                         n_item, 
                         n_response_options,
                         n_people, 
                         model_string)
  }
  
  if(item_type == 'ggum'){
    out = fit_ggum_model(n_dim,
                         n_item, 
                         n_response_options,
                         n_people, 
                         model_string)
  }
  
  out$M2$item_type = item_type
  
  out$M2$misspecification = misspecification
  
  return(out)
  
}












