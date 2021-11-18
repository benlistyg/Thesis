final_model_fitting = function(n_dim,
                               n_item, 
                               n_response_options, 
                               n_people, 
                               model_sting,
                               item_type){
  
  if(item_type == 'graded'){
    out = fit_grm_model(n_dim,
                        n_item, 
                        n_response_options,
                        n_people, 
                        model_sting)
  }
  
  if(item_type == 'gpcm'){
    out = fit_gpcm_model(n_dim,
                         n_item, 
                         n_response_options,
                         n_people, 
                         model_sting)
  }
  
  if(item_type == 'gpcm'){
    out = fit_ggum_model(n_dim,
                         n_item, 
                         n_response_options,
                         n_people, 
                         model_sting)
  }
  
}











