plan(multisession)

options(future.rng.onMisuse = "ignore")

results_model = simulation_conditions %>% 
  mutate(n_ = 1:nrow(.)) %>% 
  split(.$n_) %>% 
  future_map(~ final_model_fitting(model_string = .$model_string,
                                   n_dim = .$n_dim, 
                                   n_item = .$n_item, 
                                   n_response_options = .$n_response_options, 
                                   n_people =  .$n_people, 
                                   item_type = .$item_type, 
                                   misspecification = .$misspecification), 
             .progress= T)

results_M2 = do.call(rbind,lapply(seq_along(test), function(x)results_model[[x]]$M2))
