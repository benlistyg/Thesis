model_syntax = read.csv("model_syntax.csv")

model_syntax$correct_model = gsub("\\n", "\n", model_syntax$correct_model, fixed = T)
model_syntax$incorrect_model = gsub("\\n", "\n", model_syntax$incorrect_model, fixed = T)

correct_models = dplyr::select(model_syntax, correct_model, n_factors, n_items) %>% 
  rename(model_string = correct_model, n_dim = n_factors, n_item = n_items)

incorrect_models = dplyr::select(model_syntax, incorrect_model, n_factors, n_items, misspecification) %>% 
  rename(model_string = incorrect_model, n_dim = n_factors, n_item = n_items)

simulation_conditions = bind_rows(
  
  expand.grid(
    n_response_options = c(2,4,6),
    n_people = seq(250,1000,250),
    item_type = c("graded", "gpcm", "ggum"), 
    n_reps = 100) %>% 
    tidyr::crossing(correct_models, .) %>% 
    tidyr::uncount(n_reps) %>% 
    arrange(n_item, n_dim),
  
  expand.grid(
    n_response_options = c(2,4,6),
    n_people = seq(250,1000,250),
    item_type = c("graded", "gpcm", "ggum"), 
    n_reps = 100) %>% 
    tidyr::crossing(incorrect_models, .) %>% 
    tidyr::uncount(n_reps) %>% 
    arrange(n_item, n_dim)
  
)
