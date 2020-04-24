#####

test <- function(model = 'Correct',
                 N_items,
                 Response_options,
                 Misspecification,
                 N_people,
                 ...){
  
  x <- (simulation_results %>% 
          filter(Model == model,
                 n_items == N_items,
                 response_options == Response_options,
                 misspecification == Misspecification,
                 n_people == N_people) %>% 
          .$CFI < 0.9)
  
  cbind(
    table(x)['TRUE'],
    table(x)["FALSE"]
  ) %>% 
    data.frame() %>% 
    setNames(c("TRUE.","FALSE.")) %>% 
    `rownames<-`(NULL)
}


expand.grid(
  Model = c('Correct'),
  N_items = unique(simulation_conditions$n_items), 
  Response_options = unique(simulation_conditions$response_options), 
  Misspecification = unique(simulation_conditions$misspecification),
  N_people = unique(simulation_conditions$n_people),
  stringsAsFactors = F
) %>% 
  plyr::mdply(.data = ., 
              .fun = test) %>% 
print()
