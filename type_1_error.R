t1_error <- function(model = 'Correct',
                     N_items,
                     Response_options,
                     Misspecification,
                     N_people,
                     ...){
  
  CFI_results <- (simulation_results %>% 
                    filter(Model == model,
                           n_items == N_items,
                           response_options == Response_options,
                           misspecification == Misspecification,
                           n_people == N_people) %>% 
                    .$CFI < 0.9)
  
  TLI_results <- (simulation_results %>% 
                    filter(Model == model,
                           n_items == N_items,
                           response_options == Response_options,
                           misspecification == Misspecification,
                           n_people == N_people) %>% 
                    .$TLI < 0.9)
  
  RMSEA_results <- (simulation_results %>% 
                      filter(Model == model,
                             n_items == N_items,
                             response_options == Response_options,
                             misspecification == Misspecification,
                             n_people == N_people) %>% 
                      .$RMSEA > 0.05)
  
  SRMSR_results <- (simulation_results %>% 
                      filter(Model == model,
                             n_items == N_items,
                             response_options == Response_options,
                             misspecification == Misspecification,
                             n_people == N_people) %>% 
                      .$SRMSR < 0.08)
  
  cbind(
    table(CFI_results)['TRUE'],
    table(CFI_results)["FALSE"],
    table(TLI_results)['TRUE'],
    table(TLI_results)["FALSE"],
    table(RMSEA_results)['TRUE'],
    table(RMSEA_results)["FALSE"],
    table(SRMSR_results)['TRUE'],
    table(SRMSR_results)["FALSE"]
  ) %>% 
    data.frame() %>% 
    setNames(c("TRUE.CFI",
               "FALSE.CFI",
               "TRUE.TLI",
               "FALSE.TLI",
               "TRUE.RMSEA",
               "FALSE.RMSEA",
               "TRUE.SRMSR",
               "FALSE.SRMSR")) %>% 
    `rownames<-`(NULL)
}


t1_error_results <- expand.grid(
  Model = c('Correct'),
  N_items = unique(simulation_conditions$n_items), 
  Response_options = unique(simulation_conditions$response_options), 
  Misspecification = unique(simulation_conditions$misspecification),
  N_people = unique(simulation_conditions$n_people),
  stringsAsFactors = F
) %>% 
  plyr::mdply(.data = ., 
              .fun = t1_error)
