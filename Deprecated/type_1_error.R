# test_results <- data.table::fread('https://github.com/benlistyg/fitsim/blob/master/test_results.csv?raw=true')

t1_error <- function(model = 'Correct',
                     N_items,
                     Response_options,
                     Misspecification,
                     N_people,
                     ...){
  
  CFI_results <- (test_results %>% 
                    filter(Model == model,
                           n_items == N_items,
                           response_options == Response_options,
                           misspecification == Misspecification,
                           n_people == N_people) %>% 
                    .$CFI < 0.9)
  
  TLI_results <- (test_results %>% 
                    filter(Model == model,
                           n_items == N_items,
                           response_options == Response_options,
                           misspecification == Misspecification,
                           n_people == N_people) %>% 
                    .$TLI < 0.9)
  
  RMSEA_results <- (test_results %>% 
                      filter(Model == model,
                             n_items == N_items,
                             response_options == Response_options,
                             misspecification == Misspecification,
                             n_people == N_people) %>% 
                      .$RMSEA > 0.05)
  
  SRMSR_results <- (test_results %>% 
                      filter(Model == model,
                             n_items == N_items,
                             response_options == Response_options,
                             misspecification == Misspecification,
                             n_people == N_people) %>% 
                      .$SRMSR > 0.08)
  
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
    `rownames<-`(NULL) %>% 
    return()
}


t1_error_results <- expand.grid(
  Model = c('Correct'),
  N_items = unique(test_results$n_items), 
  Response_options = unique(test_results$response_options), 
  Misspecification = unique(test_results$misspecification),
  N_people = unique(test_results$n_people),
  stringsAsFactors = F
) %>% 
  plyr::mdply(.data = ., 
              .fun = t1_error)

test <- t1_error_results %>% 
  filter(Response_options == 3,
         N_items == 10) %>% 
  arrange(Misspecification) %>% 
  melt(., 
       measure.vars = c("TRUE.CFI",
                        "FALSE.CFI",
                        "TRUE.TLI",
                        "FALSE.TLI",
                        "TRUE.RMSEA",
                        "FALSE.RMSEA",
                        "TRUE.SRMSR",
                        "FALSE.SRMSR")) %>% 
  filter(., grepl("TRUE",variable))

ggplot(data = test,
       aes(x = as.factor(N_people), y = value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge())
