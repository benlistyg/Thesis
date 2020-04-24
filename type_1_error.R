library(dplyr)

simulation_results <- read.csv('simulation_results.csv')


test <- function(mis, 
                 M, 
                 n_i, 
                 r_o,
                 n_p,
                 fit_statistic,
                 direction,
                 cutoff){
  stat <- simulation_results %>% 
    filter(misspecification == mis,
           Model == M,
           n_items == n_i,
           response_options == r_o,
           n_people == n_p) %>% 
    select(fit_statistic) %>% 
    .[,1]
  
  paste('stat',direction,cutoff) %>% 
    parse(text = .) %>% 
    eval %>% 
    table %>% 
    return()
  
}

expand.grid(
  unique(simulation_conditions$misspecification),
  unique(simulation_conditions$n_items),
  unique(simulation_conditions$response_options),
  unique(simulation_conditions$n_people),
  c("CFI","TLI","SRMSR","RMSEA"),
  'Correct'
)

test_out <- simulation_results %>% 
  filter(Model == 'Correct',
         n_items == 10,
         response_options == 5,
         n_people == 150,
         misspecification == '1 correlation misspecified'
  )


test_out %>% 
  mutate(RMSEA_count = (RMSEA > 0.05),
         SRMSR_count = (SRMSR > 0.08),
         CFI_count   = (CFI   < 0.9),
         TLI_count   = (TLI   < 0.9)) %>% 
  select(RMSEA_count,
         SRMSR_count,
         CFI_count,
         TLI_count) %>% 
  apply(., 2, table) %>% 
  data.frame %>% 
  cbind(.[2,]) %>% 
  .[-2,]
