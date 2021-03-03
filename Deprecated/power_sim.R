# Power portion

for(i in c(150,250,400,600,800,1000)){
  correct <- simulation_results %>% 
    filter(n_items == 10,
           response_options == 3,
           misspecification == '20% items mis-loaded + 1 correlation misspecified',
           Model == 'Correct',
           n_people == i) %>% 
    select(RMSEA) %>% 
    c %>% 
    unname %>% 
    .[[1]] %>% 
    quantile(., 0.95)
  
  table(simulation_results %>% 
          filter(n_items == 10,
                 response_options == 3,
                 misspecification == '20% items mis-loaded + 1 correlation misspecified',
                 Model == 'Correct',
                 n_people == i) %>% 
          select(RMSEA) %>% 
          c %>% 
          unname %>% 
          .[[1]] > correct) %>% 
    print
}

#####

for(i in c(150,250,400,600,800,1000)){
  correct <- simulation_results %>% 
    filter(n_items == 10,
           response_options == 3,
           misspecification == '20% items mis-loaded + 1 correlation misspecified',
           Model == 'Correct',
           n_people == i) %>% 
    select(SRMSR) %>% 
    c %>% 
    unname %>% 
    .[[1]] %>% 
    quantile(., 0.95)
  
  table(simulation_results %>% 
          filter(n_items == 10,
                 response_options == 3,
                 misspecification == '20% items mis-loaded + 1 correlation misspecified',
                 Model == 'Incorrect',
                 n_people == i) %>% 
          select(SRMSR) %>% 
          c %>% 
          unname %>% 
          .[[1]] > correct) %>% 
    print
}

#####

for(i in c(150,250,400,600,800,1000)){
  correct <- simulation_results %>% 
    filter(n_items == 10,
           response_options == 3,
           misspecification == '20% items mis-loaded + 1 correlation misspecified',
           Model == 'Correct',
           n_people == i) %>% 
    select(CFI) %>% 
    c %>% 
    unname %>% 
    .[[1]] %>% 
    quantile(., 0.05)
  
  table(simulation_results %>% 
          filter(n_items == 10,
                 response_options == 3,
                 misspecification == '20% items mis-loaded + 1 correlation misspecified',
                 Model == 'Incorrect',
                 n_people == i) %>% 
          select(CFI) %>% 
          c %>% 
          unname %>% 
          .[[1]] < correct) %>% 
    print
}

#####


for(i in c(150,250,400,600,800,1000)){
  correct <- simulation_results %>% 
    filter(n_items == 10,
           response_options == 3,
           misspecification == '20% items mis-loaded + 1 correlation misspecified',
           Model == 'Correct',
           n_people == i) %>% 
    select(TLI) %>% 
    c %>% 
    unname %>% 
    .[[1]] %>% 
    quantile(., 0.05)
  
  table(simulation_results %>% 
          filter(n_items == 10,
                 response_options == 3,
                 misspecification == '20% items mis-loaded + 1 correlation misspecified',
                 Model == 'Incorrect',
                 n_people == i) %>% 
          select(TLI) %>% 
          c %>% 
          unname %>% 
          .[[1]] < correct) %>% 
    print
}
