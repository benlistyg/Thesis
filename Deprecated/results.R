load("C:/Users/Ben/Desktop/Simulation Results/simulation_results.Rdata")

read_simulation_data
function(x) {
  
  data <- read.csv(x, stringsAsFactors = F)
  
  C_ <- data %>% 
    arrange(Model) %>%
    filter(Model == 'Correct') %>% 
    .$CFI
  
  IC_ <- data %>% 
    arrange(Model) %>%
    filter(Model == 'Incorrect') %>% 
    .$CFI
  
  C_95 <- quantile(x = C_, 0.95)
  
  (IC_ >= .8) %>% 
    table %>% 
    rbind %>%
    data.frame %>% 
    cbind(., 
          misspecification = data$misspecification[1],
          n_items = data$n_items[1],
          response_options = data$response_options[1],
          n_people = data$n_people[1],
          correct_model = data$correct_model[1],
          incorrect_model = data$incorrect_model[1],
          n_factors = data$n_factors[1]) %>% 
    mutate(misspecification = as.character(misspecification),
           correct_model = as.character(correct_model),
           incorrect_model = as.character(incorrect_model))
  
}

results <- apply(
  X = matrix(grep(pattern = '.csv', x = list.files(), value = T)),
  MARGIN = 1,
  FUN = read_simulation_data
) %>% 
  bind_rows() %>% 
  arrange(
    misspecification,
    n_items,
    response_options
  )
