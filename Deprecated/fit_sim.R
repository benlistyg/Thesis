library(mirt)
library(dplyr)
library(data.table)

#####

model_list <- read.csv('~/Documents/models.csv',stringsAsFactors = F)

model_list$correct_model <- gsub(pattern = '\\n',
                                 replacement = '\n',
                                 model_list$correct_model,
                                 fixed = T)

model_list$incorrect_model <- gsub(pattern = '\\n',
                                   replacement = '\n',
                                   model_list$incorrect_model,
                                   fixed = T)

simulation_conditions <- expand.grid(
  n_people = c(200,400,600,1000),
  response_options = c(3,4,5)
) %>% 
  tidyr::crossing(model_list, .) %>% 
  mutate(n_reps = 100) %>% 
  filter(n_factors == 2) %>% 
  arrange(n_items)

#####

fit_sim <- function(n_items,
                    response_options,
                    n_people,
                    correct_model,
                    incorrect_model,
                    n_factors,
                    misspecification,
                    ...){
  
  print(c(n_items = n_items,
          n_factors = n_factors,
          incorrect_model = incorrect_model,
          correct_model = correct_model,
          response_options = response_options,
          n_people = n_people,
          misspecification = misspecification))
  
  if(n_factors == 2){  
    a <- matrix(data = c(
      rep(0.8, n_items/2),
      rep(NA, n_items),
      rep(0.8, n_items/2)
    ), ncol = 2)
  } 
  # Thresholds
  diffs <- t(apply(matrix(runif(n_items*(response_options-1), .3, 1), n_items), 1, cumsum))
  diffs <- -(diffs - rowMeans(diffs))
  d <- diffs + rnorm(n_items)
  
  # Factor correlations
  # if n_theta = 2
  if(n_factors == 2){
    sigma <- matrix(c(1,.5,
                      .5,1),
                    2,
                    2)
  } 

  dat <- simdata(a = a,
                 d = d,
                 n_people,
                 itemtype='graded',
                 sigma = sigma)
  
  correct_out <-mirt(dat,
                      correct_model,
                      'graded',
                      technical = list(NCYCLES = 2000),
                      optimizer = 'nlminb')
  
  incorrect_out <-mirt(dat,
                        incorrect_model,
                        'graded',
                        technical = list(NCYCLES = 2000),
                        optimizer = 'nlminb')
  
  out <- rbind(
    
    M2(correct_out) %>%
      select(RMSEA, SRMSR, TLI, CFI) %>%
      mutate(Model = 'Correct'),
    
    M2(incorrect_out) %>%
      select(RMSEA, SRMSR, TLI, CFI) %>%
      mutate(Model = 'Incorrect')
    
  )
  
  print(cbind(out,
              n_items,
              response_options,
              n_people,
              correct_model,
              incorrect_model,
              n_factors,
              misspecification))
  
  return(cbind(out,
               n_items,
               response_options,
               n_people,
               correct_model,
               incorrect_model,
               n_factors,
               misspecification))
  
}

#####

rep_fit_sim <- function(n_items,
                        response_options,
                        n_people,
                        correct_model,
                        incorrect_model,
                        n_factors,
                        n_reps,
                        misspecification,
                        ...){
  plyr::rdply(.n = n_reps,
              .expr = fit_sim(
                n_items,
                response_options,
                n_people,
                correct_model,
                incorrect_model,
                n_factors,
                misspecification,
                ...
              )
  )
}

#####

doMC::registerDoMC(cores = 8)

system.time(
  
  (out <- plyr::mdply(.data = simulation_conditions[1,], 
                      .fun = rep_fit_sim, 
                      .inform = T,
                      .parallel = T))
)
