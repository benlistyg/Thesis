library(mirt)
library(dplyr)
library(drake)
library(future)

clean(plan, destroy = T)

source('https://raw.githubusercontent.com/benlistyg/fitsim/master/fit_sim.R')
source('https://raw.githubusercontent.com/benlistyg/fitsim/master/rep_fit_sim.R')

cl <- makeCluster(8)
registerDoParallel(cl)

plan <- drake_plan(
  model_list = read.csv('https://raw.githubusercontent.com/benlistyg/fitsim/master/models.csv') %>% 
    mutate(correct_model = gsub(pattern = '\\n',
                                replacement = '\n',
                                correct_model,
                                fixed = T),
           incorrect_model = gsub(pattern = '\\n',
                                  replacement = '\n',
                                  incorrect_model,
                                  fixed = T)),
  simulation_conditions = expand.grid(
    n_people = c(150,250,400,600,800,1000),
    response_options = c(3,4,5)
  ) %>% 
    tidyr::crossing(model_list, .) %>% 
    mutate(n_reps = 100) %>% 
    filter(n_factors == 2) %>% 
    arrange(n_items)
)

make(plan)

loadd()

begin_ <- Sys.time()
refactored_and_parallel <- foreach(i=1:nrow(.fun = rep_fit_sim),
                                   .combine=rbind, 
                                   .packages=c('mirt','dplyr','plyr')) %dopar% {plyr::mdply(.data = (simulation_conditions[i,]), 
                                                                                            .fun = rep_fit_sim, 
                                                                                            .inform = T)}
refactored_end <- Sys.time() - begin_

stopCluster(cl)

refactored_and_parallel
