library(mirt)
library(dplyr)
library(drake)
library(furrr)
library(doParallel)

rm(list = ls())
clean(plan, destroy = T)

n_reps = 100
source('https://raw.githubusercontent.com/benlistyg/fitsim/master/fit_gpcm.R')

plan(multiprocess)
cl <- makeCluster(8)
registerDoParallel(cl)

set.seed(04101994)

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
    filter(n_factors == 2) %>% 
    arrange(n_items) %>% 
    slice(rep(row_number(), n_reps)) %>% 
    mutate(n_ = 1:nrow(.))
)

make(plan)

loadd()

test_conditions <- simulation_conditions %>% 
  filter(misspecification == '1 correlation misspecified',
         response_options == 5,
         n_items == 10) %>% 
  arrange(n_people)

begin_ <- Sys.time()

test_results = test_conditions[1:nrow(test_conditions),] %>%
  split(.$n_) %>%
  future_map(~ plyr::mdply(.data = ., 
                           .fun = fit_sim_n, 
                           .inform = T),
             .progress =T) %>% 
  future_map_dfr(~ as.data.frame(.))

end_ <- Sys.time() - begin_
