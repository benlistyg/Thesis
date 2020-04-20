fit_sim <- function(n_items,
                    response_options,
                    n_people,
                    correct_model,
                    incorrect_model,
                    n_factors,
                    misspecification,
                    n_
                    ...){
  
  if(n_factors == 2){  
    a <- matrix(data = c(
      runif(n = n_items/2, min = 0.8, max = 1.0),
      rep(NA, n_items),
      runif(n = n_items/2, min = 0.8, max = 1.0)
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
                     technical = list(NCYCLES = 10000),
                     optimizer = 'nlminb')
  
  incorrect_out <-mirt(dat,
                       incorrect_model,
                       'graded',
                       technical = list(NCYCLES = 10000),
                       optimizer = 'nlminb')
  
  out <- rbind(
    
    M2(correct_out, 
       QMC = T, 
       quadpts = 15000) %>%
      mutate(Model = 'Correct'),
    
    M2(incorrect_out, 
       QMC = T, 
       quadpts = 15000) %>%
      mutate(Model = 'Incorrect')
    
  )
  
  print(cbind(out,
              n_items,
              response_options,
              n_people,
              correct_model,
              incorrect_model,
              n_factors,
              misspecification,
              nrow(dat)))
  
  return(cbind(out,
               n_items,
               response_options,
               n_people,
               correct_model,
               incorrect_model,
               n_factors,
               misspecification))
  
}
