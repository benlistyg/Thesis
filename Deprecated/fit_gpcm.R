fit_gpcm <- function(n_items,
                      response_options,
                      n_people,
                      correct_model,
                      incorrect_model,
                      n_factors,
                      misspecification,
                      n_,
                      ...){
  
  if(n_factors == 2){  
    a <- matrix(data = c(
      rlnorm(n = n_items/2, 0.2, 0.2),
      rep(0, n_items),
      rlnorm(n = n_items/2, 0.2, 0.2)
    ), ncol = 2)
  } 
  
  # Factor correlations
  # if n_theta = 2
  if(n_factors == 2){
    sigma <- matrix(c(1,.5,
                      .5,1),
                    2,
                    2)
  }
  
  diffs <- t(apply(matrix(runif(n_items*(response_options-1), .3, 1), n_items), 1, cumsum))
  diffs <- -(diffs - rowMeans(diffs))
  d <- matrix(diffs + rnorm(n_items), ncol = (response_options-1))
  d <- cbind(matrix(rep(0, n_items)),d)
  
  dat <- simdata(a = a,
                 d = d,
                 n_people,
                 itemtype='gpcm',
                 sigma = sigma)
  
  correct_out <-mirt(dat,
                     correct_model,
                     'gpcm',
                     technical = list(NCYCLES = 10000),
                     optimizer = 'nlminb')
  
  incorrect_out <-mirt(dat,
                       incorrect_model,
                       'gpcm',
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
