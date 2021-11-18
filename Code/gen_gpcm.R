# MGPCM
# https://journals.sagepub.com/doi/pdf/10.1177/0146621608327800?casa_token=I4qGT8cn2IoAAAAA:XJK6x1AM1-zoQj7f_eogNcK7jAqYZGDMgdXqTV9mQRsz_slu6YnqlqBFwsrlylkVDFfJHiEkNcho
# Matlock 2018: https://journals.sagepub.com/doi/pdf/10.1177/0013164416670211?casa_token=c3T0ITbryd4AAAAA:ZwsWwuUtwabhwq9ChflizBuZmIObQCZdIFhXL2jVq7dlVzZzRRu_M52w8r1KQl0zBEbF2L9DC_fQ 

generate_gpcm_item_params = function(n_dim, n_item, n_response_options){
  
  a_matrix = matrix(0, nrow = n_item, ncol = n_dim)
  
  generate_matrix = function(n_item, n_dim){split(seq(1,n_item,1), ceiling(seq_along(seq(1,n_item,1))/(n_item / n_dim)))}
  
  for(i in 1:n_dim){
    a_matrix[generate_matrix(n_item = n_item, n_dim = n_dim)[[i]],i] = NA
  }
  
  # Discrimination parameters for the GPCM and GRM were randomly sampled from a lognormal (0, 0.5^2)
  
  a_matrix[is.na(a_matrix)] = rlnorm(n = n_item, meanlog = 0, sdlog = 0.5^2)
  
  # For five-category items, four location parameters of each item were randomly drawn from normal distributions with standard deviations of 1.0 and means of –1.5, –0.5, 0.5, and 1.5, respectively.
  
  b_range = seq(-1.5, 1.5, length.out = n_response_options-1)
  
  b_matrix = matrix(NA, nrow = n_item, ncol = n_response_options-1)
  
  if(n_response_options == 2){
    for(i in 1:(n_response_options-1)){
      b_matrix[,i] = rnorm(n = n_item, mean = 0)
    }
  } else{
    
    repeat{
      
      for(i in 1:length(b_range)){
        
        b_matrix[,i] = rnorm(n = n_item, mean = b_range[i])
        
      }
      
      if(all(rowSums(abs(b_matrix)) > 1)){
        break
      }
    }
  }
  
  # Convert to intercepts
  # The relationship between the slope and intercept values with the IRT parameterized values from the GPCM is somewhat straightforward, where a = a and d =  - a * Sigma_(v=1)^k (b_k). 
  # Reminder to myself: \delta-bik is person location - category location. Category location is broken down in item location and category threshold. (Bi - tauik)
  
  if(n_response_options == 2){
    d_matrix = cbind(rep(0, length(n_item)), - a_matrix[a_matrix!=0] * b_matrix)
  } else{
    d_matrix = cbind(rep(0, length(n_item)), - a_matrix[a_matrix!=0] * t(apply(b_matrix, 1, cumsum)))
  }
  
  # Inter-factor correlation
  
  rho_matrix = matrix(ncol = n_dim, nrow = n_dim, data = 0)
  
  rho_matrix = ifelse(row(rho_matrix)==col(rho_matrix),1,0.5)
  
  return(  
    list(
      a = a_matrix,
      d = d_matrix,
      b = b_matrix,
      sigma = rho_matrix
    )
  )
}

fit_gpcm_model = function(n_dim, n_item, n_response_options, n_people, model_string){
  
  gpcm_item_params = generate_gpcm_item_params(n_dim = n_dim, 
                                               n_item = n_item, 
                                               n_response_options = n_response_options)

  response_data = simdata(a = gpcm_item_params$a, 
                          d = gpcm_item_params$d, 
                          N = n_people, 
                          sigma = gpcm_item_params$sigma,
                          itemtype = 'gpcm')
  
  mod = mirt(data = response_data,
             model = model_string,
             itemtype = 'gpcm',
             technical=list(theta_lim=c(-3,3), NCYCLES = 10000),
             optimizer="nlminb",
             quadpts = 60,
             control=list(rel.tol=1e-10,abs.tol=1e-20,iter.max=20),
             method = 'QMCEM',
             TOL=1e-03)
  
  return(
    list(
      mod = mod,
      gpcm_item_params = gpcm_item_params,
      M2 = data.frame(M2(mod, QMC = T, quadpts = 30, theta_lim = c(-3,3)), n_people, n_dim, n_item, n_response_options, model_string)
    )
  )
  
}
