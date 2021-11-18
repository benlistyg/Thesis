# MGGUM
# https://journals.sagepub.com/doi/pdf/10.1177/0146621615602855?casa_token=UCIOfQgqJzEAAAAA:ZZeRyp5ynezP_oKgI05PNxefVfCbhAtNVRY224GgaXz5Zv6PgrA90he0Zf6MV2dxSJe4PcwGLK_Q
# Wang and Wu (2016)
# Roberts, Donoghue, and Laughlin (2002)
# Zhang or Nye et al. (2020)

generate_ggum_item_params = function(n_dim, n_item, n_response_options){
  
  a_matrix = matrix(0, nrow = n_item, ncol = n_dim)
  
  generate_matrix = function(n_item, n_dim){split(seq(1,n_item,1), ceiling(seq_along(seq(1,n_item,1))/(n_item / n_dim)))}
  
  for(i in 1:n_dim){
    a_matrix[generate_matrix(n_item = n_item, n_dim = n_dim)[[i]],i] = NA
  }
  
  # True item discrimination parameters(αi) were generated from a uniform distribution, which spanned the interval of (.5, 2)
  
  a_matrix[is.na(a_matrix)] = runif(n = n_item, min = 0.5, max = 2)

  b_matrix = matrix(0, nrow = n_item, ncol = n_dim)
  
  generate_matrix = function(n_item, n_dim){split(seq(1,n_item,1), ceiling(seq_along(seq(1,n_item,1))/(n_item / n_dim)))}
  
  for(i in 1:n_dim){
    b_matrix[generate_matrix(n_item = n_item, n_dim = n_dim)[[i]],i] = NA
  }
  
  # Items (δi) were always located at equally distant positions on the latent continuum and always ranged from −2.0 to 2.0
  
  b_matrix[is.na(b_matrix)] = matrix(data = runif(n = n_item, min = -2, max = 2.0), ncol = n_dim)
  
  # The threshold parameters (τik) were generated independently for each item. For a given item, the true τiC parameter was generated from a uniform (−1.4, −.4) distribution. Successive true τik parameters were then generated with the following recursive equation:
  # τik−1 = τik − .25 + eik−1, for k = 2, 3, ... ,C,
  # where eik−1 denotes a random error term generated from a N(0, .04) distribution. The τik parameters derived with this formula were not consistently ordered across the continuum within an item.
  
  t_vector = list()
  
  t_vector[[1]] = runif(n = 1, min = -1.4, max = -.4)
  
  if(n_response_options == 2){
    t_matrix = matrix(data = runif(n = n_item, min = -1.4, max = -.4), ncol = 1)
    t_matrix = -t_matrix
  }
  
  else{
    generate_t_matrix = function(n_item, n_response_options){
      
      t_vector = list()
      
      t_vector[[1]] = runif(n = 1, min = -1.4, max = -.4)
      
      
      out = replicate(n = n_item, ({
        
        t_vector = list()
        
        t_vector[[1]] = runif(n = 1, min = -1.4, max = -.4)
        
        e_vector = list()
        
        for(i in 2:(n_response_options-1)){
          
          e = rnorm(n = 1, mean = 0, sd = 0.04)
          
          t_vector[[i]] = t_vector[[i-1]] - (0.25*e)
          
          e_vector[[i-1]] = e
          
        }
        
        list(unlist(t_vector), unlist(e_vector))
        
      })
      )
      
      return(t(out))
      
    }
    
    # Examining the t_e_matrix to ensure the recursive approach is being done correctly.
    
    t_e_matrix = generate_t_matrix(n_item = n_item, n_response_options = n_response_options)
    
    t_matrix = do.call(rbind, t_e_matrix[,1])
    
    t_matrix = -t_matrix
  }
  
  rho_matrix = matrix(ncol = n_dim, nrow = n_dim, data = 0)
  
  rho_matrix = ifelse(row(rho_matrix)==col(rho_matrix),1,0.5)
  
  return(  
    list(
      a = a_matrix,
      d = b_matrix,
      sigma = rho_matrix,
      t = t_matrix
    )
  )
}

fit_ggum_model = function(n_dim, n_item, n_response_options, n_people, model_sting){
  
  ggum_item_params = generate_ggum_item_params(n_dim = n_dim, 
                                               n_item = n_item, 
                                               n_response_options = n_response_options)
  
  
  response_data = simdata(a = ggum_item_params$a, 
                          d = ggum_item_params$d, 
                          N = n_people, 
                          t = ggum_item_params$t,
                          sigma = ggum_item_params$sigma,
                          itemtype = 'ggum')
  
  mymodifiedpars = mirt(data = response_data,
                        model = model_sting,
                        itemtype = 'ggum',
                        technical=list(theta_lim=c(-3,3), NCYCLES = 10000),
                        quadpts=30,
                        optimizer="nlminb",
                        control=list(rel.tol=1e-10,abs.tol=1e-20,iter.max=20),
                        method = 'QMCEM',
                        TOL=1e-03,
                        pars = 'values')

  startingvalues = round(matrix(apply(cbind(ggum_item_params$a,ggum_item_params$d, ggum_item_params$t), 1, c), ncol = 1), 2)

  mymodifiedpars$value[1:length(startingvalues)] = startingvalues
  
  mymodifiedpars = mymodifiedpars
  
  mod = mirt(data = response_data,
             model = model_sting,
             itemtype = 'ggum',
             technical=list(theta_lim=c(-3,3), NCYCLES = 10000),
             quadpts=30,
             optimizer="nlminb",
             control=list(rel.tol=1e-10,abs.tol=1e-20,iter.max=20),
             method = 'QMCEM',
             TOL=1e-03,
             pars = mymodifiedpars)
  
  return(
    list(
      mod = mod,
      ggum_item_params = ggum_item_params,
      M2 = data.frame(M2(mod, QMC = T, quadpts = 30, theta_lim = c(-3,3)))
    )
  )
  
}
