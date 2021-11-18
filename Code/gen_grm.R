# MGRM
# https://www.frontiersin.org/articles/10.3389/fpsyg.2016.00109/full
# https://www.tandfonline.com/doi/pdf/10.1080/00273171.2018.1455572?casa_token=LoPkmRJy-HEAAAAA:pLgXKDsTk9n2EqWiX1OSLiA1OclWtaDp9rNF16_DMDQZj6CfHR9h4xbgLmiOk-rBZysX1kbdBxag
# https://journals.sagepub.com/doi/pdf/10.1177/0013164420958060?casa_token=4vKSZeV4CpAAAAAA:4WICdv7fvp7osyTYwBN4J3csparhEYaX7RdDyxr_mtAqVjQFSVb_aCl-Q-vhrg-soP_CZOPXZNC5

generate_grm_item_params = function(n_dim, n_item, n_response_options){
  
  a_matrix = matrix(0, nrow = n_item, ncol = n_dim)
  
  generate_matrix = function(n_item, n_dim){split(seq(1,n_item,1), ceiling(seq_along(seq(1,n_item,1))/(n_item / n_dim)))}
  
  for(i in 1:n_dim){
    a_matrix[generate_matrix(n_item = n_item, n_dim = n_dim)[[i]],i] = NA
  }
  
  # The three-dimensional graded response model with four response categories for all items was used in this study. The ajhs were randomly sampled from U[1.1, 2.8]
  
  a_matrix[is.na(a_matrix)] = runif(n = n_item, min = 1.1, max = 2.8)
  
  # A simple structure was assumed so that for each item only one of the three discrimination parameters was non-zero, and every dimension was represented by an equal number of items. bjks ranged from [−2, 2] (De Ayala, 1994), and each was uniformly distributed along an equidistant interval within this range for each item. Thus, the three boundary parameters were sampled randomly from U[−2, −0.67], U[−0.67, 0.67], and U[0.67, 2], respectively.
  
  b_range = seq(-2, 2, length.out = n_response_options)
  
  b_matrix = matrix(NA, nrow = n_item, ncol = (n_response_options-1))
  
  if(n_response_options == 2){
    for(i in 1:(n_response_options-1)){
      b_matrix[,i] = runif(n = n_item, min = b_range[i], max = b_range[i+1])
    }
  } else{
    repeat{
      
      for(i in 1:(n_response_options-1)){
        
        b_matrix[,i] = runif(n = n_item, min = b_range[i], max = b_range[i+1])
        
      }
      
      # To avoid sparseness of the response matrix, only adjacent boundary parameters with distance of at least 0.5 apart were retained.
      
      if(all(rowSums(abs(b_matrix)) > 1)){
        break
      }
    }
  }
  
  # Convert to intercepts, for GRM d_i = -a_i * b_i, use mirt2traditional() to double check this.
  
  d_matrix = - a_matrix[a_matrix!=0]*b_matrix
  
  # Inter-factor correlation set to 0.5
  
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

fit_grm_model = function(n_dim, n_item, n_response_options, n_people, model_string){
  
  grm_item_params = generate_grm_item_params(n_dim = n_dim, 
                                             n_item = n_item, 
                                             n_response_options = n_response_options)
  
  if(n_dim > 1 & n_response_options > 2){
    
    response_data = simdata(a = grm_item_params$a, 
                            d = grm_item_params$d, 
                            N = n_people, 
                            sigma = grm_item_params$sigma,
                            itemtype = 'graded')
    
  } else if(n_dim > 1 & n_response_options == 2){
    
    response_data = simdata(a = grm_item_params$a, 
                            d = grm_item_params$d, 
                            N = n_people, 
                            sigma = grm_item_params$sigma,
                            itemtype = 'dich')
    
  } else if(n_dim == 1 & n_response_options > 2){ 
    response_data = simdata(a = grm_item_params$a, 
                            d = grm_item_params$d, 
                            N = n_people, 
                            itemtype = 'graded')
    
  } else if(n_dim == 1 & n_response_options == 2){ 
    response_data = simdata(a = grm_item_params$a, 
                            d = grm_item_params$d, 
                            N = n_people, 
                            itemtype = 'dich')
    
  } 
  
  mod = mirt(data = response_data,
             model = model_string,
             itemtype = 'graded',
             technical=list(theta_lim=c(-3,3), NCYCLES = 10000),
             optimizer="nlminb",
             control=list(rel.tol=1e-10,abs.tol=1e-20,iter.max=20),
             method = 'QMCEM',
             TOL=1e-03)
  
  return(
    list(
      mod = mod,
      grm_item_params = grm_item_params,
      M2 = data.frame(M2(mod, QMC = T, quadpts = 30, theta_lim = c(-3,3)), n_people, n_dim, n_item, n_response_options, model_string)
    )
  )
  
}
