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
