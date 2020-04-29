# fitsim
Multidimensional IRT Fit Index SImulation

Simulation code for a study examining the Type 1 error / Power of M2-based global model fit statisics (`RMSEA`, `SRMSR`, `CFI`, and `TLI`) for multidimensional IRT models.

## Simulation Conditions

- Sample Size
  - 150,250,400,600,800,1000
- # of response options 
  - 3,4,5
- # of items
  - 10,20
  - Note that this may need to be increased to show how RMSEA behaves as the M2 degrees of freedom increase (see David Kenny's 2014 paper in Sociological Research Methods)
- Level of misspecification (this is for the Power study)
  - 10% of items misloaded
  - 20% of items misloaded
  - 1 correlation misspecified
  - 10% of items misloaded & 1 correlation misspecified
  - 20% of items misloaded & 1 correlation misspecified

`run_sim.R` will run the complete simulation. On a 12-core machine, this took ~6.5 hours from start to finish when ran in parallel.

To-do:
- Write/run code for 3 dimensional model
- Unit tests
