# fitsim
Multidimensional IRT Fit Index SImulation

Simulation code for a study examining the Type 1 error / Power of M2-based global model fit statisics (`RMSEA`, `SRMSR`, `CFI`, and `TLI`) for multidimensional IRT models.

For the current results, we are using the 0.08 SRMSR cut-off, 0.05 RMSEA cut-off, and 0.9 for both TLI and CFI.

## Simulation Conditions

- Sample Size
  - 250,500,750,1000
- \# of response options 
  - 2,3
- \# of items
  - 18,36
  - Note that this may need to be increased to show how RMSEA behaves as the M2 degrees of freedom increase (see David Kenny's 2014 paper in Sociological Research Methods)
- Level of misspecification (this is for the Power study)
  - 10% of items misloaded
  - 20% of items misloaded
  - 1 correlation misspecified
  - 10% of items misloaded & 1 correlation misspecified
  - 20% of items misloaded & 1 correlation misspecified

`fitsim.R` will run the complete simulation. On a 12-core machine, this took ~6.5 hours from start to finish when ran in parallel.
