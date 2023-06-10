# FFT_SSA
Step Selection Analysis using INLA

``stepselection script.R`` Generates data for step selection analysis. Appends movement data with landscape variables, and generates real steps and null steps for clogit models

``SSF server script.R`` fits clogit mixed models for each species using INLA

``Inla model results 34min.R`` generates visualization of posterior intervals and estimates of mixed models accross lags

``ssf INLA ranef plots.R`` generates posteriors for each individual at 4min timescale

``SSF used canopy values.R`` estimates differences between used values for each species and generates plots

``Step selection inla PCA.R`` Runs a PCA on the posterior estimates of the models to see how species separate out.
