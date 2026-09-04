# PLATE

Full reproducibly code for _Player Latent Ability Through Events (PLATE): Simultaneous pitcher and batter ability measurement with a novel Bayesian Item Response Theory Tree_. 

All data is accessed through the publicly available `baseballr` R package. 

PLATE is fit on plate appearances from the 2025 season of major league and triple a baseball. 

Loading all game level data and fitting the PLATE model is a time intensive procedure. 

**We recommend using quick_workflow.R to reproduce all results.** 

To reproduce all models from scratch tun full_workflow.R. This may take several days (1 day to load data, and 12-18 hours for each of the two PLATE models fit). 

quick_workflow.R uses saved game data and saved output from from model.R and validate.R. When using quick_workflow.R model.R, validate.R and pull_data.R do not need to be run. Saved data is available in the data directory. 

If you are only interested in reproducing a single figure/section of the analysis. Each analysis* script can be run independently and will produce the corresponding figure in the paper. 

Figure 1 is generated in latex and summarizes the PLATE model, thus there is no included code here. 

## R packages
`tidyverse 2.0.0`
`rstan 2.39.0.9000`
`ggrepel 0.9.6`
`archive 1.1.14`
`baseballr 1.6.0`
`latex2exp 0.9.8`

## Stan Version
To run `model.R` or `validate.R` you must use Stan version 2.39.0 or greater
