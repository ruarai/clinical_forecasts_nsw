# clinical_forecasts_nsw

R Targets pipeline for producing clinical forecasts for NSW.

### Dependencies

**Curvemush**

Install a safe version for 'curvemush', the stochastic simulation model with:
`devtools::install_github("ruarai/curvemush@release_nsw") `

**Others**

` targets, tidyverse, pracma, zoo, cowplot, scales, ggokabeito, matrixStats `


### Running

Edit the parameters defined in `_targets.R` to assign the NCIMS, APDC, forecast trajectory and occupancy counts files, as well as the forecast name desired.

Then, simply run `tar_make()` to fully run the forecast.
