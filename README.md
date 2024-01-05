WhittakerBetaChange
================
Shane Blowes

<!-- badges: start -->
<!-- badges: end -->

The WhittakerBetaChange repository includes R code to reproduce the
analyses shown in the article:

**Local changes dominate variation in biotic homogenization and
differentiation**

*by Shane A. Blowes, Brian McGill, Viviana Brambilla, Cher F. Y. Chow,
Thore Engel, Ada Fontrodona-Eslava, Inês S. Martins, Daniel McGlinn,
Faye Moyes, Alban Sagouis, Hideyasu Shimadzu, Roel van Klink, Wu-Bing
Xu, Nicholas J. Gotelli, Anne Magurran, Maria Dornelas, Jonathan M.
Chase*

Alban Sagouis created new compilations of open access data used in these
analyses. Code for the compilation can be found at:

<https://github.com/chase-lab/metacommunity_surveys>

<https://github.com/chase-lab/checklist_change>

<https://github.com/chase-lab/homogenisation-richness>

## Analysis

**00-fit-models-maintext-code-for-eve**: This folder contains scripts
that were used to fit statistical models to data shown in main text
(written for EVE (HPC))

**00-fit-time-series-models**: This folder contains scripts that were
used to fit statistical models to time series data

**01-processed-data**: This folder contains the processed data that models were
fit to, and some additional files required to identify regions to be 
included/excluded from particular sensitivity analyses

**dist2line**: Function to calculate distance to 1:1 line

**Fig2-1-wrangle.R**: Code to wrangle model results for Figure 2

**Fig2-2-plot.R**: Code to plot Figure 2

**Fig3-1-wrangle.R**: Code to wrangle model results for Figure 3

**Fig3-2-plot.R**: Code to plot Figure 3

**FigS2.R**: Code to plot Figure S2

**FigS3-and-S5.R**: Code to plot Figure S3, S5

**FigS4.R**: Code to plot Figure S4

**FigS5-1-wrangle-anti-ts-data.R**: data wrangle for Figure S5 (datasets
with only two points)

**FigS5-1-wrangle-ts-data.R**: data wrangle for Figure S5 (time series
data)

**FigS6.R**: Code to plot Figure S6

## Data wrangle

Each subfolder has the code used to standardise sampling effort for
different sources of biodiversity data at two scales: a smaller (alpha)
scale, and larger (gamma) scale. Two sets of data (each documenting the
alpha and gamma scales) are compiled: (1) effect sizes of the rate of
change in species richness, calculated as the log-ratio of richness at
the two time points, divided by the number of years elapsed between
richness observations; and, (2) time series of richness and a diversity
metric related to Simpson’s concentration.
