WhittakerBetaChange
================
Shane Blowes

<!-- badges: start -->
<!-- badges: end -->
The WhittakerBetaChange repository includes R code to reproduce the analyses shown in the article:

**Synthesis reveals biotic homogenisation and differentiation are both common**

*by Shane A. Blowes, Brian McGill, Viviana Brambilla, Cher F. Y. Chow, Thore Engel, Ada Fontrodona-Eslava, Inês S. Martins, Daniel McGlinn, Faye Moyes, Alban Sagouis, Hideyasu Shimadzu, Roel van Klink, Wu-Bing Xu, Nicholas J. Gotelli, Anne Magurran, Maria Dornelas, Jonathan M. Chase*

Alban Sagouis created new compilations of open access data used in these analyses. Code for the compilation can be found at:

(<https://github.com/chase-lab/metacommunity_surveys>)

(<https://github.com/chase-lab/checklist_change>)

(<https://github.com/chase-lab/homogenisation-richness>)

Here we give a brief overview on the code files in this repository

## Analysis

Files in the analysis folder include:

**00-fit-models-maintext-code-for-eve**: This folder contains scripts that were used to fit statistical models to data shown in main text (written for EVE (HPC))

**00-fit-models-supplement-code-for-eve**: This folder contains scripts that were used to fit statistical models to data shown in the supplement (sensitivity analyses)

**dist2line**: Function to calculate distance to 1:1 line

**ExDat-Fig1-map.R**: Code to produce Extended Data Figure 1

**ExDat-Fig2-2yr-multiyr-compare.R**: Code to produce Extended Data Figure 2: comparison of models fit to data with a single start/end year versus start/end periods for data where multiple years were available

\*ExDat-Fig3-scale-effect-size-plots.R\*\*: Code to produce Extended Data Figure 3: plot of empirical effect sizes as a function of spatial and temporal extent

**Fig2i-wrangle.R**: Code to wrangle model results for Figure 2

**Fig2ii-plot.R**: Code to plot Figure 2

**Fig3i-wrangle.R**: Code to wrangle model results for Figure 3

**Fig3ii-plot.R**: Code to plot Figure 3

## Data wrangle

This folder contains subfolders, each containing the code used to wrangle different sources of biodiversity data, and then combine them with the metadata in preparation for visualisation and model fitting.
