# rtip

[![Build Status](https://travis-ci.org/AngelBerihuete/rtip.svg?branch=master)](https://travis-ci.org/AngelBerihuete/rtip)
[![CRAN_Status_Badge](http://cranlogs.r-pkg.org/badges/grand-total/rtip)]
(http://cran.r-project.org/package=rtip)


rtip is an R package containing tools to measure and compare inequality, 
welfare and poverty using the EU statistics on income and living conditions 
surveys (EU-SILC). 

The package also estimates the ordinates of the Generalized Lorenz  
and TIP curves. Furthermore, some tests are implemented to study the dominance 
of two income distributions from Generalized Lorenz  and TIP curves.


## Installation

The development version from github:

```R
# install.packages("devtools")
devtools::install_github("AngelBerihuete/rtip")
```

## Examples 

There are many inequality and poverty indicators coded in the package. For 
instance, in order to estimate the poverty rate which is defined as the share 
of people with an equivalized disposable income below the at-risk-of-poverty 
threshold:

```R
library(rtip)
data(eusilc2)
ATdataset <- setupDataset(eusilc2, country = "AT")
arpr(ATdataset,arpt.value = arpt(ATdataset))
```

The package also has a function to make a statistical test to study Generalized 
Lorenz dominance from sample Generalized Lorenz curve estimates:

```R
library(rtip)
data(eusilc2)
ATdataset1 <- setupDataset(eusilc2, country = "AT", region = "Burgenland")
ATdataset2 <- setupDataset(eusilc2, country = "AT", region = "Carinthia")
testGL(ATdataset1, ATdataset2, generalized = TRUE, samplesize = 10)
```

It is possible to do the previous study also with TIP curves:

```R
data(eusilc2)
ATdataset <- setupDataset(eusilc2, country = "AT")
ATdataset1 <- setupDataset(eusilc2, country = "AT", region = "Burgenland")
ATdataset2 <- setupDataset(eusilc2, country = "AT", region = "Carinthia")
testTIP(ATdataset1, ATdataset, same.arpt.value = arpt(ATdataset))
```

## Citation
A BibTeX entry for LaTeX users is
```bibtex
@article{RJ-2018-029,
  author = {Angel Berihuete and Carmen D. Ramos and Miguel A. Sordo},
  title = {{Welfare, Inequality and Poverty Analysis with rtip: An
          Approach Based on Stochastic Dominance}},
  year = {2018},
  journal = {{The R Journal}},
  doi = {10.32614/RJ-2018-029},
  url = {https://doi.org/10.32614/RJ-2018-029},
  pages = {328--341},
  volume = {10},
  number = {1}
}
```
