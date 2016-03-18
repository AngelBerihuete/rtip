#
# This script transforms the eusilc dataset in the package laeken
# (https://cran.r-project.org/web/packages/laeken/index.html) to eusilc2 dataset
# in order to use the functions in rtip package.
#
load("eusilc.RData")
eusilc2 <- eusilc[!duplicated(eusilc$db030),]

eusilc2 <- data.frame(
  DB010 = rep(2006, length(eusilc2$db040)),
  DB020 = as.factor(rep("AT", length(eusilc2$db040))),
  DB040 = eusilc2$db040,
  DB090 = eusilc2$db090,
  HX040 = eusilc2$hsize,
  HX050 = eusilc2$eqSS,
  HX090 = eusilc2$eqIncome)
