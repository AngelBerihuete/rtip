#' @title Modified synthetic EU-SILC survey data
#' Synthetic dataset generated from real Austrian EU-SILC.
#' 
#' The dataset is the same as in laeken package but
#' with transformed variables in order to perform right calculations
#' using laeken2.
#' 
#' @docType data
#' 
#' @usage data(eusilc2)
#' 
#' @format A data frame with 6000 rows of 7 variables:
#' \describe{
#'  \item{DB010}{integer; year of the pull}
#'  \item{DB020}{factor; name of the country}
#'  \item{DB040}{factor; name of the region within the country}}
#'  
#' @references Moore et al. (2013) Genetics 195:1077-1086
#' (\href{http://www.ncbi.nlm.nih.gov/pubmed/23979570}{PubMed})
"eusilc2"