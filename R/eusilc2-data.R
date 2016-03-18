#' @title Modified synthetic EU-SILC survey data
#'
#' @description
#' The dataset eusilc2 is the same as in the laeken package
#' (see reference below), but transformed in order to do calculations
#' using rtip package functions. Therefore eusilc2 is a synthetic dataset
#' generated from real Austrian EU-SILC containing a data frame.
#'
#' @format
#' A data frame with 6000 rows and 7 variables:
#' \itemize{
#' \item DB010, a numeric vector containing the year of the survey.
#' \item DB020, a factor with one level which is the country considered.
#' \item DB040, a factor with as many levels as there are regions in the country.
#' \item DB090, a numeric vector containing information about household cross-sectional weight.
#' \item HX040, an integer vector containing information about households size.
#' \item HX050, a numeric vector containing information about the equivalised household size. The scale employed is the modified OECD scale.
#' \item HX090, a numeric vector containing information about equivalised disposable income (with the modified OECD scale).
#' }
#'
#' @note The original dataset (eusilc) and the transformations done to obtain
#' eusilc2 dataset are included in data-raw directory
#' (source version package only).
#'
#' @usage data(eusilc2)
#'
#' @references A. Andreas et al. (2013) \href{https://www.jstatsoft.org/index.php/jss/article/view/v054i15}{
#' Estimation of Social Exclusion Indicators from Complex Surveys: The R Package laeken},
#' Journal of Statistics Software, 54:1, 1--25
"eusilc2"
