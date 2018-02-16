#' @title Spanish living conditions survey data for the year 2014
#'
#' @usage data(LCS2014)
#'
#' @description
#' This is the Spanish National Statistics Institute (INE in Spanish)
#' release for the living conditions survey in 2014. The dataset is not modified
#' but transformed properly in order to use functions in the package.
#' You can obtain the raw datasets at
#' \href{http://www.ine.es/dyngs/INEbase/en/operacion.htm?c=Estadistica_C&cid=1254736176807&menu=ultiDatos&idp=1254735976608}{INE}.
#'
#' These datasets and the function to extract the variables are available
#' in data-raw directory (source version package only).
#'
#' @format A data frame with 11965 rows of 7 variables:
#'
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
#' @note According to the INE regulation, it is mandatory to inform users that
#' the values in this dataset were not modified.
#'
#' @docType data
#'
"LCS2014"
