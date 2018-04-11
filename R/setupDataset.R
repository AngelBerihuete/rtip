#' @title Setup datasets loaded from the living conditions survey
#'
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#'
#' @description Extracts and transforms variables taken directly from the EU-SILC survey.
#'
#' @param dataset a data.frame containing variables in the EU-SILC microdata format.
#' @param country a character string specifying the country whose data will be considered.
#' @param region a character/vector string specifying the region(s) of the country whose data will be considered. The default (NULL) considers all regions in the country.
#' @param s a numeric value between 0 and 1 specifying the equivalence scale to be used to obtain the equivalised disposable income. The default (NULL) considers the standard modified OECD scale.
#' @param deflator numeric; a number to be used as a deflator. The default (NULL) will not apply any deflation.
#' @param pppr the purchasing power parity rate (PPPR) will be used. Default is NULL.
#'
#' @details We obtain the equivalised disposable income with the equivalence
#' scale of Buhmann et al. (1988) by assigning a numeric value between 0 and 1
#' to argument s. The parameter s is called elasticity of equivalence.
#'
#' The purchasing power parity exchange rate is useful for making comparisons between countries.
#'
#' @return A data.frame with the following variables:
#' \itemize{
#'  \item DB010 a numeric vector containing the year of the survey.
#'  \item DB020 a factor with one level which is the country considered.
#'  \item DB040 a factor with as many levels as there are regions in the country.
#'  \item DB090 a numeric vector containing information about household cross-sectional weight.
#'  \item HX040 an integer vector containing information about households size.
#'  \item HX050 a numeric vector containing information about the equivalised household size. The scale employed is the modified OECD scale.
#'  \item HX090 a numeric vector containing information about equivalised disposable income (with the modified OECD scale).
#'  \item ipuc a numeric vector containing the income per unit of consumption. This variable takes into account the value assigned to s and pppr (if they are not NULL).
#' }
#'
#' @seealso loadEUSILC, loadLCS
#'
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#'
#' @examples
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT")
#' str(ATdataset)
#'
#' @export

setupDataset <- function(dataset,
                         country = 'ES' ,
                         region = NULL,
                         s = NULL,
                         deflator = NULL,
                         pppr = NULL) {

  if (is.character(s)) {
    warning("argument s = 'OECD' is deprecated; please check the documentation", call. = FALSE)
    s <- NULL
  }


  # The following line is only to overcome the note obtained by
  # R CMD check, because the using in subset function
  DB020 = DB040 = year = NULL

  if(!is.null(country)){ # only for one region
    dataset <- subset(dataset, DB020 == country)
  }else{
    stop("The variable country is mandatory")
  }

  if(!is.null(region)){ # filter the region(s)
      dataset <- subset(dataset, DB040 %in% region)
  }

  dataset <- dataset[complete.cases(dataset),]

  if(!is.null(pppr)){ # Purchasing power parity
    dataset$HX090 <- dataset$HX090/pppr
  }

  if(!is.null(deflator)){ # Deflation
    dataset$HX090 <- dataset$HX090/deflator
  }

  # income per unit of consumption
  if(is.null(s)){
    dataset$ipuc <- dataset$HX090
  }else{
    dataset$ipuc <- (dataset$HX090*dataset$HX050)/dataset$HX040^s
  }


  if(length(which(dataset$ipuc<0))!=0){
    warning("Some of the income values are negatives")
  }

  return(dataset)
}
