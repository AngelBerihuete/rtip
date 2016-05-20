#' @title Setup datasets loaded from the living conditions survey
#'
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#'
#' @description Extracts and transforms variables taken directly from the EU-SILC survey.
#'
#' @param dataset a data.frame containing variables in the EU-SILC microdata format.
#' @param country a character string specifying the country whose data will be considered.
#' @param region a character string specifying the region of the country whose data will be considered.
#' @param s either a character string or a numeric value between 0 and 1 specifying the equivalence scale to be used to obtain the equivalized disposable income. The default ("OECD") considers the standar modified OECD scale.
#' @param deflac numeric; a number to be used as a deflator. The default (NULL) will not apply any deflation.
#' @param ppp a logical; if it is TRUE the purchasing power parity (PPP) exchange rate will be used.
#'
#' @details We obtain the equivalized disposable income with the equivalence
#' scale of Buhmann et al. (1988) by assigning a numeric value between 0 and 1
#' to argument s . The parameter s is called elasticity of equivalence.
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
#'  \item ipuc a numeric vector containing the income per unit of consumption. This variable takes into account if deflac is not NULL, ppp is TRUE or/and the value assigned to \emph{s}.
#'  \item wHX040 a numeric vector which is obtained by multiplying DB090 by HX040. It represents household weights taking into account the size of the household.
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
                         region = 'all',
                         s = 'OECD',
                         deflac = NULL,
                         ppp = FALSE) {

  # The following line is only to overcome the note obtained by
  # R CMD check, because the using in subset function
  DB020 = DB040 = year = NULL

  if(!is.null(country)){ # only for one region
    dataset <- subset(dataset, DB020 == country)
  }else{
    stop("The variable country is mandatory")
  }

  if(region != 'all'){ # only for one region
    dataset <- subset(dataset, DB040 == region)
  }

  ok.cases <- complete.cases(dataset)
  dataset <- dataset[ok.cases,]


  #   remove.data <- which(is.na(dataset$HX090)) # renove NA data
#
#   if(length(remove.data) != 0){
#     dataset <- dataset[-remove.data, ]
#   }
#
  if(ppp){ # Purchasing power parity
    aux.year <- unique(dataset$DB010)
    ppp.rates <- subset(ppp.rates, year == aux.year)
    country1 <- country
    indx4ppp <- which(ppp.rates$country == country1)

    if(is.na(ppp.rates$ppp[indx4ppp])){
      stop(paste("Country ", country1, " has NA as ppp value", sep = ""))
    }else{
      ppp.rate <- ppp.rates$ppp[indx4ppp]/ppp.rates$rate[indx4ppp]
    }
    dataset$HX090 <- dataset$HX090/ppp.rate
    rm(ppp.rates)
  }

  if(!is.null(deflac)){ # Deflaction
    dataset$HX090 <- dataset$HX090/deflac
  }

  # income per unit of consumption
  if(s == "OECD"){
    dataset$ipuc <- dataset$HX090
  }else{
    dataset$ipuc <- (dataset$HX090*dataset$HX050)/dataset$HX040^s
  }

  dataset$wHX040 <- dataset$DB090*dataset$HX040

  return(dataset)
}
