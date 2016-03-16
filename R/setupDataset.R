#' @title Setup dataset from EU-SILC survey
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
#' @details The parametric scale of Buhmann et al. (1988) can also be used assigning to \emph{s} parameter a value between 0 and 1. The parameter s is called  elasticity of equivalence.
#' 
#' The purchasing power parity exchange rate is useful for making comparisons between countries.  
#' 
#' @return A data.frame with the following variables:
#' @return DB010 a numeric vector containing the year of the survey.
#' @return DB020 a factor with one level which is the country considered.
#' @return DB040 a factor with as many levels as there are regions in the country.
#' @return DB090 a numeric vector containing information about household cross-sectional weight.
#' @return HX040 an integer vector containing information about households size.
#' @return HX050 a numeric vector containing information about the equivalised household size. The scale employed is the modified OECD scale.
#' @return HX090 a numeric vector containing information about equivalised disposable income (with the modified OECD scale).
#' @return ipuc a numeric vector containing the income per unit of consumption. This variable takes into account if deflac is not NULL, ppp is TRUE or/and the value assigned d to \emph{s}.  
#' @return wHX040 a numeric vector which is obtained by multiplying DB090 by HX040. It represents household weights taking into account the size of the household.
#' 
#' @seealso 
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
  
  # SI ppp.rate = TRUE, entonces da problemas aux.year
  # ------------------------------------------------
  # Hay que incluirlo en el dataset original
  
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
    #data(ppp_rates) # NO ESTA CARGANDO DE FORMA CORRECTA
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

# aux.data > dataset
# weights2 > wHX040
# weights1 > HX040
# 
#   aux.data <- data.frame(ipuc = ipuc)
#   aux.data$region <- factor(as.character(dataset$DB040))
#   aux.data$year <- dataset$DB010
#   aux.data$weights1 <- dataset$HX040
#   aux.data$HX040 <- dataset$HX040
#   aux.data$DB090 <- dataset$DB090
#   
#   aux.data$HX050 <- dataset$HX050
#   aux.data$HX090 <- dataset$HX090
  return(dataset)
}

# aux.data <- data.frame(ipuc = eusilc2$eqIncome)
# aux.data$region <- eusilc2$db040
# aux.data$year <- rep(2006, length(eusilc2$db040))
# aux.data$weights1 <- eusilc2$hsize
# aux.data$HX040 <- eusilc2$hsize
# aux.data$DB090 <- eusilc2$db090
# aux.data$weights2 <- aux.data$DB090*aux.data$HX040
# aux.data$HX050 <- eusilc2$eqSS
# aux.data$HX090 <- eusilc2$eqIncome

