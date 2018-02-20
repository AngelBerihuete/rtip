#' @title Mean income per unit of consumption
#'
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#'
#' @description Estimates the mean income per unit of consumption which is the mean of the equivalised disposable income.
#'
#' @param dataset a data.frame containing the variables.
#' @param ipuc a character string indicating the variable name of the income per unit of consumption. Default is "ipuc".
#' @param hhcsw a character string indicating the variable name of the household cross-sectional weight. Default is "DB090".
#' @param hhsize a character string indicating the variable name of the household size. Default is "HX040".
#' @param ci a scalar or vector containing the confidence level(s) of the required interval(s). Default does not calculate the confidence interval.
#' @param rep a number to make the confidence interval using boostrap technique.
#' @param verbose logical; if TRUE the confidence interval is plotted.
#'
#' @details The equivalised disposable income is calculated using the standard equivalence scale (called the modified OECD scale) recommended by Eurostat. The parametric scale of Buhmann et al. (1988) can also be used. The default is the modified OECD scale (see setupDataset).
#'
#' @return The value of mean income per unit of consumption
#'
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' @references \url{http://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:Equivalised_disposable_income}
#'
#' @examples
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT")
#' miuc(ATdataset)
#'
#' @seealso setupDataset.
#' @import boot
#' @export

miuc <- function(dataset,
                 ipuc = "ipuc", # The income per unit of consumption
                 hhcsw = "DB090", # Household cross-sectional weight
                 hhsize = "HX040", # Household size
                 ci = NULL, rep = 1000, verbose = FALSE){

  dataset <- dataset[order(dataset[,"ipuc"]),]
  dataset$wHX040 <- dataset[,hhcsw]*dataset[,hhsize] # household weights taking into account the size of the household

  if(is.null(ci)){
    dataset$acum.wHX040 <- cumsum(dataset$wHX040)
    number.homes <- length(dataset$acum.wHX040)
    number.individuals <- dataset$acum.wHX040[number.homes]
    miuc <- sum(dataset$ipuc*dataset$wHX040)/number.individuals
    return(miuc)
  }else{
    if (ci == TRUE) {
      warning("argument ci=TRUE is deprecated; please check the documentation",
              call. = FALSE)
      ci <- 0.95
    }
    miuc2 <- function(dataset, i){
      dataset.boot <- dataset[i,]
      dataset.boot$acum.wHX040 <- cumsum(dataset.boot$wHX040)
      number.homes <- length(dataset.boot$acum.wHX040)
      number.individuals <- dataset.boot$acum.wHX040[number.homes]
      sum(dataset.boot$ipuc*dataset.boot$wHX040)/number.individuals
    }
    boot.miuc <- boot::boot(dataset, statistic = miuc2, R = rep,
                     sim = "ordinary", stype = "i")
    miuc.ci <- boot::boot.ci(boot.miuc, conf = ci, type = "basic")
    if(verbose == FALSE){
      return(miuc.ci)
    }else{
      plot(boot.miuc)
      summary(miuc.ci)
      return(miuc.ci)
    }
  }
}
