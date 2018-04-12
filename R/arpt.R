#' @title At-risk-of-poverty threshold
#'
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#'
#' @description Estimates the at-risk-of-poverty threshold which is set at 60 percent of the median equivalised disposable income using the standard definition.
#'
#' @param dataset a data.frame containing the variables.
#' @param ipuc a character string indicating the variable name of the income per unit of consumption. Default is "ipuc".
#' @param hhcsw a character string indicating the variable name of the household cross-sectional weight. Default is "DB090".
#' @param hhsize a character string indicating the variable name of the household size. Default is "HX040".
#' @param pz a number between 0 and 1 which represents the percentage to be used to calculate the at-risk-of-poverty threshold. The default is 0.6.
#' @param ci a scalar or vector containing the confidence level(s) of the required interval(s). Default does not calculate the confidence interval.
#' @param rep a number to do the confidence interval using boostrap technique.
#' @param verbose logical; if TRUE the confidence interval is plotted.
#'
#' @details The equivalised disposable income is calculated using the standard equivalence scale (called the modified OECD scale) recommended by Eurostat. The parametric scale of Buhmann et al.(1988) can also be used. The default is the modified OECD scale  (see setupDataset).
#'
#' @return The value of the at-risk-of-poverty threshold.
#'
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' @references \url{http://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:At-risk-of-poverty_rate}
#'
#' @seealso setupDataset
#'
#' @examples
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT")
#' arpt(ATdataset)
#' @import boot
#' @export

arpt <- function(dataset,
                 ipuc = "ipuc", # The income per unit of consumption
                 hhcsw = "DB090", # Household cross-sectional weight
                 hhsize = "HX040", # Household size
                 pz = 0.6, ci = NULL, rep = 1000, verbose = FALSE){

  dataset <- dataset[order(dataset[,ipuc]), ]
  dataset$wHX040 <- dataset[,hhcsw]*dataset[,hhsize] # household weights taking into account the size of the household
  if(is.null(ci)){
    dataset$acum.wHX040 <- cumsum(dataset$wHX040)
    dataset$abscisa2 <-
      dataset$acum.wHX040/dataset$acum.wHX040[length(dataset$acum.wHX040)]
    uc.median <- dataset[which(dataset$abscisa2 > 0.5)[1], ipuc]
    arpt.value <- pz*uc.median # pz is the percentage for median
    return(arpt.value)
  }else{
    if (ci == TRUE) {
      warning("argument ci=TRUE is deprecated; please check the documentation",
              call. = FALSE)
      ci <- 0.95
    }
    arpt2 <- function(dataset, i, pz){
      dataset.boot <- dataset[i,]
      dataset.boot <- dataset.boot[order(dataset.boot[,ipuc]),]
      dataset.boot$acum.wHX040 <- cumsum(dataset.boot$wHX040)
      dataset.boot$abscisa2 <-
        dataset.boot$acum.wHX040/dataset.boot$acum.wHX040[length(dataset.boot$acum.wHX040)]
      uc.median <- dataset.boot[which(dataset.boot$abscisa2 > 0.5)[1], ipuc]
      pz*uc.median
      }
    boot.arpt.value <- boot::boot(dataset, statistic = arpt2, R = rep,
                      sim = "ordinary", stype = "i", pz = pz)
    arpt.value.ci <- boot::boot.ci(boot.arpt.value, conf = ci, type = "basic")
    if(verbose == FALSE){
      return(arpt.value.ci)
    }else{
      plot(boot.arpt.value)
      summary(arpt.value.ci)
      return(arpt.value.ci)
    }
  }
}
