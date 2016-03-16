#' @title At-risk-of-poverty threshold
#' 
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' 
#' @description Estimates the at-risk-of-poverty threshold which is set at 60 percent of the median equivalized disposable income using the standard definition.
#' 
#' @param dataset a data.frame containing variables obtained by using the setupDataset function.
#' @param pz a number between 0 and 1 which represents the percentage to be used to calculate the at-risk-of-poverty threshold. The default is 0.6.
#' @param ci logical; if  TRUE, 95 percent confidence interval is given for the at-risk-of-poverty threshold.
#' @param rep a number to do the confidence interval using boostrap technique.
#' @param verbose logical; if TRUE the confindence interval is plotted. 
#' 
#' @details The equivalized disposable income is calculated using the standar equivalence scale called the modified OECD scale and recommended by Eurostat. The parametric scale of Buhmann et al.(1988) can also be used. The default is the modified OECD scale  (see setupDataset).
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
#' ATdataset <- setupDataset(eusilc2, country = "AT", s = "OECD")
#' arpt(ATdataset)
#' 
#' @export


arpt <- function(dataset, pz = 0.6, ci = FALSE, rep = 500, verbose = FALSE){ 
  
  
  if(ci == FALSE){
    dataset <- dataset[order(dataset[,"ipuc"]), ]
    dataset$acum.wHX040 <- cumsum(dataset$wHX040)
    dataset$abscisa2 <-
      dataset$acum.wHX040/dataset$acum.wHX040[length(dataset$acum.wHX040)]
    uc.median <- dataset$ipuc[which(dataset$abscisa2 > 0.5)[1]]
    arpt.value <- pz*uc.median # pz is the percentage for median
    return(arpt.value)
  }else{
    arpt2 <- function(dataset, i, pz){
      dataset.boot <- dataset[i,]
      dataset.boot <- dataset.boot[order(dataset.boot[,"ipuc"]), ]
      dataset.boot$acum.wHX040 <- cumsum(dataset.boot$wHX040) # poblacional
      dataset.boot$abscisa2 <-
        dataset.boot$acum.wHX040/dataset.boot$acum.wHX040[length(dataset.boot$acum.wHX040)]
      uc.median <- dataset.boot$ipuc[which(dataset.boot$abscisa2 > 0.5)[1]]
      pz*uc.median
      }
    boot.arpt.value <- boot(dataset, statistic = arpt2, R = rep,
                      sim = "ordinary", stype = "i", pz = pz)
    arpt.value.ci <- boot.ci(boot.arpt.value, type = "basic")
    if(verbose == FALSE){
      return(arpt.value.ci)
    }else{
      plot(boot.arpt.value)
      summary(arpt.value.ci)
      return(arpt.value.ci)
    }
  }
}