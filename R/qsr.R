#' @title Income quintile share ratio 
#' 
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' 
#' @description Estimates the  quintile share ratio of an income distribution. It is defined as the ratio of total income  received by the 20 percent of the population with the highest income to that received by the 20 percent of the population with the lowest income. 
#' 
#' @param dataset a data.frame containing variables obtained by using the setupDataset function.
#' @param ci logical; if  TRUE, 95 percent confidence interval is given for the quintile share ratio.
#' @param rep a number to do the confidence interval using boostrap technique.
#' @param verbose logical; if TRUE the confindence interval is plotted.
#' 
#' @details It is calculated using the equivalized disposable income. Two types of equivalence scales can be used, the modified OECD scale and the parametric scale of Buhmann et al. ( 1988). The default is the modified OECD scale  (see setupDataset).
#' 
#' @return The value of the income quintile share ratio.
#'  
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' @references \url{http://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:Income_quintile_share_ratio}
#' 
#' @examples 
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT", s = "OECD")
#' qsr(ATdataset)
#' 
#' @seealso setupDataset
#' 
#' @export 

qsr <- function(dataset, ci = FALSE, rep = 1000, verbose = FALSE){
  if(ci == FALSE){
    dataset <- dataset[order(dataset[,"ipuc"]), ]
    dataset$acum.wHX040 <- cumsum(dataset$wHX040)
    dataset$abscisa2 <-
      dataset$acum.wHX040/dataset$acum.wHX040[length(dataset$acum.wHX040)]
    A <- dataset$ipuc*dataset$wHX040
    uc.S20 <- sum(A[which(dataset$abscisa2 < 0.2)])
    uc.S80 <- sum(A[which(dataset$abscisa2 > 0.8)])
    qsr <- uc.S80/uc.S20
    return(qsr)
  }else{
    qsr3 <- function(dataset, i){
      dataset.boot <- dataset[i,]
      dataset.boot <- dataset.boot[order(dataset.boot[,"ipuc"]), ]
      dataset.boot$acum.wHX040 <- cumsum(dataset.boot$wHX040)
      dataset.boot$abscisa2 <-
        dataset.boot$acum.wHX040/dataset.boot$acum.wHX040[length(dataset.boot$acum.wHX040)]
      A <- dataset.boot$ipuc*dataset.boot$wHX040
      uc.S20 <- sum(A[which(dataset.boot$abscisa2 < 0.2)])
      uc.S80 <- sum(A[which(dataset.boot$abscisa2 > 0.8)])
      uc.S80/uc.S20
    }
    boot.qsr <- boot(dataset, statistic = qsr3, R = rep,
                      sim = "ordinary", stype = "i")
    qsr.ci <- boot.ci(boot.qsr, type = "basic")
    if(verbose == FALSE){
      return(qsr.ci)
    }else{
      plot(boot.qsr)
      summary(qsr.ci)
      return(qsr.ci)
    }
  } 
}