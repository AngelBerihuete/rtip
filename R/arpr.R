#' @title At-risk-of-poverty rate
#' 
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' 
#' @description Estimates the poverty rate which is defined as the share of people with an equivalized disposable income below the at-risk-of-poverty threshold.
#' 
#' @param dataset a data.frame containing variables obtained by using the setupDataset function.
#' @param arpt.value the at-risk-of-poverty threshold to be used  (see arpt).
#' @param ci logical; if  TRUE, 95 percent confidence interval is given for the at-risk-of-poverty rate.
#' @param rep a number to do the confidence interval using boostrap technique.
#' @param verbose logical; if TRUE the confindence interval is plotted.
#' 
#' @details The equivalized disposable income is calculated using the standar equivalence scale called the modified OECD scale and recommended by Eurostat. The parametric scale of Buhmann et al. (1988) can also be used. The default is the modified OECD scale  (see setupDataset).
#' 
#' @return The value of the at-risk-of-poverty rate.
#'  
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' @references \url{http://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:At-risk-of-poverty_rate}
#' 
#' @examples 
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT", s = "OECD")
#' arpr(ATdataset,arpt.value = arpt(ATdataset))
#' 
#' @seealso arpt, setupDataset
#' 
#' @export

arpr <- function(dataset, arpt.value, ci = FALSE, rep = 1000, verbose = FALSE){
  
  if(ci == FALSE){
    dataset <- dataset[order(dataset[,"ipuc"]), ]
    dataset$acum.wHX040 <- cumsum(dataset$wHX040)
    dataset$abscisa2 <-
      dataset$acum.wHX040/dataset$acum.wHX040[length(dataset$acum.wHX040)]
    arpr <- 100*(dataset$abscisa2[length(which(dataset$ipuc < arpt.value))])
    return(arpr)
  }else{
    arpr3 <- function(dataset, i, arpt.value){
      dataset.boot <- dataset[i,]
      dataset.boot <- dataset.boot[order(dataset.boot[,"ipuc"]), ]
      dataset.boot$acum.wHX040 <- cumsum(dataset.boot$wHX040) # poblacional
      dataset.boot$abscisa2 <-
      dataset.boot$acum.wHX040/dataset.boot$acum.wHX040[length(dataset.boot$acum.wHX040)]
      100*(dataset.boot$abscisa2[length(which(dataset.boot$ipuc < arpt.value))])
    }
    boot.arpr <- boot(dataset, statistic = arpr3, R = rep,
                       sim = "ordinary", stype = "i", arpt.value = arpt.value)
    arpr.ci <- boot.ci(boot.arpr, type = "basic")
    if(verbose == FALSE){
      return(arpr.ci)
    }else{
      summary(arpr.ci)
      plot(boot.arpr)
      return(arpr.ci)
    }
  } 
}