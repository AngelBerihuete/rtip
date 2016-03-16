#' @title Gini index 
#' 
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' 
#' @description Estimates the Gini inequality index of an income distribution.
#' 
#' @param dataset a data.frame containing variables obtained by using the setupDataset function.
#' @param ci logical; if  TRUE, 95 percent confidence interval is given for the Gini coefficient.
#' @param rep a number to do the confidence interval using boostrap technique.
#' @param verbose logical; if TRUE the confindence interval is plotted.
#' 
#' @details The Gini index is calculated using the equivalized disposable income of each individual. Two types of equivalence scales can be used, the modified OECD scale and the parametric scale of Buhmann et al. (1988). The default is the modified OECD scale  (see setupDataset).
#' 
#' @return The value of the Gini index. 
#' 
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' @references E. Ferreira and A. Garín (1997) Una nota sobre el cálculo del índice de Gini, Estadística Española, 39(142), 207--218.
#' @references \url{http://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:Gini_coefficient}
#' 
#' @examples 
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT", s = "OECD")
#' gini(ATdataset)
#' 
#' @seealso setupDataset
#' 
#' @export


gini <- function(dataset, ci = FALSE, rep = 1000, verbose = FALSE){
  if(ci == FALSE){
    dataset <- dataset[order(dataset[,"ipuc"]), ]
    dataset$acum.wHX040 <- cumsum(dataset$wHX040)
    dataset$X <- dataset$ipuc*dataset$wHX040
    dataset$p_i <- dataset$wHX040/dataset$acum.wHX040[length(dataset$acum.wHX040)]
    dataset$pi2 <- dataset$p_i/2
    dataset$acum.p_i <- cumsum(dataset$p_i)
    dataset$Fi <-  dataset$acum.p_i - dataset$pi2
    M <- sum(dataset$X)/dataset$acum.wHX040[length(dataset$acum.wHX040)]
    gini <- 100*(2*sum(dataset$ipuc*dataset$p_i*dataset$Fi)/M-1)
    return(gini)
  }else{
    gini3 <- function(dataset, i){
      dataset.boot <- dataset[i,]
      dataset.boot <- dataset.boot[order(dataset.boot[,"ipuc"]), ]
      dataset.boot$acum.wHX040 <- cumsum(dataset.boot$wHX040)
      dataset.boot$X <- dataset.boot$ipuc*dataset.boot$wHX040
      dataset.boot$p_i <- dataset.boot$wHX040/dataset.boot$acum.wHX040[length(dataset.boot$acum.wHX040)]
      dataset.boot$pi2 <- dataset.boot$p_i/2
      dataset.boot$acum.p_i <- cumsum(dataset.boot$p_i)
      dataset.boot$Fi <-  dataset.boot$acum.p_i - dataset.boot$pi2
      M <- sum(dataset.boot$X)/dataset.boot$acum.wHX040[length(dataset.boot$acum.wHX040)]
      100*(2*sum(dataset.boot$ipuc*dataset.boot$p_i*dataset.boot$Fi)/M-1)
    }
    boot.gini <- boot(dataset, statistic = gini3, R = rep,
                     sim = "ordinary", stype = "i")
    gini.ci <- boot.ci(boot.gini, type = "basic")
    if(verbose == FALSE){
      return(gini.ci)
    }else{
      summary(gini.ci)
      plot(boot.gini)
      return(gini.ci)
    }
  } 
}