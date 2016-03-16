#' @title Mean income per unit of consumption 
#' 
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' 
#' @description Estimates the mean income per unit of consumption which is the mean of the equivalized disposable income. 
#' 
#' @param dataset a data.frame containing variables obtained by using the setupDataset function.
#' @param ci logical; if  TRUE, 95 percent confidence interval is given for the mean income per unit of consumption.
#' @param rep a number to do the confidence interval using boostrap technique.
#' @param verbose logical; if TRUE the confindence interval is plotted.
#' 
#' @details The equivalized disposable income is calculated using the standar equivalence scale called the modified OECD scale and recommended by Eurostat. The parametric scale of Buhmann et al. (1988) can also be used. The default is the modified OECD scale (see setupDataset).
#' 
#' @return The value of mean income per unit of consumption 
#'  
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' @references \url{http://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:Equivalised_disposable_income}
#' 
#' @examples 
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT", s = "OECD")
#' miuc(ATdataset)
#' 
#' @seealso setupDataset.
#' 
#' @export  

miuc <- function(dataset, ci = FALSE, rep = 1000, verbose = FALSE){
  dataset <- dataset[order(dataset[,"ipuc"]),]
  if(ci == FALSE){
    dataset$acum.wHX040 <- cumsum(dataset$wHX040)
    number.homes <- length(dataset$acum.wHX040)
    number.individuals <- dataset$acum.wHX040[number.homes]
    miuc <- sum(dataset$ipuc*dataset$wHX040)/number.individuals
    return(miuc)
  }else{
    miuc2 <- function(dataset, i){
      dataset.boot <- dataset[i,]
      dataset.boot$acum.wHX040 <- cumsum(dataset.boot$wHX040)
      number.homes <- length(dataset.boot$acum.wHX040)
      number.individuals <- dataset.boot$acum.wHX040[number.homes]
      sum(dataset.boot$ipuc*dataset.boot$wHX040)/number.individuals
    }
    boot.miuc <- boot(dataset, statistic = miuc2, R = rep,
                     sim = "ordinary", stype = "i")
    miuc.ci <- boot.ci(boot.miuc, type = "basic")
    if(verbose == FALSE){
      return(miuc.ci)
    }else{
      plot(boot.miuc)
      summary(miuc.ci)
      return(miuc.ci)
    }
  } 
}