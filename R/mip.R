#' @title Mean income per person
#'
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#'
#' @description Estimates the mean income per person.
#'
#' @param dataset a data.frame containing variables obtained by using the setupDataset function.
#' @param ci logical; if  TRUE, 95 percent confidence interval is given for the mean income per unit of consumption.
#' @param rep a number to make the confidence interval using boostrap technique.
#' @param verbose logical; if TRUE the confindence interval is plotted.
#'
#' @return The value of mean income per person.
#'
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' @references \url{http://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:Equivalised_disposable_income}
#'
#' @examples
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT", s = "OECD")
#' mip(ATdataset)
#'
#' @seealso setupDataset.
#' @import boot
#' @export

mip <- function(dataset, ci = FALSE, rep = 1000, verbose = FALSE){
  dataset <- dataset[order(dataset[,"ipuc"]),]
  if(ci == FALSE){
    dataset$acum.wHX040 <- cumsum(dataset$wHX040)
    number.homes <- length(dataset$acum.wHX040)
    number.individuals <- dataset$acum.wHX040[number.homes]
    mip <- sum(dataset$HX090*dataset$HX050*dataset$DB090)/number.individuals
    return(mip)
  }else{
    mip2 <- function(dataset, i){
      dataset.boot <- dataset[i,]
      dataset.boot$acum.wHX040 <- cumsum(dataset.boot$wHX040)
      number.homes <- length(dataset.boot$acum.wHX040)
      number.individuals <- dataset.boot$acum.wHX040[number.homes]
      sum(dataset.boot$HX090*dataset.boot$HX050*dataset.boot$DB090)/number.individuals
    }
    boot.mip <- boot::boot(dataset, statistic = mip2, R = rep,
                     sim = "ordinary", stype = "i")
    mip.ci <- boot::boot.ci(boot.mip, type = "basic")
    if(verbose == FALSE){
      return(mip.ci)
    }else{
      plot(boot.mip)
      summary(mip.ci)
      return(mip.ci)
    }
  }
}
