#' @title Mean income per household
#'
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#'
#' @description Estimates the mean income per household.
#'
#' @param dataset a data.frame containing variables obtained by using the setupDataset function.
#' @param ci logical; if  TRUE, 95 percent confidence interval is given for the mean income per unit of consumption.
#' @param rep a number to make the confidence interval using boostrap technique.
#' @param verbose logical; if TRUE the confindence interval is plotted.
#'
#' @return The value of mean income per household.
#'
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' @references \url{http://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:Equivalised_disposable_income}
#'
#' @examples
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT", s = "OECD")
#' mih(ATdataset)
#'
#' @seealso setupDataset.
#' @import boot
#' @export

mih <- function(dataset, ci = FALSE, rep = 1000, verbose = FALSE){
  if(ci == FALSE){
    mih <- sum(dataset$HX090*dataset$HX050*dataset$DB090)/sum(dataset$DB090)
    return(mih)
  }else{
    mih2 <- function(dataset, i){
      dataset.boot <- dataset[i,]
      sum(dataset.boot$HX090*dataset.boot$HX050*dataset.boot$DB090)/sum(dataset.boot$DB090)
    }
    boot.mih <- boot::boot(dataset, statistic = mih2, R = rep,
                     sim = "ordinary", stype = "i")
    mih.ci <- boot::boot.ci(boot.mih, type = "basic")
    if(verbose == FALSE){
      return(mih.ci)
    }else{
      plot(boot.mih)
      summary(mih.ci)
      return(mih.ci)
    }
  }
}
