#' @title Mean income per household
#'
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#'
#' @description Estimates the mean income per household.
#'
#' @param dataset a data.frame containing the variables.
#' @param hhcsw a character string indicating the variable name of the household cross-sectional weight. Default is "DB090".
#' @param ehhs a character string indicating the variable name of the equivalised household size. Default is "HX050".
#' @param edi a character string indicating the variable name of the equivalised disposable income (with the modified OECD scale). Default is "HX090".
#' @param ci a scalar or vector containing the confidence level(s) of the required interval(s). Default does not calculate the confidence interval.
#' @param rep a number to make the confidence interval using boostrap technique.
#' @param verbose logical; if TRUE the confidence interval is plotted.
#'
#' @return The value of mean income per household.
#'
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' @references \url{http://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:Equivalised_disposable_income}
#'
#' @examples
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT")
#' mih(ATdataset)
#'
#' @seealso setupDataset.
#' @import boot
#' @export

mih <- function(dataset,
                hhcsw = "DB090", # Household cross-sectional weight
                ehhs = "HX050", # Equivalised household size
                edi = "HX090", # Equivalised disposable income (with the modified OECD scale)
                ci = NULL, rep = 1000, verbose = FALSE){
  if(is.null(ci)){
    mih <- sum(dataset[,edi]*dataset[,ehhs]*dataset[,hhcsw])/sum(dataset[,hhcsw])
    return(mih)
  }else{
    mih2 <- function(dataset, i){
      dataset.boot <- dataset[i,]
      sum(dataset.boot[,edi]*dataset.boot[,ehhs]*dataset.boot[,hhcsw])/sum(dataset.boot[,hhcsw])
    }
    boot.mih <- boot::boot(dataset, statistic = mih2, R = rep,
                     sim = "ordinary", stype = "i")
    mih.ci <- boot::boot.ci(boot.mih, conf = ci, type = "basic")
    if(verbose == FALSE){
      return(mih.ci)
    }else{
      plot(boot.mih)
      summary(mih.ci)
      return(mih.ci)
    }
  }
}
