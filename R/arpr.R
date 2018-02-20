#' @title At-risk-of-poverty rate
#'
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#'
#' @description Estimates the poverty rate which is defined as the share of people with an equivalised disposable income below the at-risk-of-poverty threshold.
#'
#' @param dataset a data.frame containing the variables.
#' @param ipuc a character string indicating the variable name of the income per unit of consumption. Default is "ipuc".
#' @param hhcsw a character string indicating the variable name of the household cross-sectional weight. Default is "DB090".
#' @param hhsize a character string indicating the variable name of the household size. Default is "HX040".
#' @param arpt.value the at-risk-of-poverty threshold to be used  (see arpt). Default is NULL which calculates arpt with default parameters.
#' @param ci a scalar or vector containing the confidence level(s) of the required interval(s). Default does not calculate the confidence interval.
#' @param rep a number to make the confidence interval using boostrap technique.
#' @param verbose logical; if TRUE the confidence interval is plotted.
#'
#' @details The equivalised disposable income is calculated using the standard equivalence scale (called the modified OECD scale) recommended by Eurostat. The parametric scale of Buhmann et al. (1988) can also be used. The default is the modified OECD scale  (see setupDataset).
#'
#' @return The value of the at-risk-of-poverty rate.
#'
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' @references \url{http://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:At-risk-of-poverty_rate}
#'
#' @examples
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT")
#' arpr(ATdataset,arpt.value = arpt(ATdataset))
#'
#' @seealso arpt, setupDataset
#' @import boot
#' @importFrom graphics plot
#' @export

arpr <- function(dataset,
                 ipuc = "ipuc", # The income per unit of consumption
                 hhcsw = "DB090", # Household cross-sectional weight
                 hhsize = "HX040", # Household size
                 arpt.value = NULL, ci = NULL, rep = 1000, verbose = FALSE){

  dataset <- dataset[order(dataset[,"ipuc"]), ]
  dataset$wHX040 <- dataset[,hhcsw]*dataset[,hhsize] #household weights taking into account the size of the household

  if(is.null(arpt.value)) arpt.value <- arpt(dataset, ipuc, hhcsw, hhsize)

  if(is.null(ci)){
    dataset$acum.wHX040 <- cumsum(dataset$wHX040)
    dataset$abscisa2 <-
      dataset$acum.wHX040/dataset$acum.wHX040[length(dataset$acum.wHX040)]
    arpr <- 100*(dataset$abscisa2[length(which(dataset$ipuc < arpt.value))])
    return(arpr)
  }else{

    if (ci == TRUE) {
      warning("argument ci=TRUE is deprecated; please check the documentation",
              call. = FALSE)
      ci <- 0.95
    }

    arpr3 <- function(dataset, i, arpt.value){
      dataset.boot <- dataset[i,]
      dataset.boot <- dataset.boot[order(dataset.boot[,"ipuc"]), ]
      dataset.boot$acum.wHX040 <- cumsum(dataset.boot$wHX040)
      dataset.boot$abscisa2 <-
      dataset.boot$acum.wHX040/dataset.boot$acum.wHX040[length(dataset.boot$acum.wHX040)]
      100*(dataset.boot$abscisa2[length(which(dataset.boot$ipuc < arpt.value))])
    }
    boot.arpr <- boot::boot(dataset, statistic = arpr3, R = rep,
                       sim = "ordinary", stype = "i", arpt.value = arpt.value)
    arpr.ci <- boot::boot.ci(boot.arpr, conf = ci, type = "basic")
    if(verbose == FALSE){
      return(arpr.ci)
    }else{
      summary(arpr.ci)
      plot(boot.arpr)
      return(arpr.ci)
    }
  }
}
