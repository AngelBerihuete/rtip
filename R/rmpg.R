#' @title Relative median at-risk-of-poverty gap
#'
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#'
#' @description Estimates the relative median at-risk-of-poverty gap which is the difference between the at-risk-of-poverty threshold and the median equivalised disposable income of people below the at-risk-of-poverty threshold, expressed as a percentage of this threshold.
#'
#' @param dataset a data.frame containing the variables.
#' @param ipuc a character string indicating the variable name of the income per unit of consumption. Default is "ipuc".
#' @param hhcsw a character string indicating the variable name of the household cross-sectional weight. Default is "DB090".
#' @param hhsize a character string indicating the variable name of the household size. Default is "HX040".
#' @param arpt.value the at-risk-of-poverty threshold to be used  (see arpt). Default is NULL which calculates arpt with default parameters.
#' @param ci a scalar or vector containing the confidence level(s) of the required interval(s). Default does not calculate the confidence interval.
#' @param rep a number to do the confidence interval using boostrap technique.
#' @param verbose logical; if TRUE the confidence interval is plotted.
#'
#' @details The equivalised disposable income is calculated using the standard equivalence scale (called the modified OECD scale) recommended by Eurostat. The parametric scale of Buhmann et al. (1988) can also be used. The default is the modified OECD scale  (see setupDataset).
#'
#' @return The value of the relative median at-risk-of-poverty gap.
#'
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' @references \url{http://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:Relative_median_at-risk-of-poverty_gap}
#'
#' @examples
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT")
#' rmpg(ATdataset,arpt.value = arpt(ATdataset))
#'
#' @seealso arpt, setupDataset
#' @import boot
#' @export

rmpg <- function(dataset,
                 ipuc = "ipuc", # The income per unit of consumption
                 hhcsw = "DB090", # Household cross-sectional weight
                 hhsize = "HX040", # Household size
                 arpt.value = NULL, ci = NULL, rep = 1000, verbose = FALSE){


  if(is.null(arpt.value)) arpt.value <- arpt(dataset, ipuc, hhcsw, hhsize)
  dataset <- dataset[order(dataset[,"ipuc"]),]

  if(is.null(ci)){
    rmpg.data <- dataset[which(dataset$ipuc < arpt.value),]
    rmpg.data$weights.rmpg <- rmpg.data[,hhcsw]*rmpg.data[,hhsize]
    rmpg.data$acum.weights.rmpg <- cumsum(rmpg.data$weights.rmpg)
    rmpg.data$abscisa.rmpg <-
      rmpg.data$acum.weights.rmpg/rmpg.data$acum.weights.rmpg[length(rmpg.data$acum.weights.rmpg)]
    rmpg.median <- rmpg.data$ipuc[which(rmpg.data$abscisa.rmpg > 0.5)[1]]
    rmpg <- 100*(arpt.value-rmpg.median)/arpt.value
    return(rmpg)
  }else{
    if (ci == TRUE) {
      warning("argument ci=TRUE is deprecated; please check the documentation",
              call. = FALSE)
      ci <- 0.95
    }
    rmpg3 <- function(dataset, i, arpt.value){
      dataset.boot <- dataset[i,]
      dataset.boot <- dataset.boot[order(dataset.boot[,"ipuc"]), ]
      rmpg.data <- dataset.boot[which(dataset.boot$ipuc < arpt.value),]
      rmpg.data$weights.rmpg <- rmpg.data[,hhcsw]*rmpg.data[,hhsize]
      rmpg.data$acum.weights.rmpg <- cumsum(rmpg.data$weights.rmpg)
      rmpg.data$abscisa.rmpg <-
        rmpg.data$acum.weights.rmpg/rmpg.data$acum.weights.rmpg[length(rmpg.data$acum.weights.rmpg)]
      rmpg.median <- rmpg.data$ipuc[which(rmpg.data$abscisa.rmpg > 0.5)[1]]
      100*(arpt.value-rmpg.median)/arpt.value
    }
    boot.rmpg <- boot::boot(dataset, statistic = rmpg3, R = rep,
                     sim = "ordinary", stype = "i", arpt.value = arpt.value)
    rmpg.ci <- boot::boot.ci(boot.rmpg, conf = ci, type = "basic")
    if(verbose == FALSE){
      return(rmpg.ci)
    }else{
      plot(boot.rmpg)
      summary(rmpg.ci)
      return(rmpg.ci)
    }
  }
}
