#' @title Gini index
#'
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#'
#' @description Estimates the Gini inequality index of an income distribution.
#'
#' @param dataset a data.frame containing the variables.
#' @param ipuc a character string indicating the variable name of the income per unit of consumption. Default is "ipuc".
#' @param hhcsw a character string indicating the variable name of the household cross-sectional weight. Default is "DB090".
#' @param hhsize a character string indicating the variable name of the household size. Default is "HX040".
#' @param ci a scalar or vector containing the confidence level(s) of the required interval(s). Default does not calculate the confidence interval.
#' @param rep a number to do the confidence interval using boostrap technique.
#' @param verbose logical; if TRUE the confidence interval is plotted.
#'
#' @details The Gini index is calculated using the equivalised disposable income of each individual. Two types of equivalence scales can be used, the modified OECD scale and the parametric scale of Buhmann et al. (1988). The default is the modified OECD scale  (see setupDataset).
#'
#' @return The value of the Gini index.
#'
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' @references E. Ferreira and A. Garín (1997) Una nota sobre el cálculo del índice de Gini, Estadística Española, 39(142), 207--218.
#' @references \url{http://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:Gini_coefficient}
#'
#' @examples
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT")
#' gini(ATdataset)
#'
#' @seealso setupDataset
#' @import boot
#' @export


gini <- function(dataset,
                 ipuc = "ipuc", # The income per unit of consumption
                 hhcsw = "DB090", # Household cross-sectional weight
                 hhsize = "HX040", # Household size
                 ci = NULL, rep = 1000, verbose = FALSE){

  dataset <- dataset[order(dataset[,"ipuc"]), ]
  dataset$wHX040 <- dataset[,hhcsw]*dataset[,hhsize] # household weights taking into account the size of the household

  if(is.null(ci)){
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
    if (ci == TRUE) {
      warning("argument ci=TRUE is deprecated; please check the documentation",
              call. = FALSE)
      ci <- 0.95
    }
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
    boot.gini <- boot::boot(dataset, statistic = gini3, R = rep,
                     sim = "ordinary", stype = "i")
    gini.ci <- boot::boot.ci(boot.gini, conf = ci, type = "basic")
    if(verbose == FALSE){
      return(gini.ci)
    }else{
      summary(gini.ci)
      plot(boot.gini)
      return(gini.ci)
    }
  }
}
