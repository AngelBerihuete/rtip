#' @title Relative median at-risk-of-poverty gap 
#' 
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' 
#' @description Estimates the relative median at-risk-of-poverty gap which is the difference between the at-risk-of-poverty threshold and the median equivalized disposable income of people below the at-risk-of-poverty threshold, expressed as a percentage of this threshold.
#'
#' @param dataset a data.frame containing variables obtained by using the setupDataset function.
#' @param arpt.value the at-risk-of-poverty threshold to be used  (see arpt).
#' @param ci logical; if  TRUE, 95 percent confidence interval is given for the relative median at-risk-of-poverty gap.
#' @param rep a number to do the confidence interval using boostrap technique.
#' @param verbose logical; if TRUE the confindence interval is plotted.
#'   
#' @details The equivalized disposable income is calculated using the standar equivalence scale called the modified OECD scale and recommended by Eurostat. The parametric scale of Buhmann et al. (1988) can also be used. The default is the modified OECD scale  (see setupDataset).
#' 
#' @return The value of the relative median at-risk-of-poverty gap.
#' 
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' @references \url{http://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:Relative_median_at-risk-of-poverty_gap}
#' 
#' @examples 
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT", s = "OECD")
#' rmpg(ATdataset,arpt.value = arpt(ATdataset))
#' 
#' @seealso arpt, setupDataset
#' 
#' @export  

rmpg <- function(dataset, arpt.value, ci = FALSE, rep = 1000, verbose = FALSE){
  if(ci == FALSE){
    dataset <- dataset[order(dataset[,"ipuc"]),]
    rmpg.data <- dataset[which(dataset$ipuc < arpt.value),]
    rmpg.data$weights.rmpg <- rmpg.data$DB090*rmpg.data$HX040  
    rmpg.data$acum.weights.rmpg <- cumsum(rmpg.data$weights.rmpg)
    rmpg.data$abscisa.rmpg <-
      rmpg.data$acum.weights.rmpg/rmpg.data$acum.weights.rmpg[length(rmpg.data$acum.weights.rmpg)]
    rmpg.median <- rmpg.data$ipuc[which(rmpg.data$abscisa.rmpg > 0.5)[1]]
    rmpg <- 100*(arpt.value-rmpg.median)/arpt.value
    return(rmpg)
  }else{
    rmpg3 <- function(dataset, i, arpt.value){
      dataset.boot <- dataset[i,]
      dataset.boot <- dataset.boot[order(dataset.boot[,"ipuc"]), ]
      rmpg.data <- dataset.boot[which(dataset.boot$ipuc < arpt.value),]
      rmpg.data$weights.rmpg <- rmpg.data$DB090*rmpg.data$HX040  
      rmpg.data$acum.weights.rmpg <- cumsum(rmpg.data$weights.rmpg)
      rmpg.data$abscisa.rmpg <-
        rmpg.data$acum.weights.rmpg/rmpg.data$acum.weights.rmpg[length(rmpg.data$acum.weights.rmpg)]
      rmpg.median <- rmpg.data$ipuc[which(rmpg.data$abscisa.rmpg > 0.5)[1]]
      100*(arpt.value-rmpg.median)/arpt.value
    }
    boot.rmpg <- boot(dataset, statistic = rmpg3, R = rep,
                     sim = "ordinary", stype = "i", arpt.value = arpt.value)
    rmpg.ci <- boot.ci(boot.rmpg, type = "basic")
    if(verbose == FALSE){
      return(rmpg.ci)
    }else{
      plot(boot.rmpg)
      summary(rmpg.ci)
      return(rmpg.ci)
    }
  } 
}