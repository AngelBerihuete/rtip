#' @title  Maximun of TIP curve  
#' 
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' 
#' @description Estimates the highest point of the TIP curve which is a measure of the intensity of poverty. It is equal to the mean poverty gap (difference between the poverty threshold and the equivalized disposable income).
#' 
#' @param dataset a data.frame containing variables obtained by using the setupDataset function.
#' @param arpt.value the at-risk-of-poverty threshold to be used  (see arpt).
#' @param norm logical; if  TRUE, the normalized mean poverty gap index is calculated which adds up the extent to which individuals on average fall below the poverty threshold, and expresses it as a percentage of the poverty threshold.
#' @param ci logical; if  TRUE, 95 percent confidence interval is given for the mean poverty gap (or the normalized mean poverty gap index).
#' @param rep a number to do the confidence interval using boostrap technique.
#' @param verbose logical; if TRUE the confindence interval is plotted. 
#' 
#' @details It is computed using the equivalized disposable income. The equivalence scales that can be employed are the modified OECD scale or the parametric scale of Buhmann et al. (1988). The default is the modified OECD scale (see setupDataset).
#' 
#' The normalized mean poverty gap index, also named FGT(1), is a particular case of the family of poverty indexes proposed by Foster, Greer and Thorbecke (1984).
#' 
#' @return The value of the poverty measure.
#' 
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' @references J.E. Foster, J. Greer and E. Thorbecke (1984) Notes and comments. A class of descomposable poverty measures, Econometrica, 52, 761--766.
#' @references M.A. Sordo and C.D. Ramos (2011) Poverty comparisons when TIP curves intersect, SORT, 31, 65--80.
#' 
#' @seealso tip, setupDataset, arpt
#' 
#' @examples 
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT", , s = "OECD")
#' s1(ATdataset,arpt.value = arpt(ATdataset), norm = TRUE)
#' 
#' @export

s1 <- function(dataset, arpt.value, norm = FALSE, ci = FALSE, 
               rep = 1000, verbose = FALSE){
  if( ci == FALSE){
    maxtip <- max(tip(dataset, arpt.value, norm)[,2])
    return(maxtip)  
  }else{
    s11 <- function(dataset, i, arpt.value, norm){
      max(tip(dataset[i,], arpt.value, norm)[,2]) # s1 index
    }
    boot.s1 <- boot(dataset, statistic = s11, R = rep,
                    sim = "ordinary", stype = "i",
                    arpt.value = arpt.value, norm = norm)
    s1.ci <- boot.ci(boot.s1, type = "basic")
    if(verbose == FALSE){
      return(s1.ci)
    }else{
      plot(boot.s1)
      summary(s1.ci)
      return(s1.ci)
    } 
  }
}