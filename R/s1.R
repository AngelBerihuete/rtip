#' @title  Maximum of TIP curve
#'
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#'
#' @description Estimates the highest point of the TIP curve which is a measure of the intensity of poverty. It is equal to the mean poverty gap (difference between the poverty threshold and the equivalised disposable income).
#'
#' @param dataset a data.frame containing the variables.
#' @param ipuc a character string indicating the variable name of the income per unit of consumption. Default is "ipuc".
#' @param hhcsw a character string indicating the variable name of the household cross-sectional weight. Default is "DB090".
#' @param hhsize a character string indicating the variable name of the household size. Default is "HX040".
#' @param arpt.value the at-risk-of-poverty threshold to be used  (see arpt). Default is NULL which calculates arpt with default parameters.
#' @param norm logical; if  TRUE, the normalised mean poverty gap index is calculated which adds up the extent to which individuals on average fall below the poverty threshold, and expresses it as a percentage of the poverty threshold.
#' @param ci a scalar or vector containing the confidence level(s) of the required interval(s). Default does not calculate the confidence interval.
#' @param rep a number to do the confidence interval using boostrap technique.
#' @param verbose logical; if TRUE the confidence interval is plotted.
#'
#' @details It is computed using the equivalised disposable income. The equivalence scales that can be employed are the modified OECD scale or the parametric scale of Buhmann et al. (1988). The default is the modified OECD scale (see setupDataset).
#'
#' The normalised mean poverty gap index, also named FGT(1), is a particular case of the family of poverty indexes proposed by Foster, Greer and Thorbecke (1984).
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
#' ATdataset <- setupDataset(eusilc2, country = "AT")
#' s1(ATdataset,arpt.value = arpt(ATdataset), norm = TRUE)
#' @import boot
#' @export

s1 <- function(dataset,
               ipuc = "ipuc", # The income per unit of consumption
               hhcsw = "DB090", # Household cross-sectional weight
               hhsize = "HX040", # Household size
               arpt.value = NULL,
               norm = FALSE, ci = NULL, rep = 1000, verbose = FALSE){

  if(is.null(arpt.value)) arpt.value <- arpt(dataset, ipuc, hhcsw, hhsize)

  if(is.null(ci)){
    maxtip <- max(tip(dataset,
                      ipuc = ipuc,
                      hhcsw = hhcsw,
                      hhsize = hhsize,
                      arpt.value = arpt.value, samplesize="complete", norm)[,2])
    return(maxtip)
  }else{
    if (ci == TRUE) {
      warning("argument ci=TRUE is deprecated; please check the documentation",
              call. = FALSE)
      ci <- 0.95
    }
    s11 <- function(dataset, i, arpt.value, norm){
      max(tip(dataset[i,],
              ipuc = ipuc,
              hhcsw = hhcsw,
              hhsize = hhsize,
              arpt.value = arpt.value, samplesize="complete", norm)[,2]) # s1 index
    }
    boot.s1 <- boot::boot(dataset, statistic = s11, R = rep,
                    sim = "ordinary", stype = "i",
                    arpt.value = arpt.value, norm = norm)
    s1.ci <- boot::boot.ci(boot.s1, conf = ci, type = "basic")
    if(verbose == FALSE){
      return(s1.ci)
    }else{
      plot(boot.s1)
      summary(s1.ci)
      return(s1.ci)
    }
  }
}
