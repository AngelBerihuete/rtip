#' @title  Twice the area under the TIP curve  
#' 
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' 
#' @description Estimates the poverty measure which is twice the area under the TIP curve.   
#' 
#' @param dataset a data.frame containing variables obtained by using the setupDataset function.
#' @param arpt.value the at-risk-of-poverty threshold to be used  (see arpt).
#' @param norm logical; if  TRUE, the area under the normalized TIP curve is then estimated (see tip). 
#' @param ci logical; if  TRUE, 95 percent confidence interval is given for this area.
#' @param rep a number to do the confidence interval using boostrap technique.
#' @param verbose logical; if TRUE the confindence interval is plotted. 
#' 
#' @details It is computed using the equivalized disposable income. The equivalence scales that can be employed are the modified OECD scale or the parametric scale of Buhmann et al. (1988). The default is the modified OECD scale (see setupDataset).
#' 
#' This poverty index coincides with the Sen-Shorrocks-Thon index and the S(2,z) index of Sordo and Ramos (2011).
#' 
#' @return The value of the poverty measure.
#' 
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' @references A.F. Shorrocs (1995) Revisiting the Sen poverty index, Econometrica, 63, 1225--1230.
#' @references D. Thon (1979) On measuring poverty, Review of Income and Wealth, 25, 429--439.
#' @references D. Thon (1983) A poverty measure, The Indian Economic Journal, 30, 55--70.
#' @references M.A. Sordo and C.D. Ramos (2011) Poverty comparisons when TIP curves intersect, SORT, 31, 65--80.
#' 
#' @seealso tip, setupDataset, arpt
#' 
#' @examples 
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT", s = "OECD")
#' s2(ATdataset,arpt.value = arpt(ATdataset), norm = TRUE)
#' 
#' @export

s2 <- function(dataset, arpt.value, norm = FALSE, ci = FALSE, rep = 1000, verbose = FALSE){
  if(ci == FALSE){
    # 
    # REVISAR CON CI = TRUE, argumentos de diferentes longitud
    # ---------------------
    
    dataset <- dataset[order(dataset[,"ipuc"]), ]
    dataset$acum.wHX040 <- cumsum(dataset$wHX040)
    dataset$abscisa2 <-
      dataset$acum.wHX040/dataset$acum.wHX040[length(dataset$acum.wHX040)]
    
    gap.aux <- arpt.value-dataset$ipuc
    dataset$pg <- pmax(gap.aux, 0) # poverty gaps
    
    dataset$aux.prod <- dataset$wHX040*dataset$pg
    dataset$acum.pg <- cumsum(dataset$aux.prod)
    
    if(norm == FALSE){
      dataset$tip <- dataset$acum.pg/dataset$acum.wHX040[length(
        dataset$acum.wHX040)]
    }else{
      dataset$tip <- dataset$acum.pg/dataset$acum.wHX040[length(
        dataset$acum.wHX040)]/arpt.value
    }
    
    alturas.triang <- dataset$tip[2:length(dataset$tip)] - dataset$tip[-length(dataset$tip)]
    length.x <- dataset$abscisa2[2:length(dataset$abscisa2)] - dataset$abscisa2[-length(dataset$abscisa2)]
    
    area.triang <- alturas.triang*length.x/2
    area.rectan <- length.x*dataset$tip[-length(dataset$tip)]
    areas <- area.triang + area.rectan
    areas <- c(dataset$abscisa2[1]*dataset$tip[1]/2, areas)
    cum.areas <- cumsum(areas)
    abscisas2 <- dataset$abscisa2

    s2 <- 2*cum.areas[length(cum.areas)] # s2 index
    #results <- c(s2, abscisas2, cum.areas)
    return(s2)
  
    }else{
    s23 <- function(dataset, i, arpt.value, norm){
      dataset.boot <- dataset[i,]
      dataset.boot <- dataset.boot[order(dataset.boot[,"ipuc"]), ]
      dataset.boot$acum.wHX040 <- cumsum(dataset.boot$wHX040) # poblacional
      dataset.boot$abscisa2 <-
        dataset.boot$acum.wHX040/dataset.boot$acum.wHX040[length(dataset.boot$acum.wHX040)]
      
      gap.aux <- arpt.value-dataset.boot$ipuc
      dataset.boot$pg <- pmax(gap.aux, 0) # poverty gaps
      
      dataset.boot$aux.prod <- dataset.boot$wHX040*dataset.boot$pg
      dataset.boot$acum.pg <- cumsum(dataset.boot$aux.prod)
      
      if(norm == FALSE){
        dataset.boot$tip <- dataset.boot$acum.pg/dataset.boot$acum.wHX040[length(
          dataset.boot$acum.wHX040)]
      }else{
        dataset.boot$tip <- dataset.boot$acum.pg/dataset.boot$acum.wHX040[length(
          dataset.boot$acum.wHX040)]/arpt.value
      }
      
      alturas.triang <- dataset.boot$tip[2:length(dataset.boot$tip)] - dataset.boot$tip[-length(dataset.boot$tip)]
      length.x <- dataset.boot$abscisa2[2:length(dataset.boot$abscisa2)] - dataset.boot$abscisa2[-length(dataset.boot$abscisa2)]
      
      area.triang <- alturas.triang*length.x/2
      area.rectan <- length.x*dataset.boot$tip[-length(dataset.boot$tip)]
      areas <- area.triang + area.rectan
      areas <- c(dataset.boot$abscisa2[1]*dataset.boot$tip[1]/2, areas)
      cum.areas <- cumsum(areas)
      abscisas2 <- dataset.boot$abscisa2
      2*cum.areas[length(cum.areas)] # s2 index
    }
    boot.s2 <- boot(dataset, statistic = s23, R = rep,
                      sim = "ordinary", stype = "i",
                    arpt.value = arpt.value, norm = norm)
    s2.ci <- boot.ci(boot.s2, type = "basic")
    if(verbose == FALSE){
      return(s2.ci)
    }else{
      plot(boot.s2)
      summary(s2.ci)
      return(s2.ci)
    }
  } 
}