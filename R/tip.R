#' @title  TIP curve
#'
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#'
#' @description Estimates TIP curve ordinates. The TIP curve is defined by plotting the cumulated proportion of population on the x-axis and the cumulated per capita poverty gap (the distance between each income and the poverty threshold) on the y-axis from the biggest one downwards.
#'
#' @param dataset a data.frame containing the variables.
#' @param ipuc a character string indicating the variable name of the income per unit of consumption. Default is "ipuc".
#' @param hhcsw a character string indicating the variable name of the household cross-sectional weight. Default is "DB090".
#' @param hhsize a character string indicating the variable name of the household size. Default is "HX040".
#' @param arpt.value the at-risk-of-poverty threshold to be used  (see arpt). Default is NULL which calculates arpt with default parameters.
#' @param samplesize an integer which specifies the number of (equally spaced) percentiles to be used in the estimation of the TIP ordinates The default is 50. If samplesize is set to ''complete'', ordinates are computed in each value along the whole distribution.
#' @param norm logical; if  TRUE, the normalised TIP curve ordinates are computed using the normalised poverty gaps (poverty gaps divided by the poverty threshold).
#' @param plot logical; if TRUE plots the TIP curve.
#' @details The TIP (Three I's of Poverty) curve ordinates are computed using the equivalised disposable income. The equivalence scales that can be employed are the modified OECD scale or the parametric scale of Buhmann et al. (1988). The default is the modified OECD scale (see setupDataset).
#'
#' @return A data.frame with the following components:
#' @return x.tip vector of cumulated proportion of population.
#' @return y.tip vector with values of tip curve ordinates.
#'
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' @references S.P. Jenkins and P.J. Lambert (1997) Three I's of poverty curves, with an analysis of UK poverty trends, Oxford Economic Papers, 49, 317--327.
#'
#' @seealso setupDataset, arpt
#'
#' @examples
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT")
#' tip.curve <- tip(ATdataset, arpt.value = arpt(ATdataset), norm = TRUE)
#' str(tip.curve)
#' @import ggplot2
#' @export

tip <- function(dataset,
                ipuc = "ipuc", # The income per unit of consumption
                hhcsw = "DB090", # Household cross-sectional weight
                hhsize = "HX040", # Household size
                arpt.value = NULL, samplesize = 50, norm = FALSE, plot = FALSE){
  # Following line to avoid Notes in Travis CI Check because ggplot
  x.tip <- y.tip <- arpr.value <- NULL

  if(is.null(arpt.value)) arpt.value <- arpt(dataset, ipuc, hhcsw, hhsize)

  if(samplesize != "complete"){
    res.tip <- OmegaTIP(dataset,
                        ipuc = ipuc, # The income per unit of consumption
                        hhcsw = hhcsw, # Household cross-sectional weight
                        hhsize = hhsize, # Household size
                        arpt.value=arpt.value, samplesize = samplesize, norm = norm)
    y.tip <- res.tip$tip.curve
    x.tip <- (1:samplesize)/samplesize
    tip.curve <- data.frame(x.tip = c(0, x.tip[1:samplesize-1]), y.tip = y.tip)
  }else{
    dataset <- dataset[order(dataset[, ipuc]), ]
    dataset$wHX040 <- dataset[,hhcsw]*dataset[,hhsize] # household weights taking into account the size of the household
    dataset$pg <- pmax(arpt.value - dataset[,ipuc], 0) # poverty gaps
    w2xpg <- dataset$wHX040*dataset$pg
    acum.w2xpg <- cumsum(w2xpg)
    acum.wHX040 <- cumsum(dataset$wHX040)
    y.tip <- acum.w2xpg/acum.wHX040[length(acum.wHX040)]
    x.tip <- acum.wHX040/acum.wHX040[length(acum.wHX040)]
    if(norm == TRUE) y.tip <- y.tip/arpt.value
    tip.curve <- data.frame(x.tip = c(0, x.tip), y.tip = c(0, y.tip))
  }


  if(plot){
    arpr.value <- arpr(dataset,
                       ipuc = ipuc, # The income per unit of consumption
                       hhcsw = hhcsw, # Household cross-sectional weight
                       hhsize = hhsize, # Household size
                       arpt.value=arpt.value)

    xlim.aux <- (arpr.value/100 + 0.2)
       p <- ggplot2::ggplot(data = tip.curve, aes(x.tip, y.tip)) +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous("Cumulated proportion of population",
                           limits = c(0,xlim.aux)) +
      ggplot2::scale_y_continuous("") +
      ggplot2::ggtitle("TIP curve")
      print(p)
       }

  return(tip.curve)
}
