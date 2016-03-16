#' @title  TIP curve  
#' 
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' 
#' @description Estimates TIP curve ordinates. The TIP curve is defined by plotting the cumulated proportion of population on the x-axis and the cumulated per capita poverty gap (the distance between each income and the poverty threshold) on the y-axis from the biggest one downwards.   
#' 
#' @param dataset a data.frame containing variables obtained by using the setupDataset function.
#' @param arpt.value the at-risk-of-poverty threshold to be used  (see arpt).
#' @param norm logical; if  TRUE, the normalized TIP curve ordinates are computed using the normalized poverty gaps (poverty gaps divided by the poverty threshold).
#' 
#' @details The TIP (Three I's of Poverty) curve ordinates are computed using the equivalized disposable income. The equivalence scales that can be employed are the modified OECD scale or the parametric scale of Buhmann et al. (1988). The default is the modified OECD scale (see setupDataset).
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
#' ATdataset <- setupDataset(eusilc2, country = "AT", s = "OECD")
#' tip.curve <- tip(ATdataset,arpt.value = arpt(ATdataset), norm = TRUE)
#' str(tip.curve)
#' 
#' @export

tip <- function(dataset, arpt.value, norm = FALSE, plot = FALSE){
  dataset <- dataset[order(dataset[, "ipuc"]), ]
  dataset$pg <- pmax(arpt.value - dataset$ipuc, 0) # poverty gaps
  w2xpg <- dataset$wHX040*dataset$pg
  acum.w2xpg <- cumsum(w2xpg)
  acum.wHX040 <- cumsum(dataset$wHX040)
  y.tip <- acum.w2xpg/acum.wHX040[length(acum.wHX040)]
  x.tip <- acum.wHX040/acum.wHX040[length(acum.wHX040)]
  if(norm == TRUE) y.tip <- y.tip/arpt.value
  tip.curve <- data.frame(x.tip = c(0, x.tip), y.tip = c(0, y.tip))
  if(plot){
    arpr.value <- arpr(dataset, arpt.value)
    xlim.aux <- (arpr.value/100 + 0.2)
       p <- ggplot(data = tip.curve, aes(x.tip, y.tip)) + geom_line() +
         geom_segment(aes(x = arpr.value/100, y = 0, 
                          xend = arpr.value/100, yend = max(y.tip)),
                      linetype = "longdash",
                      color = "red") +
         geom_segment(aes(x = 0, y = max(y.tip), 
                          xend = arpr.value/100, yend = max(y.tip)),
                      linetype = "longdash",
                      color = "red") +
         annotate("text", label = "arpr value", x = (arpr.value/100),
                  y = -max(y.tip)/80, size = 4, colour = "red") +
         annotate("text", label = "s1 value", x = 0,
                  y = (max(y.tip)+max(y.tip)/80), size = 4, colour = "red") +
        scale_x_continuous("Cumulated proportion of population",
                           limits = c(0,xlim.aux)) +
        scale_y_continuous("") +
        ggtitle("TIP curve")
       print(p)
       }
    
  return(tip.curve)
}