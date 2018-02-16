#' @title Lorenz and Generalized Lorenz curves
#'
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#'
#' @description Estimates the Lorenz and the Generalized Lorenz curves ordinates.
#'
#' @param dataset a data.frame containing the variables.
#' @param ipuc a character string indicating the variable name of the income per unit of consumption. Default is "ipuc".
#' @param hhcsw a character string indicating the variable name of the household cross-sectional weight. Default is "DB090".
#' @param hhsize a character string indicating the variable name of the household size. Default is "HX040".
#' @param samplesize an integer which specifies the number of (equally spaced) percentiles to be used in the estimation of the Lorenz (or the Generalized Lorenz) ordinates. The default value is 10. If samplesize is set to ''complete'', ordinates are computed in each value along the whole distribution.
#' @param generalized logical; if TRUE the Generalized Lorenz curve ordinates will be estimated.
#' @param plot logical; if TRUE plots the Lorenz or Generalized Lorenz curve.
#'
#' @details Lorenz and Generalized Lorenz curves ordinates are computed using the equivalised disposable income. The equivalence scales employed are the modified OECD scale and the parametric scale of Buhmann et al. (1988) (see setupDataset).
#'
#' @return A data.frame with the following components:
#' \itemize{
#' \item x.lg, vector of cumulated proportion of population.
#' \item y.lg, vector with values of the Lorenz or the Generalized Lorenz curves ordinates.
#' }
#'
#' @references B C Arnold (1987) Majorization and the Lorenz order: A brief introduction, Lecture Notes in Statistics, 43, Springer-Verlag.
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#'
#' @examples
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT")
#' lc.curve <- lc(ATdataset)
#' str(lc.curve)
#'
#' @seealso setupDataset
#' @import ggplot2
#' @export

lc <- function(dataset,
               ipuc = "ipuc", # The income per unit of consumption
               hhcsw = "DB090", # Household cross-sectional weight
               hhsize = "HX040", # Household size
               samplesize = 10, generalized = FALSE, plot = FALSE){
  x.lg <- y.lg <- NULL # To avoid Notes in Travis CI checking (ggplot2)

  if(samplesize != "complete"){
    res.glc <- OmegaGL(dataset,
                       ipuc = "ipuc", # The income per unit of consumption
                       hhcsw = "DB090", # Household cross-sectional weight
                       hhsize = "HX040", # Household size
                       samplesize = samplesize, generalized = generalized)
    results <- data.frame(x.lg = c(0, res.glc$p),
                          y.lg = c(0, res.glc$gl.curve))
  }else{
    dataset <- dataset[order(dataset[, "ipuc"]), ]
    dataset$wHX040 <- dataset[,hhcsw]*dataset[,hhsize] # household weights taking into account the size of the household

    w2xpg <- dataset$wHX040*dataset$ipuc
    acum.w2xpg <- cumsum(w2xpg)
    acum.wHX040 <- cumsum(dataset$wHX040)

    x.lc <- acum.wHX040/acum.wHX040[length(acum.wHX040)]
    if(generalized){
      y.lc <- acum.w2xpg/acum.wHX040[length(acum.wHX040)]
    }else{
      y.lc <- acum.w2xpg/acum.w2xpg[length(acum.w2xpg)]
    }
    results <- data.frame(x.lg = c(0, x.lc),
                          y.lg = c(0, y.lc))
    }
  if(plot){
    if(generalized){
      p <- ggplot2::ggplot(data = results, aes(x.lg, y.lg)) +
        ggplot2::geom_line() +
        ggplot2::scale_x_continuous("Cumulated proportion of population") +
        ggplot2::scale_y_continuous("") +
        ggplot2::ggtitle("Generalized Lorenz curve")
    }else{
      p <- ggplot2::ggplot(data = results, aes(x.lg, y.lg)) +
        ggplot2::geom_line() +
        ggplot2::geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
                              linetype = "longdash",
                              color = "grey") +
        ggplot2::scale_x_continuous("Cumulated proportion of population") +
        ggplot2::scale_y_continuous("") +
        ggplot2::ggtitle("Lorenz curve")
    }
    print(p)
  }
  return(results)
}
