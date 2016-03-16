#' @title Lorenz and Generalized Lorenz curves 
#' 
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' 
#' @description Estimates the Lorenz and the Generalized Lorenz curves ordinates.
#' 
#' @param dataset a data.frame containing variables obtained by using the setupDataset function.
#' @param samp an integer which represents the number of ordinates to be estimated. The default is 10.  
#' @param generalized logical; if TRUE the Generalized Lorenz curve ordinates will be estimated.
#' 
#' @details Lorenz and Generalized Lorenz curves ordinates are computed using the equivalized disposable income. The equivalence scales employed are the modified OECD scale and the parametric scale of Buhmann et al. (1988) (see setupDataset).
#' 
#' @return A data.frame with the following components:
#' @return x.lg vector of cumulated proportion of population.
#' @return y.lg vector with values of the Lorenz or the Generalized Lorenz curves ordinates. 
#' 
#' @references B C Arnold (1987) Majorization and the Lorenz order: A brief introduction, Lecture Notes in Statistics, 43, Springer-Verlag.
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' 
#' @examples 
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT", s = "OECD")
#' lc.curve <- lc(ATdataset)
#' str(lc.curve)
#'  
#' @seealso setupDataset
#' 
#' @export  

lc <- function(dataset, samp = 10, generalized = FALSE, plot = FALSE){
  res.glc <- OmegaGL(dataset, samp = samp)
  
  if(generalized == FALSE){
    results <- data.frame(x.lg = c(0, res.glc$p_i),
                          y.lg = c(0, res.glc$gl.curve)/miuc(dataset))
    if(plot){
      p <- ggplot(data = results, aes(x.lg, y.lg)) + geom_line() +
        scale_x_continuous("Cumulated proportion of population") +
        scale_y_continuous("") +
        ggtitle("Lorenz curve")
      print(p)
    }
    
    return(results)  
  }else{
    results <- data.frame(x.lg = c(0, res.glc$p_i),
                          y.lg = c(0, res.glc$gl.curve))
    
    if(plot){
      p <- ggplot(data = results, aes(x.lg, y.lg)) + geom_line() +
        scale_x_continuous("Cumulated proportion of population") +
        scale_y_continuous("") +
        ggtitle("Generalized Lorenz curve")
    print(p)
      }
    
    return(results)
  }
}