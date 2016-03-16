#' @title Matrix for testing Generalized Lorenz dominance 
#'  
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' 
#' @description The auxiliary function OmegaGL computes the (empirical) vector of Generalized Lorenz (GL) curve ordinates and its corresponding covariance matrix. This matrix will be used to compute the test-statistic to test for the Generalized Lorenz dominance relation between two GL curves.
#' 
#' @param dataset a data.frame containing variables obtained by using the setupDataset function.
#' @param samp an integer which represents the number of the GL ordinates to be estimated. These ordinates will be estimated at points \eqn{p_i}, where \eqn{p_i=i/samp, \quad i=1, \dots, samp}.
#' 
#' @details Estimation of GL curve ordinates and their covariance matrix are calculated following Beach and Davidson (1983).
#' 
#' Calculations are made using the equivalized disposable income. The equivalence scales that can be employed are the modified OECD scale or the parametric scale of Buhmann et al. (1988). The default is the modified OECD scale (see setupDataset).
#' 
#' @return A list with the following components:
#' @return Omega covariance matrix for the estimated vector of GL curve ordinates.
#' @return gl.curve estimated vector of GL curve ordinates.
#' @return p_i vector with components p_i=i/samp, i=1, ..., samp.  
#' @return quantile_i estimated vector of quantiles of income corresponding to these p_i's. 
#' @return gamma.i vector of estimated conditional means of income less than the quantil corresponding to p_i=i/samp, i=1, ..., samp. 
#' 
#' @references C. M. Beach and R. Davidson (1983) Distribution-free statistical inference with Lorenz curves and income shares, Review of Economic Studies, 50, 723--735.
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' @references K. Xu (1997) Asymptotically distribution-free statistical test for generalized Lorenz curves: An alternative approach, Journal of Income Distribution, 7, 45--62.
#' 
#' @examples 
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT", s = "OECD")
#' omega.gl <- OmegaGL(ATdataset, samp = 10)
#' str(omega.gl)
#' 
#' @seealso testGL, setupDataset
#' 
#' @export  

OmegaGL <- function(dataset, samp){

select <- (1:samp)/samp
dataset1 <- dataset[order(dataset[,'ipuc']), ]

dataset1$Acum <- cumsum(dataset1$wHX040)
dataset1$Acum.P_i <- dataset1$Acum/dataset1$Acum[length(dataset1$Acum)]

number.individuals <- dataset1$Acum[length(dataset1$Acum)]

p_i <- (1:samp)/samp
np_i <- floor(p_i*number.individuals)

sigma <- mat.or.vec(samp, samp)
vector.gamma.i <- c()
quantile.i <- c()
for(i in 1:samp){
  pos.i <- which(dataset1$Acum.P_i>=p_i[i])[1]
  quantile.i[i] <- dataset1$ipuc[pos.i]
  if(pos.i == 1){
    gamma.i <- vector.gamma.i[i] <- dataset1$ipuc[pos.i]
    lambda.i <- 0
  }else{
    gamma.i <- sum(dataset1$ipuc[1:(pos.i-1)]*dataset1$wHX040[1:(pos.i-1)]) +
      dataset1$ipuc[pos.i]*(np_i[i]-dataset1$Acum[pos.i-1])
    gamma.i <- vector.gamma.i[i] <- gamma.i/np_i[i]
    lambda.i <- sum((dataset1$ipuc[1:(pos.i-1)]-gamma.i)^2*dataset1$wHX040[1:(pos.i-1)])+
      (dataset1$ipuc[pos.i]-gamma.i)^2*(np_i[i]-dataset1$Acum[pos.i-1]) 
    
    lambda.i <- lambda.i/np_i[i]
  }
  
  for(j in i:samp){
    pos.j <- which(dataset1$Acum.P_i>=p_i[j])[1]
    if(pos.j == 1){
      gamma.j <- dataset1$ipuc[pos.j]
    }else{
      gamma.j <- sum(dataset1$ipuc[1:(pos.j-1)]*dataset1$wHX040[1:(pos.j-1)]) + 
        dataset1$ipuc[pos.j]*(np_i[j]-dataset1$Acum[pos.j-1])
      gamma.j <- gamma.j/np_i[j]
    }
    sigma[i,j] <- p_i[i]*(lambda.i+(1-p_i[j])*(dataset1$ipuc[pos.i]-gamma.i)*(dataset1$ipuc[pos.j]-gamma.j)+(dataset1$ipuc[pos.i]-gamma.i)*(gamma.j-gamma.i))
  }
}

sigma <- sigma + t(sigma)
diag(sigma) <- diag(sigma)/2

Omega.gl <- sigma/number.individuals
gl.curve <- vector.gamma.i*p_i

return(list(Omega = Omega.gl, gl.curve = gl.curve, p_i = p_i,
            quantile_i = quantile.i, gamma.i = vector.gamma.i))
}