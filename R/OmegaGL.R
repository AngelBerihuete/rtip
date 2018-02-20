#' @title Matrix for testing Generalized Lorenz dominance
#'
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#'
#' @description The auxiliary function OmegaGL computes the (empirical) vector
#' of Generalized Lorenz (GL) curve ordinates and its corresponding covariance
#' matrix. Given two income distributions, this matrix will be used to test the
#' null hypothesis that one distribution dominates the other in the Generalized
#' Lorenz sense.
#'
#' @param dataset a data.frame containing the variables.
#' @param ipuc a character string indicating the variable name of the income per unit of consumption. Default is "ipuc".
#' @param hhcsw a character string indicating the variable name of the household cross-sectional weight. Default is "DB090".
#' @param hhsize a character string indicating the variable name of the household size. Default is "HX040".
#' @param samplesize An integer representing the number of GL ordinates to be estimated. Default is 10.
#' These ordinates are estimated at points \eqn{p_i}, where \eqn{p_i=i/samplesize, \quad i=1, \dots, samplesize}.
#' @param generalized logical; if FALSE the matrix for testing Lorenz dominance will be calculated.
#' @details Estimation of GL curve ordinates and their covariance matrix are calculated following Beach and Davidson (1983) and Beach and Kaliski (1986).
#'
#' Calculations are made using the equivalised disposable income. The equivalence scales that can be employed are the modified OECD scale or the parametric scale of Buhmann et al. (1988). The default is the modified OECD scale (see setupDataset).
#'
#' @return A list with the following components:
#' \itemize{
#' \item Omega, covariance matrix for the estimated vector of GL curve ordinates.
#' \item gl.curve, estimated vector of GL curve ordinates.
#' \item p, vector with components \eqn{p_i=i/samplesize, \quad i=1,..., samplesize}.
#' \item quantiles, estimated vector of quantiles of income corresponding to these \eqn{p_i}.
#' \item gamma, vector of estimated conditional means of income less than the quantile corresponding to \eqn{p_i=i/samplesize, \quad i=1, \dots,  samplesize}.
#' }
#'
#'
#' @references C. M. Beach and R. Davidson (1983) Distribution-free statistical inference with Lorenz curves and income shares, Review of Economic Studies, 50, 723--735.
#' @references C. M. Beach and S. F. Kaliski (1986) Curve inference with sample weights: and application to the distribution of unemployment experience, Journal of the Royal Statistical Society. Series C (Applied Statistics), Vol. 35, No. 1, 38--45.
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' @references K. Xu (1997) Asymptotically distribution-free statistical test for generalized Lorenz curves: An alternative approach, Journal of Income Distribution, 7, 45--62.
#'
#' @seealso testGL, setupDataset
#' @importFrom stats constrOptim pchisq complete.cases
#'
#' @export

OmegaGL <- function(dataset,
                    ipuc = "ipuc", # The income per unit of consumption
                    hhcsw = "DB090", # Household cross-sectional weight
                    hhsize = "HX040", # Household size
                    samplesize = 10, generalized = TRUE){

select <- (1:samplesize)/samplesize
dataset1 <- dataset[order(dataset[,'ipuc']), ]
dataset$wHX040 <- dataset[,hhcsw]*dataset[,hhsize] # household weights taking into account the size of the household

dataset1$Acum <- cumsum(dataset1$wHX040)
dataset1$Acum.P_i <- dataset1$Acum/dataset1$Acum[length(dataset1$Acum)]

number.individuals <- dataset1$Acum[length(dataset1$Acum)]

p_i <- (1:samplesize)/samplesize
np_i <- floor(p_i*number.individuals)

sigma <- mat.or.vec(samplesize, samplesize)
vector.gamma_i <- c()
quantile.i <- c()
for(i in 1:samplesize){
  pos.i <- which(dataset1$Acum.P_i>=p_i[i])[1]
  quantile.i[i] <- dataset1$ipuc[pos.i]
  if(pos.i == 1){
    gamma_i <- vector.gamma_i[i] <- dataset1$ipuc[pos.i]
    lambda.i <- 0
  }else{
    gamma_i <- sum(dataset1$ipuc[1:(pos.i-1)]*dataset1$wHX040[1:(pos.i-1)]) +
      dataset1$ipuc[pos.i]*(np_i[i]-dataset1$Acum[pos.i-1])
    gamma_i <- vector.gamma_i[i] <- gamma_i/np_i[i]
    lambda.i <- sum((dataset1$ipuc[1:(pos.i-1)]-gamma_i)^2*dataset1$wHX040[1:(pos.i-1)])+
      (dataset1$ipuc[pos.i]-gamma_i)^2*(np_i[i]-dataset1$Acum[pos.i-1])

    lambda.i <- lambda.i/np_i[i]
  }

  for(j in i:samplesize){
    pos.j <- which(dataset1$Acum.P_i>=p_i[j])[1]
    if(pos.j == 1){
      gamma.j <- dataset1$ipuc[pos.j]
    }else{
      gamma.j <- sum(dataset1$ipuc[1:(pos.j-1)]*dataset1$wHX040[1:(pos.j-1)]) +
        dataset1$ipuc[pos.j]*(np_i[j]-dataset1$Acum[pos.j-1])
      gamma.j <- gamma.j/np_i[j]
    }
    sigma[i,j] <- p_i[i]*(lambda.i+(1-p_i[j])*(dataset1$ipuc[pos.i]-gamma_i)*(dataset1$ipuc[pos.j]-gamma.j)+(dataset1$ipuc[pos.i]-gamma_i)*(gamma.j-gamma_i))
  }
}

sigma <- sigma + t(sigma)
diag(sigma) <- diag(sigma)/2

if(generalized == FALSE){

  J <- diag(rep(1/gamma.j, (samplesize-1)))
  end.col <- -p_i[-samplesize]*vector.gamma_i[-samplesize]/gamma.j^2
  J <- cbind(J, end.col)
  Omega.gl <- J %*% sigma %*% t(J)
  Omega.gl <- Omega.gl/number.individuals

  gl.curve <- vector.gamma_i*p_i/gamma.j

} else {
  Omega.gl <- sigma/number.individuals
  gl.curve <- vector.gamma_i*p_i
}

return(list(Omega = Omega.gl, gl.curve = gl.curve, p = p_i,
            quantiles = quantile.i, gamma = vector.gamma_i))
}
