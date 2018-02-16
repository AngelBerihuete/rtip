#' @title Matrix for testing TIP dominance
#'
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#'
#' @description The auxiliary function OmegaTIP computes the (empirical) vector
#' of TIP curve ordinates and its corresponding covariance matrix.
#' Given two income distributions, this matrix will be used to test the
#' null hypothesis that one distribution dominates the other in the TIP sense.
#'
#' @param dataset a data.frame containing the variables.
#' @param ipuc a character string indicating the variable name of the income per unit of consumption. Default is "ipuc".
#' @param hhcsw a character string indicating the variable name of the household cross-sectional weight. Default is "DB090".
#' @param hhsize a character string indicating the variable name of the household size. Default is "HX040".
#' @param arpt.value the at-risk-of-poverty threshold to be used (see arpt). Default is NULL which calculates arpt with default parameters.
#' @param samplesize an integer which represents the number of TIP curve ordinates to be estimated. These ordinates will be estimated at points \eqn{p_i}, where \eqn{p_i=i/samplesize, i=1, \dots, samplesize}. Default is 50.
#' @param norm logical; if  TRUE, the normalised TIP curve ordinates are computed using the normalised poverty gaps (poverty gaps divided by the poverty threshold).
#'
#'
#' @details Estimation of TIP curve ordinates and their covariance matrix are made following Beach and Davidson (1983), Beach and Kaliski (1986) and Xu and Osberg (1998).
#'
#' Calculations are made using the equivalised disposable income. The equivalence scales that can be employed are the modified OECD scale or the parametric scale of Buhmann et al. (1988). The default is the modified OECD scale (see setupDataset).
#'
#' @return A list with the following components:
#' \itemize{
#' \item Omega, covariance matrix for the estimated vector of TIP curve ordinates.
#' \item tip.curve estimated vector of TIP curve ordinates.
#' }
#'
#' @references C. M. Beach and R. Davidson (1983) Distribution-free statistical inference with Lorenz curves and income shares, Review of Economic Studies, 50, 723--735.
#' @references C. M. Beach and S. F. Kaliski (1986) Curve inference with sample weights: and application to the distribution of unemployment experience, Journal of the Royal Statistical Society. Series C (Applied Statistics), Vol. 35, No. 1, 38--45.
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' @references K. Xu and L. Osberg (1998) A distribution-free test for deprivation dominance, Econometric Reviews,17, 415--429.
#'
#' @seealso testTIP, setupDataset, arpt
#'
#' @export

OmegaTIP <- function(dataset,
                     ipuc = "ipuc", # The income per unit of consumption
                     hhcsw = "DB090", # Household cross-sectional weight
                     hhsize = "HX040", # Household size
                     arpt.value = NULL, samplesize = 50, norm = FALSE){

  if(is.null(arpt.value)) arpt.value <- arpt(dataset, ipuc, hhcsw, hhsize)

  dataset$wHX040 <- dataset[,hhcsw]*dataset[,hhsize] #household weights taking into account the size of the household

  G <- arpt.value-dataset[,ipuc]
  dataset$pg <- pmax(G, 0) # poverty gaps
  dataset1 <- dataset[order(dataset[,'pg']), ] # order with pg

  dataset1$Acum <- cumsum(dataset1$wHX040)
  dataset1$Acum.P_i <- dataset1$Acum/dataset1$Acum[length(dataset1$Acum)]

  number.individuals <- dataset1$Acum[length(dataset1$Acum)]

  p_i <- (1:samplesize)/samplesize
  np_i <- floor(p_i*number.individuals)

  sigma <- mat.or.vec(samplesize, samplesize)
  vector.gamma.i <- c()

  for(i in 1:samplesize){
    pos.i <- which(dataset1$Acum.P_i>=p_i[i])[1]
    if(pos.i == 1){
      gamma.i <- vector.gamma.i[i] <- dataset1$pg[pos.i]
      lambda.i <- 0
    }else{
      gamma.i <- sum(dataset1$pg[1:(pos.i-1)]*dataset1$wHX040[1:(pos.i-1)]) +
        dataset1$pg[pos.i]*(np_i[i]-dataset1$Acum[pos.i-1])
      gamma.i <- gamma.i/np_i[i]
      vector.gamma.i[i] <- gamma.i
      lambda.i <- sum((dataset1$pg[1:(pos.i-1)]-gamma.i)^2*dataset1$wHX040[1:(pos.i-1)])+
                        (dataset1$pg[pos.i]-gamma.i)^2*(np_i[i]-dataset1$Acum[pos.i-1])

      lambda.i <- lambda.i/np_i[i]
    }

    for(j in i:samplesize){
      pos.j <- which(dataset1$Acum.P_i>=p_i[j])[1]

      if(pos.j == 1){
        gamma.j <- dataset1$pg[pos.j]
      }else{
        gamma.j <- sum(dataset1$pg[1:(pos.j-1)]*dataset1$wHX040[1:(pos.j-1)]) +
          dataset1$pg[pos.j]*(np_i[j]-dataset1$Acum[pos.j-1])
        gamma.j <- gamma.j/np_i[j]
      }
         sigma[i,j] <- p_i[i]*(lambda.i+(1-p_i[j])*(dataset1$pg[pos.i]-gamma.i)*(dataset1$pg[pos.j]-gamma.j)+(dataset1$pg[pos.i]-gamma.i)*(gamma.j-gamma.i))
    }
  }

  sigma <- sigma + t(sigma)
  diag(sigma) <- diag(sigma)/2

  dim.sigma <- dim(sigma)
  aux.R <- mat.or.vec(nr=(dim.sigma[1]-1),nc=(dim.sigma[1]-1))

  dim.aux.R <- dim(aux.R)

  for(k in 1:dim.aux.R[1]){
    aux.R[k,(dim.aux.R[1]-k+1)] <- -1
  }

  matrixR <- mat.or.vec(nr=dim(sigma)[1],nc=dim(sigma)[2])
  dim.matrixR <- dim(matrixR)
  matrixR[,dim.matrixR[2]] <- rep(1,length.out=dim.matrixR[1])
  matrixR[1:dim.aux.R[1], 1:dim.aux.R[2]] <- aux.R

  Omega.tip <- matrixR %*% sigma %*% t(matrixR)
  Omega.tip <- Omega.tip/number.individuals

  # Obtaining the TIP curve
  K <- length(vector.gamma.i)
  gammak <- rep(vector.gamma.i[K], times = K)
  gammak.pk <- p_i*vector.gamma.i
  rev.gammak.pk <- rev(gammak.pk)
  tip.curve <- gammak-rev.gammak.pk

  if(norm == TRUE){
    Omega.tip <- Omega.tip/arpt.value^2
    tip.curve <- tip.curve/arpt.value
  }

  return(list(Omega = Omega.tip, tip.curve = tip.curve))
}
