#' @title Matrix for testing TIP dominance
#' 
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' 
#' @description The auxiliary function OmegaTIP computes the (empirical) vector of TIP curve ordinates and its corresponding covariance matrix. This matrix will be used to compute the test-statistic to test for the TIP dominance relation between two TIP curves. 
#' 
#' @param dataset a data.frame containing variables obtained by using the setupDataset function.
#' @param arpt.value the at-risk-of-poverty threshold to be used (see arpt).
#' @param norm logical; if  TRUE, the normalized TIP curve ordinates are computed using the normalized poverty gaps (poverty gaps divided by the poverty threshold).
#' @param samp an integer which represents the number of TIP curve ordinates to be estimated. These ordinates will be estimated at points p_i, where p_i=i/samp, i=1, ..., samp.
#'
#'  
#' @details Estimation of TIP curve ordinates and their covariance matrix are made following Beach and Davidson (1983) and Xu and Osberg (1998).
#' 
#' Calculations are made using the equivalized disposable income. The equivalence scales that can be employed are the modified OECD scale or the parametric scale of Buhmann et al. (1988). The default is the modified OECD scale (see setupDataset).
#' 
#' @return A list with the following components:
#' @return Omega covariance matrix for the estimated vector of TIP curve ordinates.
#' @return tip.curve estimated vector of TIP curve ordinates. 
#'  
#' @references C. M. Beach and R. Davidson (1983) Distribution-free statistical inference with Lorenz curves and income shares, Review of Economic Studies, 50, 723--735.
#' @references B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.
#' @references K. Xu and L. Osberg (1998) A distribution-free test for deprivation dominance, Econometric Reviews,17, 415--429.   
#' 
#' @seealso testTIP, setupDataset, arpt
#' 
#' @export  

OmegaTIP <- function(dataset, arpt.value, norm = FALSE, samp){
  
  G <- arpt.value-dataset$ipuc
  dataset$pg <- pmax(G, 0) # poverty gaps
  dataset1 <- dataset[order(dataset[,'pg']), ] # order with pg
  
  dataset1$Acum <- cumsum(dataset1$wHX040)
  dataset1$Acum.P_i <- dataset1$Acum/dataset1$Acum[length(dataset1$Acum)]
  
  number.individuals <- dataset1$Acum[length(dataset1$Acum)]

  p_i <- (1:samp)/samp
  np_i <- floor(p_i*number.individuals)
  
  sigma <- mat.or.vec(samp, samp)
  vector.gamma.i <- c()
  
  for(i in 1:samp){
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
    
    for(j in i:samp){
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