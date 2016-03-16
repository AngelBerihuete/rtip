#' @title Test for Lorenz and Generalized Lorenz dominance 
#' 
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' 
#' @description Statistical test procedure given by Xu (1997) to study Generalized Lorenz dominance from sample Generalized Lorenz curve estimates.  Lorenz dominance from sample Lorenz curve estimates can also be studied (Beach and Davidson, 1983).
#'   
#' 
#' @param dataset1 a data.frame containing variables obtained by the using setupDataset function.
#' @param dataset2 a data.frame containing variables obtained by the using setupDataset function.
#' @param generalized logical; if TRUE the test will be applied to compare two Generalized Lorenz curves. In another case Lorenz curves will be compared.
#' @param samplesize an integer which represents the number of Lorenz (Generalized Lorenz) curve ordinates to be estimated for comparison. The default is 10.
#' 
#' 
#' @details The null hypotesis to be tested is if the  Lorenz (Generalized Lorenz) curve calculated from dataset1 dominates the one calculated from dataset2. 
#' 
#' 
#' @return A list with the following components:
#' @return Tvalue the value of the test-statistic
#' @return p.value simulated p-value of the test-statistic Tvalue (Wolak, 1989). It is calculated only when the Tvalue falls into an inconclusive region.
#' @return decision if the Tvalue is less than the lower-bound of the critical value at the 5 percent significance level the decision is "Do not reject null hypothesis". If the Tvalue is greater than the upper-bound of the critical value at the 5 percent significance level the decision is "Reject null hypothesis". Lower and upper-bounds critical values are obtained from Kodde and Palm (1986). If Tvalue falls into an inconclusive region (between the lower- and upper-bounds) the p-value will be estimated following Wolak (1989).
#' 
#' 
#' @references C. M. Beach and R. Davidson (1983) Distribution-free statistical inference with Lorenz curves
#' @references D.A. Kodde and F.C. Palm (1986) Wald criteria for jointly testing equality and inequality restrictions, Econometrica, 50, 1243--1248.
#' @references F.A. Wolak (1989), Testing inequality constrains in linear econometric models, Journal of Econometrics, 41, 205--235.
#' @references K. Xu (1997) Asymptotically distribution-free statistical test for generalized Lorenz curves: An alternative approach, Journal of Income Distribution, 7(1), 45--62.
#' 
#' @examples 
#' data(eusilc2)
#' ATdataset1 <- setupDataset(eusilc2, country = "AT", region = "Burgenland")
#' ATdataset2 <- setupDataset(eusilc2, country = "AT", region = "Carinthia")
#' testGL(ATdataset1, ATdataset2, generalized = TRUE, samplesize = 10)
#' 
#' @seealso OmegaGL, setupDataset
#' 
#' @export  


testGL <- function(dataset1, dataset2, generalized = FALSE, samplesize = 10){
  
  list1 <- OmegaGL(dataset1, samp = samplesize)
  list2 <- OmegaGL(dataset2, samp = samplesize)
  
  if(generalized == FALSE){
    gl1 <- list1$gl.curve/miuc(dataset1)
    gl2 <- list2$gl.curve/miuc(dataset2)
  }else{
    gl1 <- list1$gl.curve
    gl2 <- list2$gl.curve
  }
  
  estim.gl <- gl1 - gl2
  Omega1 <- list1$Omega
  Omega2 <- list2$Omega
  OmegaTotal <-  Omega1 + Omega2
  chol.OmegaTotal <- chol(OmegaTotal)
  M <- chol2inv(chol.OmegaTotal)
  
  
  fr <- function(x){
    (estim.gl - x) %*% M %*% (estim.gl-x)
  }
  
  gr <- function(x){
    -2*M %*% (estim.gl - x)
  }
  
  res <- constrOptim(rep(0.5, length(estim.gl)), fr, gr,
                     ui = diag(1,length(estim.gl)), 
                     ci = rep(0, length = length(estim.gl)))  
  
  #phi.tilde <- sol$solution
  #Tvalue <- t(as.matrix(estim.phi-phi.tilde)) %*% M %*% t(t(as.matrix(estim.phi-phi.tilde)))
  gl.tilde <- res$par
  Tvalue <- res$value
  
  # Upper and Lower bounds for the critical value for jointly testing equality and inequality restrictions (David & Palm). alpha = 0.05, K = 1 to 17
  bounds4critical.values <- c(2.706, 5.138, 7.045, 8.761, 10.371, 
                              11.911, 13.401, 14.853, 16.274, 17.670, 
                              19.045, 20.410, 21.742, 23.069, 24.384, 
                              25.689, 26.983, 28.268, 29.545, 30.814,
                              32.077, 33.333, 34.583, 35.827, 37.066,
                              38.301, 39.531, 40.756, 41.977, 43.194,
                              44.408, 45.618, 46.825, 48.029, 49.229)
  
  if(Tvalue < bounds4critical.values[1]){
    p.value <- NA
    return(list(Tvalue = Tvalue,
                p.value = p.value,
                decision = "Do not reject null hypothesis"))
  }else if(Tvalue > bounds4critical.values[10]){
    p.value <- NA
    return(list(Tvalue = Tvalue,
                p.value = p.value, 
                decision = "Reject null hypothesis"))
  }else{
    print("Inconclusive region ... calculating p-value (10000 simulations)")
    vec.solved <- matrix(NA, 1000, length(estim.gl))
    i <- 1
    iterations <- 1
    while(i < 1001){
      estim.gl <- as.numeric(rmvnorm(n=1, sigma=OmegaTotal))
      
      res <- try(constrOptim(rep(0.5, length(estim.gl)), fr, gr,
                             ui = diag(1, length(estim.gl)), 
                             ci = rep(0, length = length(estim.gl)))$par, silent = TRUE)
      
      if(is.numeric(res)){
        vec.solved[i,] <- res
        i <- i + 1
      }
      iterations <- iterations + 1
      stopifnot(iterations < 3000)
    }
    
    diff.gl <- vec.solved
  
    count.pos <- function(diff.gl.vec){
      positv <- length(which(diff.gl.vec > 1e-15))
      return(positv)
      }
    
    n.positiv <- aaply(diff.gl,.margins=1, count.pos)
    props.positive <- table(n.positiv)/length(n.positiv)
    prob.chi <- rev(pchisq(Tvalue, df=0:length(estim.gl), lower.tail = FALSE))
    
    pos.weights <- as.numeric(names(props.positive)) + 1
    
    p.value <- sum(props.positive*prob.chi[pos.weights])
    
    return(list(Tvalue = Tvalue,
                p.value = p.value, 
                decision = NA))
  }
}