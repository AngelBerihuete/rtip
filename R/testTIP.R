#' @title Test for TIP dominance
#'
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#'
#' @description Statistical test procedure given by Xu and Osberg (1998) to study TIP dominance from sample
#' TIP curve estimates.
#'
#' @param dataset1 a data.frame containing the variables.
#' @param dataset2 a data.frame containing the variables.
#' @param ipuc a character string indicating the variable name of the income per unit of consumption. Default is "ipuc".
#' @param hhcsw a character string indicating the variable name of the household cross-sectional weight. Default is "DB090".
#' @param hhsize a character string indicating the variable name of the household size. Default is "HX040".
#' @param pz a number between 0 and 1 which represents the percentage to be used to calculate the at-risk-of-poverty threshold. The default is 0.6.
#' @param same.arpt.value a number that will be used as a common poverty threshold. If NULL, poverty thresholds will be calculated from each datasets (see arpt).
#' @param norm logical; if  TRUE, the normalised TIP curve ordinates are computed using the normalised poverty gaps (poverty gaps divided by the poverty threshold).
#' @param samplesize an integer which represents the number of TIP curve ordinates to be estimated. The default is 50.
#' @param alpha a scalar indicating the significance level. Default is 0.05.
#'
#' @details Because the TIP curve becomes horizontal at the arpr value, it is only necessary to have the test implemented over the interval \eqn{(0, \max \{ arpr1, arpr2 \})}{(0, max {arpr1, arpr2})}. For that reason both TIP curves are truncated at the same value equal to \eqn{\max \{ arpr1, arpr2 \} }{max{arpr1, arpr2}} and ordinates are only compared at points \eqn{p_i = i/samplesize}{p_i = i/samplesize}, where \eqn{i=1, \dots, k} in the interval \eqn{(0, \max \{ arpr1, arpr2 \})}{(0, max { arpr1, arpr2})} (see \code{arpr} function).
#'
#' The null hypothesis to be tested is that the TIP curve calculated from dataset1 dominates the one calculated from dataset2.
#'
#' @return A list with the following components:
#' \itemize{
#' \item Tvalue, the value of the test-statistic.
#' \item p.value, simulated p-value of the test-statistic Tvalue (Wolak, 1989). It is calculated only when the Tvalue falls into an inconclusive region.
#' \item decision, if the Tvalue is less than the lower-bound of the critical value at the \eqn{alpha} significance level the decision is "Do not reject null hypothesis".
#' If the Tvalue is greater than the upper-bound of the critical value at the \eqn{alpha} significance level the decision is "Reject null hypothesis". Lower and upper-bounds
#' critical values are obtained from Kodde and Palm (1986). If Tvalue falls into an inconclusive region (between the lower- and upper-bounds) the p-value will
#' be estimated following Wolak (1989).
#' }
#'
#' @seealso OmegaTIP, setupDataset, arpt, arpr
#' @references C. M. Beach and S. F. Kaliski (1986) Curve inference with sample weights: and application to the distribution of unemployment experience, Journal of the Royal Statistical Society. Series C (Applied Statistics), Vol. 35, No. 1, 38--45.
#' @references D.A. Kodde and F.C. Palm (1986) Wald criteria for jointly testing equality and inequality restrictions, Econometrica, 50, 1243--1248.
#' @references F.A. Wolak (1989), Testing inequality constrains in linear econometric models, Journal of Econometrics, 41, 205--235.
#' @references K. Xu and L. Osberg (1998) A distribution-free test for deprivation dominance, Econometric Reviews, 17, 415--429.
#'
#' @examples
#' data(eusilc2)
#' ATdataset <- setupDataset(eusilc2, country = "AT")
#' ATdataset1 <- setupDataset(eusilc2, country = "AT", region = "Burgenland")
#' ATdataset2 <- setupDataset(eusilc2, country = "AT", region = "Carinthia")
#' testTIP(ATdataset1, ATdataset2, same.arpt.value = arpt(ATdataset), samplesize = 50, alpha = 0.05)
#'
#' @import plyr
#' @import mvtnorm
#' @import rootSolve
#' @export

testTIP <- function(dataset1, dataset2,
                    ipuc = "ipuc", # The income per unit of consumption
                    hhcsw = "DB090", # Household cross-sectional weight
                    hhsize = "HX040", # Household size
                    pz = 0.6,
                    same.arpt.value = NULL,
                    norm = FALSE, samplesize = 50, alpha = 0.05){

  if(is.null(same.arpt.value)){

    arpt.value1 <- arpt(dataset1,
                        ipuc = ipuc, # The income per unit of consumption
                        hhcsw = hhcsw, # Household cross-sectional weight
                        hhsize = hhsize, # Household size
                        pz = pz)

    arpt.value2 <- arpt(dataset2,
                        ipuc = ipuc, # The income per unit of consumption
                        hhcsw = hhcsw, # Household cross-sectional weight
                        hhsize = hhsize, # Household size
                        pz = pz)
  }else{
    arpt.value1 <- arpt.value2 <- same.arpt.value
  }

  list1 <- OmegaTIP(dataset1,
                    ipuc = ipuc, # The income per unit of consumption
                    hhcsw = hhcsw, # Household cross-sectional weight
                    hhsize = hhsize, # Household size
                    arpt.value = arpt.value1,
                    samplesize = samplesize, norm = norm)

  list2 <- OmegaTIP(dataset2,
                    ipuc = ipuc, # The income per unit of consumption
                    hhcsw = hhcsw, # Household cross-sectional weight
                    hhsize = hhsize, # Household size
                    arpt.value = arpt.value2,
                    samplesize = samplesize, norm = norm)

  phi1 <- list1$tip.curve
  phi2 <- list2$tip.curve

  phi1 <- phi1[-1]
  phi2 <- phi2[-1]

  threshold <- max(which.max(phi1), which.max(phi2))
  phi1 <- phi1[1:threshold]
  phi2 <- phi2[1:threshold]

  estim.phi <- phi1 - phi2
  Omega1 <- list1$Omega[1:threshold, 1:threshold]
  Omega2 <- list2$Omega[1:threshold, 1:threshold]
  OmegaTotal <-  Omega1 + Omega2
  chol.OmegaTotal <- chol(OmegaTotal)
  #M <- solve(OmegaTotal)
  M <- chol2inv(chol.OmegaTotal)

#   Dmat <- M
#   dvec <- as.numeric(M %*% estim.phi)
#   Amat  <- diag(length(phi1))
#   bvec <- rep(0,length(phi1))
#
#  sol <- solve.QP(Dmat,dvec,Amat,bvec=bvec) #

  fr <- function(x){
    (estim.phi - x) %*% M %*% (estim.phi-x)
  }

  gr <- function(x){
    -2*M %*% (estim.phi - x)
  }

  res <- constrOptim(rep(0.5, threshold), fr, gr,
                     ui = diag(1, threshold),
                     ci = rep(0, length = threshold))

  #phi.tilde <- sol$solution
  #t.value <- t(as.matrix(estim.phi-phi.tilde)) %*% M %*% t(t(as.matrix(estim.phi-phi.tilde)))
  phi.tilde <- res$par
  Tvalue <- res$value

  # Upper and Lower bounds for the critical value for jointly testing equality and inequality restrictions (David & Palm). alpha = 0.05, K = 1 to 17
  # bounds4critical.values <- c(2.706, 5.138, 7.045, 8.761, 10.371,
  #                             11.911, 13.401, 14.853, 16.274, 17.670,
  #                             19.045, 20.410, 21.742, 23.069, 24.384,
  #                             25.689, 26.983, 28.268, 29.545, 30.814,
  #                             32.077, 33.333, 34.583, 35.827, 37.066,
  #                             38.301, 39.531, 40.756, 41.977, 43.194,
  #                             44.408, 45.618, 46.825, 48.029, 49.229)

    # --- Thanks to the suggestions made by the referees in Rjournal ---
  f <- function(x, alpha, K) 1 - 0.5 * pchisq(x, K)-0.5 * pchisq(x, K-1)-alpha
  valInf <- rootSolve::uniroot.all(f, c(0, 10 * threshold), alpha = alpha, K = 1)
  valSup <- rootSolve::uniroot.all(f, c(0, 10 * threshold), alpha = alpha, K = threshold)
  # ---

  if(Tvalue < valInf){
    p.value <- NA
    return(list(Tvalue = Tvalue,
                p.value = p.value,
                decision = "Do not reject null hypothesis" ))
  }else if(Tvalue > valSup){
    p.value <- NA
    return(list(Tvalue = Tvalue,
                p.value = p.value,
                decision = "Reject null hypothesis"))
  }else{
    print("Inconclusive region... calculating p-value (10000 simulations)")
    vec.solved <- matrix(NA, 1000, threshold)
    i <- 1
    iterations <- 1
    while(i < 1001){
      estim.phi <- as.numeric(mvtnorm::rmvnorm(n=1, sigma=OmegaTotal))

      res <- try(constrOptim(rep(0.5, threshold), fr, gr,
                  ui = diag(1, threshold),
                  ci = rep(0, length = threshold))$par, silent = TRUE)

      if(is.numeric(res)){
       vec.solved[i,] <- res
       i <- i + 1
      }
      iterations <- iterations + 1
      stopifnot(iterations < 3000)
    }

    diff.phi <- vec.solved
    #diff.phi <- data.sim-vec.solved

    count.pos <- function(diff.phi.vec){
      positv <- length(which(diff.phi.vec > 1e-15))
      return(positv)
    }

    n.positiv <- plyr::aaply(diff.phi,.margins=1, count.pos)
    props.positive <- table(n.positiv)/length(n.positiv)
    prob.chi <- rev(pchisq(Tvalue, df=0:threshold, lower.tail = FALSE))

    pos.weights <- as.numeric(names(props.positive)) + 1

    p.value <- sum(props.positive*prob.chi[pos.weights])

    return(list(Tvalue = Tvalue,
                p.value = p.value,
                decision = NA))
  }
}
