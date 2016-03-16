#' @title mip
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is mean income person function
#' @details Todo
#' @export
mip <- function(dataset, ci = FALSE, rep = 1000, verbose = FALSE){
  dataset <- dataset[order(dataset[,"ipuc"]),]
  if(ci == FALSE){
    dataset$acum.wHX040 <- cumsum(dataset$wHX040)
    number.homes <- length(dataset$acum.wHX040)
    number.individuals <- dataset$acum.wHX040[number.homes]
    mip <- sum(dataset$HX090*dataset$HX050*dataset$DB090)/number.individuals
    return(mip)
  }else{
    mip2 <- function(dataset, i){
      dataset.boot <- dataset[i,]
      dataset.boot$acum.wHX040 <- cumsum(dataset.boot$wHX040)
      number.homes <- length(dataset.boot$acum.wHX040)
      number.individuals <- dataset.boot$acum.wHX040[number.homes]
      sum(dataset.boot$HX090*dataset.boot$HX050*dataset.boot$DB090)/number.individuals
    }
    boot.mip <- boot(dataset, statistic = mip2, R = rep,
                     sim = "ordinary", stype = "i")
    mip.ci <- boot.ci(boot.mip, type = "basic")
    if(verbose == FALSE){
      return(mip.ci)
    }else{
      plot(boot.mip)
      summary(mip.ci)
      return(mip.ci)
    }
  } 
}