#' @title mih
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is mean income household function
#' @details Todo
#' @export
mih <- function(dataset, ci = FALSE, rep = 1000, verbose = FALSE){
  if(ci == FALSE){
    mih <- sum(dataset$HX090*dataset$HX050*dataset$DB090)/sum(dataset$DB090)
    return(mih)
  }else{
    mih2 <- function(dataset, i){
      dataset.boot <- dataset[i,]
      sum(dataset.boot$HX090*dataset.boot$HX050*dataset.boot$DB090)/sum(dataset.boot$DB090)
    }
    boot.mih <- boot(dataset, statistic = mih2, R = rep,
                     sim = "ordinary", stype = "i")
    mih.ci <- boot.ci(boot.mih, type = "basic")
    if(verbose == FALSE){
      return(mih.ci)
    }else{
      plot(boot.mih)
      summary(mih.ci)
      return(mih.ci)
    }
  } 
}