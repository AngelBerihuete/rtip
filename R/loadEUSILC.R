#' @title Load the living conditions survey (EUSILC)
#'
#' @description
#'
#' \code{loadEUSILC()} extracts some variables from the EUSILC
#' survey files and transforms them into a suitable data frame in order to do
#' the calculations.
#'
#' @param eusilc_d_file a string with the filename of \code{D}-file.
#' @param eusilc_h_file a string with the filename of \code{H}-file.
#'
#' @details Vector strings \code{varD} and \code{varH} contains the names of the
#' variables needed to do the calculations with rtip package. These variables are given
#' by Eurostat in two different files, namely basic household register
#' (\code{H}-file) and household data (\code{D}-file).
#'
#' @note We do not give examples in this function because the EUSILC survey
#' datasets have a restricted licence for use.
#'
#' @return A data frame containing the variables required to use the functions
#' in the package.
#'
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @export

loadEUSILC <- function(eusilc_d_file, eusilc_h_file){

  dataset1 <- read.table(eusilc_d_file, header=TRUE, sep= ",")
  dataset2 <- read.table(eusilc_h_file, header=TRUE, sep= ",")

  # selecting same ID homes
  check1 <- identical(dataset1$DB010, dataset2$HB010) # check if you've the same identification for homes
  check2 <- identical(dataset1$DB030, dataset2$HB030) # check if you've the same identification for homes
  if(!check1){
    stop('Different years!')
  }else if (!check2){
    stop('You do not have the same identification for homes')
  }else{

    varD = c("DB010", "DB020", "DB040", "DB090")
    varH = c("HX040", "HX050", "HX090")

    sub.dataset1 <- subset(dataset1, select = varD)
    sub.dataset2 <- subset(dataset2, select = varH)
    dataset <- cbind(sub.dataset1, sub.dataset2)
  }
  return(dataset)
}
