#' @title Load the living conditions survey (INE)
#'
#' @description
#'
#' \code{loadLCS()} loads the living conditions survey from Spanish National
#' Statistics Institute (\acronym{INE} in Spanish).
#'
#' @param lcs_d_file, a string with the filename of \code{D}-file.
#' @param lcs_h_file, a string with the filename of \code{H}-file.
#'
#' @details
#' Regularly the INE releases the living conditions survey through two different
#' files which can be downloaded for free. The filename of these files contains
#' the letters \code{D} and \code{H}, and these files include dozens of variables. Only some of these
#' variables are needed to do the calculations with \code{rtip} package.
#'
#'
#' @return A data frame containing the variables required.
#'
#' @note We have included two files in dat-raw to test the function
#' (source version package only).
#' @examples
#' \dontrun{lcs2014 <- loadLCS("esudb14d.csv","esudb14h.csv")}
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @importFrom utils read.table
#' @export

loadLCS <- function(lcs_d_file, lcs_h_file){

  dataset1 <- read.table(lcs_d_file, header=TRUE, sep= ",")
  dataset2 <- read.table(lcs_h_file, header=TRUE, sep= ",")

  # selecting same ID homes
  check1 <- identical(dataset1$DB010, dataset2$HB010) # check if you've the same identification for years
  check2 <- identical(dataset1$DB030, dataset2$HB030) # check if you've the same identification for homes
  if(!check1){
    stop('Different years!')
  }else if (!check2){
    stop('You do not have the same identification for homes')
  }else{
    sub.dataset1 <- subset(dataset1, select = c("DB010", "DB020", "DB030",
                                                "DB040", "DB090"))
    sub.dataset2 <- subset(dataset2, select = c("HB010", "HB030",
                                                "HX040", "HX240","vhRentaa"))
    sub.dataset2$HX050 <- sub.dataset2$HX240
    sub.dataset2$HX090 <- sub.dataset2$vhRentaa/sub.dataset2$HX240

    dataset <- cbind(sub.dataset1, sub.dataset2)
    dataset <- subset(dataset, select = c("DB010", "DB020", "DB040", "DB090",
                                          "HX040", "HX050", "HX090"))
  }
}
