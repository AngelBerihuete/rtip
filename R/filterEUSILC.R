#' @title Filter datasets from EUSILC survey datasets
#'
#' @description
#'
#' \code{filterEUSILC()} extracts and transforms some variables in the EUSILC
#' survey files in a suitable data frame in order to do the calculations. In
#' general, you will receive two files from Eurostat for each year you have
#' requested the EUSILC survey (usually the filenames have letters D and H to
#' distingish them). For each file there will be dozens of variables containing
#' the dataset, but you will not need all of them to the calculations with
#' rtip package.
#'
#' @note We do not give examples in this function because the EUSILC survey
#' datasets have a restricted license.
#'
#' @param year Default year of the survey.
#' @param varH Default string vector containing the variables to filter in the
#' file D.
#' @param varH Default string vector containing the variables to filter in the
#' file H.
#'
#' @return A data frame containing the variables required.
#'
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @export

filterEUSILC <- function(year = "2006",
                         varD = c("DB010", "DB020", "DB030", "DB040", "DB090"),
                         varH = c("HB010", "HB020", "HB030",
                                  "HY020", "HY025", "HX010",
                                  "HX040", "HX050", "HX080", "HX090")){
  # Function to load the dataset from CD's
  main.directory <- getwd()

  files <- dir(pattern = year)
  dataset1 <- read.table(file=files[1], header=TRUE, sep= ",")
  dataset2 <- read.table(file=files[2], header=TRUE, sep= ",")

  sub.dataset1 <- subset(dataset1, select = varD)
  sub.dataset2 <- subset(dataset2, select = varH)

  # selecting same ID homes
  check1 <- identical(sub.dataset1$DB010, sub.dataset2$HB010) # check if you've the same identification for homes
  check2 <- identical(sub.dataset1$DB030, sub.dataset2$HB030) # check if you've the same identification for homes
  if(!check1){
    stop('Different years!')
  }else if (!check2){
    stop('You do not have the same identification for homes')
  }else{
    dataset <- cbind(sub.dataset1, sub.dataset2)
  }
  return(dataset)
}
