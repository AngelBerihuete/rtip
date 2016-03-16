#' @title Living conditions survey 
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description Function to load the LCS dataset from INE
#' @details Todo
#' @export

loadLCS <- function(lcs_d_file, lcs_h_file){
  
  
  dataset1 <- read.table(lcs_d_file, header=TRUE, sep= ",")
  dataset2 <- read.table(lcs_h_file, header=TRUE, sep= ",")
  
  sub.dataset1 <- subset(dataset1, select = c("DB010", "DB020", "DB030",
                                              "DB040", "DB090"))
  sub.dataset2 <- subset(dataset2, select = c("HB010", "HB020", "HB030",
                                              "HY020", "HX040", "HX240",
                                              "vhRentaa"))
  sub.dataset2$HX050 <- sub.dataset2$HX240
  sub.dataset2$HX090 <- sub.dataset2$vhRentaa/sub.dataset2$HX240

  check1 <- identical(sub.dataset1$DB010, sub.dataset2$HB010) # check if you've the same identification for homes
  check2 <- identical(sub.dataset1$DB030, sub.dataset2$HB030) # check if you've the same identification for homes
  if(!check1){
    stop('Different years!')
  }else if (!check2){
    stop('You do not have the same identification for homes')
  }else{
    dataset <- cbind(sub.dataset1, sub.dataset2) # ELIMINAR LAS VARIABLES REPETIDAS!!
    dataset <- subset(dataset, select = c("DB010", "DB020", "DB030","DB040",
                                          "DB090", "HB020", "HB030",
                                          "HY020", "HX040", "HX050",
                                          "HX090"))
  } 
}