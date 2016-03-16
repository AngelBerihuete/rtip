#' Filter datasets from EUSILC survey
#' 
#' \code{filterEUSILC} uses some packages
#' to do the
#' @param year is the year when the survey was done.
#' @param varD are a vector containing the DB variables to filter
#' @title ToDo
#' @name filterEUSILC
#' @aliases filtereusilc
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is an auxiliary function to obtain specific variables from EUSILC datasets files.
#' @details Todo
#' @return a data frame containing the variables required
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
  
  # Fichero D: 
  # DB010 Año de la encuesta (4 digits) 
  # DB020 País (=BE, CY, DE, EL/GR, ES, FR, IE, IT, NL, PT)
  # DB030 Identificación del hogar (max. 6 digits)
  # DB040 Región (=ES61 Andalucía)
  # DB090 Factor transversal del hogar (at least one integer and 5 decimals)
  
  sub.dataset2 <- subset(dataset2, select = varH)
  
  
  # Fichero H:  
  # HB010 Año de la encuesta (4 digits)
  # HB020 País (=BE, CY, DE, EL/GR, ES, FR, IE, IT, NL, PT)
  # HB030 Identificación del hogar (max 6 digits)
  # HY020 Renta disponible total del hogar (antes usábamos HY020)
  # Observación: La variable de renta a utilizar es la HY020. Un análisis en profundidad en ambos años, 2006 y 2010, arroja que dicha variable en ambos casos suma los mismos componentes de ingresos. En ambos casos sin factor de inflación. En euros. 
  # HY025 Factor de inflación por falta de respuesta dentro del hogar (EUROSTAT recomienda que no se utilice.  A partir de 2006 lo utilizan DE, GR/EL, PT. El resto de países emplean la imputación. Si no la usamos ni imputamos, igual hay que hacer algún comentario).
  # HX010 (nueva) factor de conversión: euro*HX010=national currency (por si acaso)
  # HX040 Número de miembros del hogar  (max. 2 digits)
  # HX050 Unidades de consumo. Escala OECD modificada (antes era HX040)
  # HX080 (nueva) Poverty indicator (puede ser interesante, asigna 1 al individuo pobre, 0 si no es pobre, en base al 60%Me(HX090))
  # HX090 (nueva) Renta disponible equivalente
  # Observación: EUROSTAT obtiene si un individuo es o no pobre en base a la variable HX090 y emplea el factor de inflación HY025
  # En 2006 HX090=( HY020* HY025)/HX050
  # Teniendo en cuenta que PY080G recoge pensiones recibidas de planes de pensiones privados individuales
  # En 2010 HX090=[HY020+SUM(PY080G)]* HY025/HX050
  # Tomo nota de esta variable  (por curiosidad, puede ser comparada con la que empleemos sin factor de inflación)
  
  # selecting same ID homes 
  
  check1 <- identical(sub.dataset1$DB010, sub.dataset2$HB010) # check if you've the same identification for homes
  check2 <- identical(sub.dataset1$DB030, sub.dataset2$HB030) # check if you've the same identification for homes
  if(!check1){
    stop('Different years!')
  }else if (!check2){
    stop('You do not have the same identification for homes')
  }else{
    dataset <- cbind(sub.dataset1, sub.dataset2) # ELIMINAR LAS VARIABLES REPETIDAS!!!
  }
  return(dataset)
}