library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
rm(list = ls())
dirWD <- "C://DatosBGA//ScoreAdmisionTarjetas//06-ScoreInfocred"
setwd(dirWD)

####	1. Se recuperan los datos de personas de los datos ASFI ####
load("datosPersonaAsfi.RData")


#### 2. Se cargan los datos de admisiones etiquetados ####
datosTarjetas <- read.csv( file = "C://DatosBGA//ScoreAdmisionTarjetas//02-DatosAdmision//tarjetaDatosEtiquetadosConSaldos36meses90dias.csv", sep = "|", dec = ".", stringsAsFactors = FALSE)
# str(datosTarjetas)
# nrow(datosTarjetas)

datosCIclientesTarjetas <- datosTarjetas %>% 
  # filter( CODIGO_CLIENTE %in% datosPersonaAsfi$CODIGO_CLIENTE | OPERACION %in% datosPersonaAsfi$NRO_OP ) %>% 
  select( OPERACION, CODIGO_CLIENTE, NRO_PERSONA, FECHA_INICIO ) %>% distinct()

datosCIclientesTarjetas <- datosCIclientesTarjetas %>% 
  left_join( datosPersonaAsfi %>% mutate(OPERACION = NRO_OP) %>% group_by(OPERACION) %>% summarise( IDENTIFICACION = min(IDENTIFICACION), NOMBRE = min(NOMBRE) ) #, FECHA_OP = min(FECHA_OP)
    , by = "OPERACION")

datosCIclientesTarjetas <- datosCIclientesTarjetas %>% 
  left_join( datosPersonaAsfi %>% group_by(CODIGO_CLIENTE) %>% summarise( IDENTIFICACION = min(IDENTIFICACION), NOMBRE = min(NOMBRE) ) #, FECHA_OP = min(FECHA_OP)
    , by = "CODIGO_CLIENTE") %>% 
  mutate( IDENTIFICACION = ifelse( !is.na(IDENTIFICACION.x), IDENTIFICACION.x, IDENTIFICACION.y ),
          NOMBRE = ifelse( !is.na(NOMBRE.x), NOMBRE.x, NOMBRE.y ) #, FECHA_OP = coalesce(FECHA_OP.x,FECHA_OP.y)
        ) %>% 
  select(OPERACION, CODIGO_CLIENTE, NRO_PERSONA, FECHA_INICIO, IDENTIFICACION, NOMBRE)
datosCIclientesTarjetas <- datosCIclientesTarjetas %>% 
  mutate(
    DOC_NRO = case_when(grepl("LP",IDENTIFICACION) ~ str_replace(IDENTIFICACION,"LP",""), grepl("SC",IDENTIFICACION) ~ str_replace(IDENTIFICACION,"SC",""), grepl("CB",IDENTIFICACION) ~ str_replace(IDENTIFICACION,"CB",""), grepl("BE",IDENTIFICACION) ~ str_replace(IDENTIFICACION,"BE",""), grepl("TJ",IDENTIFICACION) ~ str_replace(IDENTIFICACION,"TJ",""), grepl("CH",IDENTIFICACION) ~ str_replace(IDENTIFICACION,"CH",""), grepl("OR",IDENTIFICACION) ~ str_replace(IDENTIFICACION,"OR",""), grepl("PA",IDENTIFICACION) ~ str_replace(IDENTIFICACION,"PA",""), grepl("PO",IDENTIFICACION) ~ str_replace(IDENTIFICACION,"PO",""), grepl("PE",IDENTIFICACION) ~ str_replace(IDENTIFICACION,"PE",""), TRUE ~ IDENTIFICACION ),
    DOC_EXT = case_when(grepl("LP",IDENTIFICACION) ~ "LP", grepl("SC",IDENTIFICACION) ~ "SC", grepl("CB",IDENTIFICACION) ~ "CB", grepl("BE",IDENTIFICACION) ~ "BE", grepl("TJ",IDENTIFICACION) ~ "TJ", grepl("CH",IDENTIFICACION) ~ "CH", grepl("OR",IDENTIFICACION) ~ "OR", grepl("PA",IDENTIFICACION) ~ "PA", grepl("PO",IDENTIFICACION) ~ "PO", grepl("PE",IDENTIFICACION) ~ "PE", TRUE ~ IDENTIFICACION )
  )
for(i in 1:nrow(datosCIclientesTarjetas)) {
  if( grepl("-", datosCIclientesTarjetas[i,c("IDENTIFICACION")] ) ) {
    datosCIclientesTarjetas[i,c("DOC_NRO")] <- str_split( datosCIclientesTarjetas[i,c("IDENTIFICACION")] , "-")[[1]][[1]]
    datosCIclientesTarjetas[i,c("DOC_EXT")] <- paste0("-", str_split( datosCIclientesTarjetas[i,c("IDENTIFICACION")] , "-")[[1]][[2]] )
  }
}
# datosCIclientesTarjetas

# Se guardan los datos preparados para la búsqueda
write.table(datosCIclientesTarjetas, file = "datosCIclientesTarjetas.csv", sep = "|", dec = ".", na = "", quote = FALSE, row.names = FALSE)


####	3. Se recuperan los datos del Score de Infocred ####
datosInfocred <- read.csv( file = "datosCIclientesInfocred.csv", sep = ";", dec = ".", stringsAsFactors = FALSE)
funDocBuro <- function( cadena ) {
  iniChar <- gregexpr("[[:alpha:]]+", cadena)[[1]][1]
  finChar <- attr(gregexpr("[[:alpha:]]+", cadena)[[1]],"match.length")[1]
  if( iniChar > -1) {
    if( grepl("BGA", cadena ) ) {
      finDocBuro <- c( str_replace_all( cadena, "BGA", "" ) , "BGA" )
    } else if( grepl("LP", cadena ) ) {
      finDocBuro <- c( str_replace_all( cadena, "LP", "" ) , "LP" )
    } else if( grepl("SC", cadena ) ) {
      finDocBuro <- c( str_replace_all( cadena, "SC", "" ) , "SC" )
    } else {
      iniNum <- gregexpr("[[:digit:]]+", cadena)[[1]][1]
      finNum <- attr(gregexpr("[[:digit:]]+", cadena)[[1]],"match.length")[1]
      finDocBuro <- c( substr(cadena, iniNum, iniNum+finNum-2) , substr(cadena, iniChar-1, iniChar+finChar) )
    }
  } else {
    finDocBuro <- c(cadena,"")
  }
}
for (i in 1:nrow(datosInfocred)) {
  res <- funDocBuro(datosInfocred[i,c("DocumentoIdentidad")])
  datosInfocred[i,c("DocumentoIdentidad")] <- res[1]
  datosInfocred[i,c("Extension")] <- paste0(res[2],datosInfocred[i,c("Extension")]) 
}

cruceDatosTarjetasInfocred <- datosCIclientesTarjetas %>% left_join(
  datosInfocred %>% mutate( DOC_NRO = DocumentoIdentidad, icExt = Extension, icNombre = NombreCompletoTitular, icScore = Score, icPred = Pred, icProb = Prob ) %>% select(DOC_NRO, icExt, icNombre, icScore, icPred, icProb ),
  by = "DOC_NRO"
) %>% filter( !is.na(icScore) ) %>% select(OPERACION, icScore, icPred, icProb)

# Se retiran coincidencias múltiples
criterioDuplicados <- cruceDatosTarjetasInfocred %>% group_by( OPERACION ) %>% summarise( numReg = n(), scoreMenor = min(icScore) ) %>% filter( numReg > 1 )
indexDrop <- c()
for( j in 1:nrow(criterioDuplicados) ) {
  for ( i in 1:nrow(cruceDatosTarjetasInfocred) ) {
    dupOperacion <- criterioDuplicados[j,c("OPERACION")]
    dupScoreMenor <- criterioDuplicados[j,c("scoreMenor")]
    regOperacion <- cruceDatosTarjetasInfocred[i,c("OPERACION")]
    regIcScore <- cruceDatosTarjetasInfocred[i,c("icScore")]
    if( regOperacion == dupOperacion ) {
      if ( regIcScore != dupScoreMenor ) {
        indexDrop <- c(indexDrop,i)
      }
    }
  }
}
cruceDatosTarjetasInfocred <- cruceDatosTarjetasInfocred[-indexDrop,]

# Se guardan los datos encontrados, junto con el Score asignado por Infocred
write.table( cruceDatosTarjetasInfocred, file("relacionTarjetasInfocred.csv"), sep = ";", quote = FALSE, row.names = FALSE)


