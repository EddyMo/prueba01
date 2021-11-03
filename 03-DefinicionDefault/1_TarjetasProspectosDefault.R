library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
rm(list = ls())
dirWD <- "C://DatosBGA//ScoreAdmisionTarjetas//03-DefinicionDefault"
setwd(dirWD)

#### 1. Recuperación de Datos de Cartera ####
datosCartera1 <- data.frame()
archivosCartera1 <- c("Asfi510_11_2017.csv", "Asfi510_12_2017.csv", "Asfi510_01_2018.csv", "Asfi510_02_2018.csv", "Asfi510_03_2018.csv", "Asfi510_04_2018.csv", "Asfi510_05_2018.csv", "Asfi510_06_2018.csv", "Asfi510_07_2018.csv", "Asfi510_08_2018.csv", "Asfi510_09_2018.csv", "Asfi510_10_2018.csv", "Asfi510_11_2018.csv", "Asfi510_12_2018.csv", "Asfi510_01_2019.csv", "Asfi510_02_2019.csv", "Asfi510_03_2019.csv", "Asfi510_04_2019.csv", "Asfi510_05_2019.csv", "Asfi510_06_2019.csv", "Asfi510_07_2019.csv", "Asfi510_08_2019.csv", "Asfi510_09_2019.csv", "Asfi510_10_2019.csv", "Asfi510_11_2019.csv", "Asfi510_12_2019.csv", "Asfi510_01_2020.csv", "Asfi510_02_2020.csv", "Asfi510_03_2020.csv", "Asfi510_04_2020.csv")
for (nombreArchivo1 in archivosCartera1) {
  archivoCartera1 <- paste0("C://DatosBGA//ScoreAdmisionTarjetas//01-DatosBase//Asfi510-Nov17-Abr20//",nombreArchivo1)
  datosArchivoCartera1 <- read.csv( archivoCartera1, sep = '|', dec = ".", stringsAsFactors=FALSE )
  if( nrow(datosCartera1) == 0 ) {
    datosCartera1 <- datosArchivoCartera1
  } else {
    datosCartera1 <- rbind(datosCartera1, datosArchivoCartera1)
  }
}
datosCartera1 <- datosCartera1 %>% mutate(
  FECHA_PROCESO = as.Date(FECHA_PROCESO, format="%d/%m/%Y"),
  FECHAOP = as.Date(FECHAOP, format="%d/%m/%Y"),
  FECHA_PROX_PAGO_INT = as.Date(FECHA_PROX_PAGO_INT, format="%d/%m/%Y"),
  FECHA_VTO_FINAL = as.Date(FECHA_VTO_FINAL, format="%d/%m/%Y"),
  CODIGO_ASFI510 = NULL,
  FECHA_PROCESO_1 = NULL
)

datosCartera2 <- data.frame()
archivosCartera2 <- c("Asfi510_10_2015.csv", "Asfi510_11_2015.csv", "Asfi510_12_2015.csv", "Asfi510_01_2016.csv", "Asfi510_02_2016.csv", "Asfi510_03_2016.csv", "Asfi510_04_2016.csv", "Asfi510_05_2016.csv", "Asfi510_06_2016.csv", "Asfi510_07_2016.csv", "Asfi510_08_2016.csv", "Asfi510_09_2016.csv", "Asfi510_10_2016.csv", "Asfi510_11_2016.csv", "Asfi510_12_2016.csv", "Asfi510_01_2017.csv", "Asfi510_02_2017.csv", "Asfi510_03_2017.csv", "Asfi510_04_2017.csv", "Asfi510_05_2017.csv", "Asfi510_06_2017.csv", "Asfi510_07_2017.csv", "Asfi510_08_2017.csv", "Asfi510_09_2017.csv", "Asfi510_10_2017.csv")
for (nombreArchivo2 in archivosCartera2) {
  archivoCartera2 <- paste0("C://DatosBGA//ScoreAdmisionTarjetas//01-DatosBase//Asfi510-Oct15-Oct17//", nombreArchivo2)
  datosArchivoCartera2 <- read.csv( archivoCartera2, sep = '|', dec = ".", stringsAsFactors=FALSE )
  if( nrow(datosCartera2) == 0 ) {
    datosCartera2 <- datosArchivoCartera2
  } else {
    datosCartera2 <- rbind(datosCartera2, datosArchivoCartera2)
  }
}
datosCartera2 <- datosCartera2 %>% mutate(
  FECHA_PROCESO = as.Date(FECHA_PROCESO, format="%d/%m/%Y"),
  FECHAOP = as.Date(FECHAOP, format="%d/%m/%Y"),
  FECHA_PROX_PAGO_INT = as.Date(FECHA_PROX_PAGO_INT, format="%d/%m/%Y"),
  FECHA_VTO_FINAL = as.Date(FECHA_VTO_FINAL, format="%d/%m/%Y")
)

datosCartera <- rbind(datosCartera1, datosCartera2)

save(datosCartera, file = "datosCartera.RData")
load("datosCartera.RData")

#### 2. Equivalencia de meses ####
mesesNum <- c(Ene="01",Feb="02",Mar="03",Abr="04",May="05",Jun="06",Jul="07",Ago="08",Sep="09",Oct="10",Nov="11",Dic="12")

#### 3. Identificadores de operaciones de Tarjetas ####
# Para Tarjetas
archivoOpAdmision <- "C://DatosBGA//ScoreAdmisionTarjetas//02-DatosAdmision//OperacionesTarjeta.csv"
datosOpAdmision <- read.csv( archivoOpAdmision )
datosOpAdmision$OPERACION <- as.numeric(datosOpAdmision$OPERACION)

save(datosOpAdmision, file = "datosOpAdmision.RData")
load("datosOpAdmision.RData")

#### 4. Análisis de atributos a exportar de los datos ASFI ####
datosTarget <- datosCartera %>%
  filter( NRO_OP %in% datosOpAdmision$OPERACION ) %>%
  mutate( Fecha = FECHA_PROCESO,
          FechaNum = year(FECHA_PROCESO) * 100 + month(FECHA_PROCESO),
          FECHA_OP = FECHAOP,
          DIAS_MORA = DIASATRASOOMORA,
          ESTADO = ESTADO_CREDITO) %>%
  mutate( FECHAOP = NULL, DIASATRASOOMORA = NULL, ESTADO_CREDITO = NULL )

#### 5. Identificación de la secuencia de los meses reportados: (respecto a la secuencia general de meses) #### 
mesesSeqBase <- (datosTarget %>% select(FechaNum) %>% distinct() %>% arrange(FechaNum))$FechaNum
mesesSeq <- seq(1:length(mesesSeqBase)); names(mesesSeq) <- mesesSeqBase
datosTarget <- datosTarget %>% mutate( FechaSeqGeneral = mesesSeq[paste(FechaNum)], NroMesCicloOp = as.numeric(0) )
#tabla guía por operación
datosOperacion <- datosTarget %>%
  group_by(NRO_OP) %>%
  summarise( FechaNumMin = min(FechaNum), FechaNumMax = max(FechaNum), FechaSeqMin = min(FechaSeqGeneral), FechaSeqMax = max(FechaSeqGeneral), PeriodosNum = n(),
             maxMONEDA = max(MONEDA),
             maxTC_FECHA_OP = max(TC_FECHA_OP),
             maxNRO_CUOTAS = max(NRO_CUOTAS),
             maxFRECUENCIA_PAGO = max(FRECUENCIA_PAGO),
             maxMONTO_DES_ORI = max(MONTO_DES_ORI)
             ) %>%
  as.data.frame()

#### 6. Creación de vector con el número menor de la secuencia numérica de meses por operación ###
datosSeqMin0 <- datosOperacion %>% select(NRO_OP,FechaSeqMin)
datosSeqMin1 <- datosSeqMin0$FechaSeqMin
names(datosSeqMin1) <- datosSeqMin0$NRO_OP
#identificación de la secuencia particular de pagos, de acuerdo al mes, por operación
for (i in 1:nrow(datosTarget)) {
  current_NRO_OP <- datosTarget[i,c("NRO_OP")]
  current_FechaSeqGeneral_delPago <- datosTarget[i,c("FechaSeqGeneral")]
  # current_FechaSeqMin_deLaOperacion <- datosOperacion %>% filter(NRO_OP == current_NRO_OP) %>% select(FechaSeqMin) %>% as.numeric()
  current_FechaSeqMin_deLaOperacion <- as.numeric(datosSeqMin1[as.character(current_NRO_OP)])
  datosTarget[i,c("NroMesCicloOp")] <- current_FechaSeqGeneral_delPago - current_FechaSeqMin_deLaOperacion + 1
}

save(datosTarget, file = "datosTargetTarjeta.RData")
save(datosOperacion, file = "datosOperacionTarjeta.RData")
load("datosTargetTarjeta.RData")
load("datosOperacionTarjeta.RData")



#### 7. Iteraciones para probar los distintos prospectos de Default, con variantes de días mora y horizonte temporal en meses ####
# # la siguiente línea para iteraciones de días de mora
# for(numDiasMora in c(30,60,90)) { # seq(30, 90, by = 10))

numDiasMora <- 90 # 30 60 90

datosTarget <- datosTarget %>% 
  mutate( DIAS_MORA = replace_na(DIAS_MORA, 0) ) %>% 
  mutate( Default = ifelse( (ESTADO == "EJECUCION" | ESTADO == "CASTIGADO" | DIAS_MORA >= numDiasMora) , 1, 0))

# Porcentaje de defaults históricos por periodo
# periodoDefaultAcumulado <- data.frame()
# for( NroMesCiclo in 1:max(datosOperacion$FechaSeqMax) ) { #
#   # operaciones <- (datosTarget %>% group_by(NRO_OP) %>% summarise( numMesMax = max(NroMesCicloOp) ) %>% filter( numMesMax >= NroMesCiclo ))$NRO_OP
#   operaciones <- (datosOperacion %>% filter( FechaSeqMax >= NroMesCiclo ))$NRO_OP
#   # numOp <- length(operaciones)
#   numOp <- datosOperacion %>% nrow()
#   # numDefault <- datosTarget %>% filter( NRO_OP %in% operaciones ) %>% group_by( NRO_OP ) %>% summarise( numDefault = sum(Default) ) %>% nrow()
#   numDefaults <- datosTarget %>% filter( NRO_OP %in% operaciones, NroMesCicloOp <= NroMesCiclo ) %>% group_by( NRO_OP ) %>% summarise( numDefault = sum(Default) ) %>% filter( numDefault > 0 ) %>% nrow()
#   porcentajeDefault <- ifelse( numOp > 0, round((numDefaults / numOp) * 100, 2) , 0.00 )
#   res <- data.frame(NroMesCicloOp = NroMesCiclo, numOp = numOp, numDefault = numDefaults, porcentajeDefault = porcentajeDefault)
#   if ( nrow(periodoDefaultAcumulado) == 0 ) {
#     periodoDefaultAcumulado <- res
#   } else {
#     periodoDefaultAcumulado <- rbind(periodoDefaultAcumulado, res)
#   }
# }
# table(datosTarget$Default)
# periodoDefaultAcumulado
# ggplot(data=periodoDefaultAcumulado, aes(x=NroMesCicloOp, y=porcentajeDefault, group=1)) +
#   geom_line() + geom_point() +
#   scale_x_continuous(minor_breaks = seq(1, max(periodoDefaultAcumulado$NroMesCicloOp), by = 1), breaks = seq(1, max(periodoDefaultAcumulado$NroMesCicloOp), by = 1)) +
#   ylim(0, ceiling(max(periodoDefaultAcumulado$porcentajeDefault) * 1.25)) +
#   labs( title = paste0("Evolución del % de operaciones en Default (",numDiasMora," días mora)") ) +
#   theme( plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90)) +
#   labs(x = "Número de meses de evolución de las operaciones", y = "% de operaciones en Default")


# Parametros
listaTransicion <- list()
datosConteoOp <- data.frame()

#las siguientes 3 líneas para iteraciones de periodos de evaluación

# for (pTam in seq(12,48,by = 2)) { # 12:36 14:27
# periodoTam <- pTam
# print(paste("Dias mora =", numDiasMora, "; Meses =", periodoTam))

periodoTam <- 36


periodoNum <- 2 
estados <- as.character((datosTarget %>% select(Default) %>% distinct())$Default)

resEstadoOperaciones <- data.frame()
for( periodo in 1:periodoNum ) {
  PeriodoIni <- ((periodo - 1) * periodoTam) + 1
  PeriodoFin <- (periodo) * periodoTam
  
  for (filaOp in 1:nrow(datosOperacion) ) {
    numOp <- datosOperacion[filaOp,c("NRO_OP")]
    NumTotal <- datosTarget %>% filter( NRO_OP == numOp, NroMesCicloOp >= PeriodoIni , NroMesCicloOp <= PeriodoFin ) %>% nrow()
    if( NumTotal > 0 ) {
      NumDefault <- datosTarget %>% filter( NRO_OP == numOp, NroMesCicloOp >= PeriodoIni , NroMesCicloOp <= PeriodoFin, Default == 1 ) %>% nrow()
      peorEstado <- ifelse( NumDefault > 0, 1 , 0 )
      resOperacionPeriodo <- data.frame(periodo = periodo, numOp = numOp, estado = peorEstado )
      if ( nrow(resEstadoOperaciones) == 0 ) {
        resEstadoOperaciones <- resOperacionPeriodo
      } else {
        resEstadoOperaciones <- rbind(resEstadoOperaciones, resOperacionPeriodo)
      }
    }
  }
}
# resEstadoOperaciones

resCambioEstado <- data.frame()
for ( pIni in 1:(periodoNum - 1) ) {
  pFin <- pIni + 1
  for ( eIni in estados ) {
    conjunto1 <- (resEstadoOperaciones %>% filter( periodo == pIni, estado == eIni ) %>% select( numOp ))$numOp 
    for ( eFin in estados ) {
      conjunto2 <- (resEstadoOperaciones %>% filter( periodo == pFin, estado == eFin ) %>% select( numOp ))$numOp
      # numOperaciones <- inner_join( conjunto1, conjunto2, by = c("numOp"), copy = FALSE ) %>% nrow()
      numOperaciones <- length( conjunto1[conjunto1 %in% conjunto2] )
      
      resEstado <- data.frame( periodoIni = pIni, periodoFin = pFin, estadoIni = eIni, estadoFin = eFin, numCasos = numOperaciones, porCasos = 0 )
      if ( nrow(resCambioEstado) == 0 ) {
        resCambioEstado <- resEstado
      } else {
        resCambioEstado <- rbind(resCambioEstado, resEstado)
      }
    }
  }
}
# resCambioEstado

resCambioEstadoPorGen <- resCambioEstado %>% group_by(periodoIni, estadoIni) %>% summarise( numCasosEstado = sum(numCasos) ) %>% as.data.frame()
for ( i in 1:nrow(resCambioEstado) ) {
  numCasosEstado <- (resCambioEstadoPorGen %>% filter( periodoIni == resCambioEstado[i, c("periodoIni")], estadoIni == resCambioEstado[i, c("estadoIni")] ))$numCasosEstado
  if ( numCasosEstado > 0) {
    resCambioEstado[i,c("porCasos")] <- round( (resCambioEstado[i,c("numCasos")] / numCasosEstado)*100 , 2)
  }
}
resCambioEstado <- resCambioEstado %>% mutate( estadoIni = ifelse( estadoIni == "1" , "Default", "Vigente"), estadoFin = ifelse( estadoFin == "1" , "Default", "Vigente"))
estados <- c("Vigente", "Default")

for ( pIni in 1:(periodoNum-1) ) {
  # Definicion del nombre del resultado
  pIniMesIni <- ((pIni - 1) * periodoTam) + 1
  pIniMesFin <- ((pIni) * periodoTam)
  pIniMesNombre <- ifelse( periodoTam > 1 , paste0(pIniMesIni,"-",pIniMesFin), paste(pIniMesIni) )
  pFin <- pIni + 1
  pFinMesIni <- ((pFin - 1) * periodoTam) + 1
  pFinMesFin <- ((pFin) * periodoTam)
  pFinMesNombre <- ifelse( periodoTam > 1 , paste0(pFinMesIni,"-",pFinMesFin), paste(pFinMesIni) )
  resNombre <- paste0("TransicionMeses_",pIniMesNombre,"_a_",pFinMesNombre)
  # Generacion del resultado 
  resTransition <- data.frame()
  for( eIni in estados ) {
    datosIni <- resCambioEstado %>% filter( periodoIni == pIni, periodoFin == pFin, estadoIni == eIni ) %>% select(estadoFin, porCasos)
    vectorIni <- datosIni$porCasos
    names(vectorIni) <- datosIni$estadoFin
    resIni <- as.data.frame(t(vectorIni), row.names = eIni)
    if ( nrow(resTransition) == 0 ) {
      resTransition <- resIni
    } else {
      resTransition <- rbind( resTransition , resIni )
    }
  }
  listaTransicion[[resNombre]] <- resTransition
}

# Registro tabular del conteo y resumen de operaciones etiquetadas
resTransicionTabular <- data.frame()
if( is.null(listaTransicion[[resNombre]]) == FALSE ) {
  resTransicionTabular <- data.frame(
    "Vigente-Vigente" = listaTransicion[[resNombre]][1,c("Vigente")],
    "vigente-Default" = listaTransicion[[resNombre]][1,c("Default")],
    "Default-Vigente" = listaTransicion[[resNombre]][2,c("Vigente")],
    "Default-Default" = listaTransicion[[resNombre]][2,c("Default")]
  )
}
datosEtiquetados <- resEstadoOperaciones[resEstadoOperaciones$periodo == 1, c("numOp","estado")]
datosEtiquetadosConteo <- datosEtiquetados %>% group_by(estado) %>% summarise( numOp = n() ) %>% as.data.frame()
datosEtiquetadosRes <- cbind( data.frame( diasMora = numDiasMora, numMeses = periodoTam, vigente = datosEtiquetadosConteo[ datosEtiquetadosConteo$estado == 0, c("numOp") ], default = datosEtiquetadosConteo[ datosEtiquetadosConteo$estado == 1, c("numOp") ]), resTransicionTabular)

# Se guarda el resultado parcial
# nomArchivoRes <- paste0("C://DatosBGA//Score interno Tarjetas//transicionTarjetas//transicionTarjetaConteo",numDiasMora,".csv")
# write.table(datosEtiquetadosRes, nomArchivoRes, sep = ";", dec = ".", quote = FALSE, row.names = FALSE, col.names = !file.exists(nomArchivoRes), append = T)

if( nrow(datosConteoOp) == 0 ) {
  datosConteoOp <- datosEtiquetadosRes
} else {
  datosConteoOp <- rbind( datosConteoOp , datosEtiquetadosRes )
}

# la siguiente línea para iteraciones de meses de evaluación
# }
save(listaTransicion, file = paste0("transicionTarjetaLista",numDiasMora,".RData"))
save(datosConteoOp, file = paste0("transicionTarjetaConteo",numDiasMora,".RData"))
# la siguiente línea para iteraciones de Días Mora
# }


# Se recuperan los datos
# resNumDias <- 90
# load(paste0("transicionTarjetaLista",resNumDias,".RData"))
# load(paste0("transicionTarjetaConteo",resNumDias,".RData"))
# # listaTransicion
# datosConteoOp


#### 8. Se exportan las operaciones etiquetadas ####
resDatosDefault <- data.frame()
operacionesTarjeta <- datosTarget %>% 
  filter( NRO_OP %in% datosEtiquetados$numOp ) %>% # , DIAS_MORA >= numDiasMora
  select(NRO_OP, FECHA_PROCESO, DIAS_MORA, ESTADO, SALDO_ORI, NroMesCicloOp, Default) %>% 
  arrange(NRO_OP, FECHA_PROCESO, desc(DIAS_MORA), desc(SALDO_ORI) )
for( i in 1:nrow(operacionesTarjeta) ) {
  NRO_OPER <- operacionesTarjeta[i,c("NRO_OP")]
  Default <- operacionesTarjeta[i,c("Default")]
  NroMesCicloOp <- operacionesTarjeta[i,c("NroMesCicloOp")]
  NumRegEnRes <- ifelse( nrow(resDatosDefault) == 0, 0 , nrow(resDatosDefault[resDatosDefault$NRO_OP == NRO_OPER,]) )
  if( NumRegEnRes == 0 & Default == 1 & NroMesCicloOp <= periodoTam) {
    if ( nrow(resDatosDefault) == 0 ) {
      resDatosDefault <- operacionesTarjeta[i,]
    } else {
      resDatosDefault <- rbind(resDatosDefault, operacionesTarjeta[i,])
    }
  }
}
resDatosDefault <- resDatosDefault %>% mutate( numOp = NRO_OP ) %>% mutate( NRO_OP = NULL )
datosExportar <- inner_join(
  datosEtiquetados, 
  (datosOperacion %>%
     mutate( 
       numOp = NRO_OP,
       MONEDA = maxMONEDA,
       TC_FECHA_OP = maxTC_FECHA_OP,
       NRO_CUOTAS = maxNRO_CUOTAS,
       FRECUENCIA_PAGO = maxFRECUENCIA_PAGO,
       MONTO_DES_ORI = maxMONTO_DES_ORI
     ) %>%
     select( numOp, MONEDA, TC_FECHA_OP, NRO_CUOTAS, FRECUENCIA_PAGO, MONTO_DES_ORI )), by = "numOp")
datosExportar <- datosExportar %>% left_join(resDatosDefault %>% select(numOp,DIAS_MORA,ESTADO,FECHA_PROCESO,NroMesCicloOp,SALDO_ORI), by = "numOp" ) #%>% filter( estado == 1 ) %>% nrow()

write.table(datosExportar, file = paste0("tarjetaOperacionesEtiquetadasConSaldos", periodoTam , "meses" , numDiasMora , "dias.csv"), row.names = FALSE , sep = ";", dec = ".", quote = FALSE, na = '' )





