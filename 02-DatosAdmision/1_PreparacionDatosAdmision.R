rm(list = ls())
library(openxlsx)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(GoodmanKruskal)
library(FSelector)

setwd('C://DatosBGA//ScoreAdmisionTarjetas//02-DatosAdmision')
options(scipen=999)

#### 1. Se recuperan los datos base de admisión ####
dataAdmision <- read.csv("C://DatosBGA//ScoreAdmisionTarjetas//01-DatosBase//Analisis_Prestamos (Admision) V2.csv", sep = "|", dec = ".")
# str(dataAdmision)

#### 2. Se identifican los datos de consumo ####
dataAdmision <- dataAdmision %>% mutate( FECHA_INICIO = as.Date(FECHA_INICIO, format = "%d/%m/%Y") )
colFactor <- c("ESTADO_CIVIL", "NIVEL_EDUCACION", "CARGO", "TIPO_VIVIENDA", "GENERO", "FUENTE_INGRESO", "SECTOR_ECONOMICO", "COD_PROFESION", "SUCURSAL", "CAEDEC", "PRODUCTO", "PROFESION", "DEPARTAMENTO", "GRUPO_CAEDEC", "NIVEL_INGRESO", "DESC_PRODUCTO", "TIPO_OPR")
dataAdmision[colFactor] <- lapply(dataAdmision[colFactor], factor)
dataAdmision <- dataAdmision %>% mutate( SEGMENTO = ifelse( grepl("CONSUMAX|TARJETA", DESC_PRODUCTO, ignore.case = T) & !grepl("EMPRESA|PYME|MYPE|MICRO|HIP|VIV", DESC_PRODUCTO, ignore.case = T), "CONSUMO", "OTRO") )
dataAdmision <- dataAdmision %>% mutate( SEGMENTO = ifelse( SEGMENTO == "OTRO", SEGMENTO, ifelse( grepl("CONSUMAX", DESC_PRODUCTO, ignore.case = T), "CONSUMAX", "TARJETA" ) ) )
dataConsumo <- dataAdmision %>% filter( SEGMENTO != "OTRO")

save(dataConsumo, file = "dataConsumo.RData")
load("dataConsumo.RData")

# str(dataAdmision)
# head(dataAdmision)
# colnames(dataAdmision)
# dataAdmision %>% group_by(SECTOR_ECONOMICO) %>% summarise( numOps = n() )
# dataAdmision %>% group_by(FUENTE_INGRESO) %>% summarise( numOps = n() )
# dataAdmision %>% group_by(DIAS_PLAZO) %>% summarise( numOps = n() ) %>% arrange( desc(numOps) ) %>% as.data.frame()
# dataAdmision %>% group_by(SEGMENTO) %>% summarise( numOps = n() )

#### 3. Elección solo los registros únicos de operaciones de consumo ####
dataConsumo <- dataConsumo %>% arrange( OPERACION, FECHA_INICIO, desc(MONTO_OPERACION) )
idxRegistrosUnicos <- c()
listaOperacionesUnicas <- c()
for( i in 1:nrow(dataConsumo) ) { # 1:100
  current_OPERACION <- dataConsumo [i, c("OPERACION")]
  existeEnConsumoUnico <- current_OPERACION %in% listaOperacionesUnicas
  if( existeEnConsumoUnico == FALSE ) {
    idxRegistrosUnicos <- c(idxRegistrosUnicos, i)
    listaOperacionesUnicas <- c(listaOperacionesUnicas, current_OPERACION)
  }
}
dataConsumoUnico <- dataConsumo[idxRegistrosUnicos,]
dataConsumoUnico %>% select( OPERACION ) %>% distinct() %>% nrow()
dataConsumoUnico %>% nrow()
save(dataConsumoUnico, file = "dataConsumoUnico.RData")
load("dataConsumoUnico.RData")



#### 4. Identificación del número de registros por periodo ####
mesesLabel <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
datosParaConteo <- dataConsumoUnico %>% 
  filter( is.na(FECHA_INICIO) == FALSE ) %>% 
  mutate( FECHA_ANIO = year(FECHA_INICIO), FECHA_NUM = (year(FECHA_INICIO) * 100) + month(FECHA_INICIO), FECHA_LABEL = paste0(year(FECHA_INICIO) , "-" , mesesLabel[month(FECHA_INICIO)] ) ) %>% 
  select( FECHA_ANIO, FECHA_INICIO, FECHA_NUM, FECHA_LABEL, SEGMENTO, OPERACION )
conteoTotal <- datosParaConteo %>% group_by( FECHA_ANIO, FECHA_NUM, FECHA_LABEL ) %>% summarise( numOpTotal = n() )
conteoConsumax <- datosParaConteo %>% filter( SEGMENTO == "CONSUMAX") %>% group_by( FECHA_ANIO, FECHA_NUM, FECHA_LABEL ) %>% summarise( numOpConsumax = n() )
conteoTarjeta <- datosParaConteo %>% filter( SEGMENTO == "TARJETA") %>% group_by( FECHA_ANIO, FECHA_NUM, FECHA_LABEL ) %>% summarise( numOpTarjeta = n() )

conteobase0 <- left_join( conteoTotal, conteoConsumax, by = c("FECHA_ANIO", "FECHA_NUM", "FECHA_LABEL") )
conteobase1 <- left_join( conteobase0, conteoTarjeta, by = c("FECHA_ANIO", "FECHA_NUM", "FECHA_LABEL") )
conteobase2 <- conteobase1 %>% as.data.frame() %>% mutate_all(~replace(., is.na(.), 0)) %>% arrange( FECHA_NUM )
conteobase3 <- conteobase2 %>% group_by( FECHA_ANIO ) %>% summarise( numOpTotal = sum(numOpTotal), numOpConsumax = sum(numOpConsumax), numOpTarjeta = sum(numOpTarjeta) ) %>% as.data.frame()
parcial1 <- conteobase3 %>% mutate ( ANIO = FECHA_ANIO, TIPO = "TOTAL", NUM_OPS = numOpTotal ) %>% select ( ANIO, TIPO, NUM_OPS )
parcial2 <- conteobase3 %>% mutate ( ANIO = FECHA_ANIO, TIPO = "CONSUMAX", NUM_OPS = numOpConsumax ) %>% select ( ANIO, TIPO, NUM_OPS )
parcial3 <- conteobase3 %>% mutate ( ANIO = FECHA_ANIO, TIPO = "TARJETA", NUM_OPS = numOpTarjeta ) %>% select ( ANIO, TIPO, NUM_OPS )
conteobase4 <- rbind( parcial3, rbind( parcial1, parcial2 ) )
conteobase2 %>% filter( FECHA_NUM >= 201510, FECHA_NUM <= 201905)

ggplot(conteobase4, aes(x = ANIO, y = NUM_OPS)) + 
  geom_line(aes(color = TIPO)) +  # , linetype = variable
  theme_bw()+
  ggtitle("Número de operaciones por año") +
  scale_x_continuous(breaks = seq(1997, 2020, by = 1), limits = c(1997, 2020)) +
  scale_color_manual(values = c("blue", "green", "black")) + 
  theme(axis.text.x = element_text(angle = 90))


#### 5. Se exportan los identificadores de las operaciones de Tarjetas ####
OperacionesTarjeta <- datosParaConteo %>% filter( SEGMENTO == "TARJETA", FECHA_NUM >= 201510, FECHA_NUM <= 201905) %>% select( OPERACION )
write.table( OperacionesTarjeta, file = "OperacionesTarjeta.csv", sep = ";", dec = ".", quote = FALSE, row.names = FALSE )


#### 6. Se etiquetan y guardan los datos de Consumax con el resultado de la definición de Default realizada ####
deMeses <- 36 # 18 24 36
deDiasMora <- 90


datosEtiquetadosTarjeta <- read.csv( paste0("C://DatosBGA//ScoreAdmisionTarjetas//03-DefinicionDefault//tarjetaOperacionesEtiquetadasConSaldos",deMeses,"meses",deDiasMora,"dias.csv"), sep = ";", dec = ".", stringsAsFactors = FALSE)
# str(datosEtiquetadosTarjeta)
datosEtiquetadosTarjeta <- datosEtiquetadosTarjeta %>%
  mutate( OPERACION = as.numeric(numOp) , DEFAULT = estado, SALDO = SALDO_ORI, NRO_MES_DEFAULT = NroMesCicloOp ) %>%
  mutate( numOp = NULL, estado = NULL, # MONEDA = NULL, TC_FECHA_OP = NULL,
          DIAS_MORA = NULL, ESTADO = NULL, FECHA_PROCESO = NULL, NroMesCicloOp = NULL, SALDO_ORI = NULL)
dataTarjetaEtiquetada <- inner_join( dataConsumoUnico, datosEtiquetadosTarjeta, by = c("OPERACION"), copy = FALSE )
# str(dataTarjetaEtiquetada)
write.table(dataTarjetaEtiquetada, file = paste0("tarjetaDatosEtiquetadosConSaldos", deMeses,"meses", deDiasMora,"dias.csv"), sep = "|", dec = ".", quote = FALSE, row.names = FALSE, na = "" )


