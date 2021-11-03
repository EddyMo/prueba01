rm(list = ls())
library(openxlsx)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(GoodmanKruskal)
library(FSelector)
library(scorecard)

setwd('C://DatosBGA//ScoreAdmisionTarjetas//04-PreparacionDatos')
options(scipen=999)

#### 1. Se importan las operaciones etiquetadas #### 
datos <- read.csv("C://DatosBGA//ScoreAdmisionTarjetas//02-DatosAdmision//tarjetaDatosEtiquetadosConSaldos36meses90dias.csv", sep = "|", dec = ".", stringsAsFactors = FALSE)
colsFactor <- c("ESTADO_CIVIL", "NIVEL_EDUCACION", "CARGO", "TIPO_VIVIENDA", "GENERO", "FUENTE_INGRESO", "SECTOR_ECONOMICO", "COD_PROFESION", "PROFESION", "SUCURSAL", "DEPARTAMENTO", "CAEDEC", "GRUPO_CAEDEC", "NIVEL_INGRESO", "PRODUCTO", "DESC_PRODUCTO", "TIPO_OPR", "FORMA_PAGO", "SEGMENTO", "DEFAULT","MONEDA")
datos[colsFactor] <- lapply(datos[colsFactor], factor)

# str(datos)
# datos %>% select(MONTO_OPERACION,MONEDA,MONTO_DES_ORI) %>% head(500)
# datos %>% select(MONEDA) %>% distinct()
# datos %>% select(TC_USD,TC_FECHA_OP) %>% distinct()



#### 2. Identificadores de atributos ID que no aportan al modelado ####
# Se eliminan identificadores o redundantes
datos <- datos %>% mutate( 
  JTS_OID_SALDOS = NULL,
  # Se comentan los 4 atributos siguientes para la verificación de Scores de Buro
  # NRO_SOLICITUD = NULL,
  # CODIGO_CLIENTE = NULL,
  # NRO_PERSONA = NULL,
  # FECHA_INICIO = NULL, # 388 valores
  
  TC_USD = NULL, # Dato para transformacion
  TC_FECHA_OP = NULL,
  MONEDA = NULL,
  SEGMENTO = NULL # porque solo tiene el valor "TARJETA"

  # MONTO_OPERACION = MONTO_DES_ORI,
  # PROFESION = NULL, # Redundante con "COD_PROFESION"
  # # SUCURSAL = NULL, # Puede considerarse un Identificador
  # CAEDEC = NULL, # Solo tiene 2 valores: "22210", "52111"
  # GRUPO_CAEDEC = NULL, # Solo tiene 2 valores: "ACTIVIDADES DE IMPRESION", "VENTA AL POR MENOR EN SUPERMERCADOS CON"
  # OPERACION = NULL
) 
# %>% mutate(
#   MONTO_DES_ORI = NULL
# )
# str( datos )


#### 3. Construcción de datos ####
# Se agrega un indicador entre pasivos y activos, apalancamiento
datos <- datos %>% mutate( PASIVO_ACTIVO = ifelse( TOT_ACTIVOS > 0 , round(TOT_PASIVOS / TOT_ACTIVOS,2) , 0 )  )

# Se transforma el MONTO_OPERACION en bolivianos
# datos  %>% group_by(MONTO_DES_ORI) %>% summarise( numOps = n() ) %>% arrange( MONTO_DES_ORI, numOps ) %>% as.data.frame()
datos <- datos %>% mutate( MONTO_OPERACION = MONTO_DES_ORI ) %>% mutate( MONTO_DES_ORI = NULL )


#### 4. Identificación de Redundancia semántica de datos ####
# Redundancia semántica de DIAS_PLAZO y NRO_CUOTAS (todos entorno a los 60 y 61 meses o menores a 0)
# datos %>% group_by( DIAS_PLAZO ) %>% summarise( n() ) %>% arrange( DIAS_PLAZO ) %>% as.data.frame()
# datos %>% mutate( MESES_PLAZO = round(DIAS_PLAZO / 30.4375 , 1 ) ) %>% group_by( MESES_PLAZO ) %>% summarise( n() ) %>% as.data.frame()
# cor(datos$DIAS_PLAZO, datos$NRO_CUOTAS)
datos <- datos %>% mutate( DIAS_PLAZO = NULL, MESES_PLAZO = NULL )

# Redundancia semántica de COD_PROFESION y PROFESION
# datos %>% group_by(COD_PROFESION,PROFESION) %>% summarise( numOp = n() ) %>%  arrange( desc(numOp), COD_PROFESION,PROFESION) %>% as.data.frame()
# datos %>% group_by(COD_PROFESION, DEFAULT) %>% summarise( numOp = n() ) %>%  arrange( desc(COD_PROFESION), numOp) %>% as.data.frame()
datos <- datos %>% mutate( PROFESION = NULL )

# Redundancia semántica de CAEDEC y GRUPO_CAEDEC (por capacidad de discriminación todos en: CAEDEC = 52111)
# datos %>% group_by(CAEDEC,GRUPO_CAEDEC, DEFAULT) %>% summarise( numOp = n() ) %>%  arrange( desc(numOp), CAEDEC,GRUPO_CAEDEC) %>% as.data.frame()
# datos %>% group_by(CAEDEC, DEFAULT) %>% summarise( numOp = n() ) %>%  arrange( desc(numOp), CAEDEC) %>% as.data.frame()
datos <- datos %>% mutate( GRUPO_CAEDEC = NULL, CAEDEC = NULL )

# Redundancia semántica de PRODUCTO y DESC_PRODUCTO
# datos %>% group_by(PRODUCTO,DESC_PRODUCTO) %>% summarise( numOp = n() ) %>%  arrange( desc(numOp), PRODUCTO,DESC_PRODUCTO) %>% as.data.frame()
# datos %>% group_by(PRODUCTO,DESC_PRODUCTO, DEFAULT) %>% summarise( numOp = n() ) %>%  arrange( PRODUCTO,DESC_PRODUCTO, DEFAULT) %>% as.data.frame()
datos <- datos %>% mutate( PRODUCTO = NULL, DESC_PRODUCTO = NULL )

# Redundancia semántica de DEPARTAMENTO y SUCURSAL
# datos %>% group_by(DEPARTAMENTO,SUCURSAL) %>% summarise( numOp = n() ) %>%  arrange( DEPARTAMENTO,SUCURSAL,numOp) %>% as.data.frame()
# datos %>% group_by(DEPARTAMENTO,DEFAULT) %>% summarise( numOp = n() ) %>%  arrange( DEPARTAMENTO,DEFAULT,numOp) %>% as.data.frame()
datos <- datos %>% mutate( SUCURSAL = NULL )


#### 5. Evaluación de la capacidad de discriminación de datos numéricos ####
# Capacidad de discriminación de CREDITOS_VENCIDOS_BGA
# datos %>% group_by(CREDITOS_VENCIDOS_BGA,DEFAULT) %>% summarise( numOp = n() ) %>%  arrange( CREDITOS_VENCIDOS_BGA,numOp) %>% as.data.frame()
datos <- datos %>% mutate( CREDITOS_VENCIDOS_BGA = NULL )

# Capacidad de discriminación de CANTIDAD_HIJOS
datos %>% group_by(CANTIDAD_HIJOS,DEFAULT) %>% summarise( numOp = n() ) %>%  arrange( CANTIDAD_HIJOS,numOp) %>% as.data.frame()
grafCANTIDAD_HIJOS <- datos %>% group_by(CANTIDAD_HIJOS,DEFAULT) %>% summarise( numOp = n() )
ggplot(grafCANTIDAD_HIJOS %>% filter(CANTIDAD_HIJOS < 10), aes(fill=DEFAULT, y=numOp, x=CANTIDAD_HIJOS)) + geom_bar(position="fill", stat="identity") + scale_fill_manual("Default", values = c("0" = "blue", "1" = "red"))

# Capacidad de discriminación de NRO_CODEUDORES
datos %>% group_by(NRO_CODEUDORES,DEFAULT) %>% summarise( numOp = n() ) %>%  arrange( NRO_CODEUDORES,numOp) %>% as.data.frame()
grafNRO_CODEUDORES <- datos %>% group_by(NRO_CODEUDORES,DEFAULT) %>% summarise( numOp = n() )
ggplot(grafNRO_CODEUDORES, aes(fill=DEFAULT, y=numOp, x=NRO_CODEUDORES)) + geom_bar(position="fill", stat="identity") + scale_fill_manual("Default", values = c("0" = "blue", "1" = "red"))


# Capacidad de discriminación de FRECUENCIA_PAGO (los valores son solo 0 y 30)
# datos %>% group_by( FRECUENCIA_PAGO ) %>% summarise( numOps = n() ) %>% arrange( FRECUENCIA_PAGO )
# datos %>% group_by( FRECUENCIA_PAGO,DEFAULT ) %>% summarise( numOps = n() ) %>% arrange( FRECUENCIA_PAGO,DEFAULT ) %>% as.data.frame()
datos <- datos %>% mutate( FRECUENCIA_PAGO = NULL )

# Corrección del NIVEL_INGRESO en blanco
# datos %>% filter(NIVEL_INGRESO == "")
# # datos <- datos %>% mutate( NIVEL_INGRESO = as.character(NIVEL_INGRESO) ) %>% mutate(NIVEL_INGRESO = ifelse( NIVEL_INGRESO == "", "NO INDICADO", NIVEL_INGRESO ) ) %>% mutate( NIVEL_INGRESO = as.factor(NIVEL_INGRESO) )


#### 6. Evaluación de la capacidad de discriminación de datos categóricos ####

# Capacidad de discriminación de ESTADO_CIVIL
datos %>% group_by(ESTADO_CIVIL,DEFAULT) %>% summarise( numOp = n() ) %>%  arrange( ESTADO_CIVIL,numOp) %>% as.data.frame()
grafESTADO_CIVIL <- datos %>% group_by(ESTADO_CIVIL,DEFAULT) %>% summarise( numOp = n() )
ggplot(grafESTADO_CIVIL, aes(fill=DEFAULT, y=numOp, x=ESTADO_CIVIL)) + geom_bar(position="fill", stat="identity") + scale_fill_manual("Default", values = c("0" = "blue", "1" = "red"))

# Capacidad de discriminación de NIVEL_EDUCACION
datos %>% group_by(NIVEL_EDUCACION,DEFAULT) %>% summarise( numOp = n() ) %>%  arrange( NIVEL_EDUCACION,numOp) %>% as.data.frame()
grafNIVEL_EDUCACION <- datos %>% group_by(NIVEL_EDUCACION,DEFAULT) %>% summarise( numOp = n() )
ggplot(grafNIVEL_EDUCACION, aes(fill=DEFAULT, y=numOp, x=NIVEL_EDUCACION)) + geom_bar(position="fill", stat="identity") + scale_fill_manual("Default", values = c("0" = "blue", "1" = "red"))

# Capacidad de discriminación de CARGO
datos %>% group_by(CARGO,DEFAULT) %>% summarise( numOp = n() ) %>%  arrange( CARGO,numOp) %>% as.data.frame()
grafCARGO <- datos %>% group_by(CARGO,DEFAULT) %>% summarise( numOp = n() )
ggplot(grafCARGO, aes(fill=DEFAULT, y=numOp, x=CARGO)) + geom_bar(position="fill", stat="identity") + scale_fill_manual("Default", values = c("0" = "blue", "1" = "red"))

# Capacidad de discriminación de TIPO_VIVIENDA
grafTIPO_VIVIENDA <- datos %>% group_by(TIPO_VIVIENDA,DEFAULT) %>% summarise( numOp = n() )
ggplot(grafTIPO_VIVIENDA, aes(fill=DEFAULT, y=numOp, x=TIPO_VIVIENDA)) + geom_bar(position="fill", stat="identity") + scale_fill_manual("Default", values = c("0" = "blue", "1" = "red"))

# Capacidad de discriminación de GENERO
grafGENERO <- datos %>% group_by(GENERO,DEFAULT) %>% summarise( numOp = n() )
ggplot(grafGENERO, aes(fill=DEFAULT, y=numOp, x=GENERO)) + geom_bar(position="fill", stat="identity") + scale_fill_manual("Default", values = c("0" = "blue", "1" = "red"))

# Capacidad de discriminación de FUENTE_INGRESO
grafFUENTE_INGRESO <- datos %>% group_by(FUENTE_INGRESO,DEFAULT) %>% summarise( numOp = n() )
ggplot(grafFUENTE_INGRESO, aes(fill=DEFAULT, y=numOp, x=FUENTE_INGRESO)) + geom_bar(position="fill", stat="identity") + scale_fill_manual("Default", values = c("0" = "blue", "1" = "red"))
datos <- datos %>% mutate( FUENTE_INGRESO = NULL )

# Capacidad de discriminación de SECTOR_ECONOMICO
grafSECTOR_ECONOMICO <- datos %>% group_by(SECTOR_ECONOMICO,DEFAULT) %>% summarise( numOp = n() )
ggplot(grafSECTOR_ECONOMICO, aes(fill=DEFAULT, y=numOp, x=SECTOR_ECONOMICO)) + geom_bar(position="fill", stat="identity") + scale_fill_manual("Default", values = c("0" = "blue", "1" = "red"))

# Capacidad de discriminación de DEPARTAMENTO
grafDEPARTAMENTO <- datos %>% group_by(DEPARTAMENTO,DEFAULT) %>% summarise( numOp = n() )
ggplot(grafDEPARTAMENTO, aes(fill=DEFAULT, y=numOp, x=DEPARTAMENTO)) + geom_bar(position="fill", stat="identity") + scale_fill_manual("Default", values = c("0" = "blue", "1" = "red"))

# Capacidad de discriminación de NIVEL_INGRESO
grafNIVEL_INGRESO <- datos %>% group_by(NIVEL_INGRESO,DEFAULT) %>% summarise( numOp = n() )
ggplot(grafNIVEL_INGRESO, aes(fill=DEFAULT, y=numOp, x=NIVEL_INGRESO)) + geom_bar(position="fill", stat="identity") + scale_fill_manual("Default", values = c("0" = "blue", "1" = "red"))

# Capacidad de discriminación de TIPO_OPR
grafTIPO_OPR <- datos %>% group_by(TIPO_OPR,DEFAULT) %>% summarise( numOp = n() )
ggplot(grafTIPO_OPR, aes(fill=DEFAULT, y=numOp, x=TIPO_OPR)) + geom_bar(position="fill", stat="identity") + scale_fill_manual("Default", values = c("0" = "blue", "1" = "red"))
datos <- datos %>% mutate( TIPO_OPR = NULL )

# Capacidad de discriminación de FORMA_PAGO
grafFORMA_PAGO <- datos %>% group_by(FORMA_PAGO,DEFAULT) %>% summarise( numOp = n() )
ggplot(grafFORMA_PAGO, aes(fill=DEFAULT, y=numOp, x=FORMA_PAGO)) + geom_bar(position="fill", stat="identity") + scale_fill_manual("Default", values = c("0" = "blue", "1" = "red"))

# Capacidad de discriminación de CUOTA_ING_NETO
(grafCUOTA_ING_NETO <- datos %>% group_by(CUOTA_ING_NETO,DEFAULT) %>% summarise( numOp = n() ))
datos <- datos %>% mutate( CUOTA_ING_NETO = NULL )

# Capacidad de discriminación de CANTIDAD_HIJOS
(grafCANTIDAD_HIJOS <- datos %>% filter( CANTIDAD_HIJOS < 10 ) %>% group_by(CANTIDAD_HIJOS,DEFAULT) %>% summarise( numOp = n() ))
ggplot(grafCANTIDAD_HIJOS, aes(fill=DEFAULT, y=numOp, x=CANTIDAD_HIJOS)) + geom_bar(position="fill", stat="identity") + scale_fill_manual("Default", values = c("0" = "blue", "1" = "red"))

# Capacidad de discriminación de NRO_CUOTAS
# datos %>% group_by(NRO_CUOTAS,DEFAULT) %>% summarise( numOp = n() )
datos <- datos %>% mutate( NRO_CUOTAS = NULL )


#### 7. Datos ausentes ####
# sapply(datos, function(x) sum(is.na(x)))
# datos %>% nrow()
# datos %>% filter( is.na(SALDO), DEFAULT == "0" ) %>% nrow()
# datos %>% filter( is.na(NRO_MES_DEFAULT), DEFAULT == "0" ) %>% nrow()


#### 8. Identificación de datos numéricos anómalos ####
# str(datos)
columnasNum <- c("EDAD", "ANTIGUEDAD_LABORAL", "MONTO_OPERACION", "INGRESOS_GASTOS", "CANTIDAD_HIJOS", "TOT_ACTIVOS", "TOT_PASIVOS", "NRO_CODEUDORES", "PASIVO_ACTIVO")
columnasCat <- colnames(datos)[!colnames(datos) %in% c(columnasNum, "DEFAULT", "NRO_SOLICITUD", "CODIGO_CLIENTE", "NRO_PERSONA", "OPERACION","SALDO","NRO_MES_DEFAULT","FECHA_INICIO")]

graficoBoxPlot <- function( col ) {
  plt <- ggplot(data = datos[,c(col,"DEFAULT")], aes(y = datos[,c(col)] , x = DEFAULT, fill = DEFAULT)) +
    geom_boxplot() + scale_fill_manual(values=c("green","red")) +
    theme(legend.position="none", axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5)) +
    ggtitle(col) + xlab("Default")
  plotBoxPlot <- plt
}
listaBoxPlot <- list()
for( col in columnasNum) { listaBoxPlot[[col]] <- graficoBoxPlot(col) }
listaBoxPlot[["EDAD"]] # outliers: 252V, 4D ; > 65
listaBoxPlot[["MONTO_OPERACION"]] # outliers: 527V, 2D ; > 35000 | < 1000
listaBoxPlot[["INGRESOS_GASTOS"]] # outliers: 590V, 12D ; > 7
listaBoxPlot[["CANTIDAD_HIJOS"]] # outliers: 8V ; > 4
listaBoxPlot[["TOT_ACTIVOS"]] # outliers: 851V, 13D ; > 150000
listaBoxPlot[["TOT_PASIVOS"]] #  outliers: 349V, 8D ; > 60000
listaBoxPlot[["NRO_CODEUDORES"]] # nivel de discriminación poco significativo (solo 1 caso en Default con NRO_CODEUDORES = 2)
listaBoxPlot[["PASIVO_ACTIVO"]] # outliers: 12V ; > 1
listaBoxPlot[["ANTIGUEDAD_LABORAL"]] # 

# datos %>% filter( EDAD > 65 ) %>% group_by(DEFAULT) %>% summarise( numOp = n() )
datos <- datos %>% filter( EDAD <= 65 )

# datos %>% filter( MONTO_OPERACION <= 1000 ) %>% group_by(DEFAULT) %>% summarise( numOp = n() )
# datos %>% filter( MONTO_OPERACION > 35000 | MONTO_OPERACION < 1000 ) %>% group_by(DEFAULT) %>% summarise( numOp = n() )
datos <- datos %>% filter( MONTO_OPERACION <= 35000 & MONTO_OPERACION >= 1000 )

# datos %>% filter( INGRESOS_GASTOS > 7 ) %>% group_by(DEFAULT) %>% summarise( numOp = n() )
datos <- datos %>% filter( INGRESOS_GASTOS <= 7 )

# datos %>% filter( CANTIDAD_HIJOS > 4 ) %>% group_by(DEFAULT) %>% summarise( numOp = n() )
datos <- datos %>% filter( CANTIDAD_HIJOS <= 4 )

# datos %>% filter( TOT_ACTIVOS > 150000 ) %>% group_by(DEFAULT) %>% summarise( numOp = n() )
datos <- datos %>% filter( TOT_ACTIVOS <= 150000 )

# datos %>% filter( TOT_PASIVOS > 60000 | TOT_PASIVOS < 0 ) %>% group_by(DEFAULT) %>% summarise( numOp = n() )
# datos %>% filter( TOT_PASIVOS < 0 ) %>% group_by(DEFAULT) %>% summarise( numOp = n() )
datos <- datos %>% filter( TOT_PASIVOS <= 60000 , TOT_PASIVOS >= 0 )

# datos %>% group_by(NRO_CODEUDORES, DEFAULT) %>% summarise( numOps = n()) %>% arrange(NRO_CODEUDORES, DEFAULT)

# datos %>% filter( PASIVO_ACTIVO > 1 ) %>% group_by(DEFAULT) %>% summarise( numOp = n() )
datos <- datos %>% filter( PASIVO_ACTIVO <= 1 )

# datos %>% filter( ANTIGUEDAD_LABORAL > 60 ) %>% select(DEFAULT, ANTIGUEDAD_LABORAL) %>% group_by(ANTIGUEDAD_LABORAL, DEFAULT) %>% summarise( numOps = n() ) %>% as.data.frame()
# datos %>% filter( ANTIGUEDAD_LABORAL > 60 ) %>% group_by(DEFAULT) %>% summarise( numOp = n() )
# # datos <- datos %>% filter( ANTIGUEDAD_LABORAL <= 10 )
# # datos %>% group_by(DEFAULT) %>% summarise( numOp = n() )

# SALDO y MONTO_OPERACION (4 outliers)
# datos %>% filter( SALDO > MONTO_OPERACION )
datos <- datos %>% filter( is.na(SALDO) | SALDO <= MONTO_OPERACION )
datos <- datos %>% filter( !is.na(COD_PROFESION) )


#### 9. Análisis de correlación #### 
# Correlación numérica
(CNumMatrix <- round(cor(datos[,columnasNum]),2))
corrplot(CNumMatrix, type = "upper", tl.col = "black", tl.srt = 35, use = "complete.obs")
#TOT_PASIVO - PASIVO_ACTIVO (0.73) ; TOT_ACTIVOS - TOT_PASIVOS (0.52) ; EDAD - TOT_ACTIVOS (0.31) ; EDAD - CANTIDAD_HIJOS (0.28) ; TOT_ACTIVOS - MONTO_OPERACION (0.27) ; CANTIDAD_HIJOS - TOT_ACTIVOS (0.25) ; TOT_ACTIVOS - PASIVO_ACTIVO (0.22)
(CNumMatrix2 <- round(cor(datos[,c("EDAD","CANTIDAD_HIJOS","TOT_ACTIVOS","MONTO_OPERACION", "TOT_PASIVOS", "PASIVO_ACTIVO")]),2))
corrplot(CNumMatrix2, type = "upper", tl.col = "black", tl.srt = 35, use = "complete.obs")

# Correlación categórica
(GKmatrix1 <- GKtauDataframe(datos[,columnasCat]))
plot(GKmatrix1, corrColors = "blue", diagSize = 0.5,  dgts = 1)
# COD_PROFESION - NIVEL_EDUCACION (0.616) ; COD_PROFESION - GENERO (0.214) ; NIVEL_EDUCACION - COD_PROFESION (0.100)
(GKmatrix2 <- GKtauDataframe(datos[,c("NIVEL_EDUCACION", "COD_PROFESION","GENERO")]))
plot(GKmatrix2, corrColors = "blue", diagSize = 0.5,  dgts = 1)


#### 10. Verificación de la relevancia de los atributos ####
dataPrep <- datos[,colnames(datos)[! colnames(datos) %in% c("OPERACION","NRO_SOLICITUD", "CODIGO_CLIENTE", "NRO_PERSONA", "FECHA_INICIO", "SALDO", "NRO_MES_DEFAULT") ] ]
dataPrepVMcols <- c()
# dataPrep <- vistaMinable[,colnames(vistaMinable)[! colnames(vistaMinable) %in% c("OPERACION") ] ]
# dataPrepVMcols <- c("log_EDAD", "log_MONTO_OPERACION", "log_INGRESOS_GASTOS", "log_CUOTA_ING_NETO", "log_TOT_ACTIVOS")
# str(dataPrep)
set.seed(1234)
relevanciaGen <- data.frame(
  columna =  row.names(chi.squared(DEFAULT~., dataPrep)),
  chi.squared = round( chi.squared(DEFAULT~., dataPrep)$attr_importance ,4),
  information.gain = round( information.gain(DEFAULT~., dataPrep)$attr_importance ,4),
  gain.ratio = round( gain.ratio(DEFAULT~., dataPrep)$attr_importance ,4),
  symmetrical.uncertainty = round( symmetrical.uncertainty(DEFAULT~., dataPrep)$attr_importance ,4)
)
# dataPrep$DEFAULT <- as.numeric(dataPrep$DEFAULT)
set.seed(1234)
relevanciaNum <- data.frame(
  columna =  row.names(chi.squared(DEFAULT~., dataPrep[,c(columnasNum,"DEFAULT", dataPrepVMcols)])),
  linear.correlation = round( linear.correlation(DEFAULT~., (dataPrep %>% mutate( DEFAULT = as.numeric(DEFAULT) ) %>% select( c("DEFAULT",columnasNum, dataPrepVMcols) ) ))$attr_importance ,4),
  rank.correlation = round( rank.correlation(DEFAULT~., (dataPrep %>% mutate( DEFAULT = as.numeric(DEFAULT) ) %>% select( c("DEFAULT",columnasNum, dataPrepVMcols) ) ))$attr_importance ,4)
)
relevanciaIV <- iv(datos[,c(columnasCat,"DEFAULT")], "DEFAULT", x = columnasCat, positive = "1", order = TRUE)
relevanciaIV <- relevanciaIV %>% mutate(columna = variable, info_value = round(info_value,4) ) %>% mutate(variable = NULL)
relevancia <- left_join(relevanciaGen , relevanciaNum, by = c("columna")) %>% mutate( linear.correlation = ifelse( is.na(linear.correlation), 0, linear.correlation ), rank.correlation = ifelse( is.na(rank.correlation), 0, rank.correlation ) )
left_join(relevancia , relevanciaIV, by = c("columna")) %>% mutate( info_value = ifelse( is.na(info_value), 0.0000, info_value ) )


#### 11. Se guardan los datos preparados ####
# table(datos$DEFAULT)
vistaMinable <- datos %>%
  # select( -OPERACION ) %>%
  mutate(
    log_EDAD = log(EDAD),
    log_MONTO_OPERACION = log(MONTO_OPERACION),
    log_INGRESOS_GASTOS = log(INGRESOS_GASTOS)
    # , log_TOT_ACTIVOS = log(TOT_ACTIVOS)
  )
# str(vistaMinable)
write.table(vistaMinable %>% select(-one_of(c("NRO_SOLICITUD","CODIGO_CLIENTE","NRO_PERSONA","NRO_MES_DEFAULT"))),
            file = "TarjetaVistaMinable_36meses90dias_Iteracion_1.csv", sep = ";", dec = ".", quote = FALSE, row.names = FALSE, na = "")
