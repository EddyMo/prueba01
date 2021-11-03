rm(list = ls())
library(dplyr)
library(ggplot2)
library(gridExtra)
library(caret)
library(scorecard)
library(ROCR)
library(Metrics)
library(hmeasure)

setwd('C://DatosBGA//ScoreAdmisionTarjetas//05-Modelado')
options(scipen=999)

#### 1. Carga de datos.- que se utilizan como base para el modelado de datos ####
data <- read.csv("C://DatosBGA//ScoreAdmisionTarjetas//04-PreparacionDatos//TarjetaVistaMinable_36meses90dias_Iteracion_1.csv", sep = ";", dec = ".", stringsAsFactors = FALSE)
colFactor <- c("ESTADO_CIVIL","NIVEL_EDUCACION","CARGO","TIPO_VIVIENDA","GENERO","SECTOR_ECONOMICO","COD_PROFESION","DEPARTAMENTO","NIVEL_INGRESO","FORMA_PAGO","DEFAULT")
data <- data %>% mutate( NIVEL_INGRESO = ifelse( NIVEL_INGRESO == "", "NO INDICADO", NIVEL_INGRESO  ) )
data[colFactor] <- lapply(data[colFactor], factor)
# data %>% select( NIVEL_INGRESO ) %>% distinct()
# str(data)


#### 2. División del conjunto de datos en Train y Test ####
# Parámetros
target <- "DEFAULT"
clasePositiva <- 1

numBalanceNegPos <- 3
proporcionTrainTest <- 0.8

# División del conjunto de datos
# c(nrow(data),table(data$DEFAULT))
# table(data$DEFAULT)["0"]/table(data$DEFAULT)["1"]

set.seed(1234) #
trainIndex <- createDataPartition(data[,c(target)], p = proporcionTrainTest, list = FALSE, times = 1)
ds_Test  <- data[-trainIndex,]
ds_Train_NoBalanceado <- data[trainIndex,]
# ds_Test %>% group_by( DEFAULT ) %>% summarise( n() )
# ds_Train_NoBalanceado %>% group_by( DEFAULT ) %>% summarise( n() )


#### Equilibrio de clases en datos de entrenamiento ####
ds_Train_NoBalanceado_Clase0 <- ds_Train_NoBalanceado[ds_Train_NoBalanceado[,c(target)] == 0,]
ds_Train_NoBalanceado_Clase1 <- ds_Train_NoBalanceado[ds_Train_NoBalanceado[,c(target)] == 1,]
numClase0 <- nrow(ds_Train_NoBalanceado_Clase0)
numClase1 <- nrow(ds_Train_NoBalanceado_Clase1)

set.seed(3456)
indexMuestraClase0 <- sample(numClase0, ifelse(numClase1*numBalanceNegPos < numClase0, numClase1*numBalanceNegPos , numClase0 ) )
ds_Train_NoBalanceado_Clase0_bal <- ds_Train_NoBalanceado_Clase0[indexMuestraClase0,] #(numClase0,numClase1*2)
ds_Train_base <- rbind(ds_Train_NoBalanceado_Clase1,ds_Train_NoBalanceado_Clase0_bal)
# table(ds_Train$DEFAULT)


# Intercambio equilibrado de COD_PROFESION (VIGENTES)
CODPRO_TestNoTrain0 <- as.character((ds_Test %>% filter( DEFAULT == "0" ) %>% select(COD_PROFESION) %>% distinct() %>% filter( !COD_PROFESION %in% ds_Train_base$COD_PROFESION ))$COD_PROFESION)
CODPRO_Train4Change0 <- as.character( (ds_Train_base %>% filter( DEFAULT == "0" ) %>% group_by(COD_PROFESION) %>% summarise( numOps = n() ) %>% arrange( desc(numOps),COD_PROFESION ) %>% head(length(CODPRO_TestNoTrain0)))$COD_PROFESION )
idxTrainChange <- c()
for( CP_train in CODPRO_Train4Change0 ){
  for (i in 1:nrow(ds_Train_base)) {
    if( as.character(ds_Train_base[i,c("DEFAULT")]) == "0" & as.character(ds_Train_base[i,c("COD_PROFESION")]) == CP_train ){
      idxTrainChange <- c(idxTrainChange,i); break;
    }
  }  
}
idxTestChange <- c()
for( CP_test in CODPRO_TestNoTrain0 ){
  for (i in 1:nrow(ds_Test)) {
    if( as.character(ds_Test[i,c("DEFAULT")]) == "0" & as.character(ds_Test[i,c("COD_PROFESION")]) == CP_test ){
      idxTestChange <- c(idxTestChange,i); break;
    }
  }  
}
ds_Train_base <- rbind(ds_Train_base,ds_Test[idxTestChange,])
ds_Test <- rbind(ds_Test,ds_Train_base[idxTrainChange,])
ds_Train_base <- ds_Train_base[!(1:nrow(ds_Train_base)) %in% idxTrainChange,]
ds_Test <- ds_Test[!(1:nrow(ds_Test)) %in% idxTestChange,]

CODPRO_TestNoTrain1 <- as.character((ds_Test %>% filter( DEFAULT == "1" & (!COD_PROFESION %in% c(CODPRO_Train4Change0,CODPRO_TestNoTrain0)) ) %>% select(COD_PROFESION) %>% distinct() %>% filter( !COD_PROFESION %in% ds_Train_base$COD_PROFESION ))$COD_PROFESION) # c(, CODPRO_Train4Change0,CODPRO_TestNoTrain0)
CODPRO_Train4Change1 <- as.character( (ds_Train_base %>% filter( DEFAULT == "1" & (!COD_PROFESION %in% c(CODPRO_Train4Change0,CODPRO_TestNoTrain0)) ) %>% group_by(COD_PROFESION) %>% summarise( numOps = n() ) %>% arrange( desc(numOps),COD_PROFESION ) %>% head(length(CODPRO_TestNoTrain1)))$COD_PROFESION )
# ds_Train_base  %>% filter( is.na(COD_PROFESION) ) 
idxTrainChange <- c()
for( CP_train in CODPRO_Train4Change1 ){
  for (i in 1:nrow(ds_Train_base)) {
    if( as.character(ds_Train_base[i,c("DEFAULT")]) == "1" & as.character(ds_Train_base[i,c("COD_PROFESION")]) == CP_train ){
      idxTrainChange <- c(idxTrainChange,i); break
    }
  }  
}
idxTestChange <- c()
for( CP_test in CODPRO_TestNoTrain1 ){
  for (i in 1:nrow(ds_Test)) {
    if( as.character(ds_Test[i,c("DEFAULT")]) == "1" & as.character(ds_Test[i,c("COD_PROFESION")]) == CP_test ){
      idxTestChange <- c(idxTestChange,i); break
    }
  }  
}
ds_Train_base <- rbind(ds_Train_base,ds_Test[idxTestChange,])
ds_Test <- rbind(ds_Test,ds_Train_base[idxTrainChange,])
ds_Train_base <- ds_Train_base[!(1:nrow(ds_Train_base)) %in% idxTrainChange,]
ds_Test <- ds_Test[!(1:nrow(ds_Test)) %in% idxTestChange,]

# as.character((ds_Test %>% select(COD_PROFESION) %>% distinct() %>% filter( !COD_PROFESION %in% ds_Train_base$COD_PROFESION ))$COD_PROFESION)
# nrow(ds_Train_base) + nrow(ds_Test)
# rbind( (ds_Train_base %>% select(OPERACION)) , (ds_Test %>% select(OPERACION)) ) %>% select(OPERACION) %>% distinct() %>% nrow()
# table(ds_Train_base$DEFAULT)
# table(ds_Test$DEFAULT)






#### 3. Análisis univariante.- para la generación de rangos de valores por cada atributo, respecto al Target ####

# Rangos
# # bins$CARGO # method = "chimerge",
# breaks_list = list(
#   CARGO = c("1%,%5%,%7%,%9%,%12","2%,%4%,%6%,%8","3%,%10%,%11"),
#   ESTADO_CIVIL = c("1%,%4","2%,%3","5"),
#   DEPARTAMENTO = c("SANTA CRUZ%,%BENI","LA PAZ%,%ORURO%,%PANDO","CHUQUISACA%,%COCHABAMBA%,%TARIJA")
#   , MONTO_OPERACION = c(22850,27940,50722)
#   # , NRO_CUOTAS = c(30,35,48,60)
#   # , TOT_ACTIVOS = c(2900,7190,10998,11150)
#   # MONTO_OPERACION = c("[-Inf,22850)","[22850,27940)","[27940,50722)","[50722, Inf)")
#   # NIVEL_INGRESO = c("MENOS DE $600","$600 - $1200","$1201 - $2000%,%$2001 - $5000","MÁS DE $5000","NO INDICADO")
#   # NIVEL_EDUCACION = c("1%,%3%,%4","6%,%7","2","5")
# )
# bins = woebin( ds_Train_base, "DEFAULT", breaks_list = breaks_list, positive = "1", var_skip = c("OPERACION","FECHA_INICIO","SALDO")) # bin_num_limit = 10
bins = woebin( ds_Train_base, "DEFAULT",positive = "1", var_skip = c("OPERACION","FECHA_INICIO","SALDO")) # bin_num_limit = 10
1
# bins$NRO_CUOTAS
# ds_Train_base %>% group_by(FORMA_PAGO) %>% summarise(n())
# ds_Test %>% group_by(FORMA_PAGO) %>% summarise(n())
# library(rpart)
# library(rpart.plot)
# (fit <- rpart(DEFAULT ~ ANTIGUEDAD_LABORAL, data = data, method = "class", parms = list(split = 'information'), control=rpart.control(minsplit=10, minbucket=1, cp=0.001)))
# plot(fit)

# Exclusión de datos por nivel de relavancia
# Information Value de las columnas
resIVcols <- data.frame()
for (col in colnames(ds_Train_base)[!colnames(ds_Train_base) %in% c("DEFAULT","OPERACION","FECHA_INICIO","SALDO","SECTOR_ECONOMICO","FORMA_PAGO")]) {
  # print( col )
  if( nrow(resIVcols) == 0 ) {
    resIVcols <- data.frame( columna = col, IV = bins[[col]][1,c("total_iv")] )
  } else {
    resIVcols <- rbind(resIVcols, data.frame( columna = col, IV = bins[[col]][1,c("total_iv")] ))
  }
}
(resIVcols %>% arrange( desc(total_iv) ))


#### Selección de atributos ####
col_bajaRelevancia <- c()
col_colinealidad <- c()
col_capacidad <- c()

col_bajaRelevancia <- c("NRO_CODEUDORES")
col_colinealidad <- c("log_EDAD","log_MONTO_OPERACION","log_INGRESOS_GASTOS")
# col_bajaRelevancia <- c("FORMA_PAGO",
#                         "TIPO_OPR",
#                         "SECTOR_ECONOMICO",
#                         "CAEDEC",
#                         "FUENTE_INGRESO",
#                         "NRO_CODEUDORES",
#                         "CANTIDAD_HIJOS", 
#                         "EDAD"
#                         # ,"ESTADO_CIVIL"
# )
# col_colinealidad <- c(
#   "TOT_ACTIVOS",
#   "NIVEL_EDUCACION",
#   # "TOT_PASIVOS",
#   # "INGRESOS_GASTOS",
#   # "PASIVO_ACTIVO",
#   "MONTO_OPERACION",
#   "log_EDAD",
#   "log_CUOTA_ING_NETO",
#   "log_MONTO_OPERACION",
#   "log_TOT_ACTIVOS",
#   "log_TOT_PASIVOS",
#   "log_INGRESOS_GASTOS"
# )
# col_capacidad <- c("NIVEL_INGRESO") #, "CARGO","COD_PROFESION"



ds_Train <- ds_Train_base[, colnames(ds_Train_base)[!colnames(ds_Train_base) %in% c(col_bajaRelevancia, col_colinealidad, col_capacidad,"OPERACION","FECHA_INICIO","SALDO","SECTOR_ECONOMICO","FORMA_PAGO")]]
# str(ds_Train)
# str(ds_Train_base)
# str(ds_Test)

# Generación del conjunto de datos para modelado (WOE de cada atributo)
train <- woebin_ply(ds_Train, bins)
# str(train)


#### 4. Modelo de datos.- generación del modelo de datos Logit en base al conjunto de datos WOE ####
modelBase <- glm(DEFAULT ~ ., family = "binomial", data = train)
m_step <- step(modelBase, direction="both", trace=FALSE)
# model <- eval(m_step$call)
model <- eval(modelBase)


#### 5. Validación.- generación de las métricas y gráficos del modelo Logit, y generación del Scorecard ####
# Training Test
pred_train_log <- predict(model, newdata = train, type='response')
pred_train_obj <- prediction(pred_train_log, train$DEFAULT)
perform_train <- performance(pred_train_obj, measure = "phi", x.measure = "cutoff")
plot(perform_train)
# range(pred_train_log)

auc(train$DEFAULT,pred_train_log)
plot(performance(pred_train_obj, measure = "tpr", x.measure = "fpr"))

cutoff <- perform_train@x.values[[1]][which.max(perform_train@y.values[[1]])]
pred_train <- ifelse(pred_train_log > cutoff, 1, 0)
(cm <- caret::confusionMatrix(as.factor(pred_train), train$DEFAULT, positive = "1"))
# data.frame(real = train$DEFAULT, pred = pred_train, prob = pred_train_log) %>% filter( real == 0, pred == 1 )


# Prediccion Test
test <- woebin_ply(ds_Test, bins)
test <- as.data.frame(test) %>% mutate_all(~replace(., is.na(.), 0))
pred_test_log <- predict(model, newdata = test, type='response')
pred_test_obj <- prediction(pred_test_log, test$DEFAULT)
pred_test <- ifelse(pred_test_log > cutoff, 1, 0)
(cm <- caret::confusionMatrix(as.factor(pred_test), test$DEFAULT, positive = "1"))


# Función para generar métricas
ResultadoGrupoMes <- function(dataBase, clasePos){
  claseNeg <- ifelse(clasePos == "1","0","1")
  numReg <- dataBase %>% nrow()
  if(numReg > 0){
    data2 <- dataBase
    data2$Pred <- as.factor(data2$Pred)
    data2$Real <- as.factor(data2$Real)
    
    cm <- confusionMatrix(data2$Pred, data2$Real, positive = clasePos)
    
    #Datos de la matriz de confusión
    nTot <- sum(cm$table)
    nPos <- sum(cm$table[,clasePos])
    nNeg <- sum(cm$table[,claseNeg])
    pPos <- round((nPos / nTot), digits = 4) 
    pNeg <- round((nNeg / nTot), digits = 4) 
    vp <- as.numeric(cm$table[clasePos,clasePos])
    vn <- as.numeric(cm$table[claseNeg,claseNeg])
    fp <- as.numeric(cm$table[clasePos,claseNeg])
    fn <- as.numeric(cm$table[claseNeg,clasePos])
    
    #Metricas tradicionales
    accuracy <- cm$overall["Accuracy"]
    err <- (fp+fn)/(vp+fn+vn+fp)
    sensitivity <- cm$byClass["Sensitivity"] #igual a recall
    specificity <- cm$byClass["Specificity"]
    precision <- cm$byClass["Precision"]
    recall <- cm$byClass["Recall"]
    kappa <- cm$overall["Kappa"]
    lift <- precision/(nPos/(nPos+nNeg))
    auc <- ( 1 + sensitivity - (1 - specificity) ) / 2    # (1 + vp - fp) / 2
    gini <- (2 * auc) - 1
    
    rcResponse <- as.factor(ifelse(data2$Real == clasePos, 1, 0))
    
    resHmeasure <- HMeasure(rcResponse, data2$Score)
    sc_auc <- resHmeasure$metrics$AUC
    sc_gini <- resHmeasure$metrics$Gini
    sc_ks <- resHmeasure$metrics$KS
    
    #Metricas para datos no balanceados
    fScore <- cm$byClass["F1"]
    balanced_accuracy <- cm$byClass["Balanced Accuracy"]
    optimized_accuracy <- accuracy - (abs(specificity-recall)/(specificity+recall))
    mcc <- ((vp*vn)-(fp*fn))/sqrt((vp+fp)*(vp+fn)*(vn+fp)*(vn+fn))
    g <- as.numeric(sqrt(sensitivity*specificity))    
  } else{
    #Matriz de confusion
    nTot <- 0
    pPos <- 0
    pNeg <- 0
    vp <- 0
    vn <- 0
    fp <- 0
    fn <- 0
    #Metricas tradicionales
    accuracy <- 0
    err <- 1
    recall <- 0
    specificity <- 0
    precision <- 0
    lift <- 0
    auc <- 0
    gini <- 0
    # pd_auc <- 0
    # pd_gini <- 0
    # pd_ks <- 0
    sc_auc <- 0
    sc_gini <- 0
    sc_ks <- 0
    #kappa = kappa,
    #Métricas para datos no balanceados
    fScore <- 0
    balanced_accuracy <- 0
    optimized_accuracy <- 0
    mcc <- 0
    g <- 0  
  }
  
  resMetric <- data.frame(
    # grupo = grupo,
    # fecha = fecha,
    #Matriz de confusion
    nTot = nTot,
    pPos = round(pPos, 6), 
    pNeg = round(pNeg, 6), 
    vp = vp,
    vn = vn,
    fp = fp,
    fn = fn,
    #Metricas tradicionales
    acc = round(accuracy, 6), 
    err = round(err, 6),
    recall = round(recall, 6),
    specificity = round(specificity, 6),
    precision = round(precision, 6),
    lift = round(lift, 6),
    auc = round(auc, 6),
    gini = round(gini, 6),
    #kappa = kappa,
    #Metricas para datos no balanceados
    g = round(g, 6),
    bal_acc = round(balanced_accuracy, 6),
    opt_acc = round(optimized_accuracy, 6),
    fScore = round(fScore, 6),
    mcc = round(mcc, 6),
    #Metricas de Score
    sc_auc = round(sc_auc, 6),
    sc_gini = round(sc_gini, 6),
    sc_ks = round(sc_ks, 6)
    #,
    #Metricas de Probabilidad de Default
    # pd_auc = round(pd_auc, 6), 
    # pd_gini = round(pd_gini, 6), 
    # pd_ks = round(pd_ks, 6)
  )
  rownames(resMetric) <- NULL
  ResultadoGrupoMes <- resMetric
}


# Se guarda el modelo y lo necesario para aplicarlo
# save(model, file = "consumaxModel.RData")
# save(cutoff, file = "consumaxCutoff.RData")
# save(bins, file = "consumaxBins.RData")
# save(card, file = "consumaxCard.RData")



# Backtesting propio del Scorecard
# card <- scorecard(bins, model)
# train_score <- scorecard_ply(ds_Train, card, only_total_score=FALSE)
# test_score <-  scorecard_ply(ds_Test, card, only_total_score=FALSE)

# Calibrado de datos
PDO <- 50 # 50 40
POINT0 <- 600 # 600 500
card <- scorecard(bins, model, points0 = POINT0, odds0 = 1/19, pdo = PDO)
train_score <- scorecard_ply(ds_Train, card, only_total_score=FALSE)
test_score <-  scorecard_ply(ds_Test, card, only_total_score=FALSE)
# test_score <- test_score %>% mutate( COD_PROFESION_points = ifelse( !is.na(COD_PROFESION_points), COD_PROFESION_points, 0 ), score = ifelse ( !is.na(score), score , (ESTADO_CIVIL_points + ANTIGUEDAD_LABORAL_points + CARGO_points + TIPO_VIVIENDA_points + GENERO_points + INGRESOS_GASTOS_points + CUOTA_ING_NETO_points + 0 + DEPARTAMENTO_points + TOT_PASIVOS_points + NRO_CUOTAS_points + PASIVO_ACTIVO_points) ) ) %>% 
#   mutate( score = ifelse( score < 300, 300, ifelse( score > 850, 850, score ) ) )
range(train_score$score)
range(test_score$score, na.rm = TRUE)
range(range(train_score$score), range(test_score$score, na.rm = TRUE))



#### 6. Backtesting Train.- generación de métricas y gráficos del Scorecard aplicado al conjunto de datos de entrenamiento ####
# dataTrainBt <- data.frame( Real = train$DEFAULT, Pred = pred_train, Score = train_score$score  )
# metricasTrain <- ResultadoGrupoMes( dataTrainBt, clasePos = "1" )
# # table(dataTrainBt$Real)
# # write.table( dataTrainBt %>% select(Real,Score), file = "métricas//6taIteracionResultadosTrain.csv", sep = ";", quote = FALSE, row.names = FALSE  )
saldo_promedio <- round(mean((ds_Train_base %>% filter( !is.na(SALDO) ))$SALDO),2)
SaldoPromDefault <- (ds_Train_base %>% mutate( SaldoProm = ifelse( !is.na(SALDO) , saldo_promedio , NA )) %>% select(SaldoProm))$SaldoProm
dataTrainBt <- data.frame( Real = train$DEFAULT, Pred = pred_train, Score = train_score$score, Saldo = ds_Train_base$SALDO, SaldoProm = SaldoPromDefault, Operacion = ds_Train_base$OPERACION, LogitProb = pred_train_log )
metricasTrain <- ResultadoGrupoMes( dataTrainBt, clasePos = "1" )
# write.table( dataTrainBt %>% select(Real,Score,Saldo,SaldoProm,Operacion,LogitProb), file = "métricas//6taIteracionResultadosTrainConSaldo.csv", sep = ";", quote = FALSE, row.names = FALSE, na = "")


dataGraficoTrain <- dataTrainBt %>% mutate( Real = ifelse( Real == 1 , "Default", "Vigente" ) )
tituloGraficoTrain <- paste0("Conjunto de datos de Entrenamiento") # "Backtesting - DataSet Train"
rangoIni <- 200 # 300
rangoFin <- 800 # 850
ggplot(dataGraficoTrain, aes(x = Score)) +
  scale_color_manual(values = c("#FF0000","#008000")) +
  scale_fill_manual(values = c("#FF0000","#008000")) +
  geom_density(aes(fill = Real), alpha = 0.3) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle( tituloGraficoTrain ) +
  ylab("Densidad") +
  xlab("Score") +
  labs(fill = "Estado Real") +
  scale_x_continuous(breaks = seq(rangoIni, rangoFin, by = 50), limits = c(rangoIni, rangoFin)) + 
  coord_cartesian(xlim = c(rangoIni, rangoFin))

# ggplot(dataGraficoTrain,aes(x=Score,fill=Real)) + #y=value,
#   geom_bar(stat="bin",position="dodge") +
#   theme_bw() +
#   scale_color_manual(values = c("#FF0000","#008000")) +
#   scale_fill_manual(values = c("#FF0000","#008000")) +
#   geom_density(aes(fill = Real), alpha = 0.3) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   ggtitle( tituloGraficoTrain ) +
#   ylab("Número de operaciones") +
#   xlab("Score") +
#   labs(fill = "Estado Real") +
#   scale_x_continuous(breaks = seq(rangoIni, rangoFin, by = 50), limits = c(rangoIni, rangoFin)) + 
#   coord_cartesian(xlim = c(rangoIni, rangoFin))
# ggplot(dataGraficoTrain %>% filter(!is.na(Saldo)) ,aes(x = Score, y = Saldo, fill = Real)) + #y=value,
#   geom_bar(stat="identity") + #stat="sum" ,position="dodge"
#   theme_bw() +
#   scale_color_manual(values = c("#FF0000","#008000")) +
#   scale_fill_manual(values = c("#FF0000","#008000")) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   ggtitle( tituloGraficoTrain ) +
#   ylab("Saldo de operaciones en Default") +
#   xlab("Score") +
#   labs(fill = "Estado Real") +
#   scale_x_continuous(breaks = seq(rangoIni, rangoFin, by = 50), limits = c(rangoIni, rangoFin)) + 
#   coord_cartesian(xlim = c(rangoIni, rangoFin))
# ggplot(dataGraficoTrain %>% filter(!is.na(SaldoProm)) ,aes(x = Score, y = SaldoProm, fill = Real)) + #y=value,
#   geom_bar(stat="identity") + #stat="sum" ,position="dodge"
#   theme_bw() +
#   scale_color_manual(values = c("#FF0000","#008000")) +
#   scale_fill_manual(values = c("#FF0000","#008000")) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   ggtitle( tituloGraficoTrain ) +
#   ylab("Saldo Promedio de operaciones en Default") +
#   xlab("Score") +
#   labs(fill = "Estado Real") +
#   scale_x_continuous(breaks = seq(rangoIni, rangoFin, by = 50), limits = c(rangoIni, rangoFin)) + 
#   coord_cartesian(xlim = c(rangoIni, rangoFin))


#### 7. Backtesting Test.- generación de métricas y gráficos del Scorecard aplicado al conjunto de datos de prueba ####
# dataTestBt <- data.frame( Real = test$DEFAULT, Pred = pred_test, Score = test_score$score  )
# metricasTest <- ResultadoGrupoMes( dataTestBt, clasePos = "1" )
# # table(dataTestBt$Real)
# # write.table( dataTestBt %>% select(Real,Score) %>% filter(!is.na(Score)), file = "métricas//6taIteracionResultadosTest.csv", sep = ";", quote = FALSE, row.names = FALSE  )
saldo_promedio <- round(mean((ds_Test %>% filter( !is.na(SALDO) ))$SALDO),2)
SaldoPromDefault <- (ds_Test %>% mutate( SaldoProm = ifelse( !is.na(SALDO) , saldo_promedio , NA )) %>% select(SaldoProm))$SaldoProm
dataTestBt <- data.frame( Real = test$DEFAULT, Pred = pred_test, Score = test_score$score, Saldo = ds_Test$SALDO, SaldoProm = SaldoPromDefault, Operacion = ds_Test$OPERACION, LogitProb = pred_test_log )
metricasTest <- ResultadoGrupoMes( dataTestBt, clasePos = "1" )
# write.table( dataTestBt %>% select(Real,Score,Saldo,SaldoProm,Operacion,LogitProb), file = "métricas//6taIteracionResultadosTestConSaldo.csv", sep = ";", quote = FALSE, row.names = FALSE, na = "")


# str(dataTestBt)
# cbind(ds_Test, dataTestBt[,c("Pred","Score")]) %>% filter( Score >= 620, DEFAULT == 1 )
# cbind(ds_Test, dataTestBt[,c("Pred","Score")]) %>% filter(  DEFAULT == 1, Pred == 1 )

metricasTrain %>% select(nTot, pPos, pNeg, vp, vn, fp, fn, sc_gini, sc_ks, sc_auc)
metricasTest %>% select(nTot, pPos, pNeg, vp, vn, fp, fn, sc_gini, sc_ks, sc_auc)

dataGraficoTest <- dataTestBt %>% mutate( Real = ifelse( Real == 1 , "Default", "Vigente" ) )
tituloGraficoTest <- paste0("Conjunto de datos de Prueba")
rangoIni <- 200 # 300
rangoFin <- 800 # 850
# tituloGraficoTest <- "Logit - Scorecard - Periodo de 14 meses"

ggplot(dataGraficoTest, aes(x = Score)) +
  scale_color_manual(values = c("#FF0000","#008000")) +
  geom_density(aes(fill = Real), alpha = 0.3) +
  scale_fill_manual(values = c("#FF0000","#008000")) +
  ggtitle( tituloGraficoTest ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Densidad") +
  xlab("Score") +
  labs(fill = "Estado Real") +
  scale_x_continuous(breaks = seq(rangoIni, rangoFin, by = 50), limits = c(rangoIni, rangoFin)) + 
  coord_cartesian(xlim = c(rangoIni, rangoFin))
# ggplot(dataGraficoTest,aes(x=Score,fill=Real)) + #y=value,
#   geom_bar(stat="bin",position="dodge") +
#   theme_bw() +
#   scale_color_manual(values = c("#FF0000","#008000")) +
#   scale_fill_manual(values = c("#FF0000","#008000")) +
#   geom_density(aes(fill = Real), alpha = 0.3) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   ggtitle( tituloGraficoTest ) +
#   ylab("Número de operaciones") +
#   xlab("Score") +
#   labs(fill = "Estado Real") +
#   scale_x_continuous(breaks = seq(rangoIni, rangoFin, by = 50), limits = c(rangoIni, rangoFin)) + 
#   coord_cartesian(xlim = c(rangoIni, rangoFin))
# ggplot(dataGraficoTest %>% filter(!is.na(Saldo)) ,aes(x = Score, y = Saldo, fill = Real)) + #y=value,
#   geom_bar(stat="identity") + #stat="sum" ,position="dodge"
#   theme_bw() +
#   scale_color_manual(values = c("#FF0000","#008000")) +
#   scale_fill_manual(values = c("#FF0000","#008000")) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   ggtitle( tituloGraficoTest ) +
#   ylab("Saldo de operaciones en Default") +
#   xlab("Score") +
#   labs(fill = "Estado Real") +
#   scale_x_continuous(breaks = seq(rangoIni, rangoFin, by = 50), limits = c(rangoIni, rangoFin)) + 
#   coord_cartesian(xlim = c(rangoIni, rangoFin))
# 
# ggplot(dataGraficoTest %>% filter(!is.na(SaldoProm)) ,aes(x = Score, y = SaldoProm, fill = Real)) + #y=value,
#   geom_bar(stat="identity") + #stat="sum" ,position="dodge"
#   theme_bw() +
#   scale_color_manual(values = c("#FF0000","#008000")) +
#   scale_fill_manual(values = c("#FF0000","#008000")) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   ggtitle( tituloGraficoTest ) +
#   ylab("Saldo Promedio de operaciones en Default") +
#   xlab("Score") +
#   labs(fill = "Estado Real") +
#   scale_x_continuous(breaks = seq(rangoIni, rangoFin, by = 50), limits = c(rangoIni, rangoFin)) + 
#   coord_cartesian(xlim = c(rangoIni, rangoFin))



# Prueba con el total 
# dataTotal <- read.csv("consumaxDatosEtiquetadosConSaldos36meses90dias.csv", sep = "|", dec = ".", stringsAsFactors = FALSE)
# dataTotal <- dataTotal %>%
#   mutate( PASIVO_ACTIVO = ifelse(TOT_PASIVOS > 0 & TOT_ACTIVOS > 0, round(TOT_PASIVOS / TOT_ACTIVOS,2), 0) ) %>%
#   select( ESTADO_CIVIL, ANTIGUEDAD_LABORAL, CARGO, TIPO_VIVIENDA, GENERO, INGRESOS_GASTOS, CUOTA_ING_NETO, COD_PROFESION, DEPARTAMENTO, TOT_PASIVOS, NRO_CUOTAS, PASIVO_ACTIVO, DEFAULT )
# 
# # dataTotal <- data
# # total <- woebin_ply(dataTotal, bins)
# # pred_Total_log <- predict(model, newdata = total, type='response')
# # pred_Total_obj <- prediction(pred_Total_log, total$DEFAULT)
# # pred_Total <- ifelse(pred_Total_log > cutoff, 1, 0)
# 
# dataTotal_score <- scorecard_ply(dataTotal, card, only_total_score = FALSE)
# # Se agrega el Score al conjunto de datos y se reemplaza COD_PROFESION_points NA con 0
# # dataTotal_score %>% filter( COD_PROFESION_points == 0 )
# dataTotal_score <- dataTotal_score %>% mutate( COD_PROFESION_points = ifelse( !is.na(COD_PROFESION_points), COD_PROFESION_points, 0 ), score = ifelse ( !is.na(score), score , (ESTADO_CIVIL_points + ANTIGUEDAD_LABORAL_points + CARGO_points + TIPO_VIVIENDA_points + GENERO_points + INGRESOS_GASTOS_points + CUOTA_ING_NETO_points + 0 + DEPARTAMENTO_points + TOT_PASIVOS_points + NRO_CUOTAS_points + PASIVO_ACTIVO_points) ) )
# dataTotal <- dataTotal %>% cbind( data.frame(score = dataTotal_score$score) )
# # Se ajustan los outliers con Score < 300 > 850
# # dataTotal %>% filter(score < 300 | score > 850)
# dataTotal <- dataTotal %>% mutate( score = ifelse( score < 300, 300, ifelse( score > 850, 850, score ) ) )
# # Se grafican los resultados
# # range(dataTotal$score, na.rm = TRUE)
# 
# # dataTotalBt <- data.frame( Real = dataTotal$DEFAULT, Pred = dataTotal$DEFAULT, Score = dataTotal$score )
# # # write.table( dataTotalBt %>% select(Real,Score), file = "métricas//6taIteracionResultadosTotal.csv", sep = ";", quote = FALSE, row.names = FALSE  )
# # # dataTotalBt %>% nrow()
# # # dataTotalBt %>% filter( Score <= 300, Real ==  0) %>% nrow()
# 
# saldo_promedio <- round(mean((dataTotal %>% filter( !is.na(SALDO) ))$SALDO),2)
# SaldoPromDefault <- (dataTotal %>% mutate( SaldoProm = ifelse( !is.na(SALDO) , saldo_promedio , NA )) %>% select(SaldoProm))$SaldoProm
# dataTotalBt <- data.frame( Real = dataTotal$DEFAULT, Pred = dataTotal$DEFAULT, Score = dataTotal$score, Saldo = dataTotal$SALDO, SaldoProm = SaldoPromDefault, Operacion = dataTotal$OPERACION, LogitProb = pred_Total_log )
# # write.table( dataTotalBt %>% select(Real,Score,Saldo,SaldoProm,Operacion,LogitProb), file = "métricas//6taIteracionResultadosTotalConSaldo.csv", sep = ";", quote = FALSE, row.names = FALSE, na = "")
# 
# 
# (dataTotalMetricas <- ResultadoGrupoMes( dataTotalBt, clasePos = "1" ))
# dataTotalGrafico <- dataTotalBt %>% mutate( Real = ifelse( Real == 1 , "Default", "Vigente" ) )
# ggplot(dataTotalGrafico, aes(x = Score)) +
#   scale_color_manual(values = c("#FF0000","#008000")) +
#   geom_density(aes(fill = Real), alpha = 0.3) +
#   scale_fill_manual(values = c("#FF0000","#008000")) +
#   ggtitle( "Conjunto de datos Consumax - Vista Minable" ) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   ylab("Densidad") +
#   labs(fill = "Estado Real") +
#   scale_x_continuous(breaks = seq(300, 850, by = 50), limits = c(300, 850)) +
#   coord_cartesian(xlim = c(300, 850)) # + expand_limits(x = 0, y = 0)
# 
# ggplot(dataTotalGrafico,aes(x=Score,fill=Real)) + #y=value,
#   geom_bar(stat="bin",position="dodge") +
#   theme_bw() +
#   scale_color_manual(values = c("#FF0000","#008000")) +
#   scale_fill_manual(values = c("#FF0000","#008000")) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   ggtitle( "Conjunto de datos Consumax - Vista Minable" ) +
#   ylab("Número de operaciones") +
#   xlab("Score") +
#   labs(fill = "Estado Real") +
#   scale_x_continuous(breaks = seq(300, 850, by = 50), limits = c(300, 850)) +
#   coord_cartesian(xlim = c(300, 850))
# 
# ggplot(dataTotalGrafico %>% filter(!is.na(Saldo)) ,aes(x = Score, y = Saldo, fill = Real)) + #y=value,
#   geom_bar(stat="identity") + #stat="sum" ,position="dodge"
#   theme_bw() +
#   scale_color_manual(values = c("#FF0000","#008000")) +
#   scale_fill_manual(values = c("#FF0000","#008000")) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   ggtitle( "Conjunto de datos Consumax - Vista Minable" ) +
#   ylab("Saldo de operaciones en Default") +
#   xlab("Score") +
#   labs(fill = "Estado Real") +
#   scale_x_continuous(breaks = seq(300, 850, by = 50), limits = c(300, 850)) +
#   coord_cartesian(xlim = c(300, 850))
# 
# ggplot(dataTotalGrafico %>% filter(!is.na(SaldoProm)) ,aes(x = Score, y = SaldoProm, fill = Real)) + #y=value,
#   geom_bar(stat="identity") + #stat="sum" ,position="dodge"
#   theme_bw() +
#   scale_color_manual(values = c("#FF0000","#008000")) +
#   scale_fill_manual(values = c("#FF0000","#008000")) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   ggtitle( "Conjunto de datos Consumax - Vista Minable" ) +
#   ylab("Saldo Promedio de operaciones en Default") +
#   xlab("Score") +
#   labs(fill = "Estado Real") +
#   scale_x_continuous(breaks = seq(300, 850, by = 50), limits = c(300, 850)) +
#   coord_cartesian(xlim = c(300, 850))


