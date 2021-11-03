rm(list = ls())
library(openxlsx)
library(dplyr)
library(lubridate)
library(ggplot2)
library(caret)
library(hmeasure)
library(GoodmanKruskal)
library(FSelector)
library(scorecard)
library(cutpointr)

#Se carga el modelo
setwd('C://DatosBGA//ScoreAdmisionTarjetas//07-MetodoUsoScoreInfocredTarjetas')
options(scipen=999)

#### 1. Se carga el modelo del Score Consumax (model, cutoff, bins y card) ####
load("C://DatosBGA//ScoreAdmisionTarjetas//05-Modelado//tarjetasModel.RData")
load("C://DatosBGA//ScoreAdmisionTarjetas//05-Modelado//tarjetasCutoff.RData")
load("C://DatosBGA//ScoreAdmisionTarjetas//05-Modelado//tarjetasBins.RData")
load("C://DatosBGA//ScoreAdmisionTarjetas//05-Modelado//tarjetasCard.RData")


#### 2. Se cargan los datos de admisiones etiquetadas ####
dataTotal <- read.csv("C://DatosBGA//ScoreAdmisionTarjetas//02-DatosAdmision//tarjetaDatosEtiquetadosConSaldos36meses90dias.csv", sep = "|", dec = ".", stringsAsFactors = FALSE)
dataTotal <- dataTotal %>%
  mutate( NIVEL_INGRESO = ifelse( NIVEL_INGRESO == "", "NO INDICADO", NIVEL_INGRESO ), MONTO_OPERACION = MONTO_DES_ORI, FECHA_INICIO = as.Date(FECHA_INICIO), SALDO = round(SALDO) ) %>%
  select( EDAD, NIVEL_EDUCACION, ANTIGUEDAD_LABORAL, GENERO, MONTO_OPERACION, INGRESOS_GASTOS, COD_PROFESION, DEPARTAMENTO, TOT_ACTIVOS, NIVEL_INGRESO, DEFAULT, OPERACION, FECHA_INICIO, SALDO )

#### 3. Se aplica el Score de Tarjetas ####
# Se aplica el modelo
dataTotal_logitProb <- predict(model, newdata = woebin_ply(dataTotal, bins), type='response')
dataTotal_logitPred <- ifelse(dataTotal_logitProb > cutoff, 1, 0)
# Se aplica el Scorecard
dataTotal_score <- scorecard_ply(dataTotal, card, only_total_score = FALSE)
# sapply(dataTotal_score, function(x) sum(is.na(x)))

# Se subsana la ausencia de puntajes de COD_PROFESION
dataTotal_score <- dataTotal_score %>% 
  mutate( COD_PROFESION_points = ifelse( !is.na(COD_PROFESION_points), COD_PROFESION_points, 0 ) ) %>% 
  mutate( score = ifelse ( !is.na(score), score , ( card$basepoints$points + EDAD_points + NIVEL_EDUCACION_points + ANTIGUEDAD_LABORAL_points + GENERO_points + MONTO_OPERACION_points + INGRESOS_GASTOS_points + COD_PROFESION_points + DEPARTAMENTO_points + TOT_ACTIVOS_points + NIVEL_INGRESO_points ) ) )

# Se agrega el puntaje de Score a los datos
dataTotal <- dataTotal %>% cbind( data.frame(score = dataTotal_score$score) )
# Se limita el puntaje del Score al rango indicado (300 - 850)
dataTotal <- dataTotal %>% mutate( score = ifelse( score < 300, 300, ifelse( score > 850, 850, score ) ) )
# Se preparan los datos para la evaluación
dataTotal <- dataTotal %>% cbind( data.frame( Pred = dataTotal_logitPred ) ) %>% cbind( data.frame( LogitProb = dataTotal_logitProb ) ) 
dataEval <- dataTotal %>% mutate( Real = as.factor(DEFAULT), ScoreTarjetas = score, Saldo = SALDO, Operacion = OPERACION, Fecha = FECHA_INICIO, Monto = MONTO_OPERACION, Anio = year(FECHA_INICIO) ) %>% 
  select( Real, Pred, ScoreTarjetas, Saldo, Monto, Operacion, LogitProb, Fecha, Anio )
# str(dataEval)


#### 4. Se recupera y agrega el Score de Infocred ####
dataInfocred <- read.csv("C://DatosBGA//ScoreAdmisionTarjetas//06-ScoreInfocred//relacionTarjetasInfocred.csv", sep = ";", dec = ".", stringsAsFactors = FALSE)
dataInfocred <- dataInfocred %>% mutate( Operacion = OPERACION, ScoreInfocred = icScore ) %>% select(Operacion, ScoreInfocred)

dataEval <- dataEval %>%
  left_join( dataInfocred, by = "Operacion" ) %>% 
  mutate( Monto = round( (Monto/6.86) , 2), Saldo = round( (Saldo/6.86) , 2), Default = Real ) %>% 
  select( Operacion, Monto, Saldo, Fecha, Anio, ScoreTarjetas, ScoreInfocred, Default )


# Función para la generación de métricas
ResultadoMetricas <- function(dataBase, clasePos){
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
    #Datos de la matriz de confusión de saldos
    sa_vp <- as.numeric( (data2 %>% filter( Pred == "1", Real == "1" ) %>% summarise( sumSaldo = sum(Saldo, na.rm=TRUE)))$sumSaldo )
    sa_fn <- as.numeric( (data2 %>% filter( Pred == "0", Real == "1" ) %>% summarise( sumSaldo = sum(Saldo, na.rm=TRUE)))$sumSaldo )
    sa_vp_por <- ifelse( (sa_vp + sa_fn) > 0 , round( sa_vp / (sa_vp + sa_fn), 4), 0)
    # saPro_vp <- as.numeric( (data2 %>% filter( Pred == "1", Real == "1" ) %>% summarise( sumSaldo = sum(SaldoProm, na.rm=TRUE)))$sumSaldo )
    # saPro_fn <- as.numeric( (data2 %>% filter( Pred == "0", Real == "1" ) %>% summarise( sumSaldo = sum(SaldoProm, na.rm=TRUE)))$sumSaldo )
    mo_p <- as.numeric( (data2 %>% filter( Pred == "1" ) %>% summarise( sumMonto = sum(Monto, na.rm=TRUE)))$sumMonto )
    mo_n <- as.numeric( (data2 %>% filter( Pred == "0" ) %>% summarise( sumMonto = sum(Monto, na.rm=TRUE)))$sumMonto )

    mo_vp <- as.numeric( (data2 %>% filter( Pred == "1", Real == "1" ) %>% summarise( sumMonto = sum(Monto, na.rm=TRUE)))$sumMonto )
    mo_vn <- as.numeric( (data2 %>% filter( Pred == "0", Real == "0" ) %>% summarise( sumMonto = sum(Monto, na.rm=TRUE)))$sumMonto )
    mo_fn <- as.numeric( (data2 %>% filter( Pred == "0", Real == "1" ) %>% summarise( sumMonto = sum(Monto, na.rm=TRUE)))$sumMonto )
    mo_fp <- as.numeric( (data2 %>% filter( Pred == "1", Real == "0" ) %>% summarise( sumMonto = sum(Monto, na.rm=TRUE)))$sumMonto )
    
        
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
    #Datos de la matriz de confusión de saldos
    sa_vp <- 0
    sa_fn <- 0
    sa_vp_por <- 0
    # saPro_vp <- 0
    # saPro_fn <- 0
    mo_p <- 0
    mo_n <- 0
    mo_vp <- 0
    mo_vn <- 0
    mo_fn <- 0
    mo_fp <- 0
    
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
    nPos = nPos,
    nNeg = nNeg,
    pPos = round(pPos, 6), 
    pNeg = round(pNeg, 6), 
    vp = vp,
    vn = vn,
    fp = fp,
    fn = fn,
    #Datos de la matriz de confusión de saldos
    sa_vp = sa_vp,
    sa_fn = sa_fn,
    sa_vp_por = sa_vp_por,
    # saPro_vp = saPro_vp,
    # saPro_fn = saPro_fn,
    mo_p = mo_p,
    mo_n = mo_n,
    mo_vp = mo_vp,
    mo_vn = mo_vn,
    mo_fn = mo_fn,
    mo_fp = mo_fp,
    
    #Metricas tradicionales
    acc = round(accuracy, 6), 
    err = round(err, 6),
    
    # recall = round(recall, 6),
    sensitivity = round(sensitivity, 6),
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
  ResultadoMetricas <- resMetric
}


#### 5. Generación de métricas del Score de Infocred por puntos de corte ####
puntosCorte <- sort( c( seq(310, 840, by = 10) ) ) #, puntoCorteOptimo
resultadoInfocred <- data.frame()
for (pc in puntosCorte) {
  dataEval_pc <- dataEval %>% filter( !is.na(ScoreInfocred) ) %>% mutate( Real = Default, Pred = ifelse( ScoreInfocred <= pc, "1", "0" ), Score = ScoreInfocred )
  dataMetric <- cbind( puntoCorte = pc , ResultadoMetricas( dataEval_pc, clasePos = "1" )) %>% 
    select(puntoCorte, acc, sensitivity, specificity, precision, vp, vn, fp, fn, sa_vp, sa_fn, sa_vp_por, sc_gini, sc_ks, sc_auc, sa_vp_por, mo_p, mo_n) # , saPro_vp, saPro_fn
  if( nrow(resultadoInfocred) == 0 ) {
    resultadoInfocred <- dataMetric
  } else {
    resultadoInfocred <- rbind(resultadoInfocred, dataMetric)
  }
}
resultadoInfocred


#### 6. Generación de métricas del Score de Tarjetas por puntos de corte ####
puntosCorte <- sort( c( seq(310, 840, by = 10) ) ) #, puntoCorteOptimo
resultadoTarjetas <- data.frame()
for (pc in puntosCorte) {
  dataEval_pc <- dataEval %>% mutate( Real = Default, Pred = ifelse( ScoreTarjetas <= pc, "1", "0" ), Score = ScoreTarjetas )
  dataMetric <- cbind( puntoCorte = pc , ResultadoMetricas( dataEval_pc, clasePos = "1" )) %>% 
    select(puntoCorte, acc, sensitivity, specificity, precision, vp, vn, fp, fn, sa_vp, sa_fn, sa_vp_por, sc_gini, sc_ks, sc_auc, sa_vp_por, mo_p, mo_n) # , saPro_vp, saPro_fn
  if( nrow(resultadoTarjetas) == 0 ) {
    resultadoTarjetas <- dataMetric
  } else {
    resultadoTarjetas <- rbind(resultadoTarjetas, dataMetric)
  }
}
resultadoTarjetas



#### 7. Cuantificación de la aplicación de los Scores Infocred y de Tarjetas con distintos puntos de corte ####

# Genración de métricas por segmentos
p_anio_ini <- c(2015) # c(2015,2015,2016,2017,2018,2019)
p_anio_fin <- c(2019) # c(2019,2015,2016,2017,2018,2019)
p_corte_ini <- 310
p_corte_fin <- 840
p_corte_tam <- 10

# resutado <- data.frame()

for(i in 1:length(p_anio_ini)){
  param_anio_ini <- p_anio_ini[i]
  param_anio_fin <- p_anio_fin[i]
  for( param_corte_ic in seq(360,750, by = p_corte_tam) ) { # c(100,200)
    resutado <- data.frame()
    # las operaciones del periodo de AÑos
    data_anio <- dataEval %>%
      filter( Anio >= param_anio_ini, Anio <= param_anio_fin ) %>% 
      mutate( Real = Default, Pred = ifelse( ScoreInfocred <= param_corte_ic, "1", "0" ), Score = ScoreInfocred )
    
    # las operaciones con Score Infocred
    data_ic_eval <- data_anio %>% filter ( !is.na(ScoreInfocred) ) 
    data_ic_sinScore <- data_anio %>% filter( is.na( ScoreInfocred ) )
    dataMetric_ic1 <- ResultadoMetricas( data_ic_eval, clasePos = "1" ) # cbind( puntoCorte = param_corte_ic ,  )
    
    # las operaciones sin score Infocred
    resIc1 <- data.frame(
      anioIni = param_anio_ini,
      anioFin = param_anio_fin,
      score = "Infocred",
      pc0 = param_corte_ic,
      pc1 = NA,
      pc2 = NA,
      clasif = "Rechazo",
      num =  data_ic_eval %>% filter( Pred == "1" ) %>% nrow(),
      monto = round( (data_ic_eval %>% filter( Pred == "1" ) %>% summarise( resSuma = sum(Monto, na.rm = FALSE) ))$resSuma ,2),
      saldo = round( (data_ic_eval %>% filter( Pred == "1" ) %>% summarise( resSuma = sum(Saldo, na.rm = TRUE) ))$resSuma ,2),
      d_num = data_ic_eval %>% filter( Pred == "1" ) %>% filter(Real == "1" ) %>% nrow(),
      d_monto = round( (data_ic_eval %>% filter( Pred == "1" ) %>% filter(Real == "1" ) %>% summarise( resSuma = sum(Monto, na.rm = FALSE) ))$resSuma ,2),
      d_saldo = round((data_ic_eval %>% filter( Pred == "1" ) %>% filter(Real == "1" ) %>% summarise( resSuma = sum(Saldo, na.rm = TRUE) ))$resSuma ,2),
      v_num = data_ic_eval %>% filter( Pred == "1" ) %>% filter(Real == "0" ) %>% nrow(),
      v_monto = round( (data_ic_eval %>% filter( Pred == "1" ) %>% filter(Real == "0" ) %>% summarise( resSuma = sum(Monto, na.rm = FALSE) ))$resSuma ,2),
      v_saldo = round( (data_ic_eval %>% filter( Pred == "1" ) %>% filter(Real == "0" ) %>% summarise( resSuma = sum(Saldo, na.rm = TRUE) ))$resSuma ,2),
      pc0_sensi = dataMetric_ic1$sensitivity,
      pc0_espec = dataMetric_ic1$specificity,
      pc1_sensi = NA,
      pc1_espec = NA,
      pc2_sensi = NA,
      pc2_espec = NA,
      gini = dataMetric_ic1$sc_gini,
      ks = dataMetric_ic1$sc_ks,
      auc = dataMetric_ic1$sc_auc
    )
    if( nrow(resutado) == 0 ) {
      resutado <- resIc1
    } else {
      resutado <- rbind(resutado, resIc1)
    }
    resutado <- resutado %>% rbind(
      data.frame(
        anioIni = param_anio_ini,
        anioFin = param_anio_fin,
        score = "Infocred",
        pc0 = param_corte_ic,
        pc1 = NA,
        pc2 = NA,
        clasif = "Continua",
        num =  data_ic_eval %>% filter( Pred == "0" ) %>% nrow(),
        monto = (data_ic_eval %>% filter( Pred == "0" ) %>% summarise( resSuma = sum(Monto, na.rm = FALSE) ))$resSuma,
        saldo = (data_ic_eval %>% filter( Pred == "0" ) %>% summarise( resSuma = sum(Saldo, na.rm = TRUE) ))$resSuma,
        d_num = data_ic_eval %>% filter( Pred == "0" ) %>% filter(Real == "1" ) %>% nrow(),
        d_monto = round( (data_ic_eval %>% filter( Pred == "0" ) %>% filter(Real == "1" ) %>% summarise( resSuma = sum(Monto, na.rm = FALSE) ))$resSuma ,2),
        d_saldo = round( (data_ic_eval %>% filter( Pred == "0" ) %>% filter(Real == "1" ) %>% summarise( resSuma = sum(Saldo, na.rm = TRUE) ))$resSuma ,2),
        v_num = data_ic_eval %>% filter( Pred == "0" ) %>% filter(Real == "0" ) %>% nrow(),
        v_monto = round( (data_ic_eval %>% filter( Pred == "0" ) %>% filter(Real == "0" ) %>% summarise( resSuma = sum(Monto, na.rm = FALSE) ))$resSuma ,2),
        v_saldo = round( (data_ic_eval %>% filter( Pred == "0" ) %>% filter(Real == "0" ) %>% summarise( resSuma = sum(Saldo, na.rm = TRUE) ))$resSuma ,2),
        pc0_sensi = dataMetric_ic1$sensitivity,
        pc0_espec = dataMetric_ic1$specificity,
        pc1_sensi = NA,
        pc1_espec = NA,
        pc2_sensi = NA,
        pc2_espec = NA,
        gini = dataMetric_ic1$sc_gini,
        ks = dataMetric_ic1$sc_ks,
        auc = dataMetric_ic1$sc_auc
      )
    )
    resutado <- resutado %>% rbind(
      data.frame(
        anioIni = param_anio_ini,
        anioFin = param_anio_fin,
        score = "Infocred",
        pc0 = param_corte_ic,
        pc1 = NA,
        pc2 = NA,
        clasif = "Sin Score",
        num =  data_ic_sinScore %>% nrow(),
        monto = round( (data_ic_sinScore %>% summarise( resSuma = sum(Monto, na.rm = FALSE) ))$resSuma ,2),
        saldo = round( (data_ic_sinScore %>% summarise( resSuma = sum(Saldo, na.rm = TRUE) ))$resSuma ,2),
        d_num = data_ic_sinScore %>% filter(Real == "1" ) %>% nrow(),
        d_monto = round( (data_ic_sinScore %>% filter(Real == "1" ) %>% summarise( resSuma = sum(Monto, na.rm = FALSE) ))$resSuma ,2),
        d_saldo = round( (data_ic_sinScore %>% filter(Real == "1" ) %>% summarise( resSuma = sum(Saldo, na.rm = TRUE) ))$resSuma ,2),
        v_num = data_ic_sinScore %>% filter(Real == "0" ) %>% nrow(),
        v_monto = round( (data_ic_sinScore %>% filter(Real == "0" ) %>% summarise( resSuma = sum(Monto, na.rm = FALSE) ))$resSuma ,2),
        v_saldo = round( (data_ic_sinScore %>% filter(Real == "0" ) %>% summarise( resSuma = sum(Saldo, na.rm = TRUE) ))$resSuma ,2),
        pc0_sensi = dataMetric_ic1$sensitivity,
        pc0_espec = dataMetric_ic1$specificity,
        pc1_sensi = NA,
        pc1_espec = NA,
        pc2_sensi = NA,
        pc2_espec = NA,
        gini = dataMetric_ic1$sc_gini,
        ks = dataMetric_ic1$sc_ks,
        auc = dataMetric_ic1$sc_auc
      )
    )
    
    data_Tarjetas_eval <- data_anio %>% filter( Pred == "0" | is.na( ScoreInfocred ) )
    
    for( param_corte_Tarjetas1 in seq(p_corte_ini,p_corte_fin - p_corte_tam, by = p_corte_tam) ) { # c(390,400)
      for (param_corte_Tarjetas2 in seq(param_corte_Tarjetas1 + p_corte_tam, p_corte_fin, by = p_corte_tam) ) { # c(640,650)

        data_Tarjetas_1 <- data_Tarjetas_eval %>% mutate( Pred = ifelse( ScoreTarjetas <= param_corte_Tarjetas1, "1", "0" ), Score = ScoreTarjetas )
        data_Tarjetas_Metric1 <- ResultadoMetricas( data_Tarjetas_1, clasePos = "1" )
        data_Tarjetas_2 <- data_Tarjetas_eval %>% mutate( Pred = ifelse( ScoreTarjetas <= param_corte_Tarjetas2, "1", "0" ), Score = ScoreTarjetas )
        data_Tarjetas_Metric2 <- ResultadoMetricas( data_Tarjetas_2, clasePos = "1" )
        
        resutado <- resutado %>% rbind(
          data.frame(
            anioIni = param_anio_ini,
            anioFin = param_anio_fin,
            score = "Tarjetas",
            pc0 = param_corte_ic,
            pc1 = param_corte_Tarjetas1,
            pc2 = param_corte_Tarjetas2,
            clasif = "Rechazo",
            
            num =  data_Tarjetas_Metric1$vp + data_Tarjetas_Metric1$fp,
            monto = round( data_Tarjetas_Metric1$mo_p, 2),
            saldo = round( data_Tarjetas_Metric1$sa_vp, 2),
            
            d_num = data_Tarjetas_Metric1$vp,
            d_monto = round( data_Tarjetas_Metric1$mo_vp, 2),
            d_saldo = round( data_Tarjetas_Metric1$sa_vp, 2),
            
            v_num = data_Tarjetas_Metric1$fp,
            v_monto = round( data_Tarjetas_Metric1$mo_fp , 2),
            v_saldo = round( 0, 2),
            pc0_sensi = NA,
            pc0_espec = NA,
            pc1_sensi = data_Tarjetas_Metric1$sensitivity,
            pc1_espec = data_Tarjetas_Metric1$specificity,
            pc2_sensi = data_Tarjetas_Metric2$sensitivity,
            pc2_espec = data_Tarjetas_Metric2$specificity,
            gini = data_Tarjetas_Metric1$sc_gini,
            ks = data_Tarjetas_Metric1$sc_ks,
            auc = data_Tarjetas_Metric1$sc_auc
          )
        )
        
        resutado <- resutado %>% rbind(
          data.frame(
            anioIni = param_anio_ini,
            anioFin = param_anio_fin,
            score = "Tarjetas",
            pc0 = param_corte_ic,
            pc1 = param_corte_Tarjetas1,
            pc2 = param_corte_Tarjetas2,
            clasif = "Evaluación",
            
            num =  (data_Tarjetas_Metric1$nTot) - (data_Tarjetas_Metric1$vp + data_Tarjetas_Metric1$fp) - (data_Tarjetas_Metric2$vn + data_Tarjetas_Metric2$fn),
            monto = round( (data_Tarjetas_Metric1$mo_p + data_Tarjetas_Metric1$mo_n) - (data_Tarjetas_Metric1$mo_p) - (data_Tarjetas_Metric2$mo_n) ,2),
            saldo = round( (data_Tarjetas_Metric1$sa_vp + data_Tarjetas_Metric1$sa_fn) - (data_Tarjetas_Metric1$sa_vp) - (data_Tarjetas_Metric2$sa_fn) ,2),
            
            d_num = (data_Tarjetas_Metric1$nPos) - (data_Tarjetas_Metric1$vp) - (data_Tarjetas_Metric2$fn),
            d_monto = round( (data_Tarjetas_eval %>% filter( Real == "1" ) %>% summarise( sumaMonto=sum(Monto)))$sumaMonto - data_Tarjetas_Metric1$mo_vp - data_Tarjetas_Metric2$mo_fn ,2),
            d_saldo = round( (data_Tarjetas_eval %>% filter( Real == "1" ) %>% summarise( sumaSaldo=sum(Saldo)))$sumaSaldo - data_Tarjetas_Metric1$sa_vp - data_Tarjetas_Metric2$sa_fn ,2),
            
            v_num = (data_Tarjetas_Metric1$nNeg) - (data_Tarjetas_Metric1$fp) - (data_Tarjetas_Metric2$vn),
            v_monto = round( (data_Tarjetas_eval %>% filter( Real == "0" ) %>% summarise( sumaMonto=sum(Monto)))$sumaMonto - data_Tarjetas_Metric1$mo_fp - data_Tarjetas_Metric2$mo_vn ,2),
            v_saldo = round( 0 ,2),
            pc0_sensi = NA,
            pc0_espec = NA,
            pc1_sensi = data_Tarjetas_Metric1$sensitivity,
            pc1_espec = data_Tarjetas_Metric1$specificity,
            pc2_sensi = data_Tarjetas_Metric2$sensitivity,
            pc2_espec = data_Tarjetas_Metric2$specificity,
            gini = data_Tarjetas_Metric1$sc_gini,
            ks = data_Tarjetas_Metric1$sc_ks,
            auc = data_Tarjetas_Metric1$sc_auc
          )
        )
        
        resutado <- resutado %>% rbind(
          data.frame(
            anioIni = param_anio_ini,
            anioFin = param_anio_fin,
            score = "Tarjetas",
            pc0 = param_corte_ic,
            pc1 = param_corte_Tarjetas1,
            pc2 = param_corte_Tarjetas2,
            clasif = "Aceptación",
            
            num =  data_Tarjetas_Metric2$vn + data_Tarjetas_Metric2$fn,
            monto = round( data_Tarjetas_Metric2$mo_n ,2),
            saldo = round( data_Tarjetas_Metric2$sa_fn ,2),
            
            d_num = data_Tarjetas_Metric2$fn,
            d_monto = round( data_Tarjetas_Metric2$mo_fn ,2),
            d_saldo = round( data_Tarjetas_Metric2$sa_fn ,2),
            
            v_num = data_Tarjetas_Metric2$vn,
            v_monto = round( data_Tarjetas_Metric2$mo_vn ,2),
            v_saldo = round( 0 ,2),
            pc0_sensi = NA,
            pc0_espec = NA,
            pc1_sensi = data_Tarjetas_Metric1$sensitivity,
            pc1_espec = data_Tarjetas_Metric1$specificity,
            pc2_sensi = data_Tarjetas_Metric2$sensitivity,
            pc2_espec = data_Tarjetas_Metric2$specificity,
            gini = data_Tarjetas_Metric1$sc_gini,
            ks = data_Tarjetas_Metric1$sc_ks,
            auc = data_Tarjetas_Metric1$sc_auc
          )
        )   
        
      }
    }
    write.table(resutado, "tarjetasResultadoCascada2015-2019.csv", sep = ";", dec = ".", quote = FALSE, row.names = FALSE, na = "", col.names = !file.exists("tarjetasResultadoCascada2015-2019.csv"), append = T)
  }
}
# write.table(resutado, file = "tarjetasResultadoCascada2015-2019.csv", sep = ";", dec = ".", quote = FALSE, row.names = FALSE, na = "")






#### 8. Se generan gráficos de Densidad, Nro. operaciones y Saldo ####
dataGraficos <- dataEval %>% mutate( Real = ifelse( Default == 1 , "Default", "Vigente" ) )
subTituloGraficos <- "Operaciones de Tarjetas de Crédito (Oct.2015 - May.2019)"

# Densidad Infocred
subTituloGraficos <- "Titulares Infocred - Op. de Tarjetas de Crédito - Oct.2015 - May.2019"
ggplot(dataGraficos, aes(x = ScoreInfocred)) +
  scale_color_manual(values = c("#FF0000","#008000")) +
  geom_density(aes(fill = Real), alpha = 0.3) +
  scale_fill_manual(values = c("#FF0000","#008000")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Densidad") +
  xlab("Score Infocred") +
  labs(fill = "Estado Real",
       title = "Densidad de Puntajes Score",
       subtitle = subTituloGraficos) +
  scale_x_continuous(breaks = seq(300, 850, by = 50), limits = c(300, 850)) +
  coord_cartesian(xlim = c(300, 850))

# Número de operaciones
subTituloGraficos <- "Titulares Infocred - Op. de Tarjetas de Crédito - Oct.2015 - May.2019"
ggplot(dataGraficos,aes(x=ScoreInfocred,fill=Real)) + #y=value,
  geom_bar(stat="bin",position="dodge") +
  theme_bw() +
  scale_color_manual(values = c("#FF0000","#008000")) +
  scale_fill_manual(values = c("#FF0000","#008000")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número de operaciones") +
  xlab("Score Infocred") +
  labs(fill = "Estado Real",
       title = "Número de Operaciones",
       subtitle = subTituloGraficos) +
  scale_x_continuous(breaks = seq(300, 850, by = 50), limits = c(300, 850)) +
  coord_cartesian(xlim = c(300, 850))


# Densidad de Tarjetas
subTituloGraficos <- "Operaciones de Tarjetas de Crédito (Oct.2015 - May.2019)"
ggplot(dataGraficos, aes(x = ScoreTarjetas)) +
  scale_color_manual(values = c("#FF0000","#008000")) +
  geom_density(aes(fill = Real), alpha = 0.3) +
  scale_fill_manual(values = c("#FF0000","#008000")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Densidad") +
  xlab("Score") +
  labs(fill = "Estado Real",
       title = "Densidad de Puntajes Score",
       subtitle = subTituloGraficos) +
  scale_x_continuous(breaks = seq(300, 850, by = 50), limits = c(300, 850)) +
  coord_cartesian(xlim = c(300, 850))

# Número de operaciones
subTituloGraficos <- "Operaciones de Tarjetas de Crédito (Oct.2015 - May.2019)"
ggplot(dataGraficos,aes(x=ScoreTarjetas,fill=Real)) + #y=value,
  geom_bar(stat="bin",position="dodge") +
  theme_bw() +
  scale_color_manual(values = c("#FF0000","#008000")) +
  scale_fill_manual(values = c("#FF0000","#008000")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Número de operaciones") +
  xlab("Score") +
  labs(fill = "Estado Real",
       title = "Número de Operaciones",
       subtitle = subTituloGraficos) +
  scale_x_continuous(breaks = seq(300, 850, by = 50), limits = c(300, 850)) +
  coord_cartesian(xlim = c(300, 850))

# Saldo
subTituloGraficos <- "Operaciones de Tarjetas de Crédito (Oct.2015 - May.2019)"
ggplot(dataGraficos %>% filter(!is.na(Saldo)) ,aes(x = ScoreTarjetas, y = Saldo, fill = Real)) + #y=value,
  geom_bar(stat="identity") + #stat="sum" ,position="dodge"
  theme_bw() +
  scale_color_manual(values = c("#FF0000","#008000")) +
  scale_fill_manual(values = c("#FF0000","#008000")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Saldo de operaciones en Default") +
  xlab("Score") +
  labs(fill = "Saldo",
       title = "Saldo en Bs. de Operaciones en Default",
       subtitle = subTituloGraficos) +
  scale_x_continuous(breaks = seq(300, 850, by = 50), limits = c(300, 850)) +
  coord_cartesian(xlim = c(300, 850))

