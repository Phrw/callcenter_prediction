
CALLCENTER <- list()
CALLCENTER$clean <- function(CALLCENTER,
                             Y,
                             Datum){
  df = data.frame(val = c(Y,rep(-1,28)))
  df$Datum = as.Date(as.numeric(as.Date(Datum)) -1 + 1:nrow(df))
  df$X = 1:nrow(df) + as.numeric(as.Date(Datum)) - as.numeric(as.Date(CALLCENTER$Datum_train))
  DAY = as.Date(max(df$Datum))
  
  for(i in 1:31){
    df[paste("Y_",i,sep = "")] = 0
    df[(1+i):nrow(df),paste("Y_",i,sep = "")] = df$val[1:(nrow(df) - i)]
  }
  df$day_[(1+nrow(df)%%7):nrow(df)] = weekdays(DAY - (6:0))
  if(sum(is.na(df$day_)) > 0){
    df$day_[1:(nrow(df)%%7)] = df$day_[7 + (1:(nrow(df)%%7))]
  }
  df$day_ = as.character(df$day_)
  for(i in 0:19){
    df[paste("sin_X",i,sep = "")] = 0
    df[,paste("sin_X",i,sep = "")] = sin(2*pi*(df$X +(i*18.25))/365)
  }
  #df$sin_X = sin(2*pi*(df$X +80)/365)
  
  df$month_ = factor(months(as.Date(df$Datum)),levels = c("January","February","March","April","May","June",
                                                          "July","August","September","October","November","December"))
  df$day_nr = as.numeric(substr(df$Datum,9,10))
  df$mean_1_3 = rowMeans(df[,names(df) %in% paste("Y",1:3,sep = "_")])
  df$mean_3_7 = rowMeans(df[,names(df) %in% paste("Y",3:7,sep = "_")])
  df$mean_7_13 = rowMeans(df[,names(df) %in% paste("Y",7:13,sep = "_")])
  df$mean_28_31 = rowMeans(df[,names(df) %in% paste("Y",28:31,sep = "_")])
  df = df[32:nrow(df),]
  return(df)
}

CALLCENTER$Train.model <- function(CALLCENTER,
                                   data,
                                   LONG = F,
                                   tau = c(0.025,0.975)){
  data = data[CALLCENTER$TRAIN,]
  data_stored = data
  W = seq(1,2,1/(nrow(data)-1))
  set.seed(2243412)
  
  
  M = glmnetUtils::cv.glmnet(val ~ . - Datum  +
                               pmin(day_nr,8) + pmin(pmax(day_nr,8),16) + 
                               pmin(pmax(day_nr,16),24) + pmax(day_nr,24) + Y_1:day_,
                             family = poisson(),
                             weights = W,
                             data = data,
                             exact = T,
                             alpha = 1)
  
  
  
  M = CALLCENTER$analyze_glmnet(M)
  M$glmnet.fit$dev.ratio[M$my_lambda == M$lambda]
  
  if(LONG){
    CALLCENTER$Model_P = M
    
    M = glmnetUtils::cv.glmnet(val ~ . - Datum  +
                                 pmin(day_nr,8) + pmin(pmax(day_nr,8),16) +  
                                 pmin(pmax(day_nr,16),24) + pmax(day_nr,24)+ Y_1:day_,
                               family = gaussian(),
                               weights = W,
                               data = data,
                               alpha = 1)
    
    
    M = CALLCENTER$analyze_glmnet(M)
    M$glmnet.fit$dev.ratio[M$my_lambda == M$lambda]
    
    CALLCENTER$Model_G = M
    
    data$pred_p = predict(CALLCENTER$Model_P,
                          data,
                          s = CALLCENTER$Model_P$my_lambda,
                          type = "response")
    
    data$pred_g = predict(CALLCENTER$Model_G,
                          data,
                          s = CALLCENTER$Model_G$my_lambda,
                          type = "response")
    
    M = glmnetUtils::cv.glmnet(val ~ pred_g +pred_p,
                               family = gaussian(),
                               lower.limit = 0,
                               upper.limit = 1,
                               weights = W,
                               data = data,
                               alpha = 1)
    M = CALLCENTER$analyze_glmnet(M,1) 
  }
  
  CALLCENTER$Model = M
  
  data$pred = predict(CALLCENTER$Model,
                      data,
                      s = CALLCENTER$Model$my_lambda,
                      type = "response")
  data$error = data$val - data$pred
  # data$error_1 = -1
  # data$error_1[2:nrow(data)] = data$error[1:(nrow(data)-1)]
  # M = glm(val ~ pred,
  #        data = data,
  #        family = poisson())
  if(LONG){
    M = rq(error ~ day_ + pred_p + pred_g + Y_1,
           tau = tau,
           data = data) 
  }else{
    M = rq(error ~ day_ + pred + Y_1,
           tau = tau,
           data = data)
  }
  M$model = M$y = M$x = M$residuals = M$fitted.values = NULL
  CALLCENTER$Model.quantile = M
  
  
  
  # 3
  data_3 = data_stored[,!(names(data_stored) %in% paste("Y_",1:2,sep = ""))]
  data_3$mean_1_3 =  NULL
  M = glmnetUtils::cv.glmnet(val ~ .- Datum  +
                               pmin(day_nr,8) + pmin(pmax(day_nr,8),16) + 
                               pmin(pmax(day_nr,16),24) + pmax(day_nr,24) + Y_3:day_,
                             family = poisson(),
                             weights = W,
                             data = data_3,
                             alpha = 1)
  
  
  M = CALLCENTER$analyze_glmnet(M)
  M$glmnet.fit$dev.ratio[M$my_lambda == M$lambda]
  
  if(LONG){
    CALLCENTER$Model_P3 = M
    
    M = glmnetUtils::cv.glmnet(val ~ . - Datum  +
                                 pmin(day_nr,8) + pmin(pmax(day_nr,8),16) + 
                                 pmin(pmax(day_nr,16),24) + pmax(day_nr,24) + Y_3:day_,
                               family = gaussian(),
                               weights = W,
                               data = data_3,
                               alpha = 1)
    
    
    M = CALLCENTER$analyze_glmnet(M)
    M$glmnet.fit$dev.ratio[M$my_lambda == M$lambda]
    
    CALLCENTER$Model_G3 = M
    
    data_3$pred_p = predict(CALLCENTER$Model_P3,
                            data_3,
                            s = CALLCENTER$Model_P3$my_lambda,
                            type = "response")
    
    data_3$pred_g = predict(CALLCENTER$Model_G3,
                            data_3,
                            s = CALLCENTER$Model_G3$my_lambda,
                            type = "response")
    M = glmnetUtils::cv.glmnet(val ~ pred_g+pred_p,
                               family = gaussian(),
                               lower.limit = 0,
                               upper.limit = 1,
                               weights = W,
                               data = data_3,
                               alpha = 1)
    M = CALLCENTER$analyze_glmnet(M,1)
  }
  
  CALLCENTER$Model3 = M
  
  data_3$pred_3 = predict(CALLCENTER$Model3,
                          data_3,
                          s = CALLCENTER$Model3$my_lambda,
                          type = "response")
  
  data_3$error = data_3$val - data_3$pred_3
  if(LONG){
    M = rq(error ~ day_ + pred_p + pred_g + Y_3,
           tau = tau,
           data = data_3)
  }else{
    M = rq(error ~ day_ + pred_3 + Y_3,
           tau = tau,
           data = data_3)
  }
  M$model = M$y = M$x = M$residuals = M$fitted.values = NULL
  
  CALLCENTER$Model.quantile3 = M
  
  
  
  
  
  
  
  
  
  data_7 = data_stored[,!(names(data_stored) %in% paste("Y_",1:6,sep = ""))]
  data_7$mean_1_3 = data_7$mean_3_7 = NULL
  M = glmnetUtils::cv.glmnet(val ~ .- Datum  +
                               pmin(day_nr,8) + pmin(pmax(day_nr,8),16) + 
                               pmin(pmax(day_nr,16),24) + pmax(day_nr,24) + Y_7:day_,
                             family = poisson(),
                             weights = W,
                             data = data_7,
                             alpha = 1)
  
  
  M = CALLCENTER$analyze_glmnet(M)
  M$glmnet.fit$dev.ratio[M$my_lambda == M$lambda]
  
  if(LONG){
    CALLCENTER$Model_P7 = M
    
    M = glmnetUtils::cv.glmnet(val ~ . - Datum  +
                                 pmin(day_nr,8) + pmin(pmax(day_nr,8),16) + 
                                 pmin(pmax(day_nr,16),24) + pmax(day_nr,24) + Y_7:day_,
                               family = gaussian(),
                               weights = W,
                               data = data_7,
                               alpha = 1)
    
    
    M = CALLCENTER$analyze_glmnet(M)
    M$glmnet.fit$dev.ratio[M$my_lambda == M$lambda]
    
    CALLCENTER$Model_G7 = M
    
    data_7$pred_p = predict(CALLCENTER$Model_P7,
                            data_7,
                            s = CALLCENTER$Model_P7$my_lambda,
                            type = "response")
    
    data_7$pred_g = predict(CALLCENTER$Model_G7,
                            data_7,
                            s = CALLCENTER$Model_G7$my_lambda,
                            type = "response")
    M = glmnetUtils::cv.glmnet(val ~ pred_g+pred_p,
                               family = gaussian(),
                               lower.limit = 0,
                               upper.limit = 1,
                               weights = W,
                               data = data_7,
                               alpha = 1)
    M = CALLCENTER$analyze_glmnet(M,1)
  }
  CALLCENTER$Model7 = M
  
  data_7$pred_7 = predict(CALLCENTER$Model7,
                          data_7,
                          s = CALLCENTER$Model7$my_lambda,
                          type = "response")
  
  data_7$error = data_7$val - data_7$pred_7
  
  if(LONG){
    M = rq(error ~ day_ + pred_p + pred_g + Y_7,
           tau = tau,
           data = data_7)
  }else{
    M = rq(error ~ day_ + pred_7 + Y_7,
           tau = tau,
           data = data_7)
  }
  M$model = M$y = M$x = M$residuals = M$fitted.values = NULL
  
  CALLCENTER$Model.quantile7 = M
  
  
  
  data_28 = data_stored[,!(names(data_stored) %in% paste("Y_",1:27,sep = ""))]
  data_28$mean_1_3 = data_28$mean_3_7 = data_28$mean_7_13 = NULL  
  
  
  M = glmnetUtils::cv.glmnet(val ~ .- Datum  +
                               pmin(day_nr,8) + pmin(pmax(day_nr,8),16) + 
                               pmin(pmax(day_nr,16),24) + pmax(day_nr,24)  + Y_28:day_,
                             family = poisson(),
                             weights = W,
                             data = data_28,
                             alpha = 1)
  
  
  M = CALLCENTER$analyze_glmnet(M)
  M$glmnet.fit$dev.ratio[M$my_lambda == M$lambda]
  
  if(LONG){
    CALLCENTER$Model_P28 = M
    
    M = glmnetUtils::cv.glmnet(val ~ . - Datum  +
                                 pmin(day_nr,8) + pmin(pmax(day_nr,8),16) + 
                                 pmin(pmax(day_nr,16),24) + pmax(day_nr,24)  + Y_28:day_,
                               family = gaussian(),
                               weights = W,
                               data = data_28,
                               alpha = 1)
    
    
    M = CALLCENTER$analyze_glmnet(M)
    M$glmnet.fit$dev.ratio[M$my_lambda == M$lambda]
    
    CALLCENTER$Model_G28 = M
    
    data_28$pred_p = predict(CALLCENTER$Model_P28,
                             data_28,
                             s = CALLCENTER$Model_P28$my_lambda,
                             type = "response")
    
    data_28$pred_g = predict(CALLCENTER$Model_G28,
                             data_28,
                             s = CALLCENTER$Model_G28$my_lambda,
                             type = "response")
    
    
    M = glmnetUtils::cv.glmnet(val ~ pred_g+pred_p,
                               family = gaussian(),
                               lower.limit = 0,
                               upper.limit = 1,
                               weights = W,
                               data = data_28,
                               alpha = 1)
    M = CALLCENTER$analyze_glmnet(M,1)
  }
  CALLCENTER$Model28 = M
  
  data_28$pred_28 = predict(CALLCENTER$Model28,
                            data_28,
                            s = CALLCENTER$Model28$my_lambda,
                            type = "response")
  
  data_28$error = data_28$val - data_28$pred_28
  
  if(LONG){
    M = rq(error ~ day_ + pred_p + pred_g + Y_28,
           tau = tau,
           data = data_28)
  }else{
    M = rq(error ~ day_ + pred_28 + Y_28,
           tau = tau,
           data = data_28)
  }
  M$model = M$y = M$x = M$residuals = M$fitted.values = NULL
  CALLCENTER$Model.quantile28 = M
  
  M = data_stored = data_28 = data_7 = data_3 = data = W = NULL
  return(CALLCENTER)
}

CALLCENTER$predict <- function(CALLCENTER,
                               data,
                               LONG = F){
  
  data$pred = data$pred_p = data$pred_g = data$pred_7 = data$pred_28 = data$pred_UQ = data$pred_LQ = 
    data$pred_UQ7 = data$pred_UQ28 = data$pred_LQ7 = data$pred_LQ28 = 0
  
  # 1
  if(LONG){
    data$pred_p = predict(CALLCENTER$Model_P,
                          data,
                          s = CALLCENTER$Model_P$my_lambda,
                          type = "response")
    
    data$pred_g = predict(CALLCENTER$Model_G,
                          data,
                          s = CALLCENTER$Model_G$my_lambda,
                          type = "response")
  }
  
  data$pred = predict(CALLCENTER$Model,
                      data,
                      s = CALLCENTER$Model$my_lambda,
                      type = "response")
  data$pred_UQ = predict(CALLCENTER$Model.quantile,data)[,2] + data$pred
  data$pred_LQ = predict(CALLCENTER$Model.quantile,data)[,1] + data$pred
  
  
  
  # 3
  if(LONG){
    data$pred_p = predict(CALLCENTER$Model_P3,
                          data,
                          s = CALLCENTER$Model_P3$my_lambda,
                          type = "response")
    
    data$pred_g = predict(CALLCENTER$Model_G3,
                          data,
                          s = CALLCENTER$Model_G3$my_lambda,
                          type = "response")
  }
  
  data$pred_3 = predict(CALLCENTER$Model3,
                        data,
                        s = CALLCENTER$Model3$my_lambda,
                        type = "response")
  data$pred_UQ3 = predict(CALLCENTER$Model.quantile3,data)[,2] + data$pred_3
  data$pred_LQ3 = predict(CALLCENTER$Model.quantile3,data)[,1] + data$pred_3
  
  
  
  
  # 7
  if(LONG){
    data$pred_p = predict(CALLCENTER$Model_P7,
                          data,
                          s = CALLCENTER$Model_P7$my_lambda,
                          type = "response")
    
    data$pred_g = predict(CALLCENTER$Model_G7,
                          data,
                          s = CALLCENTER$Model_G7$my_lambda,
                          type = "response")
  }
  data$pred_7 = predict(CALLCENTER$Model7,
                        data,
                        s = CALLCENTER$Model7$my_lambda,
                        type = "response")
  data$pred_UQ7 = predict(CALLCENTER$Model.quantile7,data)[,2] + data$pred_7
  data$pred_LQ7 = predict(CALLCENTER$Model.quantile7,data)[,1] + data$pred_7
  
  
  # 28
  if(LONG){
    data$pred_p = predict(CALLCENTER$Model_P28,
                          data,
                          s = CALLCENTER$Model_P28$my_lambda,
                          type = "response")
    
    data$pred_g = predict(CALLCENTER$Model_G28,
                          data,
                          s = CALLCENTER$Model_G28$my_lambda,
                          type = "response")
  }
  
  data$pred_28 = predict(CALLCENTER$Model28,
                         data,
                         s = CALLCENTER$Model28$my_lambda,
                         type = "response")
  data$pred_UQ28 = predict(CALLCENTER$Model.quantile28,data)[,2] + data$pred_28
  data$pred_LQ28 = predict(CALLCENTER$Model.quantile28,data)[,1] + data$pred_28
  
  
  
  IND = min(which(data$val == -1))
  # data$Y_1[IND + 1] = data$pred[IND]
  # data$pred[IND + 1] = round(predict(CALLCENTER$Model,
  #                                    data[IND + 1,],
  #                                    type = "response",
  #                                    s = CALLCENTER$Model$my_lambda)[,1],2)
  data$pred[IND + 1:2] = data$pred_3[IND + 1:2]
  data$pred_UQ[IND + 1:2] = data$pred_UQ3[IND + 1:2]
  data$pred_LQ[IND + 1:2] = data$pred_LQ3[IND + 1:2]
  
  data$pred[IND + 3:6] = data$pred_7[IND + 3:6]
  data$pred_UQ[IND + 3:6] = data$pred_UQ7[IND + 3:6]
  data$pred_LQ[IND + 3:6] = data$pred_LQ7[IND + 3:6]
  
  data$pred[IND + 7:27] = data$pred_28[IND + 7:27]
  data$pred_UQ[IND + 7:27] = data$pred_UQ28[IND + 7:27]
  data$pred_LQ[IND + 7:27] = data$pred_LQ28[IND + 7:27]
  # data$error_28 = data$val - data$pred_28
  # 
  # data[,c("q_low_28","q_high_28")] = round(predict(CALLCENTER$Model.quantile_28,
  #                                            data),3)
  # data$within_28 = data$error_28 <= data$q_high_28 & data$error_28 >= data$q_low_28
  
  return(data)
}

CALLCENTER$plot <- function(CALLCENTER,
                            data){
  
  # Error lower bound
  # plot(data$error[order(data$q_low)],
  #      xlab = "Errors sorted by lower bound prediction", ylab = "Error",
  #      main = "Observed error and lower bound prediction",
  #      lwd = 2,
  #      xaxt = "n")
  # lines(data$q_low[order(data$q_low)],
  #       col = "blue",
  #       lwd = 3)
  # legend("topleft", legend = c("Lower bound prediction"), 
  #        col = c("blue"), lty = 1:1, lwd = 3)
  # 
  # 
  # # Error upper bound  
  # plot(data$error[order(data$q_high)],
  #      xlab = "Errors sorted by upper bound prediction", ylab = "Error",
  #      main = "Observed error and upper bound prediction",
  #      lwd = 2,
  #      xaxt = "n")
  # lines(data$q_high[order(data$q_high)],
  #       col = "blue",
  #       lwd = 3)
  # legend("topleft", legend = c("Upper bound prediction"), 
  #        col = c("blue"), lty = 1:1, lwd = 3)
  # 
  # 
  # #Histogram of errors
  # hist(data$error,breaks = 75,
  #      main = "Error distribution",
  #      xlab = "Error",
  #      col = "lightblue")
  # abline(v = quantile(data$error,c(0.025,0.975)),
  #        col = "red",
  #        lwd = 2)
  # legend("topright", legend = c("[0.025, 0.975] quantiles"), 
  #        col = c("red"), lty = 1:1, lwd = 2)
  # 
  #Time series testdata with prediction
  plot(data$X[CALLCENTER$TEST],data$val[CALLCENTER$TEST],
       xlab = "Day (index)", ylab = "Number of obsevations", 
       main = "Observed outcome vs predicted outcome on test data",
       lwd = 2,
       xaxt = "n")
  
  axis(1, at = data$X[CALLCENTER$TEST][c(1,40,80,120)], labels = data$date[CALLCENTER$TEST][c(1,40,80,120)])
  
  lines(data$X[CALLCENTER$TEST],
        data$pred[CALLCENTER$TEST],
        col = "red",
        lwd = 3)
  
  legend("topleft", legend = c("Observed", "Prediction made the day before [Test]"), 
         col = c("black", "red"), lty = 1:1, lwd = 3)
  
  #Time series testdata with prediction
  plot(data$X[CALLCENTER$TEST],data$val[CALLCENTER$TEST],
       xlab = "Day (index)", ylab = "Number of obsevations", 
       main = "Observed outcome vs predicted outcome on test data",
       lwd = 2,
       xaxt = "n")
  
  axis(1, at = data$X[CALLCENTER$TEST][c(1,40,80,120)], labels = data$date[CALLCENTER$TEST][c(1,40,80,120)])
  
  lines(data$X[CALLCENTER$TEST],
        data$pred_28[CALLCENTER$TEST],
        col = "red",
        lwd = 3)
  
  legend("topleft", legend = c("Observed", "Prediction made 28 days before [Test]"), 
         col = c("black", "red"), lty = 1:1, lwd = 3)
  
  
  
  #Time series all data with prediction
  plot(data$X,data$val,
       xlab = "Day (index)", ylab = "Number of obsevations", 
       main = "Observed outcome vs predicted outcome on all data",
       lwd = 2,
       xaxt = "n")
  
  axis(1, at = data$X[c(1,201,401,601)], labels = data$date[c(1,201,401,601)])
  
  lines(data$X[CALLCENTER$TRAIN],
        data$pred_28[CALLCENTER$TRAIN],
        col = "blue",
        lwd = 3)
  lines(data$X[CALLCENTER$TEST],
        data$pred_28[CALLCENTER$TEST],
        col = "red",
        lwd = 3)
  abline(v = min(which(CALLCENTER$TEST)) + min(data$X)- 2,
         col = "red",
         lwd = 2)
  
  legend("topleft", legend = c("Observed", "Prediction made the day before [Train]","Prediction made the day before [Test]"), 
         col = c("black","blue", "red"), lty = 1:1, lwd = 3)
  
  
  
  #Time series all data with prediction
  plot(data$X,data$val,
       xlab = "Day (index)", ylab = "Number of obsevations", 
       main = "Observed outcome vs predicted outcome on all data",
       lwd = 2,
       xaxt = "n")
  
  axis(1, at = data$X[c(1,201,401,601)], labels = data$date[c(1,201,401,601)])
  
  lines(data$X[CALLCENTER$TRAIN],
        data$pred_7[CALLCENTER$TRAIN],
        col = "blue",
        lwd = 3)
  lines(data$X[CALLCENTER$TEST],
        data$pred_7[CALLCENTER$TEST],
        col = "red",
        lwd = 3)
  abline(v = min(which(CALLCENTER$TEST)) + min(data$X)- 2,
         col = "red",
         lwd = 2)
  
  legend("topleft", legend = c("Observed", "Prediction made the day before [Train]","Prediction made the day before [Test]"), 
         col = c("black","blue", "red"), lty = 1:1, lwd = 3)
  
  #Time series all data with prediction
  plot(data$X,data$val,
       xlab = "Day (index)", ylab = "Number of obsevations", 
       main = "Observed outcome vs predicted outcome on all data",
       lwd = 2,
       xaxt = "n")
  
  axis(1, at = data$X[c(1,201,401,601)], labels = data$date[c(1,201,401,601)])
  
  lines(data$X[CALLCENTER$TRAIN],
        data$pred[CALLCENTER$TRAIN],
        col = "blue",
        lwd = 3)
  lines(data$X[CALLCENTER$TEST],
        data$pred[CALLCENTER$TEST],
        col = "red",
        lwd = 3)
  abline(v = min(which(CALLCENTER$TEST)) + min(data$X)- 2,
         col = "red",
         lwd = 2)
  
  legend("topleft", legend = c("Observed", "Prediction made the day before [Train]","Prediction made the day before [Test]"), 
         col = c("black","blue", "red"), lty = 1:1, lwd = 3)
  
}

CALLCENTER$analyze_glmnet <- function(M,
                                      dev.ratio = 0) {
  # plot(M)
  # plot(M$glmnet.fit, xvar = "dev", label = TRUE)
  # abline(v = M$glmnet.fit$dev.ratio[M$lambda == M$lambda.min])
  # abline(v = M$glmnet.fit$dev.ratio[M$lambda == M$lambda.1se])
  
  if (dev.ratio <= 0) {
    M$my_lambda = M$lambda.1se
    
  } else{
    M$my_lambda = M$lambda[sum(M$glmnet.fit$dev.ratio < dev.ratio)]
    
  }
  
  # abline(v = M$glmnet.fit$dev.ratio[M$lambda == M$my_lambda])
  a = M$glmnet.fit$beta[, M$lambda == M$my_lambda]
  print(sort(a[a != 0]))
  return(M)
  
}

CALLCENTER$R.squared <- function(CALLCENTER,
                                 data){
  
  Text = "R-squared on all data:"
  Text = paste(Text,round(1-var(data$val - data$pred)/var(data$val),3), sep = "\n")
  Text = paste(Text,"\n",sep = "")
  
  Text = paste(Text,"R-squared on all data [7-days before]:", sep = "\n")
  Text = paste(Text,round(1-var(data$val - data$pred_7)/var(data$val),3), sep = "\n")
  Text = paste(Text,"\n",sep = "")
  
  Text = paste(Text,"R-squared on all data [28-days before]:", sep = "\n")
  Text = paste(Text,round(1-var(data$val - data$pred_28)/var(data$val),3), sep = "\n")
  Text = paste(Text,"\n",sep = "")
  
  Text = paste(Text,"R-squared on training data:", sep = "\n")
  Text = paste(Text,round(1-var(data$val[CALLCENTER$TRAIN] - data$pred[CALLCENTER$TRAIN])/var(data$val[CALLCENTER$TRAIN]),3), sep = "\n")
  Text = paste(Text,"\n",sep = "")
  
  Text = paste(Text,"R-squared on test data:", sep = "\n")
  Text = paste(Text,round(1-var(data$val[CALLCENTER$TEST] - data$pred[CALLCENTER$TEST])/var(data$val[CALLCENTER$TEST]),3), sep = "\n")
  Text = paste(Text,"\n",sep = "")
  
  Text = paste(Text,"Ratio of errors within predicted error limits on test data:", sep = "\n")
  Text = paste(Text,round(sum(data$within[!CALLCENTER$TEST])/sum(!CALLCENTER$TEST),3), sep = "\n")
  Text = paste(Text,"\n",sep = "")
  
  Text = paste(Text,"Mean width of the prediction interval:", sep = "\n")
  Text = paste(Text,round(mean(data$q_high - data$q_low),3), sep = "\n")
  Text = paste(Text,"\n",sep = "")
  
  Text = paste(Text,"Mean width of the prediction interval [28 days before]):", sep = "\n")
  Text = paste(Text,round(mean(data$q_high_28 - data$q_low_28),3), sep = "\n")
  Text = paste(Text,"\n",sep = "")
  
  cat(Text)
}



saveRDS(CALLCENTER,
        file = "app/Modeller/CALLCENTER.rds")














mean(OK)
mean(OK2)
plot(OK)

sum(data$val[CALLCENTER$TRAIN] < data$pred_UQ[CALLCENTER$TRAIN])/(nrow(data)-28)
sum(data$val[TIMESERIES$TRAIN] < data$pred_UQ7[TIMESERIES$TRAIN])/(nrow(data)-28)
sum(data$val[TIMESERIES$TRAIN] < data$pred_UQ28[TIMESERIES$TRAIN])/(nrow(data)-28)

sum(data$val[TIMESERIES$TRAIN] > data$pred_LQ[TIMESERIES$TRAIN])/(nrow(data)-28)
sum(data$val[TIMESERIES$TRAIN] > data$pred_LQ7[TIMESERIES$TRAIN])/(nrow(data)-28)
sum(data$val[TIMESERIES$TRAIN] > data$pred_LQ28[TIMESERIES$TRAIN])/(nrow(data)-28)


df$Y = df$Y[1:1000]
new = df$Y[(length(df$Y)-27):length(df$Y)]
df$Y = df$Y[1:(length(df$Y)-28)]
input = list(input = toJSON(df))
response <- POST("http://localhost:3830/Train",
                 body = input,
                 encode = "json")
r_list <- fromJSON(content(response, "text"))
data.frame(True = new,
           Predicted = fromJSON(r_list))

Timeseries_train("SOS_alarm.json")
Timeseries_predict("SOS_alarm.json")











data = TIMESERIES$import()

Y = data$Antal.anrop[2:nrow(data)-1]
TIMESERIES$Datum_train = data$Datum[1]


data = TIMESERIES$clean(TIMESERIES,
                        Y,
                        TIMESERIES$Datum_train)



TIMESERIES$TRAIN = data$val != -1
TIMESERIES$TEST = data$val == -1

TIMESERIES <- TIMESERIES$Train.model(TIMESERIES,
                                     data)


data = TIMESERIES$predict(TIMESERIES,
                          data)


TIMESERIES$plot(TIMESERIES,
                data)


my_pred = data$pred[data$val == -1]
data = TIMESERIES$import()

data = data[(nrow(data)-31):(nrow(data)-1),]
Y = data$Antal.anrop
Datum = data$Datum[1]

data = TIMESERIES$clean(TIMESERIES,
                        Y,
                        Datum)
View(data)
data = TIMESERIES$predict(TIMESERIES,
                          data)
my_pred2 = data$pred[data$val == -1]

cbind(my_pred,my_pred2)
a = TIMESERIES$predict.API(data)

plot(c(data$val,a))

out = data.frame(Datum = data$Datum[1401:1431],
                 Prediktion = round(data$pred[1401:1431]))
write.xlsx(out,file = "Pers prediktion.xlsx")
TIMESERIES$R.squared(TIMESERIES,
                     data)


head(data[,c("date","val","pred","error","q_low","q_high","within")])
head(data[order(data$error,decreasing = T),c("date","val","pred","error","q_low","q_high","within")])


var(data$q_high)

