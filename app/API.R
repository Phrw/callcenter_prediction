
library(glmnet)
library(glmnetUtils)
library(quantreg)




#* Return a prediction
#* @param input json list
#* @get /Ping
function(input = "1") {
  return(toJSON("Pong"))
}



#* Return a prediction
#* @param input json list
#* @post /Predict

function(input = "1") {
  df = input[[1]]
  FILES = list.files("Modeller/")

  if(paste("TIMESERIES_",df$ID,".rds",sep = "") %in% FILES){
    TIMESERIES = readRDS(paste("Modeller/TIMESERIES_",df$ID,".rds",sep = ""))

    data = TIMESERIES$clean(TIMESERIES,
                            df$Y,
                            df$Datum)

    data = TIMESERIES$predict(TIMESERIES,
                              data,
			      LONG = TIMESERIES$LONG)

    my_pred = round(data[data$val == -1,c("pred","pred_LQ","pred_UQ")])

    out = list()
    out$Prediction = my_pred$pred
    out$Lower = my_pred$pred_LQ
    out$Upper = my_pred$pred_UQ
    
    result_json <- toJSON(out)
    # Return the JSON result
    return(result_json)    
  }else{
    return(toJSON(0))
  }
}



#* Return a prediction
#* @param input json list
#* @post /TrainLong
function(input = "1") {
  TIMESERIES = readRDS("Modeller/TIMESERIES.rds")
  df = input[[1]]
  TIMESERIES$Datum_train = df$Datum
  data = TIMESERIES$clean(TIMESERIES,
                          df$Y,
                          df$Datum)
  
  
  
  TIMESERIES$TRAIN = data$val != -1
  TIMESERIES$TEST = data$val == -1
  TIMESERIES$LONG = T

  TIMESERIES <- TIMESERIES$Train.model(TIMESERIES,
                                       data,
				       LONG = TIMESERIES$LONG)
  
  
  data = TIMESERIES$predict(TIMESERIES,
                            data, LONG = TIMESERIES$LONG)
  
  my_pred = round(data[data$val == -1,c("pred","pred_LQ","pred_UQ")])
  out = list()
  out$Prediction = my_pred$pred
  out$Lower = my_pred$pred_LQ
  out$Upper = my_pred$pred_UQ
  
  result_json <- toJSON(out)
  saveRDS(TIMESERIES,
          file = paste("Modeller/TIMESERIES_",df$ID,".rds",sep = ""))
  # Return the JSON result
  return(result_json)
}






#* Return a prediction
#* @param input json list
#* @post /Train
function(input = "1") {
  TIMESERIES = readRDS("Modeller/TIMESERIES.rds")
  df = input[[1]]
  TIMESERIES$Datum_train = df$Datum
  data = TIMESERIES$clean(TIMESERIES,
                          df$Y,
                          df$Datum)
  
  
  
  TIMESERIES$TRAIN = data$val != -1
  TIMESERIES$TEST = data$val == -1
  TIMESERIES$LONG = F

  TIMESERIES <- TIMESERIES$Train.model(TIMESERIES,
                                       data)
  
  
  data = TIMESERIES$predict(TIMESERIES,
                            data)
  
  my_pred = round(data[data$val == -1,c("pred","pred_LQ","pred_UQ")])
  out = list()
  out$Prediction = my_pred$pred
  out$Lower = my_pred$pred_LQ
  out$Upper = my_pred$pred_UQ
  
  result_json <- toJSON(out)
  saveRDS(TIMESERIES,
          file = paste("Modeller/TIMESERIES_",df$ID,".rds",sep = ""))
  # Return the JSON result
  return(result_json)
}