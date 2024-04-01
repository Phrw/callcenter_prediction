
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

  if(paste("CALLCENTER_",df$ID,".rds",sep = "") %in% FILES){
    CALLCENTER = readRDS(paste("Modeller/CALLCENTER_",df$ID,".rds",sep = ""))

    data = CALLCENTER$clean(CALLCENTER,
                            df$Y,
                            df$Datum)

    data = CALLCENTER$predict(CALLCENTER,
                              data,
			      LONG = CALLCENTER$LONG)

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
  CALLCENTER = readRDS("Modeller/CALLCENTER.rds")
  df = input[[1]]
  CALLCENTER$Datum_train = df$Datum
  data = CALLCENTER$clean(CALLCENTER,
                          df$Y,
                          df$Datum)
  
  
  
  CALLCENTER$TRAIN = data$val != -1
  CALLCENTER$TEST = data$val == -1
  CALLCENTER$LONG = T

  CALLCENTER <- CALLCENTER$Train.model(CALLCENTER,
                                       data,
				       LONG = CALLCENTER$LONG)
  
  
  data = CALLCENTER$predict(CALLCENTER,
                            data, LONG = CALLCENTER$LONG)
  
  my_pred = round(data[data$val == -1,c("pred","pred_LQ","pred_UQ")])
  out = list()
  out$Prediction = my_pred$pred
  out$Lower = my_pred$pred_LQ
  out$Upper = my_pred$pred_UQ
  
  result_json <- toJSON(out)
  saveRDS(CALLCENTER,
          file = paste("Modeller/CALLCENTER_",df$ID,".rds",sep = ""))
  # Return the JSON result
  return(result_json)
}






#* Return a prediction
#* @param input json list
#* @post /Train
function(input = "1") {
  CALLCENTER = readRDS("Modeller/CALLCENTER.rds")
  df = input[[1]]
  CALLCENTER$Datum_train = df$Datum
  data = CALLCENTER$clean(CALLCENTER,
                          df$Y,
                          df$Datum)
  
  
  
  CALLCENTER$TRAIN = data$val != -1
  CALLCENTER$TEST = data$val == -1
  CALLCENTER$LONG = F

  CALLCENTER <- CALLCENTER$Train.model(CALLCENTER,
                                       data)
  
  
  data = CALLCENTER$predict(CALLCENTER,
                            data)
  
  my_pred = round(data[data$val == -1,c("pred","pred_LQ","pred_UQ")])
  out = list()
  out$Prediction = my_pred$pred
  out$Lower = my_pred$pred_LQ
  out$Upper = my_pred$pred_UQ
  
  result_json <- toJSON(out)
  saveRDS(CALLCENTER,
          file = paste("Modeller/CALLCENTER_",df$ID,".rds",sep = ""))
  # Return the JSON result
  return(result_json)
}