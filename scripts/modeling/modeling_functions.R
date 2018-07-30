##--------------------------------------------------------------------
## function to fit/tune random forest model
my_rf <- function(data, response){
  # input data, and response
  # output the best random forest model
  num_features <- ncol(data) - 1
  cur_formula <- paste(response, "~.", sep = "")
  
  # two rounds of "tuning"
  ## Round 1
  ### initialize
  mtrys <- seq(1,ceiling(num_features/5),3)
  nsizes <- seq(1,num_features,5)
  cur_best_mtry <- -1
  cur_best_nsize <- -1
  cur_best_r2 <- -1
  
  for (nsize in nsizes){
    for (mt in mtrys){
      set.seed(1)
      rForest_1 <- randomForest(formula = as.formula(cur_formula), 
                                data=data, mtry=mt, 
                                ntree = 50,nodesize = nsize, 
                                importance = FALSE) 
      cur_avg_r2 <- mean(rForest_1$rsq)
      if (cur_avg_r2 > cur_best_r2){
        cur_best_mtry <- mt
        cur_best_nsize <- nsize
        cur_best_r2 <- cur_avg_r2
      }
    }
  } 
  
  ## Round 2
  ### initialize
  mtrys <- c(max((cur_best_mtry-2),1):(cur_best_mtry+2))
  nsizes <- c(max((cur_best_nsize-4),1):(cur_best_nsize+4))
  cur_best_mtry <- -1
  cur_best_nsize <- -1
  cur_best_r2 <- -1
  
  for (nsize in nsizes){
    for (mt in mtrys){
      set.seed(1)
      rForest_1 <- randomForest(formula = as.formula(cur_formula), 
                                data=data, mtry=mt, 
                                ntree = 50,nodesize = nsize, 
                                importance = FALSE) 
      
      cur_avg_r2 <- mean(rForest_1$rsq)
      if (cur_avg_r2 > cur_best_r2){
        cur_best_mtry <- mt
        cur_best_nsize <- nsize
        cur_best_r2 <- cur_avg_r2
      }
    }
  }   
  
  ## Finally fit a best tree
  rf_best_mtry <- cur_best_mtry
  print (rf_best_mtry)
  rf_best_nsize <- cur_best_nsize
  print (rf_best_nsize)
  rForest_best <- randomForest(formula = as.formula(cur_formula), 
                               data=data, mtry=rf_best_mtry, 
                               ntree = 1000,nodesize=rf_best_nsize, 
                               importance=FALSE) 
  
  return (rForest_best)
}

##--------------------------------------------------------------------
## function to fit/tune xgboost model (gradient boosted trees)
my_xgb <- function(dtrain){
  # input the xgb.Dmatrix format dtrain
  
  # initialize
  maxTrees <- 200
  shrinkage <- 0.05
  gamma <- 2
  depth <- 10
  minChildWeight <- 20
  colSample <- 0.9
  subSample <- 0.9
  earlyStopRound <- 3
  
  # tune the tree
  set.seed(1)
  xgbCV <- xgb.cv(params=list(max_depth=depth,
                              eta=shrinkage,
                              gamma=gamma,
                              colsample_bytree=colSample,
                              min_child_weight=minChildWeight,
                              subsample=subSample,
                              objective="reg:linear",
                              silent=0),
                  data=dtrain,
                  nrounds=maxTrees,
                  # verobse=0,
                  eval_metric="rmse",
                  nfold=10,
                  stratified=TRUE,
                  early_stopping_rounds=earlyStopRound)
  
  bestIter <- xgbCV$best_iteration
  
  # get the model
  xgb_best <- xgboost(params=list(max_depth=depth,
                             eta=shrinkage,
                             gamma=gamma,
                             colsample_bytree=colSample,
                             min_child_weight=minChildWeight,
                             subsample=subSample,
                             objective="reg:linear"),
                 data=dtrain,
                 nrounds=bestIter,
                 verbose=0,
                 eval_metric ="rmse")
  
  print ("-------------------------------------------------------")
  print ("xgb this round complete!")
  print ("-------------------------------------------------------")
  
  return (xgb_best)
}