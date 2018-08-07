## functions for modeling

##--------------------------------------------------------------------
## CV function
CVInd <- function(n,K) { #n is sample size; K is number of parts; returns   K-length list of indices for each part
  m<-floor(n/K) #approximate size of each part
  r<-n-m*K
  I<-sample(n,n) #random reordering of the indices
  Ind<-list() #will be list of indices for all K parts
  length(Ind)<-K
  for (k in 1:K) {
    if (k <= r) kpart <- ((m+1)*(k-1)+1):((m+1)*k)
    else kpart<-((m+1)*r+m*(k-r-1)+1):((m+1)*r+m*(k-r))
    Ind[[k]] <- I[kpart] #indices for kth part of data
  }
  Ind
}

##--------------------------------------------------------------------
## function to fit/tune xgboost model (gradient boosted trees)
my_xgb <- function(dtrain, depth = 10000, lr = 0.01, es = 10){ # chang depth and lr if needed
  # input the xgb.Dmatrix format dtrain
  
  # initialize
  maxTrees <- depth
  shrinkage <- lr
  gamma <- 2
  depth <- 10
  minChildWeight <- 20
  colSample <- 0.9
  subSample <- 0.9
  earlyStopRound <- es
  
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
## function to fit/tune knn model
my_knn <- function(data, features, response){
  data_fea <- data[,init_features_1]
  data_res <- data[,response]
  set.seed(1)
  ctrl_knn <- trainControl(method="repeatedcv",repeats = 3, number = 10) # 10-fold cross validation
  knnFit <- train(data_fea, data_res, method = "knn", 
                  trControl = ctrl_knn, preProcess = c("center","scale"), tuneLength = 20)
  
  return (knnFit)
}

##--------------------------------------------------------------------
## function to fit/tune lm-stepwise model
my_lmstep <- function(data, features, response){
  data_fea <- data[,features]
  data_res <- data[,response]
  set.seed(1)
  ctrl_lm <- trainControl(method="repeatedcv",repeats = 3, number = 10) # 10-fold cross validation
  lmFit <- train(data_fea, data_res, method = "leapSeq", 
                 trControl = ctrl_lm, tuneLength = 20)
  
  return (lmFit)
}

##--------------------------------------------------------------------
## function to fit/tune ridge regression model
my_ridge <- function(data, features, response){
  data_fea <- data[,features]
  data_res <- data[,response]
  set.seed(1)
  ctrl_ridge <- trainControl(method="repeatedcv",repeats = 3, number = 10) # 10-fold cross validation
  ridgeFit <- train(data_fea, data_res, method = "ridge", 
                    trControl = ctrl_ridge, tuneLength = 10)
}

##--------------------------------------------------------------------
## function to fit/tune SVM model (Linear Kernel)
my_svmLinear <- function(data, features, response){
  data_fea <- data[,init_features_1]
  data_res <- data[,response]
  set.seed(1)
  ctrl_svm <- trainControl(method="repeatedcv",repeats = 3, number = 10) # 10-fold cross validation
  svmFit <- train(data_fea, data_res, method = "svmLinear", 
                  trControl = ctrl_svm, tuneLength = 20)
  
  return (svmFit)
}

##--------------------------------------------------------------------
## function to fit/tune lightgbm
my_lgb <- function(dtrain, depth = 10000, lr = 0.01, es = 10){
  # tune first
  params <- list(objective = "regression", metric = "rmse")
  lgb_cv <- lgb.cv(params, 
                   data = dtrain,
                   nrounds = depth, learning_rate = lr, nfold = 10,
                   boosting = "gbdt", max_depth = 10, feature_fraction = 0.9, bagging_fraction = 0.8,
                   early_stopping_rounds = es,
                   num_threads = 4)
  lgb_best_iter <- lgb_cv$best_iter
  
  ## fit 
  lgb_best <- lgb.train(params, 
                        data = dtrain,
                        nrounds = lgb_best_iter, learning_rate = lr, nfold = 10,
                        boosting = "gbdt", num_threads = 4)
  
  return (lgb_best)
  
}

# -----------------------------------------------------------
# Neural Network
### TO BE FILLED HERE

##--------------------------------------------------------------------
## function to fit/tune neural network

