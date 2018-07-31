# Define needed functions here

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
## function to check "min=max" columns and get rid of such
noVariation_filter <- function(data){
  # read in data and return data without invalid columns
  data_out <- data[,!apply(data,2,function(x) min(x) == max(x))]
  return (data_out)
}

##--------------------------------------------------------------------
## function to check highly collinear columns and get rid of such
### stackoverflow: https://stackoverflow.com/questions/18275639/remove-highly-correlated-variables
collinear_filter <- function(data, threshold){
  # plug in data you need check correlation and threshold of high correlation elimination
  # return a vector of features that are not highly correlated
  tmp <- cor(data)
  tmp[upper.tri(tmp)] <- 0
  diag(tmp) <- 0
  # print (tmp)
  data_out <- data[,!apply(tmp,2,function(x) any(abs(x) > threshold))]
  # print (data_out %>% head(5))
  names_out <- names(data_out)
  return (names_out)
}

##--------------------------------------------------------------------
## function to select based on correlation with response
corr_selection <- function(data, features, response, topn){
  # input data, vector of all features and response and topn correlations you want
  # output dataframe with top selected features (and target)
  cor_df <- data.frame(feature = character(), cor = double())
  for (fea in features){
    cur_cor <- cor(data[[response]], data[[fea]])
    cur_row <- data.frame(feature = fea, cor = cur_cor)
    cor_df <- rbind(cor_df, cur_row)
  }
  
  cor_df_sorted <- cor_df %>% arrange(desc(abs(cor)))
  cor_df_sorted_top <- cor_df_sorted %>% head(topn) # get top 
  # select the features there
  all_f <- c(as.vector(cor_df_sorted_top$feature), response)
  data_small <- data %>% select(all_f)
  return (data_small)
}

##--------------------------------------------------------------------
## function to select based on random forest importance
rf_selection <- function(data, features, response, topn){
  # input data, vector of all features and response and topn correlations you want
  # output dataframe with top selected features (and target)
  
  cur_formula <- paste(response, "~.", sep = "")
  
  rForest <- randomForest(formula = as.formula(cur_formula), 
                            data = data, mtry = 20, 
                            ntree = 1000,nodesize = 20, 
                            importance = TRUE)
  
  rf_importance <- data.frame(rForest$importance)
  rf_topn <- rf_importance %>% 
    mutate(feature = rownames(rf_importance)) %>%
    arrange(desc(X.IncMSE)) %>% head(topn)
  
  all_f <- c(as.vector(rf_topn$feature), response)
  data_small <- data %>% select(all_f)
  
  return (data_small)
}



