MLR <- function(data, 
                x, 
                y, 
                w, 
                centering = TRUE, 
                interaction = 1) {
  
  if(centering == TRUE){
    data = mean_center_data(data, x)  
  }
  
  if(interaction == 1) {  ### ordinary model
    model = paste(y, "~", paste(x, collapse = " + "))  
  } else {  ### interaction model
    allvars = paste(x, collapse = " + ")
    model = paste(y, "~ (", allvars, ") ^", interaction )
  }
  wt = data[, w]
  
  fit = lm(model, data = data, weights = wt)
  return (fit)
}



tuneXGB_defaultParams <- function(x, 
                                  y, 
                                  tree_height=2, 
                                  sample_weights
                                  ) {
  
  train.control <- trainControl(
    method = "repeatedcv",
    number = 10, ## 10-fold CV
    repeats = 2## repeated three times
  )
  
  tuned  <- train(x, y, 
                  weights = sample_weights,
                  method = "xgbTree", 
                  tuneGrid = grid_default,
                  trControl = train.control,
                  verbose = TRUE)
  return(tuned$finalModel)
}

tuneXgboost_randomSearch <- function(x,
                                     y,
                                     tree_height=2, 
                                     sample_weights
                                     ) {
  
  train.control <- trainControl(
    method = "repeatedcv",
    number = 10, ## 10-fold CV
    repeats = 2, ## repeated three times
    search = "random")
  
  tuned  <- train(x, y, 
                  weights = sample_weights,
                  method = "xgbTree", 
                  trControl = train.control,
                  verbose = TRUE)
  return(tuned$finalModel)
}

tuneXgboost_adaptiveSearch <- function(x,
                                       y,
                                       tree_height=2, 
                                       sample_weights
                                       ) {
  train.control <- trainControl(
    method = "adaptive_cv",
    search = "random",
    adaptive = list(min = 5, alpha = 0.05, 
                    method = "gls", complete = TRUE),
    number = 10, ## 10-fold CV
    repeats = 2 ## repeated three times
  )
  
  tuned  <- train(x, y, 
                  weights = sample_weights,
                  method = "xgbTree", 
                  trControl = train.control,
                  verbose = TRUE)
  return(tuned$finalModel)
}

Xgboost_default <- function( xdata, 
                             ydata,
                             interaction = 1,
                             sample_weights) {
  
  model = paste(y, "~", paste(x, collapse = " + "))
  dummy = dummyVars(model, data = data, fullRank = T)
  
  xdata = as.data.frame(predict(dummy, data))
  ydata = data[, y]
  
  xgfit = tuneXgboost_defaultParams(xdata, 
                                    ydata,
                                    tree_height = interaction,
                                    sample_weights)
  
  return(xgfit)
} 

Xgboost_random <- function( xdata, 
                            ydata,
                            interaction = 1,
                            sample_weights) {
  
  model = paste(y, "~", paste(x, collapse = " + "))
  dummy = dummyVars(model, data = data, fullRank = T)
  
  xdata = as.data.frame(predict(dummy, data))
  ydata = data[, y]
  
  xgfit = tuneXgboost_randomSearch(xdata, 
                                   ydata,
                                   tree_height = interaction,
                                   sample_weights)
  
  return(xgfit)
} 

Xgboost_adaptive <- function( xdata, 
                              ydata,
                              interaction = 1,
                              sample_weights) {
  
  model = paste(y, "~", paste(x, collapse = " + "))
  dummy = dummyVars(model, data = data, fullRank = T)
  
  xdata = as.data.frame(predict(dummy, data))
  ydata = data[, y]
  
  xgfit = tuneXgboost_adaptiveSearch(xdata, 
                                     ydata,
                                     tree_height = interaction,
                                     sample_weights)
  
  return(xgfit)
} 

