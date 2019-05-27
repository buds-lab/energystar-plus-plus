library(dplyr)
library(parallel)
library(dplyr)
library(plyr)

library(caret)
library(glmnet)
library(qpcR) # for PRESS stat
library(xgboost)

library(Metrics)

scale_data <- function(df) {
  for(col in names(df)) {
    if( is.numeric(df[, col]) ){
      # https://stackoverflow.com/questions/10287545/backtransform-scale-for-plotting
      # https://rdrr.io/cran/DMwR/man/unscale.html
      df[, col] = scale(df[, col], center = T, scale = T)
    }
  }
  return (df)
}

# from https://gist.github.com/tomhopper/8c204d978c4a0cbcb8c0
# https://www.r-bloggers.com/can-we-do-better-than-r-squared/
# https://www.rdocumentation.org/packages/qpcR/versions/1.4-1/topics/PRESS
PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  
  return(PRESS)
}

pred_r_squared <- function(linear.model) {
  #' Use anova() to get the sum of squares for the linear model
  lm.anova <- anova(linear.model)
  #' Calculate the total sum of squares
  tss <- sum(lm.anova$'Sum Sq')
  # Calculate the predictive R^2
  pred.r.squared <- 1-PRESS(linear.model)/(tss)
  
  return(pred.r.squared)
}

Rsquared_asPerLM <- function(obs, pred, rank) {
  
  #z = fit
  #obs  = fit$model$SOURCE_EUI
  #pred = predict(z)
  f =  pred
  r = obs - pred
  n <- length(r)
  
  # r <- z$residuals
  # f <- z$fitted.values
  # w <- z$weights
  
  # mss <- if (attr(z$terms, "intercept")) 
  #   sum((f - mean(f))^2)
  # else sum(f^2)
  mss = sum((f - mean(f))^2)
  rss <- sum(r^2)
  
  # mss <- if (attr(z$terms, "intercept")) {
  #   m <- sum(w * f/sum(w))
  #   sum(w * (f - m)^2)
  #m <- sum(w * f/sum(w))
  #mss = sum(w * (f - m)^2)
  #rss <- sum(w * r^2)
  
  r.squared <- mss/(mss + rss)
  #adj.r.squared <- 1 - (1 - r.squared) * ((n - df.int)/rdf)
  
  #rank = 7
  df.int = 1
  rdf = n - rank
  adj.r.squared <- 1 - (1 - r.squared) * ((n - df.int)/rdf)
  
  return (c("R2" = r.squared, "adjR2" = adj.r.squared))
}

getModelStats <- function(fit, obs, pred) {
  
  #ss = summary(fit)
  #sst = sum( (y-mean(y))^2 )
  #ssr = sum( (y-y1)^2 )
  #r2 = 1 - ssr/sst
  #R^2 = 1 - sum(R^2) / sum((y-mean(y))^2)
  
  #Rsq = R2(obs, pred)
  #x = model.matrix(fit)
  #n = length(fit$residuals)
  #p = fit$rank
  #adjRsq = 1 - ((1-Rsq) * (n-1)/(n-p-1))
  
  r = Rsquared_asPerLM(obs, pred, fit$rank)
  R2 = r["R2"]
  adjR2 = r["adjR2"]
  
  df = data.frame( 
    "obs" = length(fit$residuals), 
    "rank" = fit$rank, 
    "coef" = length(fit$coefficients),
    "R^2"  = round(R2,3),
    "Adj.R^2"  = round(adjR2,3),
    "mse" = round(mse(obs, pred),3),
    "rmse" = round(rmse(obs, pred),3),
    "mae" = round(mae(obs, pred),3),
    "mape" = round(mape(obs, pred),3)
  )
}

getModelStatsGLMnet <- function(fit, x, obs, pred, w) {
  
  coefm = as.data.frame(as.matrix(coef(fit)))
  coefm = subset(coefm, s0 != 0)
  
  rank = nrow(coefm)
  
  #Rsq = R2(pred,obs)
  Rsq = fit$dev.ratio
  
  #dfa = data.frame(obs = y, pred = pred)
  #defaultSummary(dfa)
  
  
  
  #sst = sum( (obs-mean(obs))^2 )
  #ssr = sum( (obs-obs1)^2 )
  #r2 = 1 - ssr/sst
  #R^2 = 1 - sum(R^2) / sum((obs-mean(obs))^2)
  
  n = nrow(x)
  p = nrow(coefm)
  adjRsq = 1 - ((1-Rsq) * (n-1)/(n-p-1))
  
  r = Rsquared_asPerLM(obs, pred, w, rank)
  R2 = r["R2"]
  adjR2 = r["adjR2"]
  
  
  #mse1 = Metrics::mse(obs, pred)
  
  df = data.frame( 
    "obs" = fit$nobs,
    "rank" = nrow(coefm), 
    "coef" = nrow(coefm),
    "R^2"  = round(R2,3),
    "Adj.R^2"  = round(adjR2,3),
    "mse" = round(mse(obs, pred),3),
    "rmse" = round(rmse(obs, pred),3),
    "mae" = round(mae(obs, pred),3),
    "mape" = round(mape(obs, pred),3)
  )
}

getModelStatsRpart <- function(fit, x, obs, pred) {
  
  vi = varImp(fit)
  vi = subset(vi, Overall>0 )
  rank = nrow(vi)
  #Rsq = R2(pred,obs)
  
  r = Rsquared_asPerLM(obs, pred, rank)
  R2 = r["R2"]
  adjR2 = r["adjR2"]
  
  
  
  #sst = sum( (obs-mean(obs))^2 )
  #ssr = sum( (obs-pred)^2 )
  #r2 = 1 - ssr/sst
  #R^2 = 1 - sum(R^2) / sum((obs-mean(obs))^2)
  
  #n = nrow(x)
  #p = length(x)
  #adjRsq = 1 - ((1-Rsq) * (n-1)/(n-p-1))
  #mse1 = Metrics::mse(obs, pred)
  
  df = data.frame( 
    "obs" = nrow(x),
    "rank" = nrow(vi), 
    "coef" = nrow(vi),
    "R^2"  = round(R2,3),
    "Adj.R^2"  = round(adjR2,3),
    "mse" = round(mse(obs, pred),3),
    "rmse" = round(rmse(obs, pred),3),
    "mae" = round(mae(obs, pred),3),
    "mape" = round(mape(obs, pred),3)
  )
}

getModelStatsXgboost <- function(fit, x, obs, pred) {
  
  vi = xgboost::xgb.importance(model=fit)
  #pred = predict(fit, as.matrix(x))
  #Rsq = R2(pred,obs)
  
  rank = nrow(vi)
  r = Rsquared_asPerLM(obs, pred, rank)
  R2 = r["R2"]
  adjR2 = r["adjR2"]
  
  
  #sst = sum( (obs-mean(obs))^2 )
  #ssr = sum( (obs-pred)^2 )
  #r2 = 1 - ssr/sst
  #R^2 = 1 - sum(R^2) / sum((obs-mean(obs))^2)
  
  #n = nrow(x)
  #p = length(x)
  #adjRsq = 1 - ((1-Rsq) * (n-1)/(n-p-1))
  
  #mse1 = Metrics::mse(obs, pred)
  
  df = data.frame( 
    "obs" = nrow(x),
    "rank" = nrow(vi), 
    "coef" = nrow(vi),
    "R^2"  = round(R2,3),
    "Adj.R^2"  = round(adjR2,3),
    "mse" = round(mse(obs, pred),3),
    "rmse" = round(rmse(obs, pred),3),
    "mae" = round(mae(obs, pred),3),
    "mape" = round(mape(obs, pred),3)
  )
}

mean_center_data <- function(df, cols) {
  for(col in cols) {
    if( is.numeric(df[, col]) ){
      # https://stackoverflow.com/questions/10287545/backtransform-scale-for-plotting
      # https://rdrr.io/cran/DMwR/man/unscale.html
      df[, col] = scale(df[, col], center = T, scale = F)
    }
  }
  return (df)
}

log_transfer_data <- function(df, cols) {
  for(col in cols) {
    if( is.numeric(df[, col]) ){
      # https://stackoverflow.com/questions/10287545/backtransform-scale-for-plotting
      # https://rdrr.io/cran/DMwR/man/unscale.html
      df[, col] = log(df[, col])
    }
  }
  return (df)
}

exp_C1 <- function(data) {
  
  dvars = c("SourceEUI", "SourceEnergy")
  ivars = setdiff(names(data), dvars)
  
  dvar = "SourceEUI"
  model1 = paste(dvar, "~", paste(ivars, collapse = " + "))
  #dummy = dummyVars(model1, data = data)
  #train1 = as.data.frame(predict(dummy, data))
  fit1  = lm(model1, data = data)
  pred1 = as.numeric(predict(fit1))
  
  s1 = getModelStats(fit1, data$SourceEUI, pred1)
  df1 = data.frame("exp" = "C1", "interaction" = 1, "model" = "Reg",
                   "dependent" = dvar, s1)
  
  dvar = "SourceEnergy"
  model2 = paste(dvar, "~", paste(ivars, collapse = " + "))
  fit2 = lm(model2, data = data)
  pred2 = as.numeric(predict(fit2))
  
  s2 = getModelStats(fit2, data$SourceEnergy, pred2)
  df2 = data.frame("exp" = "C1", "interaction" = 1, "model" = "Reg",
                   "dependent" = dvar, s2)
  
  df = rbind(df1, df2)
  return(df)
}

regLassoRidge <- function(x, y, alpha, 
                          weight = rep(1,length(y))) {
  
  cv1 <- cv.glmnet(as.matrix(x), y, weights = weight,
                   #family = "binomial", 
                   nfold = 10, 
                   type.measure = "deviance", 
                   paralle = TRUE, alpha = alpha)
  md1 <- glmnet(as.matrix(x), y, weights = weight,
                #family = "binomial", 
                lambda = cv1$lambda.1se, 
                alpha = alpha)
  return (md1)
}

regElasticNet <- function(x, y, 
                          weight = rep(1,length(y))) {
  
  #a <- seq(0.1, 0.9, 0.05)
  a <- seq(0.01, 0.99, 0.01)
  search <- foreach(i = a, .combine = rbind) %dopar% {
    cv <- cv.glmnet(as.matrix(x), y, weights = weight,
                    #family = "binomial", 
                    nfold = 5, 
                    type.measure = "deviance", 
                    paralle = TRUE, alpha = i)
    
    data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], 
               lambda.1se = cv$lambda.1se, alpha = i)
  }
  
  cv3 <- search[search$cvm == min(search$cvm), ]
  md3 <- glmnet(as.matrix(x), y, weights = weight,
                #family = "binomial", 
                lambda = cv3$lambda.1se, 
                alpha = cv3$alpha)
  
  return(md3)
}

# using lasso regresion
exp_C1a <- function(data) {
  
  dvars = c("SourceEUI", "SourceEnergy")
  ivars = setdiff(names(data), dvars)
  
  dvar = "SourceEUI"
  model1 = paste(dvar, "~", paste(ivars, collapse = " + "))
  dummy = dummyVars(model1, data = data)
  x = as.data.frame(predict(dummy, data))
  y = data$SourceEUI
  
  ## Lasso 
  fit1 = regLassoRidge(x, y, alpha = 1) # Lasso
  s1 = getModelStatsGLMnet(fit1, x, y)
  df1 = data.frame("exp" = "C1a", "interaction" = 1, "model" = "Reg-Lasso",
                   "dependent" = dvar, s1)
  
  # Rigde
  fit2 = regLassoRidge(x, y, alpha = 0) # Ridge
  s2 = getModelStatsGLMnet(fit2, x, y)
  df2 = data.frame("exp" = "C1a", "interaction" = 1, "model" = "Reg-Ridge",
                   "dependent" = dvar, s2)
  
  # Elastic net
  fit3 = regElasticNet(x, y) #Elastic net
  s3 = getModelStatsGLMnet(fit3, x, y)
  df3 = data.frame("exp" = "C1a", "interaction" = 1, "model" = "Reg-ElasticNet",
                   "dependent" = dvar, s3)
  
  
  dvar = "SourceEnergy"
  model1 = paste(dvar, "~", paste(ivars, collapse = " + "))
  dummy = dummyVars(model1, data = data)
  x = as.data.frame(predict(dummy, data))
  y = data$SourceEnergy
  
  ## Lasso 
  fit1 = regLassoRidge(x, y, alpha = 1) # Lasso
  s1 = getModelStatsGLMnet(fit1, x, y)
  df4 = data.frame("exp" = "C1a", "interaction" = 1, "model" = "Reg-Lasso",
                   "dependent" = dvar, s1)
  
  # Rigde
  fit2 = regLassoRidge(x, y, alpha = 0) # Ridge
  s2 = getModelStatsGLMnet(fit2, x, y)
  df5 = data.frame("exp" = "C1a", "interaction" = 1, "model" = "Reg-Ridge",
                   "dependent" = dvar, s2)
  
  # Elastic net
  fit3 = regElasticNet(x, y) #Elastic net
  s3 = getModelStatsGLMnet(fit3, x, y)
  df6 = data.frame("exp" = "C1a", "interaction" = 1, "model" = "Reg-ElasticNet",
                   "dependent" = dvar, s3)
  
  res = rbind(df1, df2, df3, df4, df5, df6)
  stopImplicitCluster()
  
  return(res)
}

exp_C1aOLD <- function(data) {
  
  dvars = c("SourceEUI", "SourceEnergy")
  ivars = setdiff(names(data), dvars)
  dvar = "SourceEnergy"
  
  model1 = paste(dvar, "~", paste(ivars, collapse = " + "))
  dummy = dummyVars(model1, data = data)
  x = as.data.frame(predict(dummy, data))
  y = data$SourceEnergy
  
  #cv.lasso <- cv.glmnet(as.matrix(x), y, alpha = 1, type.measure="mae")
  
  cv1 <- cv.glmnet(as.matrix(x), y, 
                   #family = "binomial", 
                   nfold = 10, 
                   type.measure = "deviance", 
                   paralle = TRUE, alpha = 1)
  md1 <- glmnet(as.matrix(x), y, 
                #family = "binomial", 
                lambda = cv1$lambda.1se, 
                alpha = 1)
  coef(md1)
  
  
  cv2 <- cv.glmnet(as.matrix(x), y, 
                   #family = "binomial", 
                   nfold = 10, 
                   type.measure = "deviance", 
                   paralle = TRUE, alpha = 0)
  md2 <- glmnet(as.matrix(x), y, 
                #family = "binomial", 
                lambda = cv2$lambda.1se, 
                alpha = 0)
  coef(md2)
  
  #a <- seq(0.1, 0.9, 0.05)
  a <- seq(0.0, 1, 0.01)
  search <- foreach(i = a, .combine = rbind) %dopar% {
    cv <- cv.glmnet(as.matrix(x), y, 
                    #family = "binomial", 
                    nfold = 3, 
                    type.measure = "deviance", 
                    paralle = TRUE, alpha = i)
    
    data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], 
               lambda.1se = cv$lambda.1se, alpha = i)
  }
  
  cv3 <- search[search$cvm == min(search$cvm), ]
  md3 <- glmnet(as.matrix(x), y, 
                #family = "binomial", 
                lambda = cv3$lambda.1se, 
                alpha = cv3$alpha)
  coef(md3)
  
  y1 = as.numeric(predict(md1, as.matrix(x), type = "response"))
  y2 = as.numeric(predict(md2, as.matrix(x), type = "response"))
  y3 = as.numeric(predict(md3, as.matrix(x), type = "response"))
  
  r1 = data.frame(RMSE = RMSE(y1,y), Rsq = R2(y1,y), method = "Lasso")
  r2 = data.frame(RMSE = RMSE(y2,y), Rsq = R2(y2,y), method = "Ridge")
  r3 = data.frame(RMSE = RMSE(y3,y), Rsq = R2(y3,y), method = "ElasticNet")
  
  res = rbind(r1,r2,r3)
  
}

exp_C2 <- function(btype, save_dir, data) {
  
  exp_name = "C2"
  
  dvars = c("SourceEUI", "SourceEnergy")
  ivars = setdiff(names(data), dvars)
  
  ## with interactions
  df = NULL
  int_depth = length(ivars)
  for(int in 2:int_depth) {
    
    print(paste( Sys.time(), "interaction ", int))
    
    dvar = "SourceEUI"
    allvars = paste(ivars, collapse = " + ")
    model1 = paste(dvar, "~ (", allvars, ") ^", int )
    fit1 = lm(model1, data = data)
    pred1 = as.numeric(predict(fit1))
    
    filepath = paste0(save_dir, paste(exp_name, dvar, int), ".RDS")
    print(filepath)
    saveRDS(fit1, filepath)
    s1 = getModelStats(fit1, data$SourceEUI, pred1)
    df1 = data.frame("exp" = "C2", "interaction" = int, "model" = "RegInt",
                     "dependent" = dvar, s1)
    
    dvar = "SourceEnergy"
    allvars = paste(ivars, collapse = " + ")
    model2 = paste(dvar, "~ (", allvars, ") ^", int )
    fit2 = lm(model2, data = data)
    pred2 = as.numeric(predict(fit2))
    
    filepath = paste0(save_dir, paste(exp_name, dvar, int), ".RDS")
    saveRDS(fit1, filepath)
    s2 = getModelStats(fit2, data$SourceEnergy, pred2)
    df2 = data.frame("exp" = "C2", "interaction" = int, "model" = "RegInt",
                     "dependent" = dvar, s2)
    
    df3 = rbind(df1, df2)
    if(is.null(df)) df = df3
    else            df = rbind(df, df3)
    
  }
  return(df)
}

exp_C2_reg_task <- function(dvar, int) {
  #ivars, btype, data, exp_name, save_dir
  
  allvars = paste(ivars, collapse = " + ")
  model1 = paste(dvar, "~ (", allvars, ") ^", int )
  #model1 = paste(dvar, "~", paste(ivars, collapse = " + "))
  
  print(paste(Sys.time(), exp_name, btype, dvar, int, 
              paste(dim(data), collapse = " x "), " started"))
  
  fit1 = lm(model1, data = data)
  pred1 = as.numeric(predict(fit1))
  filepath = paste0(save_dir, paste(exp_name, dvar, int), ".RDS")
  saveRDS(fit1, filepath)
  
  s1 = getModelStats(fit1, data[, dvar], pred1)
  df1 = data.frame("exp" = "C2", "interaction" = int, "model" = "Reg",
                   "dependent" = dvar, s1)
  
  print(paste(Sys.time(), exp_name, btype, dvar, int, 
              paste(dim(data), collapse = " x "), " ended"))
  
  return(df1)
}

exp_C2_parallel <- function(btype, save_dir, data) {
  
  exp_name = "C2"
  dvars = c("SourceEUI", "SourceEnergy")
  ivars = setdiff(names(data), dvars)
  
  cluster <- makeCluster(detectCores(), outfile = "output.log" )
  clusterExport(cluster, 'save_dir', environment())
  clusterExport(cluster, 'btype', environment())
  clusterExport(cluster, 'exp_name', environment())
  clusterExport(cluster, 'ivars', environment())
  clusterExport(cluster, 'data', environment())
  clusterExport(cluster, 'getModelStats')
  clusterExport(cluster, 'Rsquared_asPerLM')
  clusterEvalQ(cluster, library("Metrics"))
  #clusterEvalQ(cluster, library("caret"))
  #clusterEvalQ(cluster, library("data.table"))
  
  int_depth = length(ivars)
  params <- expand.grid(dvar=dvars, int=2:int_depth, 
                        stringsAsFactors = F)
  
  res = clusterMap(cluster, 
                   exp_C2_reg_task, 
                   #ivars = ivars,
                   dvar = params$dvar,
                   int = params$int
                   #MoreArgs = c(ivars, data, btype, exp_name, save_dir)
  )
  stopCluster(cluster)
  
  resall <- ldply(res, data.frame)
  resall = type.convert(resall)
  
  return(resall)
}

exp_C2_metrics <- function(btype, save_dir, data) {
  
  exp_name = "C2"
  
  dvars = c("SourceEUI", "SourceEnergy")
  ivars = setdiff(names(data), dvars)
  
  ## with interactions
  df = NULL
  int_depth = length(ivars)
  for(int in 2:int_depth) {
    
    print(paste( Sys.time(), "interaction ", int))
    
    dvar = "SourceEUI"
    filepath = paste0(save_dir, paste(exp_name, dvar, int), ".RDS")
    print(filepath)
    fit1 = readRDS(filepath)
    pred1 = as.numeric(predict(fit1))
    s1 = getModelStats(fit1, data$SourceEUI, pred1)
    df1 = data.frame("exp" = "C2", "interaction" = int, "model" = "RegInt",
                     "dependent" = dvar, s1)
    
    dvar = "SourceEnergy"
    filepath = paste0(save_dir, paste(exp_name, dvar, int), ".RDS")
    fit2 = readRDS(filepath)
    pred2 = as.numeric(predict(fit2))
    s2 = getModelStats(fit2, data$SourceEnergy, pred2)
    df2 = data.frame("exp" = "C2", "interaction" = int, "model" = "RegInt",
                     "dependent" = dvar, s2)
    
    df3 = rbind(df1, df2)
    if(is.null(df)) df = df3
    else            df = rbind(df, df3)
    
  }
  return(df)
}

tuneRpart <- function(x, y, weight = rep(1,length(y))) {
  
  train.control <- trainControl(
    method = "repeatedcv",
    number = 10, ## 10-fold CV
    repeats = 3## repeated three times
    #search = "random"
  )
  
  tune.gridcart <- expand.grid(cp = seq(0,1,0.01))
  
  rpartFit2 <- train(x, y, 
                     #weights = weight,
                     method = "rpart", 
                     tuneGrid =tune.gridcart,
                     trControl = train.control)
  return(rpartFit2$finalModel)
}

tuneRpart2 <- function(x, y, weight = rep(1,length(y))) {
  
  train.control <- trainControl(
    method = "repeatedcv",
    number = 10, ## 10-fold CV
    repeats = 3## repeated three times
  )
  
  tune.gridcart <- expand.grid(maxdepth = 2:10)
  
  rpartFit2 <- train(x, y, 
                     weights = weight,
                     method = "rpart2", 
                     tuneGrid =tune.gridcart,
                     trControl = train.control)
  return(rpartFit2$finalModel)
}

exp_C3a <- function(btype, save_dir, data) {
  
  exp_name = "C3a"
  
  dvars = c("SourceEUI", "SourceEnergy")
  ivars = setdiff(names(data), dvars)
  
  dvar = "SourceEUI"
  model1 = paste(dvar, "~", paste(ivars, collapse = " + "))
  
  x = data[, ivars]
  y = data$SourceEUI
  
  print(paste(Sys.time(), "rpart", dvar))
  rpart1 = tuneRpart(x, y)
  #rp1 = rpart(model1, data)
  #pr1 = predict(rp1)
  
  pred1  = predict(rpart1)
  s1 = getModelStatsRpart(rpart1, x, y, pred1)
  df1 = data.frame("exp" = "C3a", "interaction" = 1, "model" = "rpart",
                   "dependent" = dvar, s1)
  filepath = paste0(save_dir, paste(exp_name, dvar), ".RDS")
  saveRDS(rpart1, filepath)
  
  dvar = "SourceEnergy"
  y = data$SourceEnergy
  print(paste(Sys.time(), "rpart", dvar))
  rpart2 = tuneRpart(x, y)
  pred2  = predict(rpart2)
  s2 = getModelStatsRpart(rpart2, x, y, pred2)
  df2 = data.frame("exp" = "C3a", "interaction" = 1, "model" = "rpart",
                   "dependent" = dvar, s2)
  filepath = paste0(save_dir, paste(exp_name, dvar), ".RDS")
  saveRDS(rpart2, filepath)
  
  
  res = rbind(df1, df2)
  
  return(res)
}

exp_C3a_metrics <- function(btype, save_dir, data) {
  
  exp_name = "C3a"
  
  dvars = c("SourceEUI", "SourceEnergy")
  ivars = setdiff(names(data), dvars)
  
  dvar = "SourceEUI"
  model1 = paste(dvar, "~", paste(ivars, collapse = " + "))
  
  x = data[, ivars]
  y = data$SourceEUI
  
  print(paste(Sys.time(), "rpart", dvar))
  filepath = paste0(save_dir, paste(exp_name, dvar), ".RDS")
  print(filepath)
  rpart1 = readRDS(filepath)
  # rpart1 = tuneRpart(x, y)
  #rp1 = rpart(model1, data)
  #pr1 = predict(rp1)
  
  pred1  = predict(rpart1)
  s1 = getModelStatsRpart(rpart1, x, y, pred1)
  df1 = data.frame("exp" = "C3a", "interaction" = 1, "model" = "rpart",
                   "dependent" = dvar, s1)
  
  dvar = "SourceEnergy"
  y = data$SourceEnergy
  print(paste(Sys.time(), "rpart", dvar))
  filepath = paste0(save_dir, paste(exp_name, dvar), ".RDS")
  print(filepath)
  rpart2 = readRDS(filepath)
  # rpart2 = tuneRpart(x, y)
  pred2  = predict(rpart2)
  s2 = getModelStatsRpart(rpart2, x, y, pred2)
  df2 = data.frame("exp" = "C3a", "interaction" = 1, "model" = "rpart",
                   "dependent" = dvar, s2)
  
  res = rbind(df1, df2)
  
  return(res)
}

tuneXgboost_defaultParams <- function(x, y, weight = rep(1,length(y))) {
  
  train.control <- trainControl(
    method = "repeatedcv",
    number = 10, ## 10-fold CV
    repeats = 3## repeated three times
  )
  
  gridxgFit1  <- train(x, y, 
                       weights = weight,
                       method = "xgbTree", 
                       trControl = train.control,
                       verbose = TRUE)
  return(gridxgFit1$finalModel)
}

tuneXgboost_randomSearch <- function(x, y, weight = rep(1,length(y))) {
  
  train.control <- trainControl(
    method = "repeatedcv",
    number = 10, ## 10-fold CV
    repeats = 3, ## repeated three times
    search = "random")
  
  gridxgFit1  <- train(x, y, 
                       weights = weight,
                       method = "xgbTree", 
                       trControl = train.control,
                       verbose = TRUE)
  return(gridxgFit1$finalModel)
}

tuneXgboost_gridSearch <- function(x, y, weight = rep(1,length(y))) {
  
  train.control <- trainControl(
    method = "repeatedcv",
    number = 10, ## 10-fold CV
    repeats = 3, ## repeated three times
  )
  
  #tune.gridcart <- expand.grid(maxdepth = 2:10)
  tune.gridxgb <- expand.grid(
    eta = seq(0.1, 0.9, 0.1),
    nrounds = seq(10, 100, 10),
    max_depth = 2:4,  # 4
    #min_child_weight = seq(1,5,1),
    #colsample_bytree = seq(0.1, 1, 0.1),
    gamma = seq(0, 1, 0.1),
    subsample = seq(0.2, 1, 0.2))
  
  dim(tune.gridxgb)
  
  gridxgFit1  <- train(x, y, 
                       weights = weight,
                       method = "xgbTree", 
                       #tuneGrid =tune.gridxgb,
                       trControl = train.control,
                       verbose = TRUE)
  return(gridxgFit1$finalModel)
}

tuneXgboost <- function(x, y, weight = rep(1,length(y))) {
  
  train.control <- trainControl(
    method = "repeatedcv",
    number = 10, ## 10-fold CV
    repeats = 3## repeated three times
  )
  
  #tune.gridcart <- expand.grid(maxdepth = 2:10)
  tune.gridxgb <- expand.grid(eta = c(0.05,0.3, 0.075), # 3 
                              nrounds = c(50, 75, 100),  # 3
                              max_depth = 4:7,  # 4
                              min_child_weight = c(2.0, 2.25), #2 
                              colsample_bytree = c(0.3, 0.4, 0.5), # 3
                              gamma = 0, #1
                              subsample = 1)  # 1
  # 3*3*4*2*3*1*1 = 216
  dim(tune.gridxgb)
  
  gridxgFit1  <- train(x, y, 
                       weights = weight,
                       method = "xgbTree", 
                       tuneGrid = tune.gridxgb,
                       trControl = train.control)
  return(gridxgFit1$finalModel)
}

exp_C3b <- function(btype, save_dir, data) {
  
  exp_name = "C3b"
  
  dvars = c("SourceEUI", "SourceEnergy")
  ivars = setdiff(names(data), dvars)
  
  dvar = "SourceEUI"
  model1 = paste(dvar, "~", paste(ivars, collapse = " + "))
  
  dummy = dummyVars(model1, data = data)
  x = as.data.frame(predict(dummy, data))
  y = data$SourceEUI
  
  print(paste(Sys.time(), "xgboost", dvar))
  xg1 = tuneXgboost_randomSearch(x, y)
  #xg1 = xgboost(data = as.matrix(x), label=y, nrounds = 20)
  pr1 = as.numeric(predict(xg1, as.matrix(x)))
  s1 = getModelStatsXgboost(xg1, x, y, pr1)
  
  df1 = data.frame("exp" = "C3b", "interaction" = 1, "model" = "xgboost",
                   "dependent" = dvar, s1)
  #xgb.save(bst, 'xgb.model')
  filepath = paste0(save_dir, paste(exp_name, dvar), ".RDS")
  saveRDS(xg1, filepath)
  
  
  
  dvar = "SourceEnergy"
  y = data$SourceEnergy
  print(paste(Sys.time(), "xgboost", dvar))
  xg2 = tuneXgboost_randomSearch(x, y)
  pr2 = as.numeric(predict(xg2, as.matrix(x)))
  s2 = getModelStatsXgboost(xg2, x, y, pr2)
  df2 = data.frame("exp" = "C3b", "interaction" = 1, "model" = "xgboost",
                   "dependent" = dvar, s2)
  filepath = paste0(save_dir, paste(exp_name, dvar), ".RDS")
  saveRDS(xg2, filepath)
  
  res = rbind(df1, df2)
  
  return(res)
}

exp_C3b_default <- function(btype, save_dir, data) {
  
  exp_name = "C3b"
  
  dvars = c("SourceEUI", "SourceEnergy")
  ivars = setdiff(names(data), dvars)
  
  dvar = "SourceEUI"
  model1 = paste(dvar, "~", paste(ivars, collapse = " + "))
  
  dummy = dummyVars(model1, data = data)
  x = as.data.frame(predict(dummy, data))
  y = data$SourceEUI
  
  print(paste(Sys.time(), "xgboost", dvar))
  xg1 = tuneXgboost_defaultParams(x, y)
  #xg1 = xgboost(data = as.matrix(x), label=y, nrounds = 20)
  pr1 = as.numeric(predict(xg1, as.matrix(x)))
  s1 = getModelStatsXgboost(xg1, x, y, pr1)
  
  df1 = data.frame("exp" = "C3b", "interaction" = 1, "model" = "xgboost_default",
                   "dependent" = dvar, s1)
  #xgb.save(bst, 'xgb.model')
  filepath = paste0(save_dir, paste(exp_name, dvar), ".RDS")
  saveRDS(xg1, filepath)
  
  
  
  dvar = "SourceEnergy"
  y = data$SourceEnergy
  print(paste(Sys.time(), "xgboost", dvar))
  xg2 = tuneXgboost_defaultParams(x, y)
  pr2 = as.numeric(predict(xg2, as.matrix(x)))
  s2 = getModelStatsXgboost(xg2, x, y, pr2)
  df2 = data.frame("exp" = "C3b", "interaction" = 1, "model" = "xgboost_default",
                   "dependent" = dvar, s2)
  filepath = paste0(save_dir, paste(exp_name, dvar), ".RDS")
  saveRDS(xg2, filepath)
  
  res = rbind(df1, df2)
  
  return(res)
}

exp_C3b_random <- function(btype, save_dir, data) {
  
  exp_name = "C3b"
  
  dvars = c("SourceEUI", "SourceEnergy")
  ivars = setdiff(names(data), dvars)
  
  dvar = "SourceEUI"
  model1 = paste(dvar, "~", paste(ivars, collapse = " + "))
  
  dummy = dummyVars(model1, data = data)
  x = as.data.frame(predict(dummy, data))
  y = data$SourceEUI
  
  print(paste(Sys.time(), "xgboost", dvar))
  xg1 = tuneXgboost_randomSearch(x, y)
  #xg1 = xgboost(data = as.matrix(x), label=y, nrounds = 20)
  pr1 = as.numeric(predict(xg1, as.matrix(x)))
  s1 = getModelStatsXgboost(xg1, x, y, pr1)
  
  df1 = data.frame("exp" = "C3b", "interaction" = 1, "model" = "xgboost_random",
                   "dependent" = dvar, s1)
  #xgb.save(bst, 'xgb.model')
  filepath = paste0(save_dir, paste(exp_name, dvar), ".RDS")
  saveRDS(xg1, filepath)
  
  
  
  dvar = "SourceEnergy"
  y = data$SourceEnergy
  print(paste(Sys.time(), "xgboost", dvar))
  xg2 = tuneXgboost_randomSearch(x, y)
  pr2 = as.numeric(predict(xg2, as.matrix(x)))
  s2 = getModelStatsXgboost(xg2, x, y, pr2)
  df2 = data.frame("exp" = "C3b", "interaction" = 1, "model" = "xgboost_random",
                   "dependent" = dvar, s2)
  filepath = paste0(save_dir, paste(exp_name, dvar), ".RDS")
  saveRDS(xg2, filepath)
  
  res = rbind(df1, df2)
  
  return(res)
}

exp_C3b_metrics <- function(btype, save_dir, data) {
  
  exp_name = "C3b"
  
  dvars = c("SourceEUI", "SourceEnergy")
  ivars = setdiff(names(data), dvars)
  
  dvar = "SourceEUI"
  model1 = paste(dvar, "~", paste(ivars, collapse = " + "))
  
  dummy = dummyVars(model1, data = data)
  x = as.data.frame(predict(dummy, data))
  y = data$SourceEUI
  
  print(paste(Sys.time(), "xgboost", dvar))
  filepath = paste0(save_dir, paste(exp_name, dvar), ".RDS")
  print(filepath)
  xg1 = readRDS(filepath)
  #xg1 = tuneXgboost(x, y)
  #xg1 = xgboost(data = as.matrix(x), label=y, nrounds = 20)
  pr1 = as.numeric(predict(xg1, as.matrix(x)))
  s1 = getModelStatsXgboost(xg1, x, y, pr1)
  
  df1 = data.frame("exp" = "C3b", "interaction" = 1, "model" = "xgboost",
                   "dependent" = dvar, s1)
  #xgb.save(bst, 'xgb.model')
  
  
  
  dvar = "SourceEnergy"
  y = data$SourceEnergy
  print(paste(Sys.time(), "xgboost", dvar))
  filepath = paste0(save_dir, paste(exp_name, dvar), ".RDS")
  print(filepath)
  xg2 = readRDS(filepath)
  # xg2 = tuneXgboost(x, y)
  pr2 = as.numeric(predict(xg2, as.matrix(x)))
  s2 = getModelStatsXgboost(xg2, x, y, pr2)
  df2 = data.frame("exp" = "E3b", "interaction" = 1, "model" = "xgboost",
                   "dependent" = dvar, s2)
  
  res = rbind(df1, df2)
  
  return(res)
}

clean_data <- function(data) {
  data1 = data %>% 
    filter(!is.na(ENERGYSTARScore)) %>%
    filter(!is.na(Heating.System)) %>%
    mutate(Age = 2018 - YearBuilt) %>%
    dplyr::select(-c(Elevators, Sprinklers))
  return(data1)
}

# https://www.r-bloggers.com/variable-selection-with-elastic-net/  
pkgs <- list("glmnet", "doParallel", "foreach", "pROC")
lapply(pkgs, require, character.only = T)

save_dir = "./data/usetypes_models/"
load_dir = "./data/usetypes_filtered/"
result_dir = "./data/usetype_results/"

dir.create(save_dir, showWarnings = F, recursive = T)
dir.create(result_dir, showWarnings = F, recursive = T)

ncore = 4

btypes = c(
  "office", "retail", "k12school", 
  "warehouse",
  "hotel", "worship", "multifamily"
  #"hospital", "medical_office"
  )

for(btype in btypes) {
  #btype = "multifamily"
  filepath = paste0(load_dir, btype, ".csv")
  print(paste(Sys.time(), btype, filepath))
  data0 = read.csv(filepath)
  data1 = data0 %>% 
    dplyr::select(-c(SiteEUI,SiteEnergy,ENERGYSTARScore))
  
  print(paste(btype, paste(dim(data1), collapse = " x ")))

  save_dir1 = paste0(save_dir, btype, "/")
  dir.create(save_dir1, showWarnings = F)

  c1 = NULL
  tryCatch({
    c1  = exp_C1(data1)  
  })
  
  #registerDoParallel(cores = 3)
  #c1a = exp_C1a(data2)
  #stopImplicitCluster()
  
  c2 = NULL
  tryCatch({
    #c2 = exp_C2_parallel(btype, save_dir1, data1)
    #c2 = c2[, -1]
    c2 = exp_C2(btype, save_dir1, data1)
    #c2 = exp_C2_metrics(btype, save_dir1, data1)
  })
  
  c3a = NULL
  tryCatch({
    registerDoParallel(cores = ncore)
    c3a = exp_C3a(btype, save_dir1, data1)
    #c3a = exp_C3a_metrics(btype, save_dir1, data1)
    stopImplicitCluster()
  })
  
  # c3b = NULL
  # tryCatch({
  #   registerDoParallel(cores = ncore)
  #   c3b = exp_C3b(btype, save_dir1, data1)
  #   #c3b = exp_C3b_metrics(btype, save_dir1, data1)
  #   stopImplicitCluster()
  # })
  
  c3b1 = NULL
  tryCatch({
    registerDoParallel(cores = ncore)
    c3b1 = exp_C3b_default(btype, save_dir1, data1)
    #c3b = exp_C3b_metrics(btype, save_dir1, data1)
    stopImplicitCluster()
  })
  
  c3b2 = NULL
  tryCatch({
    registerDoParallel(cores = ncore)
    c3b2 = exp_C3b_random(btype, save_dir1, data1)
    #c3b = exp_C3b_metrics(btype, save_dir1, data1)
    stopImplicitCluster()
  })
  
  tryCatch({
    cc = rbind(c1, c2, c3a, c3b1, c3b2)
    write.csv(cc, paste0(result_dir, btype, "_result.csv"))
  })
}



## calculate normalized rmse = rmse / Interquartile range

for (btype in btypes) {
  data = read.csv(paste0(load_dir, btype, ".csv"))
  print(paste(Sys.time(), btype, paste(dim(data), collapse = " x ")))
  
  res = read.csv(paste0(result_dir, btype, "_result.csv"))
  nrmse_iqr   = c()
  nrmse_range = c()
  nrmse_mean  = c()
  
  for(r in 1:nrow(res)) {
    row = res[r, ]
    if(row$dependent == 'SourceEnergy') {
      
      iqr  = IQR(data$SourceEnergy)  
      rang = max(data$SourceEnergy) - min(data$SourceEnergy)
      mean = mean(data$SourceEnergy)
      
      nrmse_iqr[r]   = round(row$rmse / iqr * 100,  3)
      nrmse_range[r] = round(row$rmse / rang * 100, 3)
      nrmse_mean[r]  = round(row$rmse / mean * 100, 3)
    } else if(row$dependent == 'SourceEUI') {
      
      iqr  = IQR(data$SourceEUI)  
      rang = max(data$SourceEUI) - min(data$SourceEUI)
      mean = mean(data$SourceEUI)
      
      nrmse_iqr[r]   = round(row$rmse / iqr * 100,  3)
      nrmse_range[r] = round(row$rmse / rang * 100, 3)
      nrmse_mean[r]  = round(row$rmse / mean * 100, 3)
    } else {
      nrmse_iqr[r]   = 0
      nrmse_range[r] = 0
      nrmse_mean[r]  = 0
    }
  }
  
  res = res[, setdiff(names(res), c("nrmse_iqr", "nrmse_range", 
                                    "nrmse_mean", "nrmse"))]
  res1 = cbind(res, nrmse_iqr, nrmse_range, nrmse_mean)
  res1 = res1[, setdiff(names(res1), c("X.2", "X.1", "X"))]
  write.csv(res1, paste0(result_dir, btype, "_result.csv"), row.names = F)
}





### data summary
summary_all = NULL
for(btype in btypes) {
  #btype = "multifamily"
  filepath = paste0(load_dir, btype, ".csv")
  print(paste(Sys.time(), btype, filepath))
  data0 = read.csv(filepath)
  data1 = data0 %>% 
    dplyr::select(-c(SiteEUI,ENERGYSTARScore))
  
  print(paste(btype, paste(dim(data1), collapse = " x ")))
  summary(data1)
}
ss = do.call(cbind, lapply(data1, summary))



############## comparison plots

save_dir2 = "./data/usetype_results/"

combined = NULL
## combine all results
for (btype in btypes) {
  #btype = "multifamily"
  data = read.csv(paste0(save_dir2, btype, "_result.csv"))
  print(paste(Sys.time(), btype, paste(dim(data), collapse = " x ")))
  data = cbind(btype = btype, data)
  combined = rbind(combined, data, make.row.names = F)
}

library(ggplot2)
library(reshape2)

## comparision 1: compare the R^2, Adj.R^2, MSE among all Ex experiments
comp1 = combined
comp1$model = paste0(comp1$model, comp1$interaction)
comp1 = subset(comp1, R.2 >= 0 & Adj.R.2 >= 0)

## plot type 1 - all combined
comp2 = melt(comp1, measure.vars = c("R.2", "Adj.R.2"))
ggplot(comp2, aes(x=model, y=value, group=variable, fill=variable)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid(dependent ~ btype) + coord_flip()

## plot type 3 - only adj.R^2 
comp3 = comp1
ggplot(comp3, aes(x=model, y=Adj.R.2, group=dependent, fill=dependent)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  facet_wrap(. ~ btype, ncol = 3)  + coord_flip()

## plot type 3 - only adj.R^2  SiteEnergy
comp4 = subset(comp1, dependent == 'SiteEnergy')
ggplot(comp4, aes(x=model, y=Adj.R.2, group=btype, fill=btype)) + 
  geom_bar(stat="identity", position=position_dodge())

ggplot(comp4, aes(x=btype, y=Adj.R.2, group=model, fill=model)) + 
  geom_bar(stat="identity", position=position_dodge()) 

ggplot(comp4, aes(x=btype, y=sqrt(mse), group=model, fill=model)) + 
  geom_bar(stat="identity", position=position_dodge()) 

## plot type 4 - only adj.R^2  SourceEUI
comp5 = subset(comp1, dependent == 'SourceEUI' & model != "xgboost1")
ggplot(comp5, aes(x=model, y=Adj.R.2, group=btype, fill=btype)) + 
  geom_bar(stat="identity", position=position_dodge())

ggplot(comp5, aes(x=btype, y=Adj.R.2, group=model, fill=model)) + 
  geom_bar(stat="identity", position=position_dodge()) 

ggplot(comp5, aes(x=btype, y=sqrt(mse), group=model, fill=model)) + 
  geom_bar(stat="identity", position=position_dodge())







### create cluster on remote machine
library("future")

cl <- makeClusterPSOCK(c("localhost", "172.25.186.105"), 
                       revtunnel = T,
                       user = "sbbadmin", 
                       rshcmd = c("plink", "-ssh", "-i", "C:/SBB/aws/sbb vm.ppk"),
                       homogeneous = FALSE, verbose = TRUE,
                       outfile = "output.log")

foo <- function(n) {
  print(paste(Sys.time(), n))
  #s = sample(9999999,10)
  for (r in runif(10000)) {
    for (s in runif(10000)) {
      x = r*s
    }
  }
  #Sys.sleep(n)
  #print(paste(n))
  return(n*rs)
}

res = clusterMap(cl, foo, 1:100)
unlist(res)

stopCluster(cl)


local = "SBB-5CG5473W2K"
primary <- local
machineAddresses <- list(
  list(host=primary,user='sbbadmin',
       ncore=4),
  list(host='172.25.186.105',user='sbbadmin',
       ncore=4)
)

spec <- lapply(machineAddresses,
               function(machine) {
                 rep(list(list(host=machine$host,
                               user=machine$user)),
                     machine$ncore)
               })
spec <- unlist(spec,recursive=FALSE)

parallelCluster <- parallel::makeCluster(type='PSOCK',
                                         master=primary,
                                         spec=spec,
                                         rshcmd = rshcmd1,
                                         verbose = T)
stopCluster(parallelCluster)

rshcmd1 = "plink -ssh -i 'C:/SBB/aws/sbb vm.ppk'"

cluster <- makeCluster(detectCores(), outfile = "output.log" )





############################## end #################################


school = read.csv(paste0(load_dir, "multifamily.csv"))
summary(school)
data1 = clean_data(school)

ivars = c("PropertyGFATotal", 
          "NumberofBuildings",
          "NumberofFloors",
          "Building.Quality", 
          "Construction.Class",
          "Heating.System",
          "Shape",
          "Age")
dvar1 = "SourceEUI"
dvar2 = "SiteEnergy"

data2 = data1[, c(ivars, dvar1, dvar2)]
btype = "school"
save_dir1 = paste0(save_dir, btype, "/")
dir.create(save_dir1, showWarnings = F)

c1 = exp_C1(data2)
c2 = exp_C2(btype, save_dir1, data2)

cc = rbind(c1,c2)
write.csv(cc, paste0(save_dir, btype, ".csv"))



