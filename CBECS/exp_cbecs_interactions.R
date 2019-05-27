library(readxl)
library(readr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(rpart.utils)
library(sfa)
library(frontier)
library(likert)
library(mosaic)
library(ggpubr)
library(stringr)
library(ggridges)
library(egg) # same width for legends
library(stringi)
library(caret)
library(ipred)
library(fitdistrplus)   # fitting distributions, e.g. gamma

library(glmnet)
library(qpcR) # for PRESS stat
library(xgboost)
library(reshape2)
library(ggplot2)
library(Metrics)

library(ggpubr)
library(jtools)
library(effects)

set.seed(123) 

load_dir = './data/features/'

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

Rsquared_asPerLM <- function(obs, pred, w, rank) {
  
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
  #mss = sum((f - mean(f))^2)
  
  
  # mss <- if (attr(z$terms, "intercept")) {
  #   m <- sum(w * f/sum(w))
  #   sum(w * (f - m)^2)
  m <- sum(w * f/sum(w))
  mss = sum(w * (f - m)^2)
  
  rss <- sum(w * r^2)
  
  r.squared <- mss/(mss + rss)
  #adj.r.squared <- 1 - (1 - r.squared) * ((n - df.int)/rdf)
  
  #rank = 7
  df.int = 1
  rdf = n - rank
  adj.r.squared <- 1 - (1 - r.squared) * ((n - df.int)/rdf)
  
  return (c("R2" = r.squared, "adjR2" = adj.r.squared))
}

getModelStats <- function(fit, obs, pred, w, islog = F) {
  
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
  
  r = Rsquared_asPerLM(obs, pred, w, fit$rank)
  R2 = r["R2"]
  adjR2 = r["adjR2"]
  
  if(islog) {
    obs = exp(obs)
    pred = exp(pred)
  }

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

getModelStatsRpart <- function(fit, x, obs, pred, w) {
  
  vi = varImp(fit)
  vi = subset(vi, Overall>0 )
  rank = nrow(vi)
  #Rsq = R2(pred,obs)
  
  r = Rsquared_asPerLM(obs, pred, w, rank)
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

getModelStatsXgboost <- function(fit, x, obs, pred, w) {
  
  vi = xgboost::xgb.importance(model=fit)
  #pred = predict(fit, as.matrix(x))
  #Rsq = R2(pred,obs)
  
  rank = nrow(vi)
  r = Rsquared_asPerLM(obs, pred, w, rank)
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

mean_center_data1 <- function(df, cols) {
  for(col in cols) {
    if( is.numeric(df[, col]) ){
      # https://stackoverflow.com/questions/10287545/backtransform-scale-for-plotting
      # https://rdrr.io/cran/DMwR/man/unscale.html
      df[, col] = scale(df[, col], center = T, scale = F)[, 1]
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


exp_E1_old <- function(data) {
  
  dvars = c("SOURCE_EUI", "SOURCE_ENERGY", "FINALWT")
  ivars = setdiff(names(data), dvars)
  
  dvar = "SOURCE_EUI"
  model1 = paste(dvar, "~", paste(ivars, collapse = " + "))
  data1 = mean_center_data(data, ivars)
  fit1 = lm(model1, data = data1, weights = data$FINALWT)
  
  # pre = predict(fit1)
  # R2(data1$SOURCE_EUI, pre)
  # cor(data1$SOURCE_EUI, pre)^2
  # 
  # Metrics::rmse(data1$SOURCE_EUI, pre)
  # df = data.frame(obs=data$SOURCE_EUI, pred = pre)
  # defaultSummary(df)
  
  s1 = getModelStats(fit1, data$SOURCE_EUI)
  df1 = data.frame("exp" = "E1", "interaction" = 1, "model" = "Reg",
                   "dependent" = dvar, s1)
  
  dvar = "SOURCE_ENERGY"
  model2 = paste(dvar, "~", paste(ivars, collapse = " + "))
  fit2 = lm(model2, data = data, weights = data$FINALWT)
  pre = predict(fit2)
  Metrics::mape(data1$SOURCE_ENERGY, pre)
  
  s2 = getModelStats(fit2, data$SOURCE_ENERGY)
  df2 = data.frame("exp" = "E1", "interaction" = 1, "model" = "Reg",
                   "dependent" = dvar, s2)
  
  df = rbind(df1, df2)
  return(df)
}

exp_E1 <- function(data, int=1) {
  
  dvars = c("SOURCE_EUI", "SOURCE_ENERGY")
  wvar  = "FINALWT"
  ivars = setdiff(names(data), c(dvars, wvar))
  fweight = data[, wvar]
  
  res = NULL
  for(dvar in dvars) {
    x = mean_center_data(data, ivars)
    
    if(int == 1) {
      model = paste(dvar, "~", paste(ivars, collapse = " + "))  
    } else {
      allvars = paste(ivars, collapse = " + ")
      model = paste(dvar, "~ (", allvars, ") ^", int )
    }
    
    fit = lm(model, data = x, weights = data$FINALWT)
    pred = as.numeric(predict(fit))
    
    r2  = summary(fit)$r.squared
    r2a = summary(fit)$adj.r.squared
    print(paste(dvar, r2, r2a))
    
    # res = x$SOURCE_EUI - pred
    # r2 = 1 - var(res) / var(x$SOURCE_EUI)
    # #as per ?summary.lm
    # t1 = sum(res^2)
    # y = pred
    # t2 = sum((y - mean(y))^2)
    # r2a = 1 - t1/t2
    
    # 1 - (1 - r2) * ((n - df.int)/rdf)

    y = data[,dvar]
    s1 = getModelStats(fit, y, pred, data$FINALWT)
    df1 = data.frame("exp" = "E1", 
                     "dependent" = dvar, 
                     "model" = "Reg",
                     "interaction" = int,
                     "transform" = "meanCent",
                     s1)
    res = rbind(res, df1)
    
    x = mean_center_data(data, ivars)
    x[,dvar] = log(x[,dvar])
    fit = lm(model, data = x, weights = data$FINALWT)
    pred = as.numeric(predict(fit))
    
    r2  = summary(fit)$r.squared
    r2a = summary(fit)$adj.r.squared
    print(paste(dvar, r2, r2a))
    
    y = data[,dvar]
    s1 = getModelStats(fit, x[,dvar], pred, data$FINALWT, islog = T)
    df1 = data.frame("exp" = "E1", 
                     "dependent" = dvar, 
                     "model" = "Reg",
                     "interaction" = int,
                     "transform" = "meanCent_logDV",
                     s1)
    res = rbind(res, df1)
    
    
    # pre = predict(fit1)
    # R2(data1$SOURCE_EUI, pre)
    # cor(data1$SOURCE_EUI, pre)^2
    # 
    # Metrics::rmse(data1$SOURCE_EUI, pre)
    # df = data.frame(obs=data$SOURCE_EUI, pred = pre)
    # defaultSummary(df)
  }
  return(res)
}


regLassoRidge <- function(x, y, alpha, 
                          weight = rep(1,length(y))) {
  
  cv1 <- cv.glmnet(as.matrix(x), y, weights = weight,
                   #family = "binomial", 
                   nfold = 10, 
                   type.measure = "mse", 
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
                    type.measure = "mse", 
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
exp_E1a <- function(data) {

  dvars = c("SOURCE_EUI", "SOURCE_ENERGY")
  wvar  = "FINALWT"
  ivars = setdiff(names(data), c(dvars, wvar))
  wt    = data$FINALWT
  
  res = NULL
  for(dvar in dvars) {
    x = mean_center_data(data, ivars)
    y = data[,dvar]
    
    model = paste(dvar, "~", paste(ivars, collapse = " + "))
    dummy = dummyVars(model, data = x)
    x0 = as.data.frame(predict(dummy, x))
    
    ## Lasso 
    fit = regLassoRidge(x0, y, alpha = 1, weight = wt) # Lasso
    pred = as.numeric(predict(fit, as.matrix(x0)))
    s1 = getModelStatsGLMnet(fit, x0, y, pred, wt)
    df1 = data.frame("exp" = "E1a", 
                     "dependent" = dvar, 
                     "model" = "Lasso",
                     "interaction" = 1, 
                     "transform" = "meanCent",
                     s1)
    res = rbind(res, df1)
    
    ## Rigde 
    fit = regLassoRidge(x0, y, alpha = 0, weight = wt) # Lasso
    pred = as.numeric(predict(fit, as.matrix(x0)))
    s1 = getModelStatsGLMnet(fit, x0, y, pred, wt)
    df1 = data.frame("exp" = "E1a", 
                     "dependent" = dvar, 
                     "model" = "Ridge",
                     "interaction" = 1, 
                     "transform" = "meanCent",
                     s1)
    res = rbind(res, df1)
    
    ## ElasticNet 
    fit = regElasticNet(x0, y, weight = wt) #Elastic net
    pred = as.numeric(predict(fit, as.matrix(x0)))
    s1 = getModelStatsGLMnet(fit, x0, y, pred, wt)
    df1 = data.frame("exp" = "E1a", 
                     "dependent" = dvar, 
                     "model" = "ElasticNet",
                     "interaction" = 1, 
                     "transform" = "meanCent",
                     s1)
    res = rbind(res, df1)
    
    ########### using log transformed dependent variable
    x = mean_center_data(data, ivars)
    x[,dvar] = log(x[,dvar])
    y = log(data[,dvar])
    
    model = paste(dvar, "~", paste(ivars, collapse = " + "))
    dummy = dummyVars(model, data = x)
    x0 = as.data.frame(predict(dummy, x))
    
    ## Lasso 
    fit = regLassoRidge(x0, y, alpha = 1, weight = wt) # Lasso
    pred = exp(as.numeric(predict(fit, as.matrix(x0))))
    s1 = getModelStatsGLMnet(fit, x0, exp(y), pred, wt)
    df1 = data.frame("exp" = "E1a", 
                     "dependent" = dvar, 
                     "model" = "Lasso",
                     "interaction" = 1, 
                     "transform" = "meanCent_logDV",
                     s1)
    res = rbind(res, df1)
    
    ## Rigde 
    fit = regLassoRidge(x0, y, alpha = 0, weight = wt) # Lasso
    pred = exp(as.numeric(predict(fit, as.matrix(x0))))
    s1 = getModelStatsGLMnet(fit, x0, exp(y), pred, wt)
    df1 = data.frame("exp" = "E1a", 
                     "dependent" = dvar, 
                     "model" = "Ridge",
                     "interaction" = 1, 
                     "transform" = "meanCent_logDV",
                     s1)
    res = rbind(res, df1)
    
    ## ElasticNet 
    fit = regElasticNet(x0, y, weight = wt) #Elastic net
    pred = exp(as.numeric(predict(fit, as.matrix(x0))))
    s1 = getModelStatsGLMnet(fit, x0, exp(y), pred, wt)
    df1 = data.frame("exp" = "E1a", 
                     "dependent" = dvar, 
                     "model" = "ElasticNet",
                     "interaction" = 1, 
                     "transform" = "meanCent_logDV",
                     s1)
    res = rbind(res, df1)
  }
    
  return(res)
}

# using lasso regresion
exp_E1a_old <- function(data) {
  
  dvars = c("SOURCE_EUI", "SOURCE_ENERGY", "FINALWT")
  ivars = setdiff(names(data), dvars)
  
  dvar = "SOURCE_EUI"
  model1 = paste(dvar, "~", paste(ivars, collapse = " + "))
  data1 = mean_center_data(data, ivars)
  
  dummy = dummyVars(model1, data = data1)
  x = as.data.frame(predict(dummy, data1))
  y = data$SOURCE_EUI
  
  ## Lasso 
  fit1 = regLassoRidge(x, y, alpha = 1, weight = data$FINALWT) # Lasso
  s1 = getModelStatsGLMnet(fit1, x, y)
  df1 = data.frame("exp" = "E1a", "interaction" = 1, "model" = "Reg-Lasso",
                   "dependent" = dvar, s1)
  
  # Rigde
  fit2 = regLassoRidge(x, y, alpha = 0, weight = data$FINALWT) # Ridge
  s2 = getModelStatsGLMnet(fit2, x, y)
  df2 = data.frame("exp" = "E1a", "interaction" = 1, "model" = "Reg-Ridge",
                   "dependent" = dvar, s2)
  
  # Elastic net
  fit3 = regElasticNet(x, y, weight = data$FINALWT) #Elastic net
  s3 = getModelStatsGLMnet(fit3, x, y)
  df3 = data.frame("exp" = "E1a", "interaction" = 1, "model" = "Reg-ElasticNet",
                   "dependent" = dvar, s3)
  
  
  dvar = "SOURCE_ENERGY"
  model1 = paste(dvar, "~", paste(ivars, collapse = " + "))
  data1 = mean_center_data(data, ivars)
  dummy = dummyVars(model1, data = data1)
  x = as.data.frame(predict(dummy, data1))
  y = data$SOURCE_ENERGY
  
  ## Lasso 
  fit1 = regLassoRidge(x, y, alpha = 1, weight = data$FINALWT) # Lasso
  s1 = getModelStatsGLMnet(fit1, x, y)
  df4 = data.frame("exp" = "E1a", "interaction" = 1, "model" = "Reg-Lasso",
                   "dependent" = dvar, s1)
  
  # Rigde
  fit2 = regLassoRidge(x, y, alpha = 0, weight = data$FINALWT) # Ridge
  s2 = getModelStatsGLMnet(fit2, x, y)
  df5 = data.frame("exp" = "E1a", "interaction" = 1, "model" = "Reg-Ridge",
                   "dependent" = dvar, s2)
  
  # Elastic net
  fit3 = regElasticNet(x, y, weight = data$FINALWT) #Elastic net
  s3 = getModelStatsGLMnet(fit3, x, y)
  df6 = data.frame("exp" = "E1a", "interaction" = 1, "model" = "Reg-ElasticNet",
                   "dependent" = dvar, s3)
  
  res = rbind(df1, df2, df3, df4, df5, df6)
  stopImplicitCluster()
  
  return(res)
}

exp_E2 <- function(data) {
  
  dvars = c("SOURCE_EUI", "SOURCE_ENERGY", "FINALWT")
  ivars = setdiff(names(data), dvars)
  
  ## with interactions
  resall = NULL
  int_depth = length(ivars)
  for(int in 2:int_depth) {
    print(paste("RegInt", int))
    res = exp_E1(data, int)
    
    res$exp   = "E2"
    res$model = "RegInt"
    resall = rbind(resall, res)
  }
  return(resall)
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


tuneRpart <- function(x, y, weight = rep(1,length(y))) {
  
  train.control <- trainControl(
    method = "repeatedcv",
    number = 10, ## 10-fold CV
    repeats = 3## repeated three times
  )
  
  tune.gridcart <- expand.grid(cp = seq(0,1,0.01))
  
  rpartFit2 <- train(x, y, 
                     weights = weight,
                     method = "rpart", 
                     tuneGrid =tune.gridcart,
                     trControl = train.control)
  return(rpartFit2$finalModel)
}


exp_E3a <- function(data) {
  
  dvars = c("SOURCE_EUI", "SOURCE_ENERGY")
  wvar  = "FINALWT"
  ivars = setdiff(names(data), c(dvars, wvar))
  wt    = data$FINALWT
  
  res = NULL
  for(dvar in dvars) {
    x = data[, ivars]
    y = data[, dvar]
    
    model = paste(dvar, "~", paste(ivars, collapse = " + "))
    dummy = dummyVars(model, data = data)
    x0 = as.data.frame(predict(dummy, data))
    
    rp  = tuneRpart(x0, y, wt)
    #rp2 = tuneRpart2(x0, y, wt)
    pred = as.numeric(predict(rp, x0))
    s1 = getModelStatsRpart(rp, x0, y, pred, wt)
    df1 = data.frame("exp" = "E3a", 
                     "dependent" = dvar, 
                     "model" = "rpart",
                     "interaction" = 1, 
                     "transform" = "meanCent",
                     s1)
    res = rbind(res, df1)
    
    ####### using log
    x = data[, ivars]
    y = log(data[,dvar])
    
    model = paste(dvar, "~", paste(ivars, collapse = " + "))
    dummy = dummyVars(model, data = data)
    x0 = as.data.frame(predict(dummy, data))
    
    rpart = tuneRpart(x0, y, wt)
    pred = exp(as.numeric(predict(rpart, x0)))
    s1 = getModelStatsRpart(rpart, x0, exp(y), pred, wt)
    df1 = data.frame("exp" = "E3a", 
                     "dependent" = dvar, 
                     "model" = "rpart",
                     "interaction" = 1, 
                     "transform" = "meanCent_logDV",
                     s1)
    res = rbind(res, df1)
  }

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
  #saveRDS(gridxgFit1, "cbecs_office_xgboost.RDS")
  return(gridxgFit1$finalModel)
}

tuneXgboost_gridSearch <- function(x, y, weight = rep(1,length(y))) {
  
  train.control <- trainControl(
    method = "repeatedcv",
    number = 10, ## 10-fold CV
    repeats = 2 ## repeated three times
  )
  
  #tune.gridcart <- expand.grid(maxdepth = 2:10)
  tune.gridxgb <- expand.grid(
    eta = seq(0.1, 0.9, 0.1),
    nrounds = seq(10, 100, 10),
    max_depth = 3,  # 4
    min_child_weight = seq(1,5,1),
    colsample_bytree = seq(0.1, 1, 0.1),
    gamma = seq(0, 1, 0.1),
    subsample = seq(0.2, 1, 0.2))
  
  dim(tune.gridxgb)
  
  gridxgFit1  <- train(x, y, 
                       weights = weight,
                       method = "xgbTree", 
                       tuneGrid =tune.gridxgb,
                       trControl = train.control,
                       verbose = TRUE)
  return(gridxgFit1$finalModel)
}

exp_E3b <- function(data) {
  
  dvars = c("SOURCE_EUI", "SOURCE_ENERGY")
  wvar  = "FINALWT"
  ivars = setdiff(names(data), c(dvars, wvar))
  wt    = data$FINALWT
  
  res = NULL
  for(dvar in dvars) {
    print(paste(Sys.time(), "xgboost", dvar))
    x = data[, ivars]
    y = data[, dvar]
    
    model = paste(dvar, "~", paste(ivars, collapse = " + "))
    dummy = dummyVars(model, data = data)
    x0 = as.data.frame(predict(dummy, data))
    
    #xgfit = tuneXgboost(x0, y, wt)
    xgfit = tuneXgboost_defaultParams(x0, y, wt)
    pred = as.numeric(predict(xgfit, as.matrix(x0)))
    s1 = getModelStatsXgboost(xgfit, x0, y, pred, wt)
    df1 = data.frame("exp" = "E3b", 
                     "dependent" = dvar, 
                     "model" = "xgboost",
                     "interaction" = 1, 
                     "transform" = "meanCent",
                     s1)
    res = rbind(res, df1)
    
    ####### using log
    print(paste(Sys.time(), "xgboost", dvar, "log"))
    x = data[, ivars]
    y = log(data[,dvar])
    
    model = paste(dvar, "~", paste(ivars, collapse = " + "))
    dummy = dummyVars(model, data = data)
    x0 = as.data.frame(predict(dummy, data))
    
    #xgfit = tuneXgboost(x0, y, wt)
    xgfit = tuneXgboost_defaultParams(x0, y, wt)
    pred = as.numeric(predict(xgfit, as.matrix(x0)))
    s1 = getModelStatsXgboost(xgfit, x0, exp(y), pred, wt)
    df1 = data.frame("exp" = "E3b", 
                     "dependent" = dvar, 
                     "model" = "xgboost",
                     "interaction" = 1, 
                     "transform" = "meanCent_logDV",
                     s1)
    res = rbind(res, df1)
  }
  
  
  
  return(res)
}

tuneGBM_randomSearch <- function(x, y, weight = rep(1,length(y))) {
  
  train.control <- trainControl(
    method = "repeatedcv",
    number = 10, ## 10-fold CV
    repeats = 3, ## repeated three times
    search = "random")
  
  gridxgFit1  <- train(x, y, 
                       weights = weight,
                       method = "gbm", 
                       trControl = train.control,
                       verbose = F
                       #interaction.depth = 2
                       )
  #saveRDS(gridxgFit1, "cbecs_office_xgboost.RDS")
  return(gridxgFit1$finalModel)
}

tuneGBM_gridSearch <- function(x, y, weight = rep(1,length(y))) {
  
  train.control <- trainControl(
    method = "repeatedcv",
    number = 10, ## 10-fold CV
    repeats = 3, ## repeated three times
    search = "random")
  
  gbmGrid <-  expand.grid(interaction.depth = c(1, 2, 3), 
                          n.trees = (1:30)*50, 
                          shrinkage = c(1:5)*0.1,
                          n.minobsinnode = c(1:4)*5)
  
  gridxgFit1  <- train(x, y, 
                       weights = weight,
                       method = "gbm", 
                       trControl = train.control,
                       tuneGrid = gbmGrid,
                       verbose = F)
  #saveRDS(gridxgFit1, "cbecs_office_xgboost.RDS")
  return(gridxgFit1$finalModel)
}

# https://www.r-bloggers.com/variable-selection-with-elastic-net/  
pkgs <- list("glmnet", "doParallel", "foreach", "pROC")
lapply(pkgs, require, character.only = T)
#registerDoParallel(cores = 4)
#stopImplicitCluster()

ncore = 4

#load_dir = './data/features/'
#save_dir = './data/results/'

load_dir = './data/features_all/'
save_dir = './data/results_all/'


btypes = c("office", "retail", "k12school", 
           "warehouse",
           "hotel", "worship", "multifamily")

for (btype in btypes) {
  #btype = "k12school"
  data = read.csv(paste0(load_dir, btype, ".csv"))
  print(paste(Sys.time(), btype, paste(dim(data), collapse = " x ")))
  
  #data = data[, c(-1)]
  stopImplicitCluster()
  e1  = exp_E1(data)
  
  #registerDoParallel(cores = ncore)
  #e1a = exp_E1a(data)
  #stopImplicitCluster()
  
  e2  = exp_E2(data)
  
  #data = data[, c(-1)]
  registerDoParallel(cores = ncore)
  e3a  = exp_E3a(data)
  stopImplicitCluster()
  
  registerDoParallel(cores = ncore)
  e3b  = exp_E3b(data)
  stopImplicitCluster()
  
  resall = rbind(e1, e2, e3a, e3b)
  write.csv(resall, paste0(save_dir, btype, ".csv"))
}


############# compare the interactions using OLS ##############


load_dir = './data/features_all/'
save_dir = './data/results_all/'
tab_dir = '../misc/'

btype = "office"
#btype = "multifamily"
data = read.csv(paste0(load_dir, btype, ".csv"))
print(paste(Sys.time(), btype, paste(dim(data), collapse = " x ")))

library(dplyr)
data = data %>% 
  rename('SourceEnergy' = SOURCE_ENERGY) %>%
  rename(SqFt = SQFT) %>%
  rename(OpenHours = WKHRS) %>%
  rename(WorkersCnt = NWKER) %>%
  rename(ComputersCnt = PCTERMN_TOT) %>%
  rename('CooledPercent' = COOLP) 

data = data %>% 
  mutate('SourceEnergyLog' = log(SourceEnergy))

#data = data %>% 
#  filter(ComputersCnt <= quantile(ComputersCnt, probs = c(0.95)))

dvars = c("SOURCE_EUI", "SourceEnergy", "SourceEnergyLog")
wvar  = "FINALWT"
#dvar = "SourceEnergy"
dvar = "SOURCE_EUI"
ivars = setdiff(names(data), c(dvars, wvar))
fweight = data[, wvar]

x = mean_center_data1(data, setdiff(ivars, "IsBank"))
model = paste(dvar, "~", paste(ivars, collapse = " + "))    
fit = lm(model, data = x, weights = data$FINALWT)
pred = as.numeric(predict(fit))
y = data[,dvar]
s1 = getModelStats(fit, y, pred, data$FINALWT)
int1 = fit

#interactions
int = 2
allvars = paste(ivars, collapse = " + ")
model = paste(dvar, "~ (", allvars, ") ^", int )
fit = lm(model, data = x, weights = data$FINALWT)
pred = as.numeric(predict(fit))
y = data[,dvar]
s2 = getModelStats(fit, y, pred, data$FINALWT)
int2 = fit
int2$rank

print(getModelStats(int1, y, predict(int1), data$FINALWT))
int1sw = step(int1, trace = F)
print(getModelStats(int1sw, y, predict(int1sw), data$FINALWT))

print(getModelStats(int2, y, predict(int2), data$FINALWT))
int2sw = step(int2, trace = F)
print(getModelStats(int2sw, y, predict(int2sw), data$FINALWT))

library(MASS)
int1sw = step.model <- stepAIC(int1, direction = "both", 
                               trace = FALSE)
summary(int1sw)


int2sw = step.model <- stepAIC(int2, direction = "both", 
                               trace = FALSE)
summary(int2sw)



## save the model summary in table
library(stargazer)
tab1 = stargazer(int1, align=TRUE, 
                 star.cutoffs = c(.05, .01, 0.001), digits = 2,
                 no.space = T)
write(tab1, paste0(tab_dir,btype, "lm_int1.tex"))

sum1 = as.data.frame(summary(int1)$coefficients)
all1 = row.names(sum1)
sum1 = sum1[sum1$`Pr(>|t|)` <= 0.05, ]
sum1 = sum1[order(sum1$`Pr(>|t|)`), ]
sum1col = rownames(sum1)

sum2 = as.data.frame(summary(int2)$coefficients)
all2 = row.names(sum2)
sum2 = sum2[sum2$`Pr(>|t|)` <= 0.05, ]
sum2 = sum2[order(sum2$`Pr(>|t|)`), ]
sum2col = rownames(sum2)

alls = c(all1,all2)
keeps = c(sum1col, sum2col)
omits = unique(setdiff(alls, keeps))

tab1 = stargazer(int1, int2, 
                 align=TRUE, 
                 type="html", 
                 dep.var.labels.include	 = F,
                 #star.char = c("+", "*", "**", "***"),
                 #star.cutoffs = c(0.05, 0.01, 0.001, 0.0001),
                 #notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
                 notes.append = F,
                 digits = 3,
                 no.space = T, single.row=TRUE,
                 header = FALSE, df=FALSE,
                 report = "vc*",
                 #keep = keeps,
                 #omit = omits,
                 #omit.stat = c("ser", "f"),
                 ord.intercepts = T,	
                 column.labels = c("OLS", "OLSi2"),
                 dep.var.caption = "")
#tab1 = texreg::texreg(l = list(int1, int2), single.row = T)
write(tab1, paste0(tab_dir,btype, "lm_int1.html"))

library(jtools)
library(sjPlot)

model = paste(dvar, "~", paste(ivars, collapse = " + "))    
fit1 = lm(model, data = data)
int = 2
allvars = paste(ivars, collapse = " + ")
model = paste(dvar, "~ (", allvars, ") ^", int )
fit2 = lm(model, data = data)


  
#effect_plot(int2, pred = SQFT)
#cat_plot(int2, pred = IsHighSchoolYes, modx = IsCookingYes)
library(effects)
plot(effect("SqFt", fit1))
plot(effect("OpenHours", fit1))
eff = effect("SqFt*OpenHours", fit2)


eff = effects::allEffects(mod=int2)
plot(eff)
plot(eff, c("WorkersCnt:CooledPercent", "SqFt:OpenHours"), cols=2)
plot(eff)

effect_plot(fit2, pred = WorkersCnt)
effect_plot(fit2, pred = CooledPercent)

names(fit1$model)

plot(effect("OpenHours*ComputersCnt", fit2))

qt1 = quantile(int2$model$ComputersCnt, probs = c(0.05, 0.5, 0.95))
qt2 = quantile(int2$model$ComputersCnt, probs = seq(0.1, 0.9, 0.2))
qt3 = quantile(int2$model$ComputersCnt)

qt4 = summary(int2$model$ComputersCnt)[2:5]

interact_plot(int1, pred = 'OpenHours', modx = 'ComputersCnt',
              #modx.values = round(qt4,1), 
              #modx.labels = names(qt4),
              interval=F)
interact_plot(int2, pred = 'OpenHours', modx = 'ComputersCnt',
              #modx.values = round(qt4,1), 
              #modx.labels = names(qt4),
              interval=F)

interact_plot(int1, pred = 'WorkersCnt', modx = 'ComputersCnt', rug = F)
interact_plot(int2, pred = 'WorkersCnt', modx = 'ComputersCnt', rug = F)

interact_plot(int1, pred = 'SqFt', modx = 'OpenHours') +
  scale_y_continuous(limits = c(0, 6348712))
interact_plot(int2, pred = 'SqFt', modx = 'OpenHours') + 
  scale_y_continuous(limits = c(0, 6348712))

interact_plot(int1, pred = 'SqFt', modx = 'ComputersCnt')
interact_plot(int2, pred = 'SqFt', modx = 'ComputersCnt')

interact_plot(int1, pred = 'SqFt', modx = 'ComputersCnt')
interact_plot(int2, pred = 'SqFt', modx = 'ComputersCnt')

plot(effect("WorkersCnt", int1))
plot(effect("OpenHours", int1))

interact_plot(int2, pred = 'WorkersCnt', 
              modx = 'OpenHours')

vals = c( mean(data$WorkersCnt) + sd(data$WorkersCnt),
          mean(data$WorkersCnt),
          mean(data$WorkersCnt) - sd(data$WorkersCnt))
vals = round(vals, 0)
labels = c( paste0("+ 1 SD (", vals[1], ")"),
            paste0("Mean (", vals[2], ")"),
            paste0("- 1 SD (", vals[3], ")"))
             
############ final plots
p1 = interact_plot(int1, pred = 'OpenHours', modx = 'WorkersCnt',
                   #modx.labels = labels,  
              legend.main = 'Number of employees',
              x.label = 'Total hours open per week',
              y.label = 'Source Energy (kBtu)')
print(p1)
export::graph2png(file="./plots/interactions_office1.png", 
                  width=5, height=4)

p2 = interact_plot(int2, pred = 'OpenHours', modx = 'WorkersCnt',
                   #modx.labels = labels,
                   legend.main = 'Number of employees',
                   x.label = 'Total hours open per week',
                   y.label = 'Source Energy (kBtu)')
print(p2)
export::graph2png(file="./plots/interactions_office2.png", 
                  width=5, height=4)
library(ggplot2)
library(grid)
library(scales)
p1a = 
  p1 + 
  #theme(legend.position = "none") +
  theme( legend.justification=c(0,1), legend.position=c(0.01,0.99)) + 
  theme(axis.title.x = element_blank()) + 
  theme(axis.text.x = element_blank()) + 
  #theme(legend.position = "top") + 
  scale_y_continuous(limits = c(0000, 5213457),
                     label = unit_format(unit = "M", scale = 1e-6)) + 
  theme(legend.title=element_text(size=8, face = "plain")) + 
  theme(legend.text=element_text(size=7, face = "plain")) + 
  theme(axis.title.y=element_text(size=9, face = "plain"))  + 
  theme(plot.subtitle=element_text(size=9, face = "plain"))  + 
  theme(
    legend.key.width=unit(0.5,"cm"),legend.key.height=unit(0.2,"cm")) + 
  labs(subtitle = "a) OLS model without interactions") 


p2a = p2 + 
#  theme(axis.title.x = element_blank()) + 
#  theme(axis.text.x = element_blank()) + 
  #theme(legend.position = "bottom") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0000, 5213457),
                     label = unit_format(unit = "M", scale = 1e-6)) + 
  theme(axis.title.y=element_text(size=9, face = "plain")) + 
  theme(axis.title.y=element_text(size=9, face = "plain")) + 
  theme(axis.title.x=element_text(size=9, face = "plain")) + 
  theme(axis.title.x=element_text(size=9, face = "plain")) + 
  theme(legend.title=element_text(size=9, face = "plain")) + 
  theme(legend.text=element_text(size=9, face = "plain")) + 
  theme(axis.title.y=element_text(size=9, face = "plain"))  +
  theme(plot.subtitle=element_text(size=9, face = "plain"))  + 
  theme(
    legend.key.width=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm")) + 
  labs(subtitle = "b) OLS model with two-way interactions") 

grid.newpage()
p3 = grid.draw(rbind(ggplotGrob(p1a), ggplotGrob(p2a), size = "last"))

export::graph2png(file="./plots/interactions_office_combined.png", 
                  width=5, height=6)

library(tikzDevice)
theme_set(theme_gray(base_size = 8))
tikz(file = "./plots/interactions_office_combined.tex", width = 3.4, height = 4)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1a), ggplotGrob(p2a), size = "last"))
dev.off()

interact_plot(fit1, pred = 'WKHRS', modx = 'PCTERMN_TOT',
              x.label = 'Total hours open per week',
              y.label = 'Source Energy (kBtu)',
              pred.labels = 'as',
              modx.labels = c('Number of computers', 'a', 'b'))
interact_plot(fit2, pred = 'WKHRS', modx = 'PCTERMN_TOT')





### plot top-3 interactions

labl_energy = 'Source Energy (kBtu)'
labl_SqFt = 'Gross Floor Area'
labl_OpenHours = 'Total hours open per week'
labl_WorkersCnt = 'Number of employees'
labl_ComputersCnt = 'Number of computers'


### 1st most important factors - OpenHours:ComputersCnt 
p1a = interact_plot(int1, pred = 'OpenHours', modx = 'ComputersCnt',
                    color.class = "Qual1",
                    legend.main = labl_ComputersCnt,
                    x.label = labl_OpenHours, y.label = labl_energy)
p1b = interact_plot(int2, pred = 'OpenHours', modx = 'ComputersCnt',
                    color.class = "Qual1",
                    legend.main = labl_ComputersCnt,
                    x.label = labl_OpenHours, y.label = labl_energy)

### 2nd most important factors - sqft vs openhours
p2a = interact_plot(int1, pred = 'OpenHours', modx = 'SqFt',
                    color.class = "Qual1",
                    legend.main = labl_SqFt,
                    x.label = labl_OpenHours, y.label = labl_energy)
p2b = interact_plot(int2, pred = 'OpenHours', modx = 'SqFt',
                    color.class = "Qual1",
                    legend.main = labl_SqFt,
                    x.label = labl_OpenHours, y.label = labl_energy)

### 3rd most important factors - WorkersCnt:ComputersCnt
p3a = interact_plot(int1, pred = 'WorkersCnt', modx = 'ComputersCnt',
                    color.class = "Qual1",
                    legend.main = labl_ComputersCnt,
                    x.label = labl_WorkersCnt, y.label = labl_energy)
p3b = interact_plot(int2, pred = 'WorkersCnt', modx = 'ComputersCnt',
                    color.class = "Qual1",
                    legend.main = labl_ComputersCnt,
                    x.label = labl_WorkersCnt, y.label = labl_energy)

### 4th most important factors - WorkersCnt:WorkersCnt
p4a = interact_plot(int1, pred = 'OpenHours', modx = 'WorkersCnt',
                    color.class = "Qual1",
                    legend.main = labl_WorkersCnt,
                    x.label = labl_OpenHours, y.label = labl_energy)
p4b = interact_plot(int2, pred = 'OpenHours', modx = 'WorkersCnt',
                    color.class = "Qual1",
                    legend.main = labl_WorkersCnt,
                    x.label = labl_OpenHours, y.label = labl_energy)
yall = c(
  layer_scales(p1a)$y$range$range, 
  layer_scales(p1b)$y$range$range,
  layer_scales(p2a)$y$range$range, 
  layer_scales(p2b)$y$range$range,
  layer_scales(p3a)$y$range$range, 
  layer_scales(p3b)$y$range$range,
  layer_scales(p4a)$y$range$range, 
  layer_scales(p4b)$y$range$range)

ymin = summary(yall)[1]
ymax = summary(yall)[6]

#scale_y_continuous(limits = c(0000, 5213457),
#                   label = unit_format(unit = "M", scale = 1e-6)) + 
  
  

p1a = 
  p1a + 
  scale_y_continuous(limits = c(ymin, ymax), 
                     label = unit_format(unit="", scale = 1e-6)) + 
  #labs(subtitle = "a) OLS model without using interaction terms") +
  theme_pubr(base_size=10) + 
  theme( legend.justification=c(0,1), legend.position=c(0.01,1)) + 
  theme( legend.key.width=unit(0.6,"cm"),legend.key.height=unit(0.2,"cm")) +
  theme(axis.title.x = element_blank())

p1b = p1b + 
  scale_y_continuous(limits = c(ymin, ymax),
                     label = unit_format(unit="", scale = 1e-6)) + 
  #labs(subtitle = "b) OLS model with two-way interaction terms") + 
  theme_pubr(base_size=10) + 
  theme(legend.position = "none")

p1 = ggpubr::ggarrange(p1a, p1b, 
                       ncol=1, nrow=2,
                       common.legend = F
                       #legend="top"
                       )
p1

p2a = p2a + 
  scale_y_continuous(limits = c(ymin, ymax)) + 
  #labs(subtitle = "a) OLS model without using interaction terms") + 
  theme_pubr(base_size=10) + 
  theme( legend.justification=c(0,1), legend.position=c(0.01,1)) + 
  theme( legend.key.width=unit(0.6,"cm"),legend.key.height=unit(0.2,"cm")) +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())
  
p2b = p2b + 
  scale_y_continuous(limits = c(ymin, ymax)) + 
  #labs(subtitle = "b) OLS model with two-way interaction terms") + 
  theme_pubr(base_size=10) + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  theme(legend.position = "none")

p2 = ggpubr::ggarrange(p2a, p2b, ncol=1, nrow=2)
p2


p3a = p3a + 
  scale_y_continuous(limits = c(ymin, ymax)) + 
  #labs(subtitle = "a) OLS model without using interaction terms") + 
  theme_pubr(base_size=10) + 
  theme( legend.justification=c(0,1), legend.position=c(0,1)) + 
  theme( legend.key.width=unit(0.6,"cm"),legend.key.height=unit(0.2,"cm")) +
  theme( legend.background = element_rect(fill="transparent")) +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())


p3b = p3b + 
  scale_y_continuous(limits = c(ymin, ymax)) + 
  #labs(subtitle = "b) OLS model with two-way interaction terms") + 
  theme_pubr(base_size=10) + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  theme(legend.position = "none")

p3 = ggpubr::ggarrange(p3a, p3b, ncol=1, nrow=2)
p3

p4a = p4a + 
  scale_y_continuous(limits = c(ymin, ymax)) + 
  #labs(subtitle = "a) OLS model without using interaction terms") + 
  theme_pubr(base_size=10) + 
  theme( legend.justification=c(0,1), legend.position=c(0,1)) + 
  theme( legend.key.width=unit(0.6,"cm"),legend.key.height=unit(0.2,"cm")) +
  theme( legend.background = element_rect(fill="transparent")) +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank()) 


p4b = p4b + 
  scale_y_continuous(limits = c(ymin, ymax)) + 
  #labs(subtitle = "b) OLS model with two-way interaction terms") + 
  theme_pubr(base_size=10) + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  theme(legend.position = "none")

p4 = ggpubr::ggarrange(p4a, p4b, ncol=1, nrow=2)
p4

#grid.draw(cbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3),
#                size = "first"))

#grid.draw(cbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3),
#                size = "last"))
#grid.arrange(p1, p2, p3)

library(cowplot)

prow <- cowplot::plot_grid( 
  p1, p2, p4, p3,
  align = 'h',
  #labels = c("A", "B", "C"),
  hjust = -1,
  rel_widths = c(1.2, 1, 1, 1),
  nrow = 1)
prow

library(tikzDevice)
theme_set(theme_gray(base_size = 10))
tikz(file = "./plots/interactions_office_four.tex", width = 7, height = 3.5)
print(prow)
dev.off()

export::graph2png(file="./plots/interactions_office_four.png", 
                  width=12, height=6)





#s1 = lm(SourceEnergy ~ ComputersCnt + WorkersCnt, data = data)
#s2 = lm(SourceEnergy ~ ComputersCnt * WorkersCnt, data = data)
#summary(s1)
#summary(s2)


#################### compare the interactions using xgboost #####

library(caret)
library(xgboost)

dvars = c("SOURCE_EUI", "SourceEnergy", "SourceEnergyLog")
wvar  = "FINALWT"
#dvar = "SourceEnergy"
dvar = "SOURCE_EUI"

ivars = setdiff(names(data), c(dvars, wvar))
wt    = data$FINALWT

#data = data %>% filter(ComputersCnt <= 300)

x = data[, ivars]
y = data[, dvar]

model = paste(dvar, "~", paste(ivars, collapse = " + "))

int = 2
allvars = paste(ivars, collapse = " + ")
model.int = paste(dvar, "~ (", allvars, ") ^", int )

dummy = dummyVars(model, data = data, fullRank = T)
x0 = as.data.frame(predict(dummy, data))

x <- x %>% 
  mutate_if(is.numeric, round, digits = 2)

#write.csv(x, "office_all_x.csv", row.names = F)
#write.csv(data.frame(y=y),  "office_all_y.csv", row.names = F)
#write.csv(data,  "office_all.csv", row.names = F)

doParallel::registerDoParallel(cores = 4)
xgfit1 = tuneXgboost_defaultParams(x0, y, wt)
xgfit2 = tuneXgboost_randomSearch(x0, y, wt)
xgfit3 = tuneXgboost_gridSearch(x0, y, wt)
doParallel::stopImplicitCluster()

dtrain <- xgb.DMatrix(as.matrix(x0),label = y)
# param <- list(max_depth = 2, eta = 1, silent = 1, nthread = 2, 
#               objective = "binary:logistic", eval_metric = "auc")
param = as.list(xgfit2$tuneValue[1, ])
param$max_depth = 2
param$nrounds = 4

md <-xgb.train(data=dtrain, params = param, 
               nround = param$nrounds, nthread=3)
xgb.plot.multi.trees(model = md)
xgb.plot.tree(model = md, trees = 0:1, show_node_id = F, 
              plot_height=900)

library(DiagrammeR)
gr = xgb.plot.tree(model = md, trees = 0:1, show_node_id = F, render=F)
export_graph(gr, 'tree.pdf')

library(export)
graph2png(file = "xgbtree.png", width=8, height=3)

gr = xgb.plot.tree1(model = md, trees = 0:3, show_node_id = F, render=F)
#DiagrammeR::render_graph(gr, layout='tree')
#DiagrammeR::export_graph(gr, 'tree.pdf')


dt$color = "skyblue4"
dt[dt$shape == "oval", ]$color = "peachpuff4"

dt$filledcolor = "skyblue2"
dt[dt$shape == "oval", ]$filledcolor = "peachpuff"


nodes <- DiagrammeR::create_node_df(n = nrow(dt), ID = dt$ID, 
                                    label = dt$label, fillcolor = dt$filledcolor, shape = dt$shape, 
                                    color = dt$color,
                                    data = dt$Feature, fontcolor = "black")

nodes$label = str_replace_all(nodes$label, "Leaf", "EUI")

edges <- DiagrammeR::create_edge_df(from = match(dt[Feature != 
                                                      "Leaf", c(ID)] %>% rep(2), dt$ID), to = match(dt[Feature != 
                                                                                                         "Leaf", c(Yes, No)], dt$ID), label = dt[Feature != "Leaf", 
                                                                                                                                                 paste("<", Split)] %>% c(rep("", nrow(dt[Feature != 
                                                                                                                                                                                            "Leaf"]))), style = dt[Feature != "Leaf", ifelse(Missing == 
                                                                                                                                                                                                                                               Yes, "bold", "solid")] %>% c(dt[Feature != "Leaf", ifelse(Missing == 
                                                                                                                                                                                                                                                                                                           No, "bold", "solid")]), rel = "leading_to")
edges['arrowsize'] = 0.5
graph <- DiagrammeR::create_graph(nodes_df = nodes, edges_df = edges, attr_theme = NULL) %>% 
  DiagrammeR::add_global_graph_attrs(attr_type = "graph", 
                                     attr = c("layout", "rankdir"), 
                                     value = c("tree", "LR")) %>% 
  DiagrammeR::add_global_graph_attrs(attr_type = "node", 
                                     attr = c("color", "style", "fontname", "fontsize"), 
                                     value = c("DimGray", "filled", "Helvetica", "14")) %>% 
  DiagrammeR::add_global_graph_attrs(attr_type = "edge", 
                                     attr = c("color", "arrowsize", "arrowhead", "fontname", "fontsize"), 
                                     value = c("DimGray", "0.5", "vee", "Helvetica", "12"))


DiagrammeR::render_graph(graph,layout='tree')

DiagrammeR::render_graph(graph,layout='tree', width=900, height = 300)
DiagrammeR::export_graph(graph, 'tree.pdf')


xgb.plot.tree1 <- function (feature_names = NULL, model = NULL, trees = NULL, 
          plot_width = NULL, plot_height = NULL, render = TRUE, show_node_id = FALSE, 
          ...) 
{
  
  if (!inherits(model, "xgb.Booster")) {
    stop("model: Has to be an object of class xgb.Booster")
  }
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("DiagrammeR package is required for xgb.plot.tree", 
         call. = FALSE)
  }
  dt <- xgb.model.dt.tree(feature_names = feature_names, model = model, 
                          trees = trees)
  #dt[, `:=`(label, paste0(Feature, ifelse(Feature ==  "Leaf", "\nV: ", "\nG: "), round(Quality,1)))]
  dt[, `:=`(label, paste0(Feature, 
                          ifelse(Feature ==  "Leaf", paste0("\n", round(Quality,1)), "")))]  
  
  if (show_node_id) 
    dt[, `:=`(label, paste0(ID, ": ", label))]
  dt[Node == 0, `:=`(label, paste0("Tree ", Tree, "\n", label))]
  dt[, `:=`(shape, "rectangle")][Feature == "Leaf", `:=`(shape, 
                                                         "oval")]
  dt[, `:=`(filledcolor, "lightskyblue")][Feature == "Leaf", `:=`(filledcolor, 
                                                           "gold")]
  dt <- dt[order(Tree)]
  assign("dt", dt, envir = .GlobalEnv)
  
  nodes <- DiagrammeR::create_node_df(n = nrow(dt), ID = dt$ID, 
                                      label = dt$label, fillcolor = dt$filledcolor, shape = dt$shape, 
                                      data = dt$Feature, fontcolor = "black")
  edges <- DiagrammeR::create_edge_df(from = match(dt[Feature != 
                                                        "Leaf", c(ID)] %>% rep(2), dt$ID), to = match(dt[Feature != 
                                                                                                           "Leaf", c(Yes, No)], dt$ID), label = dt[Feature != "Leaf", 
                                                                                                                                                   paste("<", Split)] %>% c(rep("", nrow(dt[Feature != 
                                                                                                                                                                                              "Leaf"]))), style = dt[Feature != "Leaf", ifelse(Missing == 
                                                                                                                                                                                                                                                 Yes, "bold", "solid")] %>% c(dt[Feature != "Leaf", ifelse(Missing == 
                                                                                                                                                                                                                                                                                                             No, "bold", "solid")]), rel = "leading_to")
  graph <- DiagrammeR::create_graph(nodes_df = nodes, edges_df = edges, 
                                    attr_theme = NULL) %>% DiagrammeR::add_global_graph_attrs(attr_type = "graph", 
                                                                                              attr = c("layout", "rankdir"), value = c("dot", "LR")) %>% 
    DiagrammeR::add_global_graph_attrs(attr_type = "node", 
                                       attr = c("color", "style", "fontname"), value = c("DimGray", 
                                                                                         "filled", "Helvetica")) %>% DiagrammeR::add_global_graph_attrs(attr_type = "edge", 
                                                                                                                                                        attr = c("color", "arrowsize", "arrowhead", "fontname"), 
                                                                                                                                                        value = c("DimGray", "1", "vee", "Helvetica"))
  if (!render) 
    return(invisible(graph))
  DiagrammeR::render_graph(graph, width = plot_width, height = plot_height, layout='nicely')
}





xgfit = xgfit2
pred = as.numeric(predict(xgfit, as.matrix(x0)))
s1 = getModelStatsXgboost(xgfit, x0, y, pred, wt)
s1

xgfit = xgfit3
pred = as.numeric(predict(xgfit, as.matrix(x0)))
s1 = getModelStatsXgboost(xgfit, x0, y, pred, wt)
s1


# library(reticulate)
# shap <- import("shap")
# xgboost <- import("xgboost")
# 
# #shap$initjs()
# 
# xgb1 = xgboost$XGBRegressor()
# # Add silent=True to avoid printing out updates with each cycle
# xgb1$fit(x0, y)
# xgb1$get_params()
# 
# xgb1$feature_importances_
# #xgboost$plot_importance(xgb1)

# treeExplainer = shap$TreeExplainer(xgb1)
# shap_values = treeExplainer$shap_values(x0)
# shap$summary_plot(shap_values, x0, plot_type = 'dot')

# ip = shap$force_plot(treeExplainer$expected_value, shap_values, x0)

#xgfit = tuneXgboost_defaultParams(x0, y, wt)
vi = xgb.importance(model = xgfit)
xgb.ggplot.importance(vi)
xgb.plot.importance(vi)

xgb.plot.deepness(model = xgfit)
xgb.plot.multi.trees(model = xgfit)

newf = xgb.create.features(model = xgfit, data=as.matrix(x0))
newf1 = as.matrix(newf)

pred = predict(xgfit, as.matrix(x0))
predc = predict(xgfit, as.matrix(x0), predcontrib = T, reshape = T)
predi = predict(xgfit, as.matrix(x0), predinteraction = T, reshape = T)
pred_leaf = predict(xgfit, as.matrix(x0), predleaf = T, reshape = T)

xgb.plot.shap(data= as.matrix(x0), shap_contrib=predc,
              features = names(x0)[1:5], pch = 16, n_col=2, top_n=5)

xgb.plot.tree(model = xgfit, trees = 0:2, show_node_id = TRUE)
xgb.plot.deepness(model = xgfit)
xgb.plot.multi.trees(model = xgfit, features_keep = 11)

library(DiagrammeR)
mt = xgb.plot.multi.trees(model = xgfit, features_keep = 3, render=F)
export_graph(mt, './plots/interactions_office_xgb.pdf', width=1800, height=1500)
export_graph(mt, './plots/interactions_office_xgb.png', width=1800, height=1500)


## compare with gbm
library(gbm)
doParallel::registerDoParallel(cores = 4)
#gbm1 = tuneGBM_randomSearch(x0, y, wt)
gbm2 = tuneGBM_gridSearch(x0, y, wt)
doParallel::stopImplicitCluster()
gbmfit = gbm2

model = paste(dvar, "~", paste(names(x0), collapse = " + "))
dat = cbind(y, x0)
names(dat)[1] = dvar

gbmfit = gbm(as.formula(model), data = dat,
             interaction.depth = 5,
             cv.folds = 5, n.cores = 4)

best.iter <- gbm.perf(gbmfit, method = "cv", plot.it = T)
print(best.iter)

interact.gbm(gbmfit, data=dat, i.var=colnames(dat)[2:3])


## 
x <- model.matrix(Species ~ .^2, iris)[,-1]
#x = scale(x)
colnames(x)
label = 1*(iris$Species == "versicolor")
dat = as.matrix(x)
dtrain <- xgb.DMatrix(x, label = label)
param <- list(booster = "gblinear", objective = "reg:logistic", 
              eval_metric = "auc",lambda = 0.0003, alpha = 0.0003, nthread = 2)

bst <- xgb.train(param, dtrain, list(tr=dtrain), nrounds = 200, eta = 1.,
                 callbacks = list(cb.gblinear.history()))

vi = xgb.importance(model = bst)
xgb.plot.importance(vi)
xgb.importance(model=bst)

dt <- xgb.model.dt.tree(feature_names = colnames(dat), model=bst)

xgb.plot.shap(data= as.matrix(dat), model = bst,
              features = colnames(dat)[2], n_col=3, top_n=5)

f = "Sepal.Length"
ord <- order(dat[, f])
x <- dat[, f][ord]


library(Matrix)
xgb.plot.deepness(model = xgfit)
xgb.plot.multi.trees(model = xgfit, features_keep = 10)

newf = xgb.create.features(model = xgfit, data=as.matrix(x0))
newf1 = as.matrix(newf)

pred = predict(xgfit, as.matrix(x0))
predc = predict(xgfit, as.matrix(x0), predcontrib = T, reshape = T)
predi = predict(xgfit, as.matrix(x0), predinteraction = T, reshape = T)





library(vip)
vint(xgfit, names(x0))

featureList <- names(x0)
featureVector <- c() 
for (i in 1:length(featureList)) { 
  featureVector[i] <- paste(i-1, featureList[i], "q", sep="\t") 
}

write.table(featureVector, "fmap.txt", row.names=FALSE, quote = FALSE, col.names = FALSE)
xgb.dump(model = xgfit, fname = 'xgb_cbecs_office.dump', fmap = "fmap.txt", with.stats = TRUE)


############### old code ############################

office = read.csv(paste0(load_dir, "office.csv"))
data = office
e1  = exp_E1(data)
e1a = exp_E1a(data)
e2  = exp_E2(data)
e3a  = exp_E3a(data)

resall = rbind(e1, e1a, e2, e3a)








#files = list.files(load_dir)
office = read.csv(paste0(load_dir, "office.csv"))

data = office
dvars = c("SOURCE_EUI", "SOURCE_ENERGY", "FINALWT")
ivars = setdiff(names(data), dvars)

dvar = "SOURCE_EUI"
model1 = paste(dvar, "~", paste(ivars, collapse = " + "))
fit1 = lm(model1, data = data), weights = data$FINALWT)



y = data$SOURCE_EUI
y1 = predict(fit1)
R2(y1,y)
summary(fit1)$r.squared

res = y-y1
fit1$residuals

1 - sum(fit1$residuals^2) / sum((y - mean(y))^2)
fit1$model[, 1 - sum(fit1$residuals^2) / sum((y - mean(y))^2)]

1-sum(residuals(fit1)^2)/sum((y - mean(y))^2)




# R^2 = 1 - Sum(R[i]^2) / Sum((y[i]- y*)^2),
R = res
t1 = sum(R^2)
me = mean(R)
t2 = sum((R-me)^2)
rs = 1 - t1/t2

r2 <- cor(y,predict(fit1))^2 
r2 <- 1 - (sum((y-predict(fit1))^2)/sum((y-mean(y))^2)) 



summary(y-y1)
summary(fit1$residuals)

plot(fit1$residuals, (y-y1))

round(mean(fit1$residuals^2),3)
mse(y, y1)



library(data.table)
set.seed(154)
dt = data.table(x = rnorm(100))
dt[, y := 1+0.2*x + rnorm(100)]
fit = lm(y~x, data = dt)
summary(fit)



dvar = "SOURCE_ENERGY"
model2 = paste(dvar, "~", paste(ivars, collapse = " + "))
fit2 = lm(model2, data = data, weights = data$FINALWT)

## with interactions
int_depth = length(ivars)
for(int in 2:int_depth) {
  dvar = "SOURCE_EUI"
  allvars = paste(ivars, collapse = " + ")
  model1 = paste(dvar, "~ (", allvars, ") ^", int )
  fit1 = lm(model1, data = data, weights = data$FINALWT)
  
  dvar = "SOURCE_ENERGY"
  allvars = paste(ivars, collapse = " + ")
  model2 = paste(dvar, "~ (", allvars, ") ^", int )
  fit2 = lm(model2, data = data, weights = data$FINALWT)
}














################# make features for each dataset #####################
office = read.csv("./filtered/office.csv")
vars = c("SQFT", "WKHRS", "NWKER", "PCTERMN", "SERVERN", "LAPTPN",
         "CDD65", "COOLP", "PBAPLUS",
         "SOURCE_ENERGY", "SOURCE_EUI", "FINALWT")
data = office[, vars]

data1 = data %>%
  mutate(NWKER_SQFT = NWKER/SQFT * 1000) %>%
  mutate(PCTERMN_TOT = rowSums(select(., c(PCTERMN,SERVERN,LAPTPN)), na.rm = T)) %>% 
  mutate(PCTERMN_SQFT = PCTERMN_TOT/SQFT * 1000) %>%
  mutate(CDD65_COOLP = log(CDD65) * COOLP / 100) %>%
  mutate(IsBank = ifelse(PBAPLUS == 3, "yes", "no")) %>%
  filter(SQFT <= 100000)

ivars = c("SQFT", "WKHRS", "NWKER_SQFT", "PCTERMN_SQFT", 
          "CDD65_COOLP", "IsBank")
dvar = "SOURCE_EUI"
#dvar = "SOURCE_ENERGY"

###########################################################################

k12school = read.csv("./filtered/k-12school.csv")
vars = c("SQFT", "WKHRS", "NWKER", "PCTERMN", "SERVERN", "LAPTPN",
         "CDD65", "COOLP", 
         "HDD65", "HEATP",
         "COOK", "OPNWE",
         "PBAPLUS", "SOURCE_ENERGY", "SOURCE_EUI", "FINALWT")
data = k12school[, vars]

data1 = data %>%
  mutate(NWKER_SQFT = NWKER/SQFT * 1000) %>%
  mutate(CDD65_COOLP = CDD65 * COOLP / 100) %>%
  mutate(HDD65_HEATP = HDD65 * HEATP / 100) %>%
  mutate(IsCooking = ifelse(COOK == 1, "Yes", "No")) %>%
  mutate(IsOpenWeekends = ifelse(OPNWE == 1, "Yes", "No")) %>%
  mutate(IsHighSchool = ifelse(PBAPLUS == 29, "Yes", "No"))
  
ivars = c("NWKER_SQFT", "CDD65_COOLP", "HDD65_HEATP",
          "CDD65_COOLP", "IsCooking",
          "IsOpenWeekends", "IsHighSchool")


#######################################################################

retail = read.csv("./filtered/retail.csv")
vars = c("SQFT", "WKHRS", "NWKER", "PCTERMN", "SERVERN", "LAPTPN",
         "CDD65", "COOLP", 
         "HDD65", "HEATP",
         "COOK", "OPNWE", "RFGWIN",
         "PBAPLUS", "SOURCE_ENERGY", "SOURCE_EUI", "FINALWT")
data = retail[, vars]

data1 = data %>%
  mutate(NWKER_SQFT = NWKER/SQFT * 1000) %>%
  mutate(RFGWIN_SQFT = RFGWIN/SQFT * 1000) %>%
  mutate(CDD65_COOLP = log(CDD65) * COOLP / 100) %>%
  mutate(HDD65_HEATP = log(HDD65) * HEATP / 100) %>%
  mutate(IsSuperMarket = ifelse(PBAPLUS == 14, "Yes", "No"))

#TODO: Adjustment for the Number of Workers per 1,000 Square Feet for a Supermarket

ivars = c("WKHRS", "NWKER_SQFT", "RFGWIN_SQFT", 
          "HDD65_HEATP", "CDD65_COOLP", 
          "IsSuperMarket")

#######################################################################
data2 = scale_data(data1[, c(ivars)])

dvar = "SOURCE_EUI"
train1 = cbind(data1[, c(dvar)], data2)
names(train1)[1] = dvar
model1 = paste(dvar, "~", paste(ivars, collapse = " + "))
fit1 = lm(model1, data = train1, weights = data1$FINALWT)

dvar = "SOURCE_ENERGY"
train2 = cbind(data1[, c(dvar)], data2)
names(train2)[1] = dvar
model2 = paste(dvar, "~", paste(ivars, collapse = " + "))
fit2 = lm(model2, data = train2, weights = data1$FINALWT)

## with interactions
int_depth = length(ivars)
for(int in 2:int_depth) {
  #print(paste(int))
  dvar = "SOURCE_EUI"
  train1 = cbind(data1[, c(dvar)], data2)
  names(train1)[1] = dvar
  allvars = paste(ivars, collapse = " + ")
  model1 = paste(dvar, "~ (", allvars, ")^", int )
  fit1 = lm(model1, data = train1, weights = data1$FINALWT)
  print(paste(int, summary(fit1)$r.squared))
  
  dvar = "SOURCE_ENERGY"
  train2 = cbind(data1[, c(dvar)], data2)
  names(train2)[1] = dvar
  allvars = paste(ivars, collapse = " + ")
  model2 = paste(dvar, "~ (", allvars, ")^", int )
  fit2 = lm(model2, data = train2, weights = data1$FINALWT)
  print(paste(int, summary(fit2)$r.squared))
}

### exp E2B - using decision tree based models
library(rpart)
dvar = "SOURCE_EUI"
train1 = cbind(data1[, c(dvar)], data2)
names(train1)[1] = dvar
model1 = paste(dvar, "~", paste(ivars, collapse = " + "))
rf1 = rpart(model1, data = train1, weights = data1$FINALWT)
#fit1 = lm(model1, data = train1, weights = data1$FINALWT)

dvar = "SOURCE_ENERGY"
train2 = cbind(data1[, c(dvar)], data2)
names(train2)[1] = dvar
model2 = paste(dvar, "~", paste(ivars, collapse = " + "))
rf2 = rpart(model1, data = train1, weights = data1$FINALWT)


### exp E2B - using xgboost
library(xgboost)
library(mlbench)
library(caret)

dvar = "SOURCE_EUI"
train1 = cbind(data1[, c(dvar)], data2)
names(train1)[1] = dvar
model1 = paste(dvar, "~", paste(ivars, collapse = " + "))

dummy = dummyVars(model1, data = train1)
train1a = as.data.frame(predict(dummy, train1))

xgbFit1 = xgboost(data = as.matrix(train1a), 
                 label = as.matrix(train1$SOURCE_EUI), 
                 booster = "dart",
                 objective = "reg:linear", 
                 eval_metric = "rmse", 
                 nfold = 10, 
                 nrounds = 10, verbose = T)


dvar = "SOURCE_ENERGY"
train2 = cbind(data1[, c(dvar)], data2)
names(train2)[1] = dvar
model2 = paste(dvar, "~", paste(ivars, collapse = " + "))

dummy = dummyVars(model2, data = train2)
train2a = as.data.frame(predict(dummy, train2))

xgbFit2 = xgboost(data = as.matrix(train2a), 
                  label = as.matrix(train2$SOURCE_ENERGY), 
                  booster = "dart",
                  objective = "reg:linear", 
                  eval_metric = "rmse", 
                  nfold = 10, 
                  nrounds = 10, verbose = T)