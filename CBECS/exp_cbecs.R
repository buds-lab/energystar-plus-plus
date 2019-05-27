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
    
    # dummy = dummyVars(model, data = x, fullRank = T)
    # x0 = as.data.frame(predict(dummy, data))
    # model = paste(dvar, "~", paste(names(x0), collapse = " + "))  
    # fit = lm(model, data = cbind(x0, SOURCE_EUI=x$SOURCE_EUI),
    #          weights = data$FINALWT)
    # 
    # step <- stepAIC(fit, direction="both")
    # 
    # models <- regsubsets(as.formula(model), data = x, weights=data$FINALWT,
    #                      nvmax = 10,
    #                      method = "seqrep")
    # summary(models)
    
    fit = lm(model, data = x, weights = data$FINALWT)
    step <- stepAIC(fit,trace = 0)
    
    #r2  = summary(fit)$r.squared
    #r2a = summary(fit)$adj.r.squared
    #print(paste(dvar, r2, r2a))
    # res = x$SOURCE_EUI - pred
    # r2 = 1 - var(res) / var(x$SOURCE_EUI)
    # #as per ?summary.lm
    # t1 = sum(res^2)
    # y = pred
    # t2 = sum((y - mean(y))^2)
    # r2a = 1 - t1/t2
    
    # 1 - (1 - r2) * ((n - df.int)/rdf)

    y = data[,dvar]
    pred = as.numeric(predict(fit))
    s1 = getModelStats(fit, y, pred, data$FINALWT)
    df1 = data.frame("exp" = "E1", 
                     "dependent" = dvar, 
                     "model" = "Reg",
                     "interaction" = int,
                     "transform" = "meanCent",
                     s1)
    res = rbind(res, df1)
    
    ## with step-wise regression
    y = data[,dvar]
    pred = as.numeric(predict(step))
    s1 = getModelStats(step, y, pred, data$FINALWT)
    df1 = data.frame("exp" = "E1", 
                     "dependent" = dvar, 
                     "model" = "Reg_AIC",
                     "interaction" = int,
                     "transform" = "meanCent",
                     s1)
    res = rbind(res, df1)
    
    
    ## using log transform
    # x = mean_center_data(data, ivars)
    # x[,dvar] = log(x[,dvar])
    # fit = lm(model, data = x, weights = data$FINALWT)
    # pred = as.numeric(predict(fit))
    # 
    # r2  = summary(fit)$r.squared
    # r2a = summary(fit)$adj.r.squared
    # print(paste(dvar, r2, r2a))
    # 
    # y = data[,dvar]
    # s1 = getModelStats(fit, x[,dvar], pred, data$FINALWT, islog = T)
    # df1 = data.frame("exp" = "E1", 
    #                  "dependent" = dvar, 
    #                  "model" = "Reg",
    #                  "interaction" = int,
    #                  "transform" = "meanCent_logDV",
    #                  s1)
    # res = rbind(res, df1)
    
    
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
    
    tryCatch({
      res = exp_E1(data, int)
      res$exp   = "E2"
      res$model = paste0(res$model, "_Int")
      resall = rbind(resall, res)
      
    }, error = function(e){cat(paste(e))})
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

tuneXgboost_defaultParams <- function(x, y, height=2, 
                                      weight = rep(1,length(y))) {
  
  grid_default = expand.grid(
    nrounds = 100,
    max_depth = height,
    eta = 0.3,
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
  
  train.control <- trainControl(
    method = "repeatedcv",
    number = 10, ## 10-fold CV
    repeats = 2## repeated three times
  )
  
  gridxgFit1  <- train(x, y, 
                     weights = weight,
                     method = "xgbTree", 
                     tuneGrid = grid_default,
                     trControl = train.control,
                     verbose = TRUE)
  return(gridxgFit1$finalModel)
}

tuneXgboost_randomSearch <- function(x, y, weight = rep(1,length(y))) {

  train.control <- trainControl(
    method = "repeatedcv",
    number = 10, ## 10-fold CV
    repeats = 2, ## repeated three times
    search = "random")

  gridxgFit1  <- train(x, y, 
                       weights = weight,
                       method = "xgbTree", 
                       trControl = train.control,
                       verbose = TRUE)
  #saveRDS(gridxgFit1, "cbecs_office_xgboost.RDS")
  return(gridxgFit1$finalModel)
}

tuneXgboost_gridSearch <- function(x, y, height=2, 
                                   weight = rep(1,length(y))) {
  
  train.control <- trainControl(
    method = "repeatedcv",
    number = 10, ## 10-fold CV
    repeats = 2 ## repeated three times
  )
  
  #tune.gridcart <- expand.grid(maxdepth = 2:10)
  tune.gridxgb <- expand.grid(
    eta = seq(0.1, 0.9, 0.1),
    nrounds = seq(10, 100, 10),
    max_depth = height,  # 4
    #min_child_weight = seq(1,5,1),
    #colsample_bytree = seq(0.1, 1, 0.1),
    gamma = seq(0, 1, 0.1),
    subsample = seq(0.2, 1, 0.2))
  
  # from https://github.com/topepo/caret/blob/master/models/files/xgbTree.R
  len = 10
  tune.gridxgb <- expand.grid(max_depth = height,
                     nrounds = floor((1:len) * 10),
                     eta = c(.3, .4),
                     gamma = 0,
                     colsample_bytree = c(.6, .8),
                     min_child_weight = c(1),
                     subsample = seq(.25, 1, length = len))
  
  dim(tune.gridxgb)
  
  gridxgFit1  <- train(x, y, 
                       weights = weight,
                       method = "xgbTree", 
                       tuneGrid =tune.gridxgb,
                       trControl = train.control,
                       verbose = TRUE)
  return(gridxgFit1$finalModel)
}


tuneXgboost_adaptiveSearch <- function(x, y, weight = rep(1,length(y))) {
  train.control <- trainControl(
    method = "adaptive_cv",
    search = "random",
    adaptive = list(min = 5, alpha = 0.05, 
                    method = "gls", complete = TRUE),
    number = 10, ## 10-fold CV
    repeats = 2 ## repeated three times
  )
  
  gridxgFit1  <- train(x, y, 
                       weights = weight,
                       method = "xgbTree", 
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
    dummy = dummyVars(model, data = data, fullRank = T)
    x0 = as.data.frame(predict(dummy, data))

    ## default - 2  
    xgfit = tuneXgboost_defaultParams(x0, y, height=2, wt)
    pred = as.numeric(predict(xgfit, as.matrix(x0)))
    s1 = getModelStatsXgboost(xgfit, x0, y, pred, wt)
    df1 = data.frame("exp" = "E3b", 
                     "dependent" = dvar, 
                     "model" = "xgboost_default",
                     "interaction" = 2, 
                     "transform" = "meanCent",
                     s1)
    res = rbind(res, df1)
    
    
    ## default - 3
    xgfit = tuneXgboost_defaultParams(x0, y, height=3, wt)
    pred = as.numeric(predict(xgfit, as.matrix(x0)))
    s1 = getModelStatsXgboost(xgfit, x0, y, pred, wt)
    df1 = data.frame("exp" = "E3b", 
                     "dependent" = dvar, 
                     "model" = "xgboost_default",
                     "interaction" = 3, 
                     "transform" = "meanCent",
                     s1)
    res = rbind(res, df1)
    
    
    ## random
    xgfit = tuneXgboost_randomSearch(x0, y, wt)
    pred = as.numeric(predict(xgfit, as.matrix(x0)))
    s1 = getModelStatsXgboost(xgfit, x0, y, pred, wt)
    df1 = data.frame("exp" = "E3b", 
                     "dependent" = dvar, 
                     "model" = "xgboost_random",
                     "interaction" = 1, 
                     "transform" = "meanCent",
                     s1)
    res = rbind(res, df1)
    

    ## grid - 2
    xgfit = tuneXgboost_gridSearch(x0, y, height=2, wt)
    pred = as.numeric(predict(xgfit, as.matrix(x0)))
    s1 = getModelStatsXgboost(xgfit, x0, y, pred, wt)
    df1 = data.frame("exp" = "E3b", 
                     "dependent" = dvar, 
                     "model" = "xgboost_grid",
                     "interaction" = 2, 
                     "transform" = "meanCent",
                     s1)
    res = rbind(res, df1)
    
    ## grid - 3
    xgfit = tuneXgboost_gridSearch(x0, y, height=3, wt)
    pred = as.numeric(predict(xgfit, as.matrix(x0)))
    s1 = getModelStatsXgboost(xgfit, x0, y, pred, wt)
    df1 = data.frame("exp" = "E3b", 
                     "dependent" = dvar, 
                     "model" = "xgboost_grid",
                     "interaction" = 3, 
                     "transform" = "meanCent",
                     s1)
    res = rbind(res, df1)
    
    ## adaptive
    xgfit = tuneXgboost_adaptiveSearch(x0, y, wt)
    pred = as.numeric(predict(xgfit, as.matrix(x0)))
    s1 = getModelStatsXgboost(xgfit, x0, y, pred, wt)
    df1 = data.frame("exp" = "E3b", 
                     "dependent" = dvar, 
                     "model" = "xgboost_adaptive",
                     "interaction" = 1, 
                     "transform" = "meanCent",
                     s1)
    res = rbind(res, df1)    
    
    
    
    # ####### using log
    # print(paste(Sys.time(), "xgboost", dvar, "log"))
    # x = data[, ivars]
    # y = log(data[,dvar])
    # 
    # model = paste(dvar, "~", paste(ivars, collapse = " + "))
    # dummy = dummyVars(model, data = data)
    # x0 = as.data.frame(predict(dummy, data))
    # 
    # #xgfit = tuneXgboost(x0, y, wt)
    # xgfit = tuneXgboost_defaultParams(x0, y, wt)
    # pred = as.numeric(predict(xgfit, as.matrix(x0)))
    # s1 = getModelStatsXgboost(xgfit, x0, exp(y), pred, wt)
    # df1 = data.frame("exp" = "E3b", 
    #                  "dependent" = dvar, 
    #                  "model" = "xgboost",
    #                  "interaction" = 1, 
    #                  "transform" = "meanCent_logDV",
    #                  s1)
    # res = rbind(res, df1)
  }
  
  
  
  return(res)
}

# https://www.r-bloggers.com/variable-selection-with-elastic-net/  
pkgs <- list("glmnet", "doParallel", "foreach", "pROC")
lapply(pkgs, require, character.only = T)
#registerDoParallel(cores = 4)
#stopImplicitCluster()

ncore = 4

load_dir = './data/features/'
save_dir = './data/results_new/'

#load_dir = './data/features_all/'
#save_dir = './data/results_all/'


btypes = c("office", "retail", 
           "k12school", 
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
  
  #registerDoParallel(cores = ncore)
  #e3a  = exp_E3a(data)
  #stopImplicitCluster()
  
  registerDoParallel(cores = ncore)
  e3b  = exp_E3b(data)
  stopImplicitCluster()
  
  resall = rbind(e1, e2, e3b)
  write.csv(resall, paste0(save_dir, btype, ".csv"))
}


## calculate normalized rmse = rmse / Interquartile range
  
for (btype in btypes) {
  data = read.csv(paste0(load_dir, btype, ".csv"))
  print(paste(Sys.time(), btype, paste(dim(data), collapse = " x ")))

  res = read.csv(paste0(save_dir, btype, ".csv"))
  nrmse_iqr   = c()
  nrmse_range = c()
  nrmse_mean  = c()
  
  for(r in 1:nrow(res)) {
    row = res[r, ]
    if(row$dependent == 'SOURCE_ENERGY' && row$transform == 'meanCent') {
      
      iqr  = IQR(data$SOURCE_ENERGY)  
      rang = max(data$SOURCE_ENERGY) - min(data$SOURCE_ENERGY)
      mean = mean(data$SOURCE_ENERGY)
      
      nrmse_iqr[r]   = round(row$rmse / iqr * 100,  3)
      nrmse_range[r] = round(row$rmse / rang * 100, 3)
      nrmse_mean[r]  = round(row$rmse / mean * 100, 3)
    } else if(row$dependent == 'SOURCE_EUI' && row$transform == 'meanCent') {
      
      iqr  = IQR(data$SOURCE_EUI)  
      rang = max(data$SOURCE_EUI) - min(data$SOURCE_EUI)
      mean = mean(data$SOURCE_EUI)
    
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
  write.csv(res1, paste0(save_dir, btype, ".csv"), row.names = F)
}
  
  


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