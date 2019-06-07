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

getMLRmetrics <- function(fit, obs, pred, w, islog = F) {
  
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
  
  mse  = round(mse(obs, pred),3)
  rmse = round(rmse(obs, pred),3)
  mae  = round(mae(obs, pred),3)
  mape = round(mape(obs, pred),3)
  
  iqr  = IQR(obs)  
  rang = max(obs) - min(obs)
  mean = mean(obs)
  std  = sd(obs)
  
  nrmse_iqr   = round(rmse / iqr  * 100, 3)
  nrmse_range = round(rmse / rang * 100, 3)
  nrmse_mean  = round(rmse / mean * 100, 3)
  nrmse_sd    = round(rmse / std  * 100, 3)
  
  df = data.frame( 
    "obs" = length(fit$residuals), 
    "rank" = fit$rank, 
    "coef" = length(fit$coefficients),
    "R^2"  = round(R2,3),
    "Adj.R^2"  = round(adjR2,3),
    "mse" = mse,
    "rmse" = rmse,
    "mae" = mae,
    "mape" = mape,
    "nrmse_iqr" = nrmse_iqr,
    "nrmse_range" = nrmse_range,
    "nrmse_mean" = nrmse_mean,
    "nrmse_sd" = nrmse_sd)
}

getXgboostmetrics <- function(fit, x, obs, pred, w) {
  
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
  
  mse  = round(mse(obs, pred),3)
  rmse = round(rmse(obs, pred),3)
  mae  = round(mae(obs, pred),3)
  mape = round(mape(obs, pred),3)
  
  iqr  = IQR(obs)  
  rang = max(obs) - min(obs)
  mean = mean(obs)
  std  = sd(obs)
  
  nrmse_iqr   = round(rmse / iqr  * 100, 3)
  nrmse_range = round(rmse / rang * 100, 3)
  nrmse_mean  = round(rmse / mean * 100, 3)
  nrmse_sd    = round(rmse / std  * 100, 3)
  
  df = data.frame( 
    "obs" = nrow(x),
    "rank" = nrow(vi), 
    "coef" = nrow(vi),
    "R^2"  = round(R2,3),
    "Adj.R^2"  = round(adjR2,3),
    "mse" = mse,
    "rmse" = rmse,
    "mae" = mae,
    "mape" = mape,
    "nrmse_iqr" = nrmse_iqr,
    "nrmse_range" = nrmse_range,
    "nrmse_mean" = nrmse_mean,
    "nrmse_sd" = nrmse_sd )
}