#### List of experiments with and without using interactions in multiple regression models
########
# 1. Using SiteEnergyUse with all data
#   A. Using log(SiteEnergyUse) and scaling all variables
#   B. Using log(SiteEnergyUse) and but NOT scaling all variables
#   C. Using SiteEnergyUse and NOT scaling all variables

# 2. Using SourceEUI with all data
#   A. Using log(SourceEUI) and scaling all variables
#   B. Using log(SourceEUI) and but NOT scaling all variables
#   C. Using SourceEUI and NOT scaling all variables

library(readr)
library(dplyr)

library(jtools)
library(caret)
library(MASS)

library(DMwR)  ## for unscale

options(scipen=10) # avoid scientific notation in plots

scale_data <- function(df, exclude) {
  for(col in setdiff(names(df), exclude)) {
    if( is.numeric(df[, col]) ){
      # https://stackoverflow.com/questions/10287545/backtransform-scale-for-plotting
      # https://rdrr.io/cran/DMwR/man/unscale.html
      df[, col] = scale( as.vector(df[, col]), center = T, scale = T)
    }
  }
  return (df)
}

unscale_data <- function(df) {
  for(col in names(df)) {
    if( is.numeric(df[, col]) ){
      # https://stackoverflow.com/questions/10287545/backtransform-scale-for-plotting
      # https://rdrr.io/cran/DMwR/man/unscale.html
      #df[, col] = scale(df[, col], center = T, scale = T)
      df[, col] = unscale(df[, col], df[, col])
    }
  }
  return (df)
}

getTrainingData <- function(){
  train = as.data.frame(read_csv("data/train.csv", na = ""))
  train0 = train %>% 
    mutate(PredominantUseCode = as.character(PredominantUseCode)) %>%
    mutate(SiteEnergyUseLog = log(SiteEnergyUse)) %>%
    mutate(SourceEUILog = log(SourceEUI)) %>%
    replace(., is.na(.), "Unknown") %>%
    mutate_if(is.character, as.factor)
  return(train0)
}

printDummies <- function(df, vars) {
  tot = 0
  for(c in vars) {
    d = df[, c]
    
    if(is.factor(d)) {
      cnt = length(unique(d)) - 1
    } else {
      cnt = 1
    }
    tot = tot + cnt
    print(paste(c, cnt, tot))
  }
  return(tot)
}


getStepWiseRegressionModelAllData <- function(expName, Y, X, train0){
  
  print(expName)
  model = as.formula( paste(Y, " ~ ", paste(X, collapse="+")))
  print(model)
  printDummies(train0, X)
  lmFit <- lm(model, data = train0)
  saveRDS(lmFit, paste(expName, "lmFit.RDS", sep = "_"))
  
  lmFitSW <- stepAIC(lmFit, direction = "both", trace = FALSE)
  saveRDS(lmFitSW, paste(expName, "lmFitSW.RDS", sep = "_"))
  
  ## get the most important attributes after step-wise regression
  lmFitSWdata = droplevels(lmFitSW$model)
  new_X = setdiff(names(lmFitSWdata), Y)
  
  ## model with interactions
  print("model with interactions")
  intModel = as.formula( paste(Y, " ~ ", paste(new_X, collapse="*")))
  print(intModel)
  printDummies(train0, new_X)
  
  print(paste(Sys.time(), " fitting interaction model - started"))
  IntlmFit <- lm(intModel, data = train0)
  saveRDS(IntlmFit, paste(expName, "IntlmFit.RDS", sep = "_"))
  print(paste(Sys.time(), " fitting interaction model - ended"))
  
  IntlmFitSW <- stepAIC(IntlmFit, direction = "both", trace = FALSE)
  saveRDS(IntlmFitSW, paste(expName, "IntlmFitSW.RDS", sep = "_"))
}

## print number of variables, including dummy variables for factors

merged = as.data.frame(read_csv("data/merged.csv", na = ""))

merged1 = subset(merged, !is.na(ENERGYSTARScore))
es = as.numeric(merged1$ENERGYSTARScore)
hist(es)

eui = merged1$SourceEUI
hist(eui)



train = getTrainingData()
vars = setdiff(names(train), c(
  "parcelId", "BuildinDescription", "ENERGYSTARScore" ,
  "SiteEnergyUse",
                                  "SiteEnergyUseLog",
                                  "SourceEUI", 
                                  "SourceEUILog",
                                  "PredominanUse", 
                                  "PredominantUseCode",
                                  "HeatinSystemCode"
                               ))


### Experiment 1A: Log(SiteEnergyUse) and scale all variable
df1 = droplevels(train)   ## drop unused factor levels
df2 = scale_data(df1, c())
train0 = as.data.frame(df2)

expName = "SiteEnergyUseLog_Scale"
Y = "SiteEnergyUseLog"
X = vars
getStepWiseRegressionModelAllData(expName, Y, X, train0)



############# prediction and ranking

m1a = readRDS("SiteEnergyUseLog_Scale_IntlmFit.RDS")
m1b = readRDS("SiteEnergyUseLog_NoScale_IntlmFit.RDS")
m1c = readRDS("SiteEnergyUse_NoScale_IntlmFit.RDS")

#lmFit = lmFitSW
lmFit = m1a
summary(lmFit)

pre0 = predict(lmFit, train0)
pre = unscale(pre0, train0$SiteEnergyUseLog)

train1 = unscale_data(train0)
train1["EER"] = train1$SiteEnergyUseLog / pre

eer = train1$EER
eer_sorted = sort(eer)
#plot(eer_sorted, xlab = "Building Id", ylab = "Energy Efficiency Ratio")

# Plot cumaltive percentage for energy efficiency ratio
eer_cs = cumsum(eer_sorted)
eer_pr = cumsum(eer_sorted) / sum(eer_sorted)


plot(eer_sorted,eer_pr, main = "Cumalativer percentile of EER",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     #cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0,
     xlab = "Energy Efficiency Ratio", 
     ylab = "Cumalative percentile")
abline(h = 0.8, col = "red", lwd = 2)
abline(v = 1.6, col = "blue", lwd = 2)

library(Ckmeans.1d.dp)
library(googleVis)

k = 4
result <- Ckmeans.1d.dp(eer, k, y=eer)
plot(result)
plot(eer, col=result$cluster)

train1["rank"] = result$cluster

train2 = subset(train1, ENERGYSTARScore != "Unknown")
train2$ENERGYSTARScore = as.integer(as.character(train2$ENERGYSTARScore))

ENERGYSTARScore = train2$ENERGYSTARScore

ENERGYSTARgrade = case_when(ENERGYSTARScore <= 19 ~ "0-19", 
                     ENERGYSTARScore >= 20 & ENERGYSTARScore < 50 ~ "20-49",
                     ENERGYSTARScore >= 50 & ENERGYSTARScore < 90 ~ "50-89",
                     ENERGYSTARScore >= 90 ~ "90-100")
train2["ENERGYSTARgrade"] = ENERGYSTARgrade

es = unique(train2$ENERGYSTARgrade)
cl = as.character(sort(unique(train2$rank)))

gmData = NA
for(c in cl) {
  for(g in es)
  {
    tr = subset(train2, ENERGYSTARgrade == g & rank == c)
    cnt = nrow(tr)
    #print(paste(g,c, cnt))
    row = data.frame(ENERGYSTARgrade = c(g), rank = c(c), weight = cnt, 
                     stringsAsFactors = F)
    if(is.data.frame(gmData) == F) {
      gmData = row
    } else {
      gmData = rbind(gmData, row)
    }
  }
}

plt = gvisSankey(gmData, from = gmData$rank, to = gmData$ENERGYSTARgrade, 
                 weight = gmData$weight)
plot(plt)


## compare the ordering of ranks using Kendall-tau correlation
library(VGAM) # for kendall.tau

train2$es = ifelse(train2$ENERGYSTARgrade == "90-100", 1, NA)
train2$es = ifelse(train2$ENERGYSTARgrade == "50-89",  2, train2$es)
train2$es = ifelse(train2$ENERGYSTARgrade == "20-49",  3, train2$es)
train2$es = ifelse(train2$ENERGYSTARgrade == "0-19",   4, train2$es)

train3 = train2
table(train3$PredominantUseCode)
train3 = subset(train2, PredominantUseCode == 300)

r1 = train3$SourceEUI
r2 = train3$ENERGYSTARScore
r3 = train3$EER

kendall.tau(r1, r2)
kendall.tau(r2, r3)
kendall.tau(r3, r1)

r1 = train2$es
r2 = train2$rank

kendall.tau(r1, r2)

v1 = c(2,2,2,3)
v2 = c(2,2,2,3)
kendall.tau(v1,v2)

library(Kendall)
Kendall(r1,r2)



#################################
#################################

#train1 = sample_n(train0, 100)
set.seed(101)
rand = sample(1:nrow(train0), 100, replace = F)
train1 = train[rand, ]
train1 = droplevels(train1)
printDummies(train1, vars)

print(expName)
model = as.formula( paste(Y, " ~ ", paste(X, collapse="+")))
print(model)

lmFit <- lm(model, data = train1)
saveRDS(lmFit, paste(expName, "lmFit.RDS", sep = "_"))
lmFit$coefficients
View(lmFit$coefficients)

lmFitSW <- stepAIC(lmFit, direction = "both", trace = FALSE)
saveRDS(lmFitSW, paste(expName, "lmFitSW.RDS", sep = "_"))
View(lmFitSW$coefficients)

lmFitSWdata = droplevels(lmFitSW$model)
new_X = setdiff(names(lmFitSWdata), Y)
printDummies(train0, new_X)

print("Most influencing variables")
print(new_X)

## model with interactions
intModel = as.formula( paste(Y, " ~ ", paste(new_X, collapse="*")))
print(intModel)
IntlmFit <- lm(intModel, data = train1)
saveRDS(IntlmFit, paste(expName, "IntlmFit.RDS", sep = "_"))
View(IntlmFit$coefficients)
ss = summary(IntlmFit)

IntlmFitSW <- stepAIC(IntlmFit, direction = "both", trace = FALSE)
saveRDS(IntlmFitSW, paste(expName, "IntlmFitSW.RDS", sep = "_"))
summary(IntlmFitSW)

######################################


### Experiment 1B: Log(SiteEnergyUse) and no scale all variable
df1 = train
df2 = df1 #scale_data(df1, c())
train0 = as.data.frame(df2)
expName = "SiteEnergyUseLog_NoScale"
Y = "SiteEnergyUseLog"
X = vars
getStepWiseRegressionModelAllData(expName, Y, X, train0)


### Experiment 1C: SiteEnergyUse and no scale all variable
df1 = train
df2 = df1 #scale_data(df1, c())
train0 = as.data.frame(df2)
expName = "SiteEnergyUse_NoScale"
Y = "SiteEnergyUse"
X = vars
getStepWiseRegressionModelAllData(expName, Y, X, train0)

##########################################################################
##########################################################################
##########################################################################

### Experiment 2A: Log(SourceEUI) and scale all variable
df1 = train
df2 = scale_data(df1, c())
train0 = as.data.frame(df2)

expName = "SourceEUILog_Scale"
Y = "SourceEUILog"
X = vars
getStepWiseRegressionModelAllData(expName, Y, X, train0)


### Experiment 2B: Log(SourceEUI) and no scale all variable
df1 = train
df2 = df1 #scale_data(df1, c())
train0 = as.data.frame(df2)
expName = "SourceEUILog_NoScale"
Y = "SourceEUILog"
X = vars
getStepWiseRegressionModelAllData(expName, Y, X, train0)


### Experiment 2C: SourceEUI and no scale all variable
df1 = train
df2 = df1 #scale_data(df1, c())
train0 = as.data.frame(df2)
expName = "SourceEUI_NoScale"
Y = "SourceEUI"
X = vars
getStepWiseRegressionModelAllData(expName, Y, X, train0)


###########################################################
############## Step-wise regression using olsrr package
###########################################################
library(olsrr)

print(expName)
model = as.formula( paste(Y, " ~ ", paste(X, collapse="+")))
print(model)
printDummies(train0, X)
lmFit <- lm(model, data = train0)
#saveRDS(lmFit, paste(expName, "lmFit.RDS", sep = "_"))

lmFitSW = ols_step_both_p(lmFit, details = T)
plot(lmFitSW)

library(leaps)
models <- regsubsets(model, data = train0, nvmax = 5,
                     method = "seqrep")
summary(models)


#lmFitSW <- stepAIC(lmFit, direction = "both", trace = FALSE)
#saveRDS(lmFitSW, paste(expName, "lmFitSW.RDS", sep = "_"))

## get the most important attributes after step-wise regression
lmFitSWdata = droplevels(lmFitSW$model)
new_X = setdiff(names(lmFitSWdata), Y)

## model with interactions
print("model with interactions")
intModel = as.formula( paste(Y, " ~ ", paste(new_X, collapse="*")))
print(intModel)
printDummies(train0, new_X)

print(paste(Sys.time(), " fitting interaction model - started"))
IntlmFit <- lm(intModel, data = train0)
saveRDS(IntlmFit, paste(expName, "IntlmFit.RDS", sep = "_"))
print(paste(Sys.time(), " fitting interaction model - ended"))
