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
library(reshape2)

merged = read_csv("data/merged.csv")

no_buildings = merged[, c("Number Of Buildings Aggregated", "NumberofBuildings")]
no_buildings["diff"] = no_buildings$`Number Of Buildings Aggregated` - no_buildings$NumberofBuildings
hist(no_buildings$diff)
table(no_buildings$diff)
no_buildings1 = no_buildings %>% filter(diff >= -1 & diff <= 1)

floors = merged[, c("NumberofFloors", "Stories")]
floors["diff"] = floors$NumberofFloors - floors$Stories
table(floors$diff)
floors1 = floors %>% filter(diff >= -1 & diff <= 1)

yr = merged[, c("Eff. Year", "Year Built", "YearBuilt")]
yr["diff"] = yr$`Year Built` - yr$YearBuilt
hist(yr$diff)
table(yr$diff)
yr1 = yr %>% filter(diff >= -2 & diff <= 2)
hist(yr1$diff)

gfa = merged[, c("Building Gross Sq Ft", "Building Net Sq Ft", "PropertyGFATotal")]
gfa["diff_gr"] = gfa$`Building Gross Sq Ft` - gfa$PropertyGFATotal
hist(gfa$diff_gr, nclass = 20, plot=F)
gfa1 = gfa %>% filter(diff_gr >= -5000 & diff_gr <= 5000)
hist(gfa1$diff_gr)

diff_nob = merged$`Number Of Buildings Aggregated` - merged$NumberofBuildings
diff_floors = merged$NumberofFloors - merged$Stories
diff_yr = merged$`Year Built` - merged$YearBuilt
diff_gfa = merged$`Building Gross Sq Ft` - merged$PropertyGFATotal

f0 = merged
f1 = f0 %>% filter( (`Number Of Buildings Aggregated` - NumberofBuildings) %in% (-1:1) )
f2 = f1 %>% filter( (NumberofFloors - Stories) %in% (-1:1) )
f3 = f2 %>% filter( (`Year Built` - YearBuilt) %in% (-2:2) )
f4 = f3 %>% filter( (`Building Gross Sq Ft` - PropertyGFATotal) %in% (-5000:5000) )

filtered = f4 %>% 
  distinct() %>%
  filter( SiteEnergyUse > 0) %>% 
  filter( SourceEUI > 0)

#hist(filtered$PropertyGFATotal)

model_cols = c(
  "parcelId",	
"Building Description",
"ENERGYSTARScore",
  "SiteEnergyUse", 
               "SourceEUI",
               "PropertyGFATotal",
               "Building Quality", "Construction Class",
               "Eff. Year",
               "YearBuilt",
               "Elevators", "Heating System", "Number Of Buildings Aggregated",
               "Predominant Use", "Shape", "Sprinklers", 
               #"Stories",
               "NumberofFloors"
               )
train0 = filtered[, model_cols]
names(train0) = gsub(". ", "", names(train0))
summary(train0)

train1 = train0 %>% 
  mutate(EffYear = (2018 - EffYear)) %>%
  mutate(YearBuilt = (2018 - YearBuilt))

train2 = train1 %>% 
  filter( NumberofFloors > 0 & NumberofFloors <= 30 ) %>% 
  filter( SourceEUI > 0)

#regexp <- "[[:digit:]]+"
#nos = str_extract(train$PredominanUse, regexp) 
#train["PredominantUseCode"] = nos
train3 = train2 %>% 
  mutate(PredominantUseCode = str_extract(PredominanUse, "[[:digit:]]+")) #get only the digits

pu = sort(table(train3$PredominantUseCode))
pu = pu[ pu >= 10]
train4 = train3[train3$PredominantUseCode %in% names(pu), ]
table(train4$PredominantUseCode)

heat = sort(table(train4$HeatinSystem))
heat = heat[ heat > 10]
train5 = train4 %>% filter(HeatinSystem %in% names(heat))
table(train5$HeatinSystem)

train5["HeatinSystemCode"] = as.factor(train5$HeatinSystem)

# train4 = train3 %>% 
#   group_by(PredominantUseCode) %>% 
#   mutate(freq = n()) %>% 
#   ungroup() %>% 
#   filter(freq >= 10) %>%
#   select(-freq)
# table(train4$PredominantUseCode)

library(GGally)
library(corrplot)

train = train5
ggscatmat(train) + theme_bw()
train["SiteEnergyUseLog"] = log(train$SiteEnergyUse)
ggscatmat(train) + theme_bw()

write_csv(train, "data/train.csv", na = "")

------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
  

###############################################################
###########################################
############ rpart based grouping #########

train = as.data.frame(read_csv("data/train.csv", na = ""))
#ggscatmat(train) + theme_bw()

train = train %>% 
  mutate(SiteEnergyUseLog = log(SiteEnergyUse)) %>%
  mutate(SourceEUILog = log(SourceEUI)) %>%
  mutate(PredominantUseCode = as.character(PredominantUseCode)) %>%
  mutate_if(is.character, as.factor)

vars = setdiff(names(train), c("SiteEnergyUse",
                               "SiteEnergyUseLog",
                               "SourceEUI", "SourceEUILog",
                               "HeatinSystemCode",
                               "PredominanUse", 
                               #"PredominantUseCode",
                               "HeatinSystemCode"
                               ))
model = as.formula( paste("SiteEnergyUseLog ~ ", paste(vars, collapse="+")))

### build model
rp = rpart(model, 
           method="anova", 
           data = train,
           control = rpart.control(
             #cp = 0.0065,
             minsplit = 10))
rp$cptable
plotcp(rp)

vi <- varImp(rp) %>% 
  mutate(Attribute=row.names(.)) %>%
  mutate(Overall=round(Overall,2)) %>%
  mutate(Percentage = round(Overall*100/sum(Overall), 2)) %>%
  arrange(-Overall)

# viSum = sum(vi$Overall)
# vi["Percentage"] = round(vi$Overall / viSum * 100,1)

library(rpart.plot)
prp(rp, faclen = 0, cex = 0.8, 
    fallen.leaves = TRUE,
    #extra = 6,
    type = 4,
    box.palette = "auto")


plot(rp, uniform = TRUE, compress = TRUE, branch = .2)
text(rp, use.n = TRUE, cex = .8, xpd = NA) # cex is a guess, depends on your window size

rpart::post(rp, file="", horizontal=F, pretty=0)
summary(exp(residuals(rp)))


library(party)
fit.ctree.party <- ctree(model, data=train,
                         controls=ctree_control(mincriterion=5.95,savesplitstats=FALSE))

plot(fit.ctree.party)

fitControl <- trainControl(method = 'cv', number=6)
Grid <- expand.grid(cp=seq(0, 0.05, 0.005))
fit.rpartCV <- train(model, data=train, 
                     method = 'rpart', 
                     trControl=fitControl, 
                     metric='RMSE',
                     maximize=FALSE, 
                     tuneGrid = Grid)


mi = getModelInfo(model="rpart")

rpart.plot(type=2, rp, fallen.leaves=T, tweak = 2.5, clip.right.labs=T, branch=.9)

gl = glm(SiteEnergyUse ~ ., data = train1)
#devtools::install_github("pat-s/sperrorest")
#https://stats.stackexchange.com/questions/215290/performance-of-regression-tree-rpart
rpart.plot(type=0, rp, fallen.leaves=F, tweak = 2.0, clip.right.labs=FALSE, branch=.9)

plot(rp, uniform=TRUE, 
     main="Regression Tree for Seatle")
text(rp, use.n=TRUE, all=TRUE, cex=.8, fancy = T)

par(xpd = NA) # otherwise on some devices the text is clipped
plot(rp)
text(rp, digits = 3)
print(rp, digits = 2)


#caret
library(caret)
set.seed(123)
model2 <- train(
  model, data = train, method = "rpart",
  parms=list(method='anova'),
  #tuneGrid = expand.grid(method = "anova"),
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

################################################################################

grouped = cbind(train, group = rp$where)

boxplot(SiteEnergyUseLog ~ group, data = grouped)

grouped$group = as.factor(grouped$group)

grouped

ggboxplot(grouped, 
          x = "PredominantUseCode", 
          y = "SiteEnergyUseLog",
          color = "PredominantUseCode")


ggboxplot(grouped, 
          x = "group", 
          y = "SiteEnergyUseLog",
          color = "group",
          shape = "group",
          add = "jitter" 
          ) + theme_bw()





               ,
               palette =c("#00AFBB", "#E7B800", "#FC4E07"),
               add = "jitter"
               #shape = "dose"
               )



gr_list = list()
gr = table(rp$where)
for(n in names(gr)) {
  gr_list[[n]] = train[ rp$where == as.numeric(n), ]
  print (paste(n, nrow(gr_list[[n]])))
}

kbtu = lapply(gr_list, function(x) {
  x$SiteEnergyUseLog
})

#### plot the energy distribution of each group ####################
boxplot(kbtu, xlab = "groups", ylab= "SiteEnergyUse(kBTu) - log scale")

kb = as.data.frame( unlist(kbtu))
kb = unlist(kbtu)

### plot the energy distribution of each psu type - after grouping  ##########
ggplot(grouped,  aes(x = SiteEnergyUseLog, y = as.factor(group))) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 2, size = 0.3 ) +
  scale_fill_gradientn( colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"), 
                        name = "SiteEnergyUse(kBTU) - log scale") + 
  theme_pubr(base_size=10, legend="bottom")


#### plot the energy distribution of each psu type - before grouping  ##########
ggplot(train,  aes(x = SiteEnergyUseLog, y = PredominanUse)) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 2, size = 0.3 ) +
  scale_fill_gradientn( colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"), 
                        name = "SiteEnergyUse(kBTU) - log scale") + 
  theme_pubr(base_size=10, legend="bottom")

boxplot(data = train1, SiteEnergyUse ~ `Predominant Use`,
        xlab = "groups", ylab= "SiteEnergyUse(kBTu) - log scale")
##################################################

########### regression model for each group calculate 

#dat1 = gr_list[["4"]]

normalize_data <- function(df) {
  for(col in names(df)) {
    if( is.numeric(df[, col]) ){
      df[, col] = log(df[, col])  
    }
  }
  return (df)
}

scale_data <- function(df, excludeList) {
  for(col in setdiff(names(df), excludeList)) {
    if( is.numeric(df[, col]) ){
      # https://stackoverflow.com/questions/10287545/backtransform-scale-for-plotting
      # https://rdrr.io/cran/DMwR/man/unscale.html
      df[, col] = scale(df[, col], center = T, scale = T)
    }
  }
  return (df)
}


dat1 = grouped %>% filter(group == 4)
ggscatmat(dat1) + theme_bw()

dat2 = dat1
dat2 = scale_data(dat2, c("SiteEnergyUseLog", "group"))
ggscatmat(dat2) + theme_bw()

dat = as.data.frame(dat2)
#ggscatmat(dat) + theme_bw()
vars = setdiff(names(dat), c("SiteEnergyUse",
                              "SiteEnergyUseLog",
                              "SourceEUI",
                              "PredominanUse",
                              "PredominantUseCode",
                              "HeatinSystemCode",
                             "PredominanUse",
                             "group",
                             "Elevators",
                             "Sprinklers"
                               ))
model = as.formula( paste("SiteEnergyUseLog ~ ", paste(vars, collapse="+")))

dat = dat[, c("SiteEnergyUseLog", vars)]

fit.model <- lm(model, data = dat)
print(fit.model)
summary(fit.model)

varImp(fit.model, scale = F)

library(caret)
library(leaps)

set.seed(123)

sapply(dat, function(x) {
  table(is.na(x))
})


# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)

# Train the model
#the tuning parameter nvmax (parm leapSeq function), which corresponds to the maximum number of predictors to be incorporated in the model.
nvmaxParm = 1 + str_count(as.character(model), "\\+")[3]
#nvmaxParm = 10
step.model <- train(model, data = dat,
                    method = "leapBackward",
                    #method = "leapSeq",
                    #method = "lmStepAIC", 
                    tuneGrid = data.frame(nvmax = 1:nvmaxParm),
                    trControl = train.control)
step.model$results
step.model$bestTune
s = summary(step.model$finalModel)

bestVar = step.model$bestTune$nvmax
res = step.model$results
res <- melt( res[, c("nvmax", "Rsquared", "RMSE", "MAE")], id="nvmax")

ggline(res, "nvmax", "value",
       color = "variable", linetype = "variable", shape = "variable",
       #palette = c("gray", "blue", "red"),
       size = 1,
       xticks.by = 1,
       #ylim = c(0.3, 0.8),
       title = "Step-wise linear regression accuray (with 10-fold cross validation)",
       xlab = "Number of variables",
       ylab = "R-squared / RMSE / MAE",
       legend.title = "",
       legend = "top") + 
  geom_vline(xintercept=bestVar, linetype="dotted") + 
  theme_bw()



## norm distribution test

minx = c()
maxx = c()
sdx = c()
dat = list()
for(i in 1:1000) {
  Y_noise <- rnorm(n = 100, mean = 0, sd = 10)
  dat[[i]] = Y_noise
  minx[i] = min(Y_noise)
  maxx[i] = max(Y_noise)
  sdx[i]  = var(Y_noise)
}
boxplot(dat)
plot(sdx)


X_data <- seq(1, 100, 1)
Y_raw <- 5 + 2 * X_data
Y_noise <- rnorm(n = 100, mean = 0, sd = 1)
summary(Y_noise)

Y <- data.frame(X = X_data, Y = Y_raw + Y_noise)
Model  <- lm(Y ~ X, data = Y)
plot(Model, ask = F)
summary(Model)
anova(Model)
summary(Model$residuals)
