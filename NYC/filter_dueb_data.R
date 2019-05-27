library(readxl)
library(readr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(rpart.utils)
library(sfa)
library(frontier)

library(pre)

# nyc_2014_merged_raw_uil.csv was receivead from authors of DUE-B paper
nycdata = read_csv(".\\data\\nyc_2014_merged_raw_uil.csv")
vars = c( "Source.EUI.kBtu.ft2.", "YearAlter1", "LandUse", "BBL",
          "logkBTU", 
          "DOF_Property_Area", 
          "BldgClass", "AssessTot", "AssessLand", 
          #"AreaUnitPrice",
          "BldgArea", "LotArea", "NumBldgs", "NumFloors", "UnitsTotal", "UnitsRes", 
          "ComArea", "ResArea", "YearBuilt", "BldgFront", "BldgDepth" )
data = nycdata[vars]
data['AreaUnitPrice'] = data$AssessTot / data$BldgArea
names(data)[1] = "SourceEUI"
unlist(lapply(data, function(x) sum(is.na(x))))

bGroup = substr(data$BldgClass,1,1)
bGroup = as.data.frame(sort(table(bGroup)))
bGroup["percent"] = bGroup$Freq / nrow(data) * 100
bGroup["cumsum"] = cumsum(bGroup$Freq)
bGroup1 = bGroup[bGroup$percent < 1, ]
bGroup2 = bGroup[bGroup$percent >= 1, ]

data = data %>% 
  filter(!substr(BldgClass,1,1) %in% bGroup1$bGroup)

# filter
data1 = data %>% 
  distinct() %>%
  filter(!is.na(SourceEUI)) %>% 
  filter(!is.na(logkBTU)) %>%
  filter(!is.na(AreaUnitPrice)) %>%
  filter(!is.na(YearBuilt)) %>%
  filter(!is.na(NumFloors)) %>% 
  filter( SourceEUI <= 1000) %>%
  filter( DOF_Property_Area <= 1687440) %>%
  filter( ifelse(YearAlter1 != 0, YearBuilt < YearAlter1, T))
unlist(lapply(data1, function(x) sum(is.na(x))))

plot(data1$DOF_Property_Area, data1$SourceEUI)
plot(2018-data1$YearBuilt, data1$SourceEUI)

dat = data1
#dat = dat[dat$SourceEUI < 500, ]
dat = dat[dat$YearBuilt > 1980, ]
#dat = dat[dat$DOF_Property_Area < 150000, ]

dat$BldgClass = substr(dat$BldgClass,1,1)
plot(table(dat$BldgClass))

plot(2018-dat$YearBuilt, dat$SourceEUI)
plot(dat$DOF_Property_Area, dat$SourceEUI)

# filter by BldgClass: exclude buildings with use type < 1%
bClass = as.data.frame(sort(table(data$BldgClass)))
bClass["percent"] = bClass$Freq / nrow(data) * 100
bClass["cumsum"] = cumsum(bClass$Freq)

#bClass1 = bClass[bClass$Freq <= 1, ]
bClass1 = bClass[bClass$percent < 0.1, ]
sum(bClass1$Freq)




data2 = data1 %>% 
  filter(!BldgClass %in% bClass1$Var1)
write_csv(data2, ".\\data\\ll84_pluto_cleaned_uil.csv", na = "")

train = data2[, -2:-4]
#train$BldgClass = substr(train$BldgClass, 1,1)

unlist(lapply(train, function(x) sum(is.na(x))))
train1 = train#[1:500,]

train1$BldgClass = substr(train1$BldgClass, 1, 1)
train1$logkBTU = exp(train1$logkBTU)

unique(train1$BldgClass)

#train1$DOF_Property_Area = log(train1$DOF_Property_Area)

#################### RuleFit
pre1 <- pre(logkBTU ~ ., 
            maxdepth = 4,
            data = train1[, -1])
#saveRDS(pre1, "pre1.RDS")
pre1 = readRDS("pre1.RDS")

corplot(pre1)
plot(pre1$glmnet.fit)
cvpre1 = cvpre(pre1, k = 10, verbose = T)

pre1i = importance(pre1, standardize = T)
pre1i$baseimps[1:5, ]

plot(pre1, nterms = 6, plot.dim = c(2,3), standardize = TRUE, cex = .9)

#singleplot(pre1, varname = "DOF_Property_Area")
#pairplot(pre1, varnames = c("DOF_Property_Area", "AssessTot"))

pre2 <- bsnullinteract(pre1)
pre3 <- interact(pre1, varnames = c("DOF_Property_Area", "AssessTot", "BldgArea"), nullmods = pre2)


##########################################
#rp = rpart(logkBTU ~ ., method="anova", data = train1)
rp <- rpart(logkBTU ~ ., 
            method="anova", 
            data = train1[, -1], 
            control = rpart.control(minsplit = 10, cp= 0.0037564))
printcp(rp)
#rp <- prune(rp, cp = 0.01)
bestcp <- rp$cptable[which.min(rp$cptable[,"xerror"]),"CP"]


tot_count <- function(x, labs, digits, varlen)
{
#  paste(labs, "\n\nn =", x$frame$n)
  paste(x$frame$n)
}
prp(rp,  faclen = 0, cex = 0.8, extra = 1 , node.fun=tot_count)
prp(rp,  faclen = 0, cex = 0.8, extra = 1)

rpart.plot(rp, fallen.leaves=F, tweak = 1.5, clip.right.labs=FALSE, branch=.3)

plot(rp, uniform=TRUE, 
     main="Regression Tree for NYC-")
text(rp, use.n=TRUE, all=TRUE, cex=.8)


## extract groups
gr_list = list()
gr = table(rp$where)
for(n in names(gr)) {
  print (n)
  gr_list[[n]] = train1[ rp$where == as.numeric(n), ]
}

kbtu = lapply(gr_list, function(x) {
  x$logkBTU
})


boxplot(kbtu)
boxplot(gr_list[["6"]]$logkBTU)
#add EUI to this dataframe

gr1 = gr_list[["7"]]


#####################
train1log = train1
train1log = train1log[, c(-2, -4)]
train1log = log(train1log)

train1log = train1log[ !is.na(train1log$BldgDepth), ]

unlist(lapply(train1log, function(x) sum(is.na(x))))
sfa1 = frontier::sfa(SourceEUI ~ ., data = train1log, ineffDecrease = F)
####################

df1 = gr_list[["10"]]
df2 = df1

cols = setdiff(names(df1),  c("BldgClass", "logkBTU"))
df2[, cols] = log(df2[, cols])

df3 = df2[, names(df2)[-2]]
df3 = df3[ !is.na(df3$BldgFront), ]
df3 = df3[ !is.na(df3$BldgDepth), ]

# remove all Inf vals
df4 <- df3[is.finite(rowSums(df3)), ]

unlist(lapply(df4, function(x) sum(is.infinite(x))))
unlist(lapply(df3, function(x) sum(is.na(x))))

sfa1 = frontier::sfa(SourceEUI ~ ., data = df4, ineffDecrease = F)
#sfa2 = frontier::sfa(sfa1)

seui = exp(df4$SourceEUI)
ee = seui - sfa1$fitted



df2 = df1
for(n in names(df2)[-1]) {
  #print ( typeof(unlist(df2[2])))
  if(n == "logkBTU")
    next
  
  if(typeof(unlist(df2[n])) == "character") 
    next
  df2[n] = log(df2[n])
}

library(dplyr)
m = inner_join(df2, data2, by = names(df2)[-1] )



frontier::sfa()

############## rule fit
#devtools::install_git("https://GravesEE@gitlab.ins.risk.regn.net/minneapolis-r-packages/rulefit.git")
#library(rulefit)

library(pre)
fit = pre(logkBTU ~ ., data = train1, family = "gaussian", verbose = T)


tr = train1[, c(4:8)]

fit = gbm.fit(tr, train1$logkBTU, distribution="bernoulli",
              interaction.depth=3, shrinkage=0.1, verbose = T)
