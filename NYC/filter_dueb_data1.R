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
print(nrow(data))

# remove duplicate rows
data1 = data %>% 
  filter(!duplicated(data))
print(nrow(data1))

# remove BldgClass with < 1% frequency
bGroup = substr(data$BldgClass,1,1)
bGroup = as.data.frame(sort(table(bGroup)))
bGroup["percent"] = bGroup$Freq / nrow(data) * 100
bGroup["cumsum"] = cumsum(bGroup$Freq)
bGroup1 = bGroup[bGroup$percent < 1, ]
bGroup2 = bGroup[bGroup$percent >= 1, ]

data2 = data1 %>% 
  filter(!substr(BldgClass,1,1) %in% bGroup1$bGroup)
print(nrow(data2))

# print the count of missing values in each column
unlist(lapply(data2, function(x) sum(is.na(x))))

# apply other filters
data3 = data2 %>% 
  #distinct() %>%
  filter(!is.na(SourceEUI)) %>% 
  filter(!is.na(logkBTU)) %>%
  filter(!is.na(AreaUnitPrice)) %>%
  filter(!is.na(YearBuilt)) %>%
  filter(!is.na(NumFloors)) %>% 
  filter( SourceEUI <= 1000) %>%
  filter( DOF_Property_Area <= 1687440) %>%
  filter( ifelse(YearAlter1 != 0, YearBuilt < YearAlter1, T))
unlist(lapply(data1, function(x) sum(is.na(x))))
print(nrow(data3))