library(readxl)
library(readr)
library(dplyr)

ll84_cols = c( BBL = "NYC Borough, Block, and Lot (BBL)",
               StreetNumber = "Street Number",                             
               StreetName = "Street Name",
               Boroguth = "Borough",
               ZipCode = "Zip Code",
               SourceEUI = "Source EUI\r\n(kBtu/ft2)",                   
               WeatherNormalizedSourceEUI = "Weather Normalized Source EUI\r\n(kBtu/ft2)",
               ENERGYSTARScore = "ENERGY STAR Score",
               Reported_Property_FloorArea_Building = "Reported Property Floor Area (Building(s)) (ft²)",
               DOF_Property_FloorArea_Buildngs_and_Parking = "DOF Property Floor Area (Buildngs and Parking)\r\n(ft2)",
               Primary_Property_Type_Self_Selected = "Primary Property Type - Self Selected",
               DOF_Number_of_Buildings = "DOF Number of Buildings")

load_ll84 <- function() {
  
  # Note that, 2014 data is in 2015 file
  ll84_file = ".\\data\\2015_nyc_cy2014__ll84_disclosure_data.xlsx"
  # there are TWo sheets
  ll84 = read_xlsx (ll84_file, "WORKING FILE")
  ll84 = ll84[ll84_cols]
  names(ll84) = names(ll84_cols)
  
  # unlist(lapply(ll841b, function(x) sum(is.na(x))))

  # ll84_cols_new = names(ll84)
  # ll84_cols_new = gsub(" ", "", ll84_cols_new)
  # ll84_cols_new = gsub("-", "", ll84_cols_new)
  # ll84_cols_new = gsub("(kBtu/ft²)", "", ll84_cols_new, fixed=TRUE)
  # ll84_cols_new = gsub("(kBtu/ft)", "", ll84_cols_new, fixed=TRUE)
  # ll84_cols_new = gsub("\r\n", "", ll84_cols_new, fixed=TRUE)
  # ll84_cols_new = gsub("\r\n", "", ll84_cols_new, fixed=TRUE)
  
  return (ll84)
}

load_pluto <- function() {
  
  #bk = read_csv(".\\data\\PLUTO_for_WEB\\BK_18v1.csv")
  #bx = read_csv(".\\data\\PLUTO_for_WEB\\BX_18v1.csv")
  #mn = read_csv(".\\data\\PLUTO_for_WEB\\MN_18v1.csv")
  #qn = read_csv(".\\data\\PLUTO_for_WEB\\QN_18v1.csv")
  #si = read_csv(".\\data\\PLUTO_for_WEB\\SI_18v1.csv")

  bk = read_csv(".\\data\\nyc_pluto_15v1\\BK.csv")
  bx = read_csv(".\\data\\nyc_pluto_15v1\\BX.csv")
  mn = read_csv(".\\data\\nyc_pluto_15v1\\MN.csv")
  qn = read_csv(".\\data\\nyc_pluto_15v1\\QN.csv")
  si = read_csv(".\\data\\nyc_pluto_15v1\\SI.csv")
  
  
  df = data.frame(bk = names(bk), 
                  bx = names(bx), 
                  mn = names(mn), 
                  qn = names(bx), 
                  si = names(mn))
  
  # naming issue in v18v1.csv
  #names(mn)[85:86] = toupper(names(mn)[85:86])
  #names(qn)[85:86] = toupper(names(qn)[85:86])
  #names(si)[85:86] = toupper(names(si)[85:86])
  
  pluto = rbind(bk, bx, mn, qn, si)
  
  pluto_cols = c( "Borough", "Block", "Lot", "ZipCode", "Address",
                  "LandUse", 
                  "BldgArea", "BldgClass", "AssessTot", "AssessLand", 
                  "LotArea", "NumBldgs",  "NumFloors", "UnitsTotal",
                  "UnitsRes", "ComArea", "ResArea",
                  "YearBuilt", "YearAlter1", "YearAlter2",
                  "BldgFront", "BldgDepth")
  pluto = pluto[pluto_cols]
  
  # there are noisy values
  pluto = pluto[ pluto$Borough %in% c("BK", "BX", "MN", "QN", "SI"), ]
  
  #add BBL to pluto
  #https://gist.github.com/clhenrick/f8e4ff2746b818b15e40
  #https://en.wikipedia.org/wiki/Borough,_Block_and_Lot
  bor = unlist(lapply(as.character(pluto$Borough), switch, 
               "MN" = 1, "BX" = 2, "BK" = 3, "QN" = 4, "SI" = 5, USE.NAMES = F) )
  
  pluto['BBL'] = sprintf("%1d%05d%04d", bor, pluto$Block, pluto$Lot)
  
  write_csv(pluto, ".\\data\\PLUTO_for_WEB\\ALL_18v1.csv", na = "")
  return (pluto)
}


ll84  = load_ll84()
pluto = load_pluto()

ll84$BBL = as.character(ll84$BBL)
merged = left_join(ll84, pluto, by = "BBL", suffix = c("_ll84", "_pluto"))


ll84a = ll84 %>% 
  mutate_at(vars(SourceEUI, WeatherNormalizedSourceEUI, ENERGYSTARScore), as.numeric) %>% 
  mutate_at(vars(Reported_Property_FloorArea_Building, DOF_Property_FloorArea_Buildngs_and_Parking), as.numeric) %>%
  mutate_at(vars(BBL), as.character)


ll84b = ll84a %>% filter(!is.na(SourceEUI))

ll84b['TotalEnergy'] = ll84b$DOF_Property_FloorArea_Buildngs_and_Parking * ll84b$SourceEUI



merged[] <- sapply(merged, function(x) {x <- gsub("Not Available",NA,x)})
merged = type.convert(merged,  as.is = TRUE)
write_csv(merged, ".\\data\\ll84_pluto_merged.csv", na = "")

cleaned = merged %>% 
  filter (! duplicated(BBL)) %>%
  filter(!is.na(DOF_Property_FloorArea_Buildngs_and_Parking)) %>%
  filter(!is.na(SourceEUI))

#cleaned1 = cleaned %>%  
#  mutate(YearAlter1 = ifelse(YearAlter1 == 0, YearBuilt_pluto, YearAlter1))

# YearAlter1 >= YearBuilt_pluto, wherever YearAlter1 is not zero
cleaned1 = cleaned %>% 
  filter( ifelse(YearAlter1 != 0, YearBuilt < YearAlter1, T))
cleaned2 = cleaned1 %>% 
  filter( SourceEUI <= 1000)
cleaned2['AreaUnitPrice'] = cleaned2$AssessTot / cleaned2$BldgArea

write_csv(cleaned2, ".\\data\\ll84_pluto_cleaned.csv", na = "")




nycdata = read_csv(".\\data\\ll84_pluto_cleaned.csv")
vars = c( "TotalEnergy", 
                "DOF_Property_FloorArea_Buildngs_and_Parking", 
                "BldgClass", "AssessTot", "AssessLand", "AreaUnitPrice",
                "BldgArea", "LotArea", "NumBldgs", "NumFloors", "UnitsTotal", "UnitsRes", 
                "ComArea", "ResArea", "YearBuilt", "BldgFront", "BldgDepth" )
train = nycdata[vars]
train$TotalEnergy = log(train$TotalEnergy)

unlist(lapply(train, function(x) sum(is.na(x))))
train1 = train[1:2000,]

library(rpart)
rp = rpart(TotalEnergy ~ ., method="class", data = train1)

rp <- rpart(TotalEnergy ~ ., 
            #method="class", 
            data = train1, 
            control = rpart.control(minsplit = 10, cp = 0.00197))

plot(rp, uniform=TRUE, 
     main="Regression Tree for NYC")
text(rp, use.n=TRUE, all=TRUE, cex=.8)



############## rule fit

#devtools::install_git("https://GravesEE@gitlab.ins.risk.regn.net/minneapolis-r-packages/rulefit.git")




