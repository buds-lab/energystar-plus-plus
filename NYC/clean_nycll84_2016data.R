library(readxl)
library(dplyr)

load_ll84 <- function() {

  ll84_file = ".\\data\\nyc_benchmarking_disclosure_data_reported_in_2017.xlsx"
  # there are four sheets, lets combine them
  ll84_a = read_xlsx (ll84_file, "Information and Metrics")
  ll84_b = read_xlsx (ll84_file, "Multi-BBL")
  ll84_c = read_xlsx (ll84_file, "Child no Parent")
  ll84_d = read_xlsx (ll84_file, "Multi-BBL")
  ll84 = rbind(ll84_a, ll84_b, ll84_c, ll84_d)
  return (ll84)
}


load_pluto <- function() {
  
  bk = read_csv(".\\data\\PLUTO_for_WEB\\BK_18v1.csv")
  bx = read_csv(".\\data\\PLUTO_for_WEB\\BX_18v1.csv")
  mn = read_csv(".\\data\\PLUTO_for_WEB\\MN_18v1.csv")
  qn = read_csv(".\\data\\PLUTO_for_WEB\\QN_18v1.csv")
  si = read_csv(".\\data\\PLUTO_for_WEB\\SI_18v1.csv")
  
  #df = data.frame(bk = names(bk), 
  #                bx = names(bx), 
  #                mn = names(mn), 
  #                qn = names(bx), 
  #                si = names(mn))
  
  # naming issue
  names(mn)[85:86] = toupper(names(mn)[85:86])
  names(qn)[85:86] = toupper(names(qn)[85:86])
  names(si)[85:86] = toupper(names(si)[85:86])
  
  all = rbind(bk, bx, mn, qn, si)
  write_csv(all, ".\\data\\PLUTO_for_WEB\\ALL_18v1.csv", na = "")
  return (all)
}


ll84  = load_ll84()
pluto = load_pluto()

pluto_cols = c( "Borough", "Block", "Lot", "ZipCode", "Address",
                "LandUse", 
                "BldgArea", "BldgClass", "AssessTot", "AssessLand", 
                "LotArea", "NumBldgs",  "NumFloors", "UnitsTotal",
                "UnitsRes", "ComArea", "ResArea",
                "YearBuilt", "YearAlter1", "YearAlter2",
                "BldgFront", "BldgDepth")
pluto = pluto[pluto_cols]


#paste(names(ll84), sep = ",", collapse = '","')
ll84_cols = c( "Property Id",
               "Property Name",
               "BBL - 10 digits",
               "Postal Code",
               "Street Number",
               "Street Name",
               "Borough",
               "DOF Gross Floor Area",
               "Year Built",
               "Occupancy",
               "ENERGY STAR Score",
               "Weather Normalized Site Electricity (kWh)",
               "Source EUI (kBtu/ft²)" )
ll84 = ll84[ll84_cols]
ll84_cols_new = names(ll84)


ll84_cols_new = gsub(" ", "", ll84_cols_new)
ll84_cols_new = gsub("-", "", ll84_cols_new)
ll84_cols_new = gsub("(kBtu/ft²)", "", ll84_cols_new, fixed=TRUE)
names(ll84) = ll84_cols_new

               
#add BBL to pluto
#https://gist.github.com/clhenrick/f8e4ff2746b818b15e40
#https://en.wikipedia.org/wiki/Borough,_Block_and_Lot
bor = sapply(as.character(pluto$Borough), switch, 
             "MN" = 1, "BX" = 2, "BK" = 3, "QN" = 4, "SI" = 5, USE.NAMES = F) 
pluto['BBL'] = sprintf("%1d%05d%04d", bor, pluto$Block, pluto$Lot)


#split ; in BBL to separate rows in ll84
#ll84a = separate_rows(ll84, 'BBL10digits', convert = F)
ll84a = ll84
ll84a['BBL'] = ll84a$BBL10digits

merged = left_join(ll84a, pluto, by = "BBL", suffix = c("_ll84", "_pluto"))
merged[] <- sapply(merged, function(x) {x <- gsub("Not Available",NA,x)})
merged = type.convert(merged,  as.is = TRUE)
write_csv(merged, ".\\data\\ll84_pluto_merged.csv", na = "")

#clean it
#merged <- sapply(merged, as.numeric )
#merged[] <- lapply(merged, function(x) as.numeric(as.character(x)))

cleaned = merged %>% 
  filter (! duplicated(BBL)) %>%
  filter(!is.na(DOFGrossFloorArea)) %>%
  filter(!is.na(SourceEUI))

#cleaned1 = cleaned %>%  
#  mutate(YearAlter1 = ifelse(YearAlter1 == 0, YearBuilt_pluto, YearAlter1))

# YearAlter1 >= YearBuilt_pluto, wherever YearAlter1 is not zero
cleaned1 = cleaned %>% 
  filter( ifelse(YearAlter1 != 0, YearBuilt_pluto < YearAlter1, T))
cleaned2 = cleaned1 %>% 
  filter( SourceEUI <= 1000)
