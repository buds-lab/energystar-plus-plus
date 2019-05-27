library(readxl)
library(readr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(rpart.utils)
library(sfa)
library(frontier)

library(pre)

#nycdata = read_csv("./data/nyc_benchmarking_disclosure_data_reported_in_2017.csv")
nycdata = read_csv("./data/ll84_pluto_cleaned_2016.csv")

names(nycdata) = make.names(names(nycdata))

t1 = nycdata$LargestPropertyUseType
t2 = nycdata$X2ndLargestPropertyUseType
t3 = nycdata$X3rdLargestPropertyUseType

t1 = t1[ !is.na(t1)]
t2 = t2[ !is.na(t2)]
t3 = t3[ !is.na(t3)]

ta = c(t1, t2, t3)
tas = sort(table(ta))
tas1 = rev(table(ta))

tab1 = table(nycdata$PrimaryPropertyTypeSelfSelected)
tab2 = table(nycdata$LargestPropertyUseType)

View(sort(tab1))
View(sort(tab2))

nycdata = nycdata %>% 
  mutate(GFA = nycdata$DOFGrossFloorArea)

save_dir = "./data/usetypes/"

cols = c("GFA",
         "Occupancy",
         "YearBuilt_ll84",
         #"Age",
         "NumBldgs", 
         "NumFloors",
         "UnitsTotal", 
         "UnitsRes",
         "ENERGYSTARScore",
         "SourceEUI",
         #"SourceEnergy",
         "SiteEUI"
         #"SiteEnergy"
         )

#Multifamily Housing
usetype = "Multifamily Housing"
filtered = nycdata %>% 
  filter( LargestPropertyUseType %in% usetype &  
            PrimaryPropertyTypeSelfSelected %in% usetype) %>%
  dplyr::select(cols)
write.csv(filtered, paste0(save_dir, "multifamily.csv"), row.names = F)


#Office
usetype = c("Office", "Bank Branch", 
            "Financial Office", "Courthouse")
filtered = nycdata %>% 
  filter( LargestPropertyUseType %in% usetype &  
            PrimaryPropertyTypeSelfSelected %in% usetype) %>%
  dplyr::select(cols)
write.csv(filtered, paste0(save_dir, "office.csv"), row.names = F)


# Retail Store & Supermarket/Grocery Store
usetype = c("Supermarket/Grocery Store", "Retail Store")
filtered = nycdata %>% 
  filter( LargestPropertyUseType %in% usetype &  
          PrimaryPropertyTypeSelfSelected %in% usetype) %>%
  dplyr::select(cols)
write.csv(filtered, paste0(save_dir, "retail.csv"), row.names = F)


# K-12 School
usetype = c("K-12 School")
filtered = nycdata %>% 
  filter( LargestPropertyUseType %in% usetype &  
            PrimaryPropertyTypeSelfSelected %in% usetype) %>%
  dplyr::select(cols)
write.csv(filtered, paste0(save_dir, "k12school.csv"), row.names = F)


# warehouses
usetype = c("Refrigerated Warehouse", "Non-Refrigerated Warehouse", 
            "Distribution Center")
filtered = nycdata %>% 
  filter( LargestPropertyUseType %in% usetype &  
            PrimaryPropertyTypeSelfSelected %in% usetype) %>%
  dplyr::select(cols)
write.csv(filtered, paste0(save_dir, "warehouse.csv"), row.names = F)


# Hotel
usetype = c("Hotel")
filtered = nycdata %>% 
  filter( LargestPropertyUseType %in% usetype &  
            PrimaryPropertyTypeSelfSelected %in% usetype) %>%
  dplyr::select(cols)
write.csv(filtered, paste0(save_dir, "hotel.csv"), row.names = F)


# Worship Facility
usetype = c("Worship Facility")
filtered = nycdata %>% 
  filter( LargestPropertyUseType %in% usetype &  
            PrimaryPropertyTypeSelfSelected %in% usetype) %>%
  dplyr::select(cols)
write.csv(filtered, paste0(save_dir, "worship.csv"), row.names = F)

# Medical offices
usetype = c("Medical Office")
filtered = nycdata %>% 
  filter( LargestPropertyUseType %in% usetype &  
            PrimaryPropertyTypeSelfSelected %in% usetype) %>%
  dplyr::select(cols)
write.csv(filtered, paste0(save_dir, "medical_office.csv"), row.names = F)


# Hospitals
usetype = c("Hospital (General Medical & Surgical)",
            "Other - Specialty Hospital", 
            "Urgent Care/Clinic/Other Outpatient")
filtered = nycdata %>% 
  filter( LargestPropertyUseType %in% usetype &  
            PrimaryPropertyTypeSelfSelected %in% usetype) %>%
  dplyr::select(cols)
write.csv(filtered, paste0(save_dir, "hospital.csv"), row.names = F)


# Residence Hall/Dormitory
usetype = c("Residence Hall/Dormitory")
filtered = nycdata %>% 
  filter( LargestPropertyUseType %in% usetype &  
            PrimaryPropertyTypeSelfSelected %in% usetype) %>%
  dplyr::select(cols)
write.csv(filtered, paste0(save_dir, "residencehall_dormitory.csv"), row.names = F)


# Senior Care Community
usetype = c("Senior Care Community")
filtered = nycdata %>% 
  filter( LargestPropertyUseType %in% usetype &  
            PrimaryPropertyTypeSelfSelected %in% usetype) %>%
  dplyr::select(cols)
write.csv(filtered, paste0(save_dir, "seniorcare_community.csv"), row.names = F)


usetype_all = c( 
  "Multifamily Housing",
  "Office", "Bank Branch", "Financial Office", "Courthouse",
  "Supermarket/Grocery Store", "Retail Store",
  "K-12 School",
  "Refrigerated Warehouse", "Non-Refrigerated Warehouse", "Distribution Center",
  "Hotel",
  "Worship Facility"
  #"Medical Office",
  #"Hospital (General Medical & Surgical)", "Other - Specialty Hospital", "Urgent Care/Clinic/Other Outpatient",
  #"Residence Hall/Dormitory",
  #"Senior Care Community"
  )

filtered0 = nycdata %>% 
  filter( LargestPropertyUseType %in% usetype_all &  
            PrimaryPropertyTypeSelfSelected %in% usetype_all)
  
remaining = nrow(nycdata) - nrow(filtered0)
remaining_p = round( remaining * 100 / nrow(nycdata), 2)
print(paste("remaining ", remaining, remaining_p ))
print(100 - remaining_p)
#92%



#### apply data filters similar to energy star's GFA
btypes = c(
  "office", "retail", "k12school", 
  "warehouse",
  "hotel", "worship", "multifamily",
  "hospital", "medical_office")

load_dir  = "./data/usetypes/"
save_dir1 = "./data/usetypes_filtered/"
dir.create(save_dir1, showWarnings = F, recursive = T)

es = read.csv("../energystar_metrics.csv", stringsAsFactors = F)
es = es %>% filter(dependent == "SOURCE_EUI")

counts = NULL

for (btype in es$btype) {
  filepath = paste0(load_dir, btype, ".csv")
  data = read.csv(filepath)
  print(paste(filepath, btype, paste(dim(data), collapse = " x ")))
  
  row = es[es$btype == btype, ]
  
  data1 = data
  if(!is.na(row$minGFA)) 
    data1 = data1 %>% filter(GFA >= row$minGFA)
  if(!is.na(row$maxGFA)) 
    data1 = data1 %>% filter(GFA <= row$maxGFA)

  # if(!is.na(row$minEUI)) 
  #   data1 = data1 %>% filter(SourceEUI >= row$minEUI)
  # if(!is.na(row$maxEUI)) 
  #   data1 = data1 %>% filter(SourceEUI <= row$maxEUI)
  
  ## other filters
  
  quan = quantile(data1$SourceEUI, probs = c(0.01, 0.99))
  data2 = data1 %>% 
    filter(SourceEUI >= quan[1]) %>%
    filter(SourceEUI <= quan[2])
  
  data3 = data2 %>% 
    dplyr::rename(Age = YearBuilt_ll84) %>%
    mutate(Age = 2017 - Age) %>%
    mutate(SiteEnergy   = GFA * SiteEUI) %>%
    mutate(SourceEnergy = GFA * SourceEUI)
  
  print(paste("min GFA", min(data3$GFA),
              "min EUI", min(data3$SourceEUI),
              "max EUI", max(data3$SourceEUI)))
  
  filepath = paste0(save_dir1, btype, ".csv")
  print(paste(filepath, btype, paste(dim(data3), collapse = " x ")))
  write.csv(data3, filepath, row.names = F)
  
  diff = data.frame(btype = btype, unfiltered = nrow(data), filtered = nrow(data3))
  counts = rbind(counts, diff)
}

tot = data.frame(btype = "Total",
                 unfiltered = sum(counts$unfiltered), 
                 filtered = sum(counts$filtered))
counts = rbind(counts, tot)
filepath = paste0(save_dir1, "filtered_summary.csv")
write.csv(counts, filepath, row.names = F)





