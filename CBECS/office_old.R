## filter office buildings
library(readr)
library(dplyr)

save_dir1 = './data/filtered/'
dir.create(save_dir1, showWarnings = F)

save_dir2 = './data/features/'
dir.create(save_dir2, showWarnings = F)

save_dir3 = './data/features_all/'
dir.create(save_dir3, showWarnings = F)

cbecs = read.csv("data/2012_public_use_data_aug2016.csv")

var1 = c( 'SQFT', 'NFLOOR', 'NELVTR', 'NESLTR', 'COURT', 
          'MONUSE', 'OPNWE',  'WKHRS', 'NWKER', 'COOK', 
          'MANU', 'HEATP',  'COOLP',  'SNACK', 'FASTFD', 'CAF',
          'FDPREP', 'KITCHN', 'BREAKRM', 'OTFDRM', 'LABEQP', 'MCHEQP',
          'POOL', 'HTPOOL', 'RFGWIN', 'RFGOPN', 'RFGCLN', 'RFGVNN',
          'RFGICN', 'PCTERMN', 'LAPTPN', 'PRNTRN', 'SERVERN', 'TVVIDEON',
          'RGSTRN', 'COPIERN', 'HDD65','CDD65')

var2 = c( "PBAPLUS", "PBA", "FINALWT",
          "MFBTU", 
          "ELBTU", "NGBTU", "FKBTU", "DHBTU",
          "ONEACT", "ACT1", "ACT2", "ACT3", "ACT1PCT", "ACT2PCT", "ACT3PCT",
          "PRAMTC", "PRUNIT",
          "CWUSED", "WOUSED", "COUSED", "SOUSED", "PRUSED")

offices = cbecs[, c(var1, var2)]

offices = offices %>% filter(PBAPLUS %in% c(2, 3, 4, 52))
o0 = offices

# Must have at least 1 computer
# NOTE: as per the doc, only 4 buildings should be filtered out
#       But, here 22 buildings are filtered out, 
#       so there will be an additional 18 buildings
o1 = o0 %>% filter(PCTERMN >= 1)
#o1 = o0 %>% filter(PCTERMN >= 1 | RGSTRN >= 1)

# Must have at least 1 worker
o2 = o1 %>% filter(NWKER >= 1)

# Must operate for at least 30 hours per week
o3 = o2 %>% filter(WKHRS >= 30)

# Must operate for at least 10 months per year
o4 = o3 %>% filter(MONUSE >= 10)

# If the variable ONEACT=1, then one activity occupies 75% or more of the building. 
# If the variable ONEACT=2, then the activities in the building are 
#   defined by ACT1, ACT2, and ACT3. One of these activities must be 
#   coded as Office/Professional (PBAX=11) or Public Order and Safety (PBAX=23),
#   with a corresponding percent (ACT1PCT, ACT2PCT, ACT3PCT) that is 
#   greater than 50.
# NOTE: only 42 buildings are filtered out. But in the document 43
o5 = o4 %>% 
  filter( (ONEACT == 1) |
            (ONEACT == 2 & 
               ((ACT1 %in% c(11,23) & ACT1PCT > 50) | 
                  (ACT2 %in% c(11,23) & ACT2PCT > 50) | 
                  (ACT3 %in% c(11,23) & ACT3PCT > 50) )))

# Must report energy usage
o6 = o5 %>% filter(!is.na(MFBTU))

# Must be less than or equal to 1,000,000 square feet
o7 = o6 %>% filter(SQFT <= 1000000)

# If propane is used, the amount category (PRAMTC) must equal 1, 2, or 3
o8 = o7 %>% filter(is.na(PRAMTC) | PRAMTC %in% c(1,2,3))

# If propane is used, the unit (PRUNIT) must be known
o9 = o8 %>% filter(is.na(PRUNIT) | PRUNIT %in% c(1,2))

# If propane is used, the maximum estimated propane amount must be 10% or less of the total source energy
# TODO: No information about the estimated propane amount is there in CBECS
o10 = o9

#must not use chilled water, wood, coal, or solar
o11 = o10 %>% 
  filter(CWUSED == 2) %>%
  filter(WOUSED == 2) %>% 
  filter(COUSED == 2) %>% 
  filter(SOUSED == 2)

# Server count must be known
o12 = o11 %>% filter(SERVERN != 9995)

#Must have no more than 8 workers per 1,000 square feet
o13 = o12 %>% filter(NWKER  / SQFT * 1000 <= 8)

# Is solar used in any buildings - '1' = 'Yes' '2' = 'No'"
#summary(o14$SOUSED)

#calculate source energy/eui as per "Source Energy.pdf"
o14 = o13 %>% 
  #mutate(EUI = round(MFBTU/SQFT*2.80, 2))
  mutate(ELBTU0 = ELBTU*2.80) %>%
  mutate(NGBTU0 = NGBTU*1.05) %>%
  mutate(FKBTU0 = FKBTU*1.01) %>%
  mutate(DHBTU0 = DHBTU*1.20) %>%
  mutate(SOURCE_ENERGY = rowSums(dplyr::select(., c(ELBTU0,NGBTU0,FKBTU0,DHBTU0)), na.rm = T)) %>% 
  mutate(SOURCE_EUI = round(SOURCE_ENERGY/SQFT, 2)) %>%
  mutate(SITE_EUI = round(MFBTU/SQFT, 2)) %>% 
  mutate(SUMBTU = rowSums(dplyr::select(., c(ELBTU,NGBTU,FKBTU,DHBTU)), na.rm = T))

#Is MFBTU the sum of ELBTU,NGBTU,FKBTU,DHBTU? YES.
summary(o14$MFBTU - o14$SUMBTU)


o14a = o14 %>% filter(PRUSED == '1')

#Banks must have Source EUI greater than 50 kBtu/ft2
o15 = o14 %>% 
  filter( PBAPLUS != "03" |  SOURCE_EUI > 50)

o16 = o15 %>% filter( !is.na(COOLP) )

write.csv(o16, paste0(save_dir1, "office.csv"), row.names = F)


########### make features
office = read.csv(paste0(save_dir1, "office.csv"))
#office = office %>% filter(PBAPLUS == 2)

data = office %>%
  mutate(NWKER_SQFT = NWKER/SQFT * 1000) %>%
  mutate(PCTERMN_TOT = rowSums(dplyr::select(., c(PCTERMN,SERVERN,LAPTPN)), na.rm = T)) %>% 
  mutate(PCTERMN_SQFT = PCTERMN_TOT/SQFT * 1000) %>%
  mutate(CDD65_COOLP = log(CDD65) * COOLP / 100) %>%
  mutate(IsBank = ifelse(PBAPLUS == 3, "Yes", "No")) %>%
  #filter(SQFT <= 100000) %>%
  mutate_if(is.numeric, round, 3)

data = data %>% filter(SOURCE_EUI <= 500)

ivars = c("SQFT", "WKHRS", "NWKER_SQFT", "PCTERMN_SQFT", 
          "CDD65_COOLP", "IsBank")
dvars  = c("SOURCE_EUI", "SOURCE_ENERGY", "FINALWT")

features = data[, c(ivars, dvars)]

write.csv(features, paste0(save_dir2, "office.csv"), row.names = F)



##############################################################
########### using all features in the tech doc ###############
##############################################################
office = read.csv(paste0(save_dir1, "office.csv"))

data = office %>%
  mutate(NWKER_SQFT = NWKER/SQFT * 1000) %>%
  mutate(PCTERMN_TOT = rowSums(dplyr::select(., c(PCTERMN,SERVERN,LAPTPN)), na.rm = T)) %>% 
  mutate(PCTERMN_SQFT = PCTERMN_TOT/SQFT * 1000) %>%
  mutate(CDD65_COOLP = log(CDD65) * COOLP / 100) %>%
  mutate(IsBank = ifelse(PBAPLUS == 3, "Yes", "No")) %>%
  #filter(SQFT <= 100000) %>%
  mutate_if(is.numeric, round, 3)

req_cols = c( "SQFT", "WKHRS", "NWKER", 
              "PCTERMN_TOT", 
              "IsBank",
              "COOLP", "CDD65"
              #"HEATP", "HDD65"
              )

dvars  = c("SOURCE_EUI", "SOURCE_ENERGY", "FINALWT")

features = data[, c(req_cols, dvars)]

features1 = features %>% na.omit()

write.csv(features1, paste0(save_dir3, "office.csv"), row.names = F)
