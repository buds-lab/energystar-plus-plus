## filter k12 school buildings
library(readr)
library(dplyr)

save_dir1 = './data/filtered/'
dir.create(save_dir1, showWarnings = F)

save_dir2 = './data/features/'
dir.create(save_dir2, showWarnings = F)

cbecs = read.csv("data/2012_public_use_data_aug2016.csv")

var1 = c( 'SQFT', 'NFLOOR', 'NELVTR', 'NESLTR', 'COURT',
          'MONUSE', 'WKHRS', 'NWKER', 'COOK', 'HEATP',
          'COOLP', 'SNACK', 'FASTFD', 'CAF', 'FDPREP',
          'KITCHN', 'BREAKRM', 'OTFDRM', 'RFGSTO', 'RFGWIN',
          'RFGOPN', 'RFGCLN', 'RFGVNN', 'RFGICN', 'RFGSTP',
          'PCTERMN', 'LAPTPN', 'PRNTRN',  'SERVERN',
          'TVVIDEON', 'RGSTRN', 'COPIERN',  'HDD65', 'CDD65',
          'RFGRES')

var2 = c( "PBAPLUS", "PBA", "FINALWT",
          "MFBTU", 
          "ELBTU", "NGBTU", "FKBTU", "DHBTU",
          "ONEACT", "ACT1", "ACT2", "ACT3", "ACT1PCT", "ACT2PCT", "ACT3PCT",
          "PRAMTC", "PRUNIT",
          "CWUSED", "WOUSED", "COUSED", "SOUSED")

var3 = c( "RFGRSN", "RFGCOMPN", "RFGWIN", "RFGOPN", "RFGCLN", "RFGVNN")

# imputed data details
var4 = c("ZWKHRS", "ZNWKER", "ZRFGWIN", "ZCOOLP", "ZHEATP")

# heat details
var5 = c("HEATP", "FURNP", "PKGHP", "BOILP", "STHWP", "HTPHP", "SLFCNP", "OTHTP")
         

retail = cbecs[, c(var1, var2, var3, var4, var5)]
retail = retail %>% filter(PBAPLUS %in% c(14, 42))
r0 = retail

# Must operate for at least 30 hours per week
r1 = r0 %>% filter(WKHRS >= 30)

# Must have at least 1 worker
r2 = r1 %>% filter(NWKER >= 1)

# Must have at least 1 computer or cash register
r3 = r2 %>% filter(PCTERMN >= 1 | RGSTRN >=1)

#If building is a supermarket, must have refrigeration equipment
r4 = r3 %>% 
  filter( PBAPLUS != "03" | RFGRES == 1)

# Must operate for at least 10 months per year
r5 = r4 %>% filter(MONUSE >= 10)

# If the variable ONEACT=1, then one activity occupies 75% or more of the building. 
# If the variable ONEACT=2, then the activities in the building are 
#   defined by ACT1, ACT2, and ACT3. One of these activities must be 
#   coded as Office/Professional (PBAX=11) or Public Order and Safety (PBAX=23),
#   with a corresponding percent (ACT1PCT, ACT2PCT, ACT3PCT) that is 
#   greater than 50.
# NOTE: only 5 buildings are filtered out. But in the document 10
r6 = r5 %>% 
  filter( (ONEACT == 1) |
            (ONEACT == 2 & 
               ((ACT1 %in% c(14,16) & ACT1PCT > 50) | 
                  (ACT2 %in% c(14,16) & ACT2PCT > 50) | 
                  (ACT3 %in% c(14,16) & ACT3PCT > 50) )))

# Must be less than or equal to 1,000,000 square feet
r7 = r6 %>% filter(SQFT <= 1000000)

# If propane is used, the amount category (PRAMTC) must equal 1, 2, or 3
r8 = r7 %>% filter(is.na(PRAMTC) | PRAMTC %in% c(1,2,3))

# If propane is used, the unit (PRUNIT) must be known
r9 = r8 %>% filter(is.na(PRUNIT) | PRUNIT %in% c(1,2))

# If propane is used, the maximum estimated propane amount must be 10% or less of the total source energy
# TODO
r10 = r9

#must not use chilled water, wood, coal, or solar
r11 = r10 %>% 
  filter(CWUSED == 2) %>%
  filter(WOUSED == 2) %>% 
  filter(COUSED == 2) %>% 
  filter(SOUSED == 2)

# Must be at least 5,000 square feet
# NOTE: filtered out - 81, in document 78
r12 = r11 %>% filter(SQFT >= 5000)

# Must have fewer than 3 open or closed refrigeration/freezer cases per 1,000 square feet
# NOTE: filtered out - 0, in document 1
r13 = r12 %>% filter( (is.na(RFGOPN) | (RFGOPN/SQFT * 1000 < 3)) | 
                        (is.na(RFGCLN) | (RFGCLN/SQFT * 1000 < 3)))


# Must have fewer than 0.7 walk-in refrigeration/freezer cases per 1,000 square feet
r14 = r13 %>% filter(is.na(RFGWIN) | (RFGWIN / SQFT * 1000 <= 0.7)) 

# Must have Source EUI greater than or equal to 20 kBtu/ft2
r15 = r14 %>% 
  #mutate(EUI = round(MFBTU/SQFT*2.80, 2))
  dplyr::mutate(ELBTU0 = ELBTU*2.80) %>%
  dplyr::mutate(NGBTU0 = NGBTU*1.05) %>%
  dplyr::mutate(FKBTU0 = FKBTU*1.01) %>%
  dplyr::mutate(DHBTU0 = DHBTU*1.20) %>%
  dplyr::mutate(SOURCE_ENERGY = rowSums(dplyr::select(., c(ELBTU0,NGBTU0,FKBTU0,DHBTU0)), na.rm = T)) %>% 
  dplyr::mutate(SOURCE_EUI = round(SOURCE_ENERGY/SQFT, 2)) %>%
  dplyr::mutate(SITE_EUI = round(MFBTU/SQFT, 2))

# NOTE: filtered out 4, in document 2
r16 = r15 %>% filter( SOURCE_EUI >= 20)

#If CDD is greater than 3,000, must be at least 60% cooled
r17 = r16 %>% filter( CDD65 <= 3000 | COOLP >= 60) 

write.csv(r17, paste0(save_dir1, "retail.csv"), row.names = F)

### imputed summary
for (var in var3) {
  print(paste(var))
  print(table(r17[, var]))
}

########### make features
retail = read.csv(paste0(save_dir1, "retail.csv"))

data = retail %>%
  dplyr::mutate(NWKER_SQFT = NWKER/SQFT * 1000) %>%
  dplyr::mutate(RFG_TOT = rowSums(dplyr::select(., c(RFGRSN,RFGCOMPN,RFGWIN,RFGOPN,RFGCLN,RFGVNN)), na.rm = T)) %>% 
  dplyr::mutate(RFG_SQFT = RFG_TOT/SQFT * 1000) %>%
  dplyr::mutate(CDD65_COOLP = log(CDD65) * COOLP / 100) %>%
  dplyr::mutate(HDD65_HEATP = log(HDD65) * HEATP / 100) %>%
  dplyr::mutate(IsSuperMarket = ifelse(PBAPLUS == 14, "Yes", "No")) %>%
  dplyr::mutate(AdjNWKER_SQFTsuperMarket = ifelse(PBAPLUS == 14, 1, 0)) %>%
  dplyr::mutate(AdjNWKER_SQFTsuperMarket = AdjNWKER_SQFTsuperMarket * NWKER_SQFT) %>%
  dplyr::mutate_if(is.numeric, round, 3)

#TODO: Adjustment for the Number of Workers per 1,000 Square Feet for a Supermarket

ivars = c(
  "SQFT",
  "WKHRS", "NWKER_SQFT", "RFG_SQFT", 
          "HDD65_HEATP", "CDD65_COOLP", 
          "IsSuperMarket", "AdjNWKER_SQFTsuperMarket")

dvars  = c("SOURCE_EUI", "SOURCE_ENERGY", "FINALWT")

features = data[, c(ivars, dvars)]
features = features %>% na.omit()
write.csv(features, paste0(save_dir2, "retail.csv"), row.names = F)
