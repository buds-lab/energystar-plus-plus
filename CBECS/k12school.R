## filter k12 school buildings

save_dir1 = './data/filtered/'
dir.create(save_dir1, showWarnings = F)

save_dir2 = './data/features/'
dir.create(save_dir2, showWarnings = F)

save_dir3 = './data/features/'
dir.create(save_dir3, showWarnings = F)


cbecs = read.csv("data/2012_public_use_data_aug2016.csv")

var1 = c( 'SQFT', 'NFLOOR', 'NELVTR', 'NESLTR', 'EDSEAT',
          'COURT', 'MONUSE', 'OPNWE', 'WKHRS', 'NWKER',
          'COOK',  'HEATP',   'COOLP',  'SNACK', 'FASTFD',
          'CAF',   'FDPREP',   'KITCHN', 'BREAKRM', 'OTFDRM',
          'LABEQP', 'POOL',   'HTPOOL', 'RFGRES', 'RFGCOMPN',
          'RFGWIN', 'RFGOPN', 'RFGCLN', 'RFGVNN', 'RFGICN',
          'PCTERMN', 'LAPTPN', 'PRNTRN', 'SERVERN', 'TRNGRM',
          'STDNRM',  'WBOARDS', 'TVVIDEON', 'RGSTRN', 'COPIERN',
          'HDD65',  'CDD65')

var2 = c( "PBAPLUS", "PBA", "FINALWT",
          "MFBTU", 
          "ELBTU", "NGBTU", "FKBTU", "DHBTU",
          "ONEACT", "ACT1", "ACT2", "ACT3", "ACT1PCT", "ACT2PCT", "ACT3PCT",
          "PRAMTC", "PRUNIT",
          "CWUSED", "WOUSED", "COUSED", "SOUSED")

schools = cbecs[, c(var1, var2)]
schools = schools %>% filter(PBAPLUS %in% c(28, 29))

s0 = schools
s1 = s0 %>% filter(WKHRS >= 30)
s2 = s1 %>% filter(MONUSE >= 8)
s3 = s2 %>% filter(NWKER >= 1)
s4 = s3 %>% filter(EDSEAT >= 1)

# If the variable ONEACT=1, then one activity occupies 75% or more of the building. 
# If the variable ONEACT=2, then the activities in the building are 
#   defined by ACT1, ACT2, and ACT3. One of these activities must be 
#   coded as Office/Professional (PBAX=11) or Public Order and Safety (PBAX=23),
#   with a corresponding percent (ACT1PCT, ACT2PCT, ACT3PCT) that is 
#   greater than 50.
s5 = s4 %>% 
  filter( (ONEACT == 1) |
            (ONEACT == 2 & 
               ((ACT1 == 17 & ACT1PCT > 50) | 
                  (ACT1 == 17 & ACT2PCT > 50) | 
                  (ACT1 == 17 & ACT2PCT > 50) )))

s6 = s5 %>% filter(!is.na(MFBTU))
s7 = s6 %>% filter(SQFT <= 1000000)
s8 = s7 %>% filter(is.na(PRAMTC) | PRAMTC %in% c(1,2,3))
s9 = s8 %>% filter(is.na(PRUNIT) | PRUNIT %in% c(1,2))

# If propane is used, the maximum estimated propane amount 
#must be 10% or less of the total source energy
s10 = s9 ## TODO

#must not use chilled water, wood, coal, or solar
s11 = s10 %>% 
  filter(CWUSED == 2) %>%
  filter(WOUSED == 2) %>% 
  filter(COUSED == 2) %>% 
  filter(SOUSED == 2)


#Must have Source EUI no greater than 250 kBt
s12 = s11 %>% 
  #mutate(EUI = round(MFBTU/SQFT*2.80, 2))
  dplyr::mutate(ELBTU0 = ELBTU*2.80) %>%
  dplyr::mutate(NGBTU0 = NGBTU*1.05) %>%
  dplyr::mutate(FKBTU0 = FKBTU*1.01) %>%
  dplyr::mutate(DHBTU0 = DHBTU*1.20) %>%
  dplyr::mutate(SOURCE_ENERGY = rowSums(dplyr::select(., c(ELBTU0,NGBTU0,FKBTU0,DHBTU0)), na.rm = T)) %>% 
  dplyr::mutate(SOURCE_EUI = round(SOURCE_ENERGY/SQFT, 2)) %>%
  dplyr::mutate(SITE_EUI = round(MFBTU/SQFT, 2))

s13 = s12 %>% filter(SOURCE_EUI <= 250)

# Must have no more than 1.9 workers per 1,000 square feet
s14 = s13 %>% filter(NWKER  / SQFT * 1000 <= 1.9)

# Must have no more than 0.06 walk-in refrigeration per 1,000 square feet
s15 = s14 %>% filter(is.na(RFGWIN) | (RFGWIN / SQFT * 1000 <= 0.06)) 

# Must have no more than 17 classroom seats per 1,000 square feet
s16 = s15 %>% filter(EDSEAT / SQFT * 1000 <= 17)

# Must operate no more than 140 hours per week
s17 = s16 %>% filter(WKHRS <= 140)

write.csv(s17, paste0(save_dir1, "k12school.csv"), row.names = F)


########### make features
save_dir1 = './data/filtered/'
save_dir2 = './data/features/'

k12school = read.csv(paste0(save_dir1, "k12school.csv"))

data = k12school %>%
  mutate(NWKER_SQFT = NWKER/SQFT * 1000) %>%
  mutate(CDD65_COOLP = CDD65 * COOLP / 100) %>%
  mutate(HDD65_HEATP = HDD65 * HEATP / 100) %>%
  mutate(IsCooking = ifelse(COOK == 1, "Yes", "No")) %>%
  mutate(IsOpenWeekends = ifelse(OPNWE == 1, "Yes", "No")) %>%
  mutate(IsHighSchool = ifelse(PBAPLUS == 29, "Yes", "No")) %>% 
  mutate_if(is.numeric, round, 3)

ivars = c(
  #"SQFT", 
          "NWKER_SQFT", "HDD65_HEATP",
          "CDD65_COOLP", "IsCooking",
          "IsOpenWeekends", "IsHighSchool")

dvars  = c("SOURCE_EUI", "SOURCE_ENERGY", "FINALWT")

features = data[, c(ivars, dvars)]
features = features %>% na.omit()
write.csv(features, paste0(save_dir2, "k12school.csv"), row.names = F)



##############################################################
########### using all features in the tech doc ###############
##############################################################

data = k12school %>%
  mutate(NWKER_SQFT = NWKER/SQFT * 1000) %>%
  mutate(CDD65_COOLP = CDD65 * COOLP / 100) %>%
  mutate(HDD65_HEATP = HDD65 * HEATP / 100) %>%
  mutate(IsCooking = ifelse(COOK == 1, "Yes", "No")) %>%
  mutate(IsOpenWeekends = ifelse(OPNWE == 1, "Yes", "No")) %>%
  mutate(IsHighSchool = ifelse(PBAPLUS == 29, "Yes", "No")) %>% 
  mutate_if(is.numeric, round, 3)

ivars = var1
dvars  = c("SOURCE_EUI", "SOURCE_ENERGY", "FINALWT")
features = data[, c(ivars, dvars)]

## calc no. of missing values in each field
sort(sapply(features, function(x) {
  sum(is.na(x))
}))

req_cols = c( "SQFT", "NFLOOR", "WKHRS", "NWKER", 
              "COOLP", "CDD65", "HEATP", "HDD65", 
              "IsCooking", "IsOpenWeekends", "IsHighSchool")

features1 = data[, c(dvars, req_cols)]

sort(sapply(features1, function(x) {
  sum(is.na(x))
}))

features1 = features1 %>% na.omit()

write.csv(features1, paste0(save_dir3, "k12school.csv"), row.names = F)
