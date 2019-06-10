## filter worship buildings

save_dir1 = './data/filtered/'
dir.create(save_dir1, showWarnings = F)

save_dir2 = './data/features/'
dir.create(save_dir2, showWarnings = F)

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
          "CWUSED", "WOUSED", "COUSED", "SOUSED", 
          "RFGSTP")

var3 = c("RWSEAT")

worships = cbecs[, c(var1, var2, var3)]

worships = worships %>% filter(PBAPLUS == 21)
w0 = worships

# Must operate at least 1 hour per week
w1 = w0 %>% filter(WKHRS >= 1)

# Must have at least 1 seat
w2 = w1 %>% filter(RWSEAT >= 1)

# Must operate for at least 10 months per year
w3 = w2 %>% filter(MONUSE >= 10)

# If the variable ONEACT=1, then one activity occupies 75% or more of the building. 
# If the variable ONEACT=2, then the activities in the building are 
#   defined by ACT1, ACT2, and ACT3. One of these activities must be 
#   coded as Warehouse/Storage (PBAX=18), with a corresponding percent 
#   (ACT1PCT, ACT2PCT, ACT3PCT) that is greater than 50.
# NOTE: filtered - 10, in document 12
w4 = w3 %>% 
  filter( (ONEACT == 1) |
            (ONEACT == 2 & 
               ((ACT1 %in% c(18) & ACT1PCT > 50) | 
                  (ACT2 %in% c(18) & ACT2PCT > 50) | 
                  (ACT3 %in% c(18) & ACT3PCT > 50) )))

## test code to verify the above filter
# f1 = w3 %>% filter(ONEACT == 1)
# f2 = w3 %>% filter(ONEACT == 2)
# summary(f2[, c("ACT1", "ACT2", "ACT3")])
# f2a = f2 %>% filter(ACT1 == 13)
# f2a1 = f2a %>% filter(ACT1PCT > 50)
# f2b = f2 %>% filter(ACT2 == 13)
# table(f2b$ACT2PCT)
# f2b1 = f2b %>% filter(ACT2PCT > 50)
# nrow(f1) + nrow(f2a1) + nrow(f2b1)

# Must report energy usage
w5 = w4 %>% filter(!is.na(MFBTU))

# Must be less than or equal to 1,000,000 square feet
w6 = w5 %>% filter(SQFT <= 1000000)

# If propane is used, the amount category (PRAMTC) must equal 1, 2, or 3
w7 = w6 %>% filter(is.na(PRAMTC) | PRAMTC %in% c(1,2,3))

# If propane is used, the unit (PRUNIT) must be known
w8 = w7 %>% filter(is.na(PRUNIT) | PRUNIT %in% c(1,2))

# If propane is used, the maximum estimated propane amount must be 10% or less of the total source energy
# TODO
w9 = w8

#must not use chilled water, wood, coal, or solar
w10 = w9 %>% 
  filter(CWUSED == 2) %>%
  filter(WOUSED == 2) %>% 
  filter(COUSED == 2) %>% 
  filter(SOUSED == 2)

#If space within the building is used for food preparation, 
# then square footage used for this purpose (FDPREPSFR) must be reported
# TODO
w11 = w10 

# Must have no more than 250 seats per 1,000 square feet
w12 = w11 %>% filter(RWSEAT / SQFT * 1000 <= 250)

#Must have no more than 2.5 workers per 1,000 square feet
w13 = w12 %>% filter(NWKER  / SQFT * 1000 <= 2.5)

#Must not operate 168 hours/week
w14 = w13 %>% filter(WKHRS != 168)

#Calculate source energy/eui
w15 = w14 %>% 
  #mutate(EUI = round(MFBTU/SQFT*2.80, 2))
  dplyr::mutate(ELBTU0 = ELBTU*2.80) %>%
  dplyr::mutate(NGBTU0 = NGBTU*1.05) %>%
  dplyr::mutate(FKBTU0 = FKBTU*1.01) %>%
  dplyr::mutate(DHBTU0 = DHBTU*1.20) %>%
  dplyr::mutate(SOURCE_ENERGY = rowSums(dplyr::select(., c(ELBTU0,NGBTU0,FKBTU0,DHBTU0)), na.rm = T)) %>% 
  dplyr::mutate(SOURCE_EUI = round(SOURCE_ENERGY/SQFT, 2)) %>%
  dplyr::mutate(SITE_EUI = round(MFBTU/SQFT, 2))

# Must have Source EUI less than or equal to 250 kBtu/ft2
w16 = w15 %>% filter(SOURCE_EUI <= 250)

# Must have Source EUI greater than or equal to 10 kBtu/ft2
w17 = w16 %>% filter(SOURCE_EUI >= 10)

write.csv(w17, paste0(save_dir1, "worship.csv"), row.names = F)


########### make features
worship = read.csv(paste0(save_dir1, "worship.csv"))

data = worship %>%
  mutate(RWSEAT_SQFT = RWSEAT/SQFT * 1000) %>%
  mutate(HDD65_HEATP = HDD65 * HEATP / 100) %>%
  mutate(CDD65_COOLP = CDD65 * COOLP / 100) %>%
  mutate_if(is.numeric, round, 3)

ivars = c(
  "SQFT",
  "WKHRS", "RWSEAT_SQFT",
          "HDD65_HEATP", "CDD65_COOLP")

dvars  = c("SOURCE_EUI", "SOURCE_ENERGY", "FINALWT")

features = data[, c(ivars, dvars)]
summary(features)

features = features %>% na.omit()

write.csv(features, paste0(save_dir2, "worship.csv"), row.names = F)
