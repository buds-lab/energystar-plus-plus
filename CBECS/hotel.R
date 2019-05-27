## filter hotel  buildings

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

var3 = c( "RFGRSN", "RFGCOMPN", "RFGWIN", "RFGOPN", "RFGCLN", "RFGVNN")
var4 = c("LODGRM", "OCCUPYP", "LODOCCP")

hotels = cbecs[, c(var1, var2, var3, var4)]

hotels = hotels %>% filter(PBAPLUS %in% c(38, 39))
h0 = hotels

# Must have at least 1 room
h1 = h0 %>% filter( LODGRM >= 1)

# Must operate for 168 hours per week
h2 = h1 %>% filter(WKHRS == 168)

# Must have at least 1 worker
h3 = h2 %>% filter(NWKER >= 1)

# Must operate for at least 10 months per year
h4 = h3 %>% filter(MONUSE >= 10)

# If the variable ONEACT=1, then one activity occupies 75% or more of the building. 
# If the variable ONEACT=2, then the activities in the building are 
#   defined by ACT1, ACT2, and ACT3. One of these activities must be 
#   coded as Warehouse/Storage (PBAX=22), with a corresponding percent 
#   (ACT1PCT, ACT2PCT, ACT3PCT) that is greater than 50.
# NOTE: filtered - 4, in document 5
h5 = h4 %>% 
  filter( (ONEACT == 1) |
            (ONEACT == 2 & 
               ((ACT1 %in% c(22) & ACT1PCT > 50) | 
                  (ACT2 %in% c(22) & ACT2PCT > 50) | 
                  (ACT3 %in% c(22) & ACT3PCT > 50) )))

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
h6 = h5 %>% filter(!is.na(MFBTU))

# Must be less than or equal to 1,000,000 square feet
h7 = h6 %>% filter(SQFT <= 1000000)

# If propane is used, the amount category (PRAMTC) must equal 1, 2, or 3
h8 = h7 %>% filter(is.na(PRAMTC) | PRAMTC %in% c(1,2,3))

# If propane is used, the unit (PRUNIT) must be known
h9 = h8 %>% filter(is.na(PRUNIT) | PRUNIT %in% c(1,2))

# If propane is used, the maximum estimated propane amount must be 10% or less of the total source energy
# TODO
h10 = h9

#must not use chilled water, wood, coal, or solar
h11 = h10 %>% 
  filter(CWUSED == 2) %>%
  filter(WOUSED == 2) %>% 
  filter(COUSED == 2) %>% 
  filter(SOUSED == 2)

#Must be at least 5,000 square feet
h12 = h11 %>% filter(SQFT >= 5000)

#Calculate source energy/eui
h13 = h12 %>% 
  #mutate(EUI = round(MFBTU/SQFT*2.80, 2))
  dplyr::mutate(ELBTU0 = ELBTU*2.80) %>%
  dplyr::mutate(NGBTU0 = NGBTU*1.05) %>%
  dplyr::mutate(FKBTU0 = FKBTU*1.01) %>%
  dplyr::mutate(DHBTU0 = DHBTU*1.20) %>%
  dplyr::mutate(SOURCE_ENERGY = rowSums(dplyr::select(., c(ELBTU0,NGBTU0,FKBTU0,DHBTU0)), na.rm = T)) %>% 
  dplyr::mutate(SOURCE_EUI = round(SOURCE_ENERGY/SQFT, 2)) %>%
  dplyr::mutate(SITE_EUI = round(MFBTU/SQFT, 2))

# Must have Source EUI less than or equal to 400 kBtu/ft2
h14 = h13 %>% filter(SOURCE_EUI <= 400)

# Must have no more than 4 rooms per 1,000 square feet
h15 = h14 %>% filter(LODGRM/SQFT *1000 <= 4)

# Must have average occupancy greater than 40%
h16 = h15 %>% filter(LODOCCP > 40)

write.csv(h16, paste0(save_dir1, "hotel.csv"), row.names = F)


########### make features
save_dir1 = './data/filtered/'
save_dir2 = './data/features/'

hotel = read.csv(paste0(save_dir1, "hotel.csv"))

data = hotel %>%
  dplyr::mutate(LODGRM_SQFT = LODGRM/SQFT * 1000) %>%
  dplyr::mutate(NWKER_SQFT = NWKER/SQFT * 1000) %>%
  dplyr::mutate(RFG_TOT = rowSums(dplyr::select(., c(RFGRSN,RFGCOMPN,RFGWIN,RFGOPN,RFGCLN,RFGVNN)), na.rm = T)) %>% 
  dplyr::mutate(RFG_SQFT = RFG_TOT/SQFT * 1000) %>%
  dplyr::mutate(HDD65_HEATP = HDD65 * HEATP / 100) %>%
  dplyr::mutate(CDD65_COOLP = CDD65 * COOLP / 100) %>%
  dplyr::mutate(Kitchen = ifelse(FDPREP == 1, "Yes", "No")) %>%
  dplyr::mutate_if(is.numeric, round, 3)

ivars = c( "SQFT",
           "LODGRM_SQFT", "NWKER_SQFT", "RFG_SQFT",
          "HDD65_HEATP", "CDD65_COOLP", "Kitchen")

dvars  = c("SOURCE_EUI", "SOURCE_ENERGY", "FINALWT")

features = data[, c(ivars, dvars)]
summary(features)

features = features %>% na.omit()
write.csv(features, paste0(save_dir2, "hotel.csv"), row.names = F)
