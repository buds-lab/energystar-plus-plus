library(e1071)
library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)

cbecs = read_csv("data/2012_public_use_data_aug2016.csv")

cols = c( "PBAPLUS", "PBA", "FINALWT",
          "MFBTU", 
          "ELBTU", "NGBTU", "FKBTU", "DHBTU",
          "ONEACT", "ACT1", "ACT2", "ACT3", "ACT1PCT", "ACT2PCT", "ACT3PCT",
          "PRAMTC", "PRUNIT",
          "CWUSED", "WOUSED", "COUSED", "SOUSED",
          #"NWKER", "RFGWIN", "EDSEAT",
          #"HDD65", "CDD65", "HEATP", "COOLP",
          "SQFT",		"NFLOOR",	"NELVTR",	"NESLTR",	"EDSEAT",	"COURT",
          "MONUSE",	"OPNWE",	"WKHRS",	"NWKER",	"COOK",		"HEATP",
          "COOLP",	"SNACK",	"FASTFD",	"CAF",		"FDPREP",	"KITCHN",
          "BREAKRM",	"OTFDRM",	"LABEQP",	"POOL",		"HTPOOL",	"RFGRES",
          "RFGCOMPN",	"RFGWIN",	"RFGOPN",	"RFGCLN",	"RFGVNN",	"RFGICN",
          "PCTERMN",	"LAPTPN",	"PRNTRN",	"SERVERN",	"TRNGRM",	"STDNRM",
          "WBOARDS",	"TVVIDEON",	"RGSTRN",	"COPIERN",	"HDD65",	"CDD65",
          "PCTRMC",
          "PRUSED", 
          "ZPRUSED", "ZPCTERM",
          "RGSTRN", "RFGEQP",
          "RFGOPN", "RFGCLN")
cbecs1 = cbecs[, cols]
cb = cbecs1


#########################################################################
############################### offices #################################
#########################################################################
offices = cb %>% filter(PBAPLUS %in% c("02", "03", "04", "52"))
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
# TODO
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
  
#Banks must have Source EUI greater than 50 kBtu/ft2
o14 = o13 %>% 
  #mutate(EUI = round(MFBTU/SQFT*2.80, 2))
  mutate(ELBTU0 = ELBTU*2.80) %>%
  mutate(NGBTU0 = NGBTU*1.05) %>%
  mutate(FKBTU0 = FKBTU*1.01) %>%
  mutate(DHBTU0 = DHBTU*1.20) %>%
  mutate(SOURCE_ENERGY = rowSums(select(., c(ELBTU0,NGBTU0,FKBTU0,DHBTU0)), na.rm = T)) %>% 
  mutate(SOURCE_EUI = round(SOURCE_ENERGY/SQFT, 2)) %>%
  mutate(SITE_EUI = round(MFBTU/SQFT, 2))

o15 = o14 %>% 
  filter( PBAPLUS != "03" |  SOURCE_EUI > 50)

o16 = o15 %>% filter( !is.na(COOLP) )

write.csv(o16, "./filtered/office.csv", row.names = F)

#########################################################################



#########################################################################
###################### k-12 schools #####################################
#########################################################################
schools = cb %>% filter(PBAPLUS %in% c("28", "29"))

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
  mutate(ELBTU0 = ELBTU*2.80) %>%
  mutate(NGBTU0 = NGBTU*1.05) %>%
  mutate(FKBTU0 = FKBTU*1.01) %>%
  mutate(DHBTU0 = DHBTU*1.20) %>%
  mutate(SOURCE_ENERGY = rowSums(select(., c(ELBTU0,NGBTU0,FKBTU0,DHBTU0)), na.rm = T)) %>% 
  mutate(SOURCE_EUI = round(SOURCE_ENERGY/SQFT, 2)) %>%
  mutate(SITE_EUI = round(MFBTU/SQFT, 2))

s13 = s12 %>% filter(SOURCE_EUI <= 250)

# Must have no more than 1.9 workers per 1,000 square feet
s14 = s13 %>% filter(NWKER  / SQFT * 1000 <= 1.9)

# Must have no more than 0.06 walk-in refrigeration per 1,000 square feet
s15 = s14 %>% filter(is.na(RFGWIN) | (RFGWIN / SQFT * 1000 <= 0.06)) 

# Must have no more than 17 classroom seats per 1,000 square feet
s16 = s15 %>% filter(EDSEAT / SQFT * 1000 <= 17)

# Must operate no more than 140 hours per week
s17 = s16 %>% filter(WKHRS <= 140)
write.csv(s17, "./filtered/k-12school.csv", row.names = F)






#########################################################################
###################### retail ########################################
#########################################################################
retail = cb %>% filter(PBAPLUS %in% c("14", "42"))
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
  mutate(ELBTU0 = ELBTU*2.80) %>%
  mutate(NGBTU0 = NGBTU*1.05) %>%
  mutate(FKBTU0 = FKBTU*1.01) %>%
  mutate(DHBTU0 = DHBTU*1.20) %>%
  mutate(SOURCE_ENERGY = rowSums(select(., c(ELBTU0,NGBTU0,FKBTU0,DHBTU0)), na.rm = T)) %>% 
  mutate(SOURCE_EUI = round(SOURCE_ENERGY/SQFT, 2)) %>%
  mutate(SITE_EUI = round(MFBTU/SQFT, 2))

# NOTE: filtered out 4, in document 2
r16 = r15 %>% filter( SOURCE_EUI >= 20)

#If CDD is greater than 3,000, must be at least 60% cooled
r17 = r16 %>% filter( CDD65 <= 3000 | COOLP >= 60) 
write.csv(r17, "./filtered/retail.csv", row.names = F)


############################################################

data_center = cb %>% filter(ACT1 == 12 | ACT2 == 12 | ACT3 == 12)

hospital = cb %>% filter(PBAPLUS == 35)

hotel = cb %>% filter(PBAPLUS == 38 | PBAPLUS == 39)

k12school = cb %>% filter(PBAPLUS == 28 | PBAPLUS == 29)

medical_offices = cb %>% filter(PBAPLUS == "08" | PBAPLUS == "05")

#multi_family_housing



dorm = cb %>% filter(PBAPLUS == 37)

retail = cb %>% filter(PBAPLUS == 14 |PBAPLUS == 42) 


#normality test for MFBTU
# http://www.sthda.com/english/wiki/normality-test-in-r
en = retail$MFBTU
log_en = log(en)

hist(en)
hist(log_en)

ggdensity(en, main = "Density plot of total energy", xlab = "Total energy")
ggdensity(log_en, main = "Density plot of total energy", xlab = "Total energy (log)")

ggqqplot(en)
ggqqplot(log_en)

# test for normality (p-value should be greater than 0.05)
shapiro.test(en)
shapiro.test(log_en)




cond = 'PBAPLUS == "02" | PBAPLUS == "03" | PBAPLUS == "04" | PBAPLUS == "52"'
exp = parse(text=cond)

offices = cb %>% filter(eval(exp))
  

offices = cb %>% filter(PBAPLUS == "02" | PBAPLUS == "03" | PBAPLUS == "04" | PBAPLUS == "52")
