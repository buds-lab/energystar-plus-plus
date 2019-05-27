## filter multifamily buildings from FannieMae's database
library(readr)

save_dir1 = './data/filtered/'
dir.create(save_dir1, showWarnings = F)

save_dir2 = './data/features/'
dir.create(save_dir2, showWarnings = F)

fm_data = read_csv("data/FannieMae/mewmr-survey-database_data.csv")
names(fm_data) =make.names(names(fm_data))

# The final regression equation includes the following variables: 
# 
# Number of Units per 1,000 square feet
# Number of Bedrooms per Unit
# Total Heating Degree Days
# Total Cooling Degree Days
# Low-Rise building (yes/no)

vars1 = c( "Unit.Density",
           "Bedrooms.Unit",
           "CDD", 
           "HDD")
  
vars2 = c( "Building.Type..Property.Level.",
           "Source.EUI...using.Total.gross.floor.area.",
           "Total.gross.floor.area",
           "Total.number.of.units")

vars3 = c("Total.SITE.kBtu",
          "Total.SOURCE.kBtu",
          "Site.EUI..using.Total.gross.floor.area.",
          "Source.EUI...using.Total.gross.floor.area.",
          "Survey.Weights.for.ENERGY.STAR.score.analysis")

multifamily = fm_data
m0 = multifamily

m0a = m0 %>% filter(Energy.Space.Served == "Whole property")

m0b = m0a %>% filter(X12.months.of.energy.data.provided == "Y")
m0c = m0b %>% filter(Total.gross.floor.area > 0)
m0d = m0c %>% filter(Total.number.of.units > 0)
m0e = m0d %>% filter(Number.of.BRs > 0)
m0f = m0e %>% filter(Building.Type..Property.Level. != "Not Provided")

m0g = m0f %>% filter(
  Total.SITE.kBtu >= 0 &
    #Total.Cost != "-$9,999.00" &
    Electricity.Site.kBtu >= 0 &
    Natural.Gas.Site.kBtu >= 0 &
    Fuel.Oil.Site.kBtu >= 0 &
    District.Steam.Site.kBtu >= 0 & 
    Other.Site.kBtu >= 0
)

# filters
m1 = m0g %>% filter(Total.number.of.units >= 20)
m2 = m1 %>% filter(Source.EUI...using.Total.gross.floor.area. >= 0 & 
                     Source.EUI...using.Total.gross.floor.area. < 290)

m3 = m2 %>% filter(Total.gross.floor.area <= 2000000)
m4 = m3 %>% filter(Unit.Density < 2.75)
m5 = m4 %>% filter(Bedrooms.Unit > 0.5 & Bedrooms.Unit < 3.5)

m6 = m5 %>% filter(Unit.Density > 0 & 
                     Bedrooms.Unit > 0 & 
                     CDD > 0 & 
                     HDD > 0 & 
                     Survey.Weights.for.ENERGY.STAR.score.analysis > 0)

m7 = m6[, c(vars1, vars2, vars3)]

write.csv(m7, paste0(save_dir1, "multifamily.csv"), row.names = F)


########### make features
save_dir1 = './data/filtered/'
save_dir2 = './data/features/'

multifamily = read.csv(paste0(save_dir1, "multifamily.csv"))

data = multifamily %>% 
  mutate(IsLowRise = 
           ifelse(Building.Type..Property.Level. == "Low-rise", "Yes", "No")) 

ivars = c( "Total.gross.floor.area",
           "Unit.Density",
           "Bedrooms.Unit",
           "CDD", 
           "HDD", 
           "IsLowRise")

dvars = c("Total.SITE.kBtu",
          "Total.SOURCE.kBtu",
          "Site.EUI..using.Total.gross.floor.area.",
          "Source.EUI...using.Total.gross.floor.area.",
          "Survey.Weights.for.ENERGY.STAR.score.analysis")

#dvars = c("SOURCE_EUI", "SOURCE_ENERGY", "FINALWT")

features = data %>% 
  dplyr::select(c(ivars, dvars)) %>%
  dplyr::rename(SQFT = Total.gross.floor.area) %>%
  dplyr::rename(SITE_ENERGY = Total.SITE.kBtu) %>%
  dplyr::rename(SOURCE_ENERGY = Total.SOURCE.kBtu) %>%
  dplyr::rename(SITE_EUI = Site.EUI..using.Total.gross.floor.area.) %>%
  dplyr::rename(SOURCE_EUI = Source.EUI...using.Total.gross.floor.area.) %>%
  dplyr::rename(FINALWT = Survey.Weights.for.ENERGY.STAR.score.analysis) %>%
  dplyr::select(-c(SITE_EUI, SITE_ENERGY))

summary(features)
features = features %>% na.omit()
write.csv(features, paste0(save_dir2, "multifamily.csv"), row.names = F)