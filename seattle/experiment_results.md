Seattle dataset results
================

``` r
variableImportance <- function(m) {
  vi.int <- varImp(m) %>% 
       mutate(Variable=row.names(.)) %>%
       #mutate(Overall=round(Overall,2)) %>%
       mutate(Percentage = round(Overall*100/sum(Overall), 2)) %>%
       arrange(-Percentage)
  return(vi.int)
}

coefTable <- function(m) {
  intCoeff <- as.data.frame(summary(m)$coefficients) %>%
       mutate(Variable = rownames(.)) %>%
       mutate(pVal = round(`Pr(>|t|)`, 4)) %>%
       arrange(pVal) %>%
       mutate(Significance = 
                case_when(pVal <= 0.001 ~ "0.001", 
                          pVal > 0.001 & pVal <= 0.01 ~ "0.01",
                          pVal > 0.01  & pVal <= 0.05 ~ "0.05",
                          pVal > 0.05  & pVal <= 0.1  ~ "0.1",
                          pVal > 0.1   & pVal <= 0.2  ~  "0.2"))
  return(intCoeff)
}

filterCoefTable <- function(coefTab) {
  coefTab1 = coefTab %>% 
    #filter(!is.na(Significance)) %>%
    filter(Significance <= 0.1) %>%
    dplyr::select(Variable,pVal,Significance)
  return(coefTab1)
}
```

``` r
# Loading all non-interacation models
lm1a = readRDS("SiteEnergyUseLog_Scale_lmFitSW.RDS")
lm1b = readRDS("SiteEnergyUseLog_NoScale_lmFitSW.RDS")
lm1c = readRDS("SiteEnergyUse_NoScale_lmFitSW.RDS")

lm1a.coef = coefTable(lm1a)
lm1b.coef = coefTable(lm1b)
lm1c.coef = coefTable(lm1c)

lm1a.coef1 = filterCoefTable(lm1a.coef)
lm1b.coef1 = filterCoefTable(lm1b.coef)
lm1c.coef1 = filterCoefTable(lm1c.coef)

lm1a.vi = variableImportance(lm1a)
lm1b.vi = variableImportance(lm1b)
lm1c.vi = variableImportance(lm1c)

######## using source eui
lm2a = readRDS("SourceEUILog_Scale_lmFitSW.RDS")
lm2b = readRDS("SourceEUILog_NoScale_lmFitSW.RDS")
lm2c = readRDS("SourceEUI_NoScale_lmFitSW.RDS")

lm2a.coef = coefTable(lm2a)
lm2b.coef = coefTable(lm2b)
lm2c.coef = coefTable(lm2c)

lm2a.coef1 = filterCoefTable(lm2a.coef)
lm2b.coef1 = filterCoefTable(lm2b.coef)
lm2c.coef1 = filterCoefTable(lm2c.coef)

lm2a.vi = variableImportance(lm2a)
lm2b.vi = variableImportance(lm2b)
lm2c.vi = variableImportance(lm2c)
# options(max.print=99)
# coef = m$coefficients[!is.na(m$coefficients) ]
# 
# ss = summary(m)
# View(ss$coefficients)
# print(nrow(ss$coefficients))
#summ(m)
```

``` r
# Loading all interacation models
m1a = readRDS("SiteEnergyUseLog_Scale_IntlmFit.RDS")
m1b = readRDS("SiteEnergyUseLog_NoScale_IntlmFit.RDS")
m1c = readRDS("SiteEnergyUse_NoScale_IntlmFit.RDS")

m1a.coef = coefTable(m1a)
m1b.coef = coefTable(m1b)
m1c.coef = coefTable(m1c)

m1a.coef1 = filterCoefTable(m1a.coef)
m1b.coef1 = filterCoefTable(m1b.coef)
m1c.coef1 = filterCoefTable(m1c.coef)

m1a.vi = variableImportance(m1a)
m1b.vi = variableImportance(m1b)
m1c.vi = variableImportance(m1c)

######## using source eui
m2a = readRDS("SourceEUILog_Scale_IntlmFit.RDS")
m2b = readRDS("SourceEUILog_NoScale_IntlmFit.RDS")
m2c = readRDS("SourceEUI_NoScale_IntlmFit.RDS")

m2a.coef = coefTable(m2a)
m2b.coef = coefTable(m2b)
m2c.coef = coefTable(m2c)

m2a.coef1 = filterCoefTable(m2a.coef)
m2b.coef1 = filterCoefTable(m2b.coef)
m2c.coef1 = filterCoefTable(m2c.coef)

m2a.vi = variableImportance(m2a)
m2b.vi = variableImportance(m2b)
m2c.vi = variableImportance(m2c)
# options(max.print=99)
# coef = m$coefficients[!is.na(m$coefficients) ]
# 
# ss = summary(m)
# View(ss$coefficients)
# print(nrow(ss$coefficients))
#summ(m)
```

Comparision of all models WITHOUT interactions
==============================================

``` r
li = list( 
  "log(SiteEnergyUse) - mean centering" = lm1a,
  "log(SiteEnergyUse) - No mean centering" = lm1b, 
  "SiteEnergyUse - No mean centering" = lm1c,
  
  "log(SourceEUI) - mean centering" = lm2a,
  "log(SourceEUI) - No mean centering" = lm2b, 
  "SourceEUI - No mean centering" = lm2c
)

all.coef = bind_rows(
  lapply(names(li), function(x){
    #print(paste("processing ", x))
    m = li[[x]]
    gl = add_column(round(glance(m),3), .before = 1, model = x)
    coef = coefTable(m)
    coef1 = filterCoefTable(coef)
    df = as.data.frame.matrix(t(table(coef1$Significance)))
    df["Variables"] = coef1$Variable[1]
    cbind(gl,df)
}))

all1 = all.coef[, c(1:3, 13:17)]
knitr::kable(all1)
```

| model                                  |  r.squared|  adj.r.squared|  0.001|  0.01|  0.05|  0.1| Variables        |
|:---------------------------------------|----------:|--------------:|------:|-----:|-----:|----:|:-----------------|
| log(SiteEnergyUse) - mean centering    |      0.574|          0.568|      9|     2|     2|    5| PropertyGFATotal |
| log(SiteEnergyUse) - No mean centering |      0.574|          0.568|     10|     2|     2|    4| (Intercept)      |
| SiteEnergyUse - No mean centering      |      0.564|          0.558|      8|     3|     6|    1| (Intercept)      |
| log(SourceEUI) - mean centering        |      0.273|          0.262|     13|     3|     2|    2| (Intercept)      |
| log(SourceEUI) - No mean centering     |      0.273|          0.262|     13|     3|     2|    2| (Intercept)      |
| SourceEUI - No mean centering          |      0.235|          0.223|     13|     3|     2|    2| (Intercept)      |

Comparision of all models WITH interactions
===========================================

``` r
# ###########
# ########### OLD CODE
# #########
# #make a table of significance for all experiments
# li = list(m1a.coef1, m1b.coef1, m1c.coef1, 
#           m2a.coef1, m2b.coef1, m2c.coef1)
# 
# all.si = bind_rows(
#   lapply(li, function(x){
#     df = as.data.frame.matrix(t(table(x$Significance)))
#     df["Variables"] = x$Variable[1]
#     df
# }))
# 
# all = add_column(round(glance(m1a),3), .before = 1, 
#                  model = "log(SiteEnergyUse) - mean centering")
# all = bind_rows(all, add_column(round(glance(m1b),3), .before = 1,
#                                 model = "log(SiteEnergyUse) - No mean centering"))
# all = bind_rows(all, add_column(round(glance(m1c),3), .before = 1, 
#                                 model = "SiteEnergyUse - No mean centering"))
# 
# all = bind_rows(all, add_column(round(glance(m2a),3), .before = 1, 
#                                 model = "log(SourceEUI) - mean centering"))
# all = bind_rows(all, add_column(round(glance(m2b),3), .before = 1, 
#                                 model = "log(SourceEUI) - No mean centering"))
# all = bind_rows(all, add_column(round(glance(m2c),3), .before = 1, 
#                                 model = "SourceEUI - No mean centering"))
# 
# all = bind_cols(all, all.si)
# 
# all1 = all[, c(1:3, 13:17)]
# 
# #so=summ(fit.model)
# #knitr::kable(glance.summ.lm(so))
# knitr::kable(all1)
```

``` r
li = list( 
  "log(SiteEnergyUse) - mean centering" = m1a,
  "log(SiteEnergyUse) - No mean centering" = m1b, 
  "SiteEnergyUse - No mean centering" = m1c,
  
  "log(SourceEUI) - mean centering" = m2a,
  "log(SourceEUI) - No mean centering" = m2b, 
  "SourceEUI - No mean centering" = m2c
)

all.coef = bind_rows(
  lapply(names(li), function(x){
    #print(paste("processing ", x))
    m = li[[x]]
    gl = add_column(round(glance(m),3), .before = 1, model = x)
    coef = coefTable(m)
    coef1 = filterCoefTable(coef)
    df = as.data.frame.matrix(t(table(coef1$Significance)))
    df["Variables"] = coef1$Variable[1]
    cbind(gl,df)
}))

all1 = all.coef[, c(1:3, 13:17)]
knitr::kable(all1)
```

| model                                  |  r.squared|  adj.r.squared|  0.001|  0.01|  0.05|  0.1| Variables                                                                  |
|:---------------------------------------|----------:|--------------:|------:|-----:|-----:|----:|:---------------------------------------------------------------------------|
| log(SiteEnergyUse) - mean centering    |      0.849|          0.725|      1|     3|     8|   14| ConstructioClassSTRUCTURAL STEEL:EffYear:NumberofFloors                    |
| log(SiteEnergyUse) - No mean centering |      0.849|          0.725|     NA|     2|    12|   11| EffYear:HeatinSystemSPACE HEATERS:SprinklersYes:NumberofFloors             |
| SiteEnergyUse - No mean centering      |      0.916|          0.859|     59|    46|    60|   30| PropertyGFATotal:BuildinQualityGOOD                                        |
| log(SourceEUI) - mean centering        |      0.671|          0.471|     NA|     6|    15|   20| BuildinQualityGOOD:EffYear:HeatinSystemWARMED AND COOLED AIR:SprinklersYes |
| log(SourceEUI) - No mean centering     |      0.671|          0.471|     NA|     4|    12|   26| BuildinQualityGOOD:EffYear:HeatinSystemWARMED AND COOLED AIR:SprinklersYes |
| SourceEUI - No mean centering          |      0.705|          0.526|      8|    10|    30|   33| BuildinQualityGOOD/EXCELLENT:EffYear:HeatinSystemWARMED AND COOLED AIR     |

Experimental results using SiteEnergyUse and interactions
=========================================================

1A. Using log(SiteEnergyUse) and scaling all variables
------------------------------------------------------

``` r
knitr::kable(m1a.coef1)
```

| Variable                                                                                     |    pVal| Significance |
|:---------------------------------------------------------------------------------------------|-------:|:-------------|
| ConstructioClassSTRUCTURAL STEEL:EffYear:NumberofFloors                                      |  0.0005| 0.001        |
| ElevatorsUnknown:HeatinSystemNO HEAT                                                         |  0.0049| 0.01         |
| HeatinSystemNO HEAT:NumberofFloors                                                           |  0.0054| 0.01         |
| PropertyGFATotal:EffYear:HeatinSystemSPACE HEATERS:SprinklersYes:NumberofFloors              |  0.0084| 0.01         |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:HeatinSystemPACKAGE UNIT:NumberofFloors |  0.0159| 0.05         |
| PropertyGFATotal:HeatinSystemNO HEAT:NumberofFloors                                          |  0.0197| 0.05         |
| PropertyGFATotal:EffYear:HeatinSystemHEAT PUMP                                               |  0.0249| 0.05         |
| ConstructioClassPREFAB STEEL:HeatinSystemSPACE HEATERS                                       |  0.0316| 0.05         |
| PropertyGFATotal:ElevatorsUnknown:HeatinSystemNO HEAT                                        |  0.0360| 0.05         |
| PropertyGFATotal:HeatinSystemELECTRIC:NumberofFloors                                         |  0.0426| 0.05         |
| ElevatorsUnknown:HeatinSystemHOT WATER:SprinklersYes                                         |  0.0450| 0.05         |
| EffYear:HeatinSystemNO HEAT:NumberofFloors                                                   |  0.0460| 0.05         |
| ElevatorsUnknown:SprinklersYes                                                               |  0.0537| 0.1          |
| PropertyGFATotal:EffYear:HeatinSystemHEAT PUMP:SprinklersYes                                 |  0.0539| 0.1          |
| PropertyGFATotal:SprinklersYes:NumberofFloors                                                |  0.0547| 0.1          |
| ConstructioClassPREFAB STEEL:SprinklersYes                                                   |  0.0576| 0.1          |
| PropertyGFATotal:EffYear:HeatinSystemELECTRIC:NumberofFloors                                 |  0.0621| 0.1          |
| PropertyGFATotal:ElevatorsUnknown:HeatinSystemFORCED AIR UNIT:SprinklersYes                  |  0.0641| 0.1          |
| PropertyGFATotal:ElevatorsUnknown:HeatinSystemHOT WATER:SprinklersYes                        |  0.0653| 0.1          |
| PropertyGFATotal:HeatinSystemHOT WATER:NumberofFloors                                        |  0.0676| 0.1          |
| PropertyGFATotal:NumberofFloors                                                              |  0.0790| 0.1          |
| PropertyGFATotal:ElevatorsUnknown:HeatinSystemHOT WATER:NumberofFloors                       |  0.0817| 0.1          |
| PropertyGFATotal:ConstructioClassPREFAB STEEL                                                |  0.0855| 0.1          |
| PropertyGFATotal:ElevatorsUnknown:NumberofFloors                                             |  0.0863| 0.1          |
| PropertyGFATotal:ElevatorsUnknown:SprinklersYes                                              |  0.0887| 0.1          |
| PropertyGFATotal:HeatinSystemHOT WATER:SprinklersYes:NumberofFloors                          |  0.0997| 0.1          |

1B. Using log(SiteEnergyUse) and but NOT scaling all variables
--------------------------------------------------------------

``` r
knitr::kable(m1b.coef1)
```

| Variable                                                                                     |    pVal| Significance |
|:---------------------------------------------------------------------------------------------|-------:|:-------------|
| EffYear:HeatinSystemSPACE HEATERS:SprinklersYes:NumberofFloors                               |  0.0071| 0.01         |
| PropertyGFATotal:EffYear:HeatinSystemSPACE HEATERS:SprinklersYes:NumberofFloors              |  0.0084| 0.01         |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:HeatinSystemPACKAGE UNIT:NumberofFloors |  0.0159| 0.05         |
| PropertyGFATotal:HeatinSystemNO HEAT:NumberofFloors                                          |  0.0197| 0.05         |
| EffYear:HeatinSystemSPACE HEATERS:SprinklersYes                                              |  0.0244| 0.05         |
| EffYear:HeatinSystemSPACE HEATERS                                                            |  0.0286| 0.05         |
| ConstructioClassPREFAB STEEL:HeatinSystemSPACE HEATERS                                       |  0.0316| 0.05         |
| PropertyGFATotal:ElevatorsUnknown:HeatinSystemNO HEAT                                        |  0.0360| 0.05         |
| ConstructioClassSTRUCTURAL STEEL:NumberofFloors                                              |  0.0381| 0.05         |
| PropertyGFATotal:EffYear:HeatinSystemELECTRIC WALL                                           |  0.0411| 0.05         |
| HeatinSystemSPACE HEATERS:SprinklersYes:NumberofFloors                                       |  0.0441| 0.05         |
| HeatinSystemSTEAM                                                                            |  0.0447| 0.05         |
| EffYear:HeatinSystemNO HEAT:NumberofFloors                                                   |  0.0460| 0.05         |
| PropertyGFATotal:EffYear:HeatinSystemSTEAM                                                   |  0.0493| 0.05         |
| PropertyGFATotal:EffYear:HeatinSystemHEAT PUMP:SprinklersYes                                 |  0.0539| 0.1          |
| PropertyGFATotal:EffYear:HeatinSystemSPACE HEATERS:SprinklersYes                             |  0.0575| 0.1          |
| ConstructioClassPREFAB STEEL:SprinklersYes                                                   |  0.0576| 0.1          |
| PropertyGFATotal:EffYear:HeatinSystemELECTRIC:NumberofFloors                                 |  0.0621| 0.1          |
| PropertyGFATotal:ElevatorsUnknown:HeatinSystemHOT WATER:SprinklersYes                        |  0.0653| 0.1          |
| PropertyGFATotal:HeatinSystemSPACE HEATERS:SprinklersYes:NumberofFloors                      |  0.0690| 0.1          |
| PropertyGFATotal:HeatinSystemSTEAM                                                           |  0.0741| 0.1          |
| EffYear:HeatinSystemNO HEAT                                                                  |  0.0770| 0.1          |
| PropertyGFATotal:EffYear:HeatinSystemSPACE HEATERS                                           |  0.0810| 0.1          |
| PropertyGFATotal:ConstructioClassPREFAB STEEL                                                |  0.0855| 0.1          |
| ElevatorsUnknown:HeatinSystemHEAT PUMP:NumberofFloors                                        |  0.0914| 0.1          |

1C. Using SiteEnergyUse and NOT scaling all variables
-----------------------------------------------------

``` r
knitr::kable(m1c.coef1)
```

| Variable                                                                                                             |    pVal| Significance |
|:---------------------------------------------------------------------------------------------------------------------|-------:|:-------------|
| PropertyGFATotal:BuildinQualityGOOD                                                                                  |  0.0000| 0.001        |
| PropertyGFATotal:YearBuilt                                                                                           |  0.0000| 0.001        |
| PropertyGFATotal:HeatinSystemFORCED AIR UNIT                                                                         |  0.0000| 0.001        |
| PropertyGFATotal:HeatinSystemHOT WATER                                                                               |  0.0000| 0.001        |
| PropertyGFATotal:HeatinSystemWARMED AND COOLED AIR                                                                   |  0.0000| 0.001        |
| BuildinQualityGOOD/EXCELLENT:HeatinSystemSTEAM                                                                       |  0.0000| 0.001        |
| PropertyGFATotal:BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE                                              |  0.0000| 0.001        |
| PropertyGFATotal:BuildinQualityGOOD:YearBuilt                                                                        |  0.0000| 0.001        |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:YearBuilt                                                       |  0.0000| 0.001        |
| PropertyGFATotal:BuildinQualityGOOD/EXCELLENT:HeatinSystemWARMED AND COOLED AIR                                      |  0.0000| 0.001        |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:HeatinSystemWARMED AND COOLED AIR                               |  0.0000| 0.001        |
| PropertyGFATotal:YearBuilt:HeatinSystemFORCED AIR UNIT                                                               |  0.0000| 0.001        |
| PropertyGFATotal:YearBuilt:HeatinSystemHOT WATER                                                                     |  0.0000| 0.001        |
| PropertyGFATotal:YearBuilt:HeatinSystemSPACE HEATERS                                                                 |  0.0000| 0.001        |
| PropertyGFATotal:YearBuilt:HeatinSystemWARMED AND COOLED AIR                                                         |  0.0000| 0.001        |
| BuildinQualityGOOD/EXCELLENT:YearBuilt:HeatinSystemWARMED AND COOLED AIR                                             |  0.0000| 0.001        |
| PropertyGFATotal:BuildinQualityEXCELLENT:NumberofFloors                                                              |  0.0000| 0.001        |
| PropertyGFATotal:YearBuilt:NumberofFloors                                                                            |  0.0000| 0.001        |
| PropertyGFATotal:HeatinSystemWARMED AND COOLED AIR:NumberofFloors                                                    |  0.0000| 0.001        |
| BuildinQualityGOOD:HeatinSystemHEAT PUMP:NumberofFloors                                                              |  0.0000| 0.001        |
| PropertyGFATotal:BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:YearBuilt                                    |  0.0000| 0.001        |
| PropertyGFATotal:BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:HeatinSystemWARMED AND COOLED AIR            |  0.0000| 0.001        |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:YearBuilt:HeatinSystemWARMED AND COOLED AIR                     |  0.0000| 0.001        |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:YearBuilt:NumberofFloors                                        |  0.0000| 0.001        |
| BuildinQualityGOOD/EXCELLENT:ConstructioClassREINFORCED CONCRETE:YearBuilt:NumberofFloors                            |  0.0000| 0.001        |
| PropertyGFATotal:BuildinQualityGOOD:HeatinSystemHEAT PUMP:NumberofFloors                                             |  0.0000| 0.001        |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:HeatinSystemWARMED AND COOLED AIR:NumberofFloors                |  0.0000| 0.001        |
| BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:HeatinSystemHEAT PUMP:NumberofFloors                          |  0.0000| 0.001        |
| PropertyGFATotal:YearBuilt:HeatinSystemFORCED AIR UNIT:NumberofFloors                                                |  0.0000| 0.001        |
| PropertyGFATotal:YearBuilt:HeatinSystemHOT WATER:NumberofFloors                                                      |  0.0000| 0.001        |
| PropertyGFATotal:YearBuilt:HeatinSystemWARMED AND COOLED AIR:NumberofFloors                                          |  0.0000| 0.001        |
| BuildinQualityGOOD:YearBuilt:HeatinSystemHEAT PUMP:NumberofFloors                                                    |  0.0000| 0.001        |
| BuildinQualityGOOD:YearBuilt:HeatinSystemWARMED AND COOLED AIR:NumberofFloors                                        |  0.0000| 0.001        |
| PropertyGFATotal:BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:YearBuilt:HeatinSystemWARMED AND COOLED AIR  |  0.0000| 0.001        |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:YearBuilt:HeatinSystemWARMED AND COOLED AIR:NumberofFloors      |  0.0000| 0.001        |
| YearBuilt                                                                                                            |  0.0001| 0.001        |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE                                                                 |  0.0001| 0.001        |
| YearBuilt:HeatinSystemHOT WATER                                                                                      |  0.0001| 0.001        |
| YearBuilt:HeatinSystemSPACE HEATERS                                                                                  |  0.0001| 0.001        |
| BuildinQualityAVERAGE/GOOD:YearBuilt:HeatinSystemHOT WATER                                                           |  0.0001| 0.001        |
| ConstructioClassSTRUCTURAL STEEL:YearBuilt:NumberofFloors                                                            |  0.0001| 0.001        |
| PropertyGFATotal:BuildinQualityGOOD:YearBuilt:HeatinSystemHEAT PUMP                                                  |  0.0001| 0.001        |
| PropertyGFATotal:YearBuilt:HeatinSystemSPACE HEATERS:NumberofFloors                                                  |  0.0001| 0.001        |
| ConstructioClassREINFORCED CONCRETE:YearBuilt                                                                        |  0.0002| 0.001        |
| PropertyGFATotal:HeatinSystemSPACE HEATERS                                                                           |  0.0002| 0.001        |
| YearBuilt:HeatinSystemFORCED AIR UNIT                                                                                |  0.0002| 0.001        |
| BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:HeatinSystemHEAT PUMP                                         |  0.0002| 0.001        |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:NumberofFloors                                                  |  0.0002| 0.001        |
| PropertyGFATotal:BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:HeatinSystemHEAT PUMP                        |  0.0002| 0.001        |
| PropertyGFATotal:BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:YearBuilt:HeatinSystemHEAT PUMP              |  0.0002| 0.001        |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD:YearBuilt:HeatinSystemHOT WATER                                          |  0.0003| 0.001        |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD:ConstructioClassREINFORCED CONCRETE                                      |  0.0005| 0.001        |
| PropertyGFATotal:HeatinSystemHOT WATER:NumberofFloors                                                                |  0.0005| 0.001        |
| PropertyGFATotal:NumberofFloors                                                                                      |  0.0006| 0.001        |
| PropertyGFATotal                                                                                                     |  0.0008| 0.001        |
| PropertyGFATotal:HeatinSystemHEAT PUMP                                                                               |  0.0008| 0.001        |
| BuildinQualityGOOD/EXCELLENT:HeatinSystemFORCED AIR UNIT                                                             |  0.0008| 0.001        |
| PropertyGFATotal:HeatinSystemSPACE HEATERS:NumberofFloors                                                            |  0.0009| 0.001        |
| PropertyGFATotal:HeatinSystemFORCED AIR UNIT:NumberofFloors                                                          |  0.0010| 0.001        |
| YearBuilt:HeatinSystemWARMED AND COOLED AIR                                                                          |  0.0014| 0.01         |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:HeatinSystemHEAT PUMP                                           |  0.0014| 0.01         |
| PropertyGFATotal:BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:NumberofFloors                               |  0.0014| 0.01         |
| HeatinSystemHOT WATER                                                                                                |  0.0015| 0.01         |
| BuildinQualityAVERAGE/GOOD:ConstructioClassREINFORCED CONCRETE:YearBuilt:HeatinSystemHOT WATER                       |  0.0017| 0.01         |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD:ConstructioClassREINFORCED CONCRETE:YearBuilt:HeatinSystemHOT WATER      |  0.0017| 0.01         |
| BuildinQualityAVERAGE/GOOD:ConstructioClassREINFORCED CONCRETE:HeatinSystemHOT WATER                                 |  0.0018| 0.01         |
| PropertyGFATotal:YearBuilt:HeatinSystemHEAT PUMP                                                                     |  0.0019| 0.01         |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD:ConstructioClassREINFORCED CONCRETE:HeatinSystemHOT WATER                |  0.0020| 0.01         |
| HeatinSystemFORCED AIR UNIT                                                                                          |  0.0021| 0.01         |
| PropertyGFATotal:BuildinQualityGOOD/EXCELLENT:HeatinSystemFORCED AIR UNIT                                            |  0.0021| 0.01         |
| (Intercept)                                                                                                          |  0.0022| 0.01         |
| PropertyGFATotal:BuildinQualityEXCELLENT:YearBuilt                                                                   |  0.0022| 0.01         |
| PropertyGFATotal:BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:HeatinSystemSTEAM WITHOUT BOILER             |  0.0024| 0.01         |
| HeatinSystemSPACE HEATERS                                                                                            |  0.0029| 0.01         |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD                                                                          |  0.0032| 0.01         |
| YearBuilt:NumberofFloors                                                                                             |  0.0034| 0.01         |
| BuildinQualityAVERAGE/GOOD:ConstructioClassREINFORCED CONCRETE:HeatinSystemWARMED AND COOLED AIR:NumberofFloors      |  0.0035| 0.01         |
| ConstructioClassREINFORCED CONCRETE:HeatinSystemWARMED AND COOLED AIR:NumberofFloors                                 |  0.0036| 0.01         |
| HeatinSystemWARMED AND COOLED AIR:NumberofFloors                                                                     |  0.0037| 0.01         |
| BuildinQualityGOOD/EXCELLENT:YearBuilt:NumberofFloors                                                                |  0.0037| 0.01         |
| YearBuilt:HeatinSystemHOT WATER:NumberofFloors                                                                       |  0.0037| 0.01         |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:YearBuilt:HeatinSystemHOT WATER:NumberofFloors                  |  0.0037| 0.01         |
| PropertyGFATotal:BuildinQualityGOOD:NumberofFloors                                                                   |  0.0038| 0.01         |
| ConstructioClassREINFORCED CONCRETE:YearBuilt:HeatinSystemWARMED AND COOLED AIR                                      |  0.0041| 0.01         |
| ConstructioClassREINFORCED CONCRETE                                                                                  |  0.0048| 0.01         |
| ConstructioClassREINFORCED CONCRETE:YearBuilt:NumberofFloors                                                         |  0.0050| 0.01         |
| HeatinSystemWARMED AND COOLED AIR                                                                                    |  0.0051| 0.01         |
| PropertyGFATotal:BuildinQualityGOOD:HeatinSystemPACKAGE UNIT                                                         |  0.0054| 0.01         |
| ConstructioClassREINFORCED CONCRETE:HeatinSystemWARMED AND COOLED AIR                                                |  0.0055| 0.01         |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:YearBuilt:HeatinSystemHOT WATER                                 |  0.0055| 0.01         |
| BuildinQualityAVERAGE/GOOD:YearBuilt:HeatinSystemHOT WATER:NumberofFloors                                            |  0.0055| 0.01         |
| YearBuilt:HeatinSystemWARMED AND COOLED AIR:NumberofFloors                                                           |  0.0056| 0.01         |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD:ConstructioClassREINFORCED CONCRETE:HeatinSystemHOT WATER:NumberofFloors |  0.0056| 0.01         |
| YearBuilt:HeatinSystemFORCED AIR UNIT:NumberofFloors                                                                 |  0.0070| 0.01         |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:HeatinSystemHEAT PUMP:NumberofFloors                            |  0.0072| 0.01         |
| ConstructioClassREINFORCED CONCRETE:NumberofFloors                                                                   |  0.0075| 0.01         |
| HeatinSystemHOT WATER:NumberofFloors                                                                                 |  0.0079| 0.01         |
| BuildinQualityGOOD:ConstructioClassSTRUCTURAL STEEL:YearBuilt:HeatinSystemWARMED AND COOLED AIR                      |  0.0080| 0.01         |
| BuildinQualityGOOD/EXCELLENT:HeatinSystemWARMED AND COOLED AIR:NumberofFloors                                        |  0.0082| 0.01         |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:YearBuilt:HeatinSystemHEAT PUMP                                 |  0.0082| 0.01         |
| BuildinQualityAVERAGE/GOOD:ConstructioClassREINFORCED CONCRETE:YearBuilt:HeatinSystemWARMED AND COOLED AIR           |  0.0088| 0.01         |
| NumberofFloors                                                                                                       |  0.0091| 0.01         |
| YearBuilt:HeatinSystemSPACE HEATERS:NumberofFloors                                                                   |  0.0093| 0.01         |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD:ConstructioClassREINFORCED CONCRETE:NumberofFloors                       |  0.0095| 0.01         |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:YearBuilt:HeatinSystemELECTRIC WALL                             |  0.0096| 0.01         |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD:ConstructioClassWOOD FRAME:NumberofFloors                                |  0.0101| 0.05         |
| BuildinQualityAVERAGE/GOOD:HeatinSystemWARMED AND COOLED AIR:NumberofFloors                                          |  0.0113| 0.05         |
| BuildinQualityAVERAGE/GOOD:ConstructioClassREINFORCED CONCRETE:HeatinSystemHOT WATER:NumberofFloors                  |  0.0115| 0.05         |
| PropertyGFATotal:YearBuilt:HeatinSystemELECTRIC WALL                                                                 |  0.0116| 0.05         |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD:NumberofFloors                                                           |  0.0119| 0.05         |
| PropertyGFATotal:BuildinQualityGOOD:HeatinSystemSTEAM WITHOUT BOILER                                                 |  0.0122| 0.05         |
| BuildinQualityAVERAGE/GOOD:ConstructioClassWOOD FRAME:HeatinSystemSPACE HEATERS                                      |  0.0134| 0.05         |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD:YearBuilt:HeatinSystemHOT WATER:NumberofFloors                           |  0.0149| 0.05         |
| BuildinQualityAVERAGE/GOOD:ConstructioClassSTRUCTURAL STEEL                                                          |  0.0153| 0.05         |
| BuildinQualityEXCELLENT:ConstructioClassWOOD FRAME                                                                   |  0.0157| 0.05         |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD:ConstructioClassSTRUCTURAL STEEL:HeatinSystemHEAT PUMP                   |  0.0157| 0.05         |
| ConstructioClassREINFORCED CONCRETE:HeatinSystemHEAT PUMP                                                            |  0.0158| 0.05         |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:HeatinSystemELECTRIC WALL:NumberofFloors                        |  0.0162| 0.05         |
| HeatinSystemFORCED AIR UNIT:NumberofFloors                                                                           |  0.0163| 0.05         |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD:ConstructioClassWOOD FRAME                                               |  0.0171| 0.05         |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:HeatinSystemFORCED AIR UNIT                                     |  0.0175| 0.05         |
| ConstructioClassSTRUCTURAL STEEL:HeatinSystemHEAT PUMP                                                               |  0.0183| 0.05         |
| ConstructioClassREINFORCED CONCRETE:YearBuilt:HeatinSystemWARMED AND COOLED AIR:NumberofFloors                       |  0.0183| 0.05         |
| HeatinSystemSPACE HEATERS:NumberofFloors                                                                             |  0.0184| 0.05         |
| BuildinQualityAVERAGE/GOOD:YearBuilt:HeatinSystemSPACE HEATERS                                                       |  0.0194| 0.05         |
| PropertyGFATotal:YearBuilt:HeatinSystemELECTRIC WALL:NumberofFloors                                                  |  0.0203| 0.05         |
| BuildinQualityEXCELLENT:YearBuilt                                                                                    |  0.0209| 0.05         |
| BuildinQualityEXCELLENT                                                                                              |  0.0211| 0.05         |
| BuildinQualityEXCELLENT:HeatinSystemSTEAM                                                                            |  0.0214| 0.05         |
| BuildinQualityAVERAGE/GOOD:ConstructioClassPREFAB STEEL                                                              |  0.0216| 0.05         |
| BuildinQualityEXCELLENT:ConstructioClassSTRUCTURAL STEEL                                                             |  0.0218| 0.05         |
| ConstructioClassPREFAB STEEL                                                                                         |  0.0220| 0.05         |
| BuildinQualityEXCELLENT:ConstructioClassREINFORCED CONCRETE                                                          |  0.0225| 0.05         |
| BuildinQualityEXCELLENT:ConstructioClassSTRUCTURAL STEEL:YearBuilt                                                   |  0.0244| 0.05         |
| BuildinQualityAVERAGE/GOOD:ConstructioClassREINFORCED CONCRETE:NumberofFloors                                        |  0.0244| 0.05         |
| PropertyGFATotal:ConstructioClassSTRUCTURAL STEEL:HeatinSystemHEAT PUMP                                              |  0.0249| 0.05         |
| PropertyGFATotal:HeatinSystemHEAT PUMP:NumberofFloors                                                                |  0.0250| 0.05         |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:HeatinSystemHOT WATER:NumberofFloors                            |  0.0250| 0.05         |
| PropertyGFATotal:HeatinSystemELECTRIC WALL:NumberofFloors                                                            |  0.0256| 0.05         |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:YearBuilt:HeatinSystemELECTRIC WALL:NumberofFloors              |  0.0262| 0.05         |
| PropertyGFATotal:BuildinQualityGOOD/EXCELLENT:ConstructioClassREINFORCED CONCRETE:NumberofFloors                     |  0.0264| 0.05         |
| ConstructioClassREINFORCED CONCRETE:HeatinSystemHEAT PUMP:NumberofFloors                                             |  0.0265| 0.05         |
| BuildinQualityEXCELLENT:ConstructioClassREINFORCED CONCRETE:YearBuilt                                                |  0.0266| 0.05         |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD:ConstructioClassSTRUCTURAL STEEL:YearBuilt                               |  0.0267| 0.05         |
| BuildinQualityAVERAGE/GOOD:NumberofFloors                                                                            |  0.0279| 0.05         |
| BuildinQualityAVERAGE/GOOD:ConstructioClassSTRUCTURAL STEEL:HeatinSystemHEAT PUMP                                    |  0.0299| 0.05         |
| PropertyGFATotal:ConstructioClassWOOD FRAME:NumberofFloors                                                           |  0.0301| 0.05         |
| BuildinQualityAVERAGE/GOOD:ConstructioClassWOOD FRAME:NumberofFloors                                                 |  0.0304| 0.05         |
| BuildinQualityGOOD/EXCELLENT:HeatinSystemHOT WATER                                                                   |  0.0310| 0.05         |
| PropertyGFATotal:BuildinQualityGOOD/EXCELLENT:NumberofFloors                                                         |  0.0321| 0.05         |
| ConstructioClassPREFAB STEEL:HeatinSystemSPACE HEATERS                                                               |  0.0326| 0.05         |
| BuildinQualityEXCELLENT:HeatinSystemHEAT PUMP                                                                        |  0.0328| 0.05         |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD:HeatinSystemSPACE HEATERS                                                |  0.0335| 0.05         |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:HeatinSystemELECTRIC WALL                                       |  0.0341| 0.05         |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD:ConstructioClassSTRUCTURAL STEEL:NumberofFloors                          |  0.0353| 0.05         |
| HeatinSystemHEAT PUMP                                                                                                |  0.0369| 0.05         |
| PropertyGFATotal:BuildinQualityEXCELLENT                                                                             |  0.0375| 0.05         |
| BuildinQualityAVERAGE/GOOD:YearBuilt                                                                                 |  0.0382| 0.05         |
| YearBuilt:HeatinSystemELECTRIC WALL                                                                                  |  0.0402| 0.05         |
| BuildinQualityAVERAGE/GOOD:ConstructioClassWOOD FRAME:HeatinSystemWARMED AND COOLED AIR                              |  0.0418| 0.05         |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD:ConstructioClassREINFORCED CONCRETE:YearBuilt:NumberofFloors             |  0.0432| 0.05         |
| YearBuilt:HeatinSystemHEAT PUMP                                                                                      |  0.0437| 0.05         |
| ConstructioClassPREFAB STEEL:HeatinSystemELECTRIC WALL                                                               |  0.0445| 0.05         |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:YearBuilt:HeatinSystemHEAT PUMP:NumberofFloors                  |  0.0467| 0.05         |
| BuildinQualityAVERAGE/GOOD:ConstructioClassREINFORCED CONCRETE                                                       |  0.0469| 0.05         |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD:HeatinSystemHOT WATER                                                    |  0.0523| 0.1          |
| ConstructioClassPREFAB STEEL:HeatinSystemHOT WATER-RADIANT                                                           |  0.0531| 0.1          |
| ConstructioClassREINFORCED CONCRETE:YearBuilt:HeatinSystemHOT WATER                                                  |  0.0547| 0.1          |
| BuildinQualityGOOD:HeatinSystemWARMED AND COOLED AIR                                                                 |  0.0552| 0.1          |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD:ConstructioClassREINFORCED CONCRETE:HeatinSystemWARMED AND COOLED AIR    |  0.0562| 0.1          |
| BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:HeatinSystemSTEAM WITHOUT BOILER                              |  0.0574| 0.1          |
| BuildinQualityAVERAGE/GOOD:HeatinSystemSPACE HEATERS                                                                 |  0.0581| 0.1          |
| PropertyGFATotal:ConstructioClassWOOD FRAME:HeatinSystemELECTRIC WALL:NumberofFloors                                 |  0.0602| 0.1          |
| BuildinQualityAVERAGE/GOOD:ConstructioClassREINFORCED CONCRETE:HeatinSystemFORCED AIR UNIT                           |  0.0606| 0.1          |
| HeatinSystemELECTRIC WALL:NumberofFloors                                                                             |  0.0644| 0.1          |
| ConstructioClassREINFORCED CONCRETE:YearBuilt:HeatinSystemHOT WATER:NumberofFloors                                   |  0.0647| 0.1          |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD:ConstructioClassWOOD FRAME:HeatinSystemFORCED AIR UNIT                   |  0.0662| 0.1          |
| BuildinQualityAVERAGE/GOOD:HeatinSystemHOT WATER                                                                     |  0.0673| 0.1          |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD:YearBuilt:NumberofFloors                                                 |  0.0681| 0.1          |
| YearBuilt:HeatinSystemELECTRIC WALL:NumberofFloors                                                                   |  0.0703| 0.1          |
| BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:HeatinSystemHOT WATER:NumberofFloors                          |  0.0705| 0.1          |
| PropertyGFATotal:YearBuilt:HeatinSystemHEAT PUMP:NumberofFloors                                                      |  0.0708| 0.1          |
| ConstructioClassREINFORCED CONCRETE:HeatinSystemHOT WATER:NumberofFloors                                             |  0.0716| 0.1          |
| PropertyGFATotal:HeatinSystemELECTRIC WALL                                                                           |  0.0720| 0.1          |
| PropertyGFATotal:ConstructioClassREINFORCED CONCRETE:YearBuilt:HeatinSystemFORCED AIR UNIT                           |  0.0774| 0.1          |
| ConstructioClassREINFORCED CONCRETE:YearBuilt:HeatinSystemHEAT PUMP                                                  |  0.0793| 0.1          |
| PropertyGFATotal:ConstructioClassSTRUCTURAL STEEL:YearBuilt:HeatinSystemWARMED AND COOLED AIR                        |  0.0849| 0.1          |
| ConstructioClassWOOD FRAME:NumberofFloors                                                                            |  0.0851| 0.1          |
| PropertyGFATotal:BuildinQualityAVERAGE/GOOD:HeatinSystemSTEAM WITHOUT BOILER                                         |  0.0858| 0.1          |
| ConstructioClassREINFORCED CONCRETE:YearBuilt:HeatinSystemSPACE HEATERS:NumberofFloors                               |  0.0862| 0.1          |
| PropertyGFATotal:BuildinQualityGOOD:HeatinSystemSTEAM                                                                |  0.0912| 0.1          |
| PropertyGFATotal:BuildinQualityGOOD/EXCELLENT:YearBuilt                                                              |  0.0920| 0.1          |
| HeatinSystemELECTRIC WALL                                                                                            |  0.0925| 0.1          |
| BuildinQualityAVERAGE/GOOD:ConstructioClassWOOD FRAME:HeatinSystemFORCED AIR UNIT:NumberofFloors                     |  0.0944| 0.1          |
| BuildinQualityAVERAGE/GOOD:YearBuilt:HeatinSystemWARMED AND COOLED AIR:NumberofFloors                                |  0.0989| 0.1          |

``` r
library(DMwR)  ## for unscale
unscale_data <- function(df) {
  for(col in names(df)) {
    if( is.numeric(df[, col]) ){
      # https://stackoverflow.com/questions/10287545/backtransform-scale-for-plotting
      # https://rdrr.io/cran/DMwR/man/unscale.html
      #df[, col] = scale(df[, col], center = T, scale = T)
      df[, col] = unscale(df[, col], df[, col])
    }
  }
  return (df)
}
```

Ranking using EER calculation - using model 1A. log(SiteEnergyUse) and scaling all variables
--------------------------------------------------------------------------------------------

``` r
step.model = m1a
dat0 = step.model$model
pre0 = predict(step.model, dat0)
```

    ## Warning in predict.lm(step.model, dat0): prediction from a rank-deficient
    ## fit may be misleading

``` r
pre = unscale(pre0, dat0$SiteEnergyUseLog)

dat = unscale_data(dat0)

eer = dat$SiteEnergyUseLog / pre
dat["eer"] = round(eer,2)

eer_sorted = sort(eer)
     #plot(eer_sorted, xlab = "Building Id", ylab = "Energy Efficiency Ratio")
     
     # Plot cumaltive percentage for energy efficiency ratio
eer_cs = cumsum(eer_sorted)
eer_pr = cumsum(eer_sorted) / sum(eer_sorted)

summary(eer_sorted)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.8292  0.9899  1.0000  1.0000  1.0081  1.1609

``` r
hist(eer_sorted)
```

![](experiment_results_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
#plot(eer_sorted)

plot(eer_sorted,eer_pr, main = "Cumalativer percentile of EER",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     #cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0,
     xlab = "Energy Efficiency Ratio", 
     ylab = "Cumalative percentile")


abline(h = 0.8, col = "red", lwd = 2)
abline(v = 1.6, col = "blue", lwd = 2)
```

![](experiment_results_files/figure-markdown_github/unnamed-chunk-11-2.png)

Ranking using EER and clustering - using model 1A. log(SiteEnergyUse) and scaling all variables
-----------------------------------------------------------------------------------------------

``` r
library(Ckmeans.1d.dp)
```

    ## Warning: package 'Ckmeans.1d.dp' was built under R version 3.5.1

``` r
k = 5
result <- Ckmeans.1d.dp(eer_sorted, k, y=eer_sorted)
plot(result)
```

![](experiment_results_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
result <- Ckmeans.1d.dp(eer_sorted, y=eer_sorted)
```

    ## Warning in cluster.1d.dp(x, k, y, method, estimate.k, "L2", deparse(substitute(x)), : Max number of clusters used. Consider increasing k!

``` r
plot(result)
```

![](experiment_results_files/figure-markdown_github/unnamed-chunk-12-2.png)

``` r
k = 5
result <- Ckmedian.1d.dp(eer_sorted, k)
plot(result)
```

![](experiment_results_files/figure-markdown_github/unnamed-chunk-12-3.png)

``` r
result <- Ckmedian.1d.dp(eer_sorted)
```

    ## Warning in cluster.1d.dp(x, k, y = 1, method, estimate.k, "L1", deparse(substitute(x)), : Max number of clusters used. Consider increasing k!

``` r
plot(result)
```

![](experiment_results_files/figure-markdown_github/unnamed-chunk-12-4.png)

Experimental results using SourceEUI and interactions
=====================================================

2A. Using log(SiteEnergyUse) and scaling all variables
------------------------------------------------------

``` r
knitr::kable(m2a.coef1)
```

| Variable                                                                                            |    pVal| Significance |
|:----------------------------------------------------------------------------------------------------|-------:|:-------------|
| BuildinQualityGOOD:EffYear:HeatinSystemWARMED AND COOLED AIR:SprinklersYes                          |  0.0016| 0.01         |
| ConstructioClassREINFORCED CONCRETE:HeatinSystemWARMED AND COOLED AIR:NumberofFloors                |  0.0023| 0.01         |
| BuildinQualityGOOD:EffYear:HeatinSystemWARMED AND COOLED AIR                                        |  0.0028| 0.01         |
| BuildinQualityAVERAGE/GOOD:EffYear:HeatinSystemHOT WATER                                            |  0.0038| 0.01         |
| SprinklersUnknown                                                                                   |  0.0046| 0.01         |
| BuildinQualityGOOD/EXCELLENT:EffYear:HeatinSystemWARMED AND COOLED AIR                              |  0.0054| 0.01         |
| BuildinQualityAVERAGE/GOOD:EffYear                                                                  |  0.0133| 0.05         |
| BuildinQualityLOW COST:ConstructioClassPREFAB STEEL                                                 |  0.0134| 0.05         |
| EffYear:SprinklersUnknown                                                                           |  0.0190| 0.05         |
| ConstructioClassWOOD FRAME:SprinklersUnknown:NumberofFloors                                         |  0.0195| 0.05         |
| BuildinQualityLOW COST:ConstructioClassWOOD FRAME:HeatinSystemELECTRIC WALL                         |  0.0228| 0.05         |
| ConstructioClassWOOD FRAME:SprinklersUnknown                                                        |  0.0232| 0.05         |
| ConstructioClassREINFORCED CONCRETE:EffYear:HeatinSystemWARMED AND COOLED AIR                       |  0.0233| 0.05         |
| SprinklersUnknown:NumberofFloors                                                                    |  0.0297| 0.05         |
| BuildinQualityLOW COST:ConstructioClassWOOD FRAME                                                   |  0.0302| 0.05         |
| ConstructioClassWOOD FRAME:EffYear:HeatinSystemFORCED AIR UNIT:SprinklersYes:NumberofFloors         |  0.0308| 0.05         |
| BuildinQualityLOW COST:EffYear:HeatinSystemHOT WATER                                                |  0.0327| 0.05         |
| ConstructioClassWOOD FRAME:EffYear:HeatinSystemFORCED AIR UNIT:SprinklersYes                        |  0.0346| 0.05         |
| BuildinQualityAVERAGE/GOOD:ConstructioClassREINFORCED CONCRETE:HeatinSystemHEAT PUMP:NumberofFloors |  0.0418| 0.05         |
| ConstructioClassREINFORCED CONCRETE:EffYear:HeatinSystemFORCED AIR UNIT                             |  0.0441| 0.05         |
| BuildinQualityGOOD:ConstructioClassWOOD FRAME:EffYear:HeatinSystemFORCED AIR UNIT:NumberofFloors    |  0.0458| 0.05         |
| BuildinQualityGOOD:HeatinSystemWARMED AND COOLED AIR                                                |  0.0581| 0.1          |
| BuildinQualityGOOD:EffYear:HeatinSystemELECTRIC:NumberofFloors                                      |  0.0641| 0.1          |
| BuildinQualityLOW COST:ConstructioClassWOOD FRAME:EffYear                                           |  0.0646| 0.1          |
| ConstructioClassREINFORCED CONCRETE:EffYear:HeatinSystemSTEAM                                       |  0.0678| 0.1          |
| BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:HeatinSystemELECTRIC                         |  0.0689| 0.1          |
| BuildinQualityLOW COST:EffYear:SprinklersYes                                                        |  0.0717| 0.1          |
| BuildinQualityGOOD:ConstructioClassWOOD FRAME:HeatinSystemSPACE HEATERS                             |  0.0733| 0.1          |
| BuildinQualityGOOD/EXCELLENT:HeatinSystemWARMED AND COOLED AIR:NumberofFloors                       |  0.0741| 0.1          |
| ConstructioClassSTRUCTURAL STEEL:HeatinSystemHOT WATER                                              |  0.0760| 0.1          |
| ConstructioClassWOOD FRAME:EffYear:HeatinSystemHOT WATER                                            |  0.0766| 0.1          |
| BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:EffYear:HeatinSystemELECTRIC                 |  0.0771| 0.1          |
| BuildinQualityGOOD:HeatinSystemSPACE HEATERS:SprinklersYes                                          |  0.0910| 0.1          |
| BuildinQualityGOOD/EXCELLENT:HeatinSystemSTEAM                                                      |  0.0922| 0.1          |
| BuildinQualityGOOD:ConstructioClassWOOD FRAME:HeatinSystemHOT WATER                                 |  0.0954| 0.1          |
| BuildinQualityGOOD:ConstructioClassWOOD FRAME:HeatinSystemELECTRIC WALL                             |  0.0958| 0.1          |
| HeatinSystemELECTRIC WALL:SprinklersUnknown                                                         |  0.0961| 0.1          |
| ConstructioClassWOOD FRAME:EffYear:HeatinSystemHOT WATER:SprinklersYes                              |  0.0987| 0.1          |
| BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:HeatinSystemWARMED AND COOLED AIR            |  0.0998| 0.1          |
| ConstructioClassWOOD FRAME:EffYear:HeatinSystemHEAT PUMP:NumberofFloors                             |  0.0998| 0.1          |
| BuildinQualityGOOD:ConstructioClassWOOD FRAME                                                       |  0.0999| 0.1          |

2B. Using log(SiteEnergyUse) and but NOT scaling all variables
--------------------------------------------------------------

``` r
knitr::kable(m2b.coef1)
```

| Variable                                                                                         |    pVal| Significance |
|:-------------------------------------------------------------------------------------------------|-------:|:-------------|
| BuildinQualityGOOD:EffYear:HeatinSystemWARMED AND COOLED AIR:SprinklersYes                       |  0.0016| 0.01         |
| BuildinQualityGOOD/EXCELLENT:EffYear:HeatinSystemWARMED AND COOLED AIR                           |  0.0054| 0.01         |
| BuildinQualityGOOD:HeatinSystemWARMED AND COOLED AIR:SprinklersYes                               |  0.0095| 0.01         |
| ConstructioClassWOOD FRAME:EffYear                                                               |  0.0098| 0.01         |
| ConstructioClassWOOD FRAME:EffYear:HeatinSystemHOT WATER                                         |  0.0108| 0.05         |
| BuildinQualityLOW COST:ConstructioClassPREFAB STEEL                                              |  0.0134| 0.05         |
| ConstructioClassWOOD FRAME:EffYear:HeatinSystemHEAT PUMP                                         |  0.0169| 0.05         |
| ConstructioClassWOOD FRAME:SprinklersUnknown:NumberofFloors                                      |  0.0195| 0.05         |
| BuildinQualityLOW COST:ConstructioClassWOOD FRAME:HeatinSystemELECTRIC WALL                      |  0.0228| 0.05         |
| ConstructioClassWOOD FRAME:EffYear:HeatinSystemFORCED AIR UNIT                                   |  0.0252| 0.05         |
| BuildinQualityLOW COST:EffYear                                                                   |  0.0299| 0.05         |
| ConstructioClassWOOD FRAME:EffYear:HeatinSystemFORCED AIR UNIT:SprinklersYes:NumberofFloors      |  0.0308| 0.05         |
| BuildinQualityLOW COST:EffYear:HeatinSystemHOT WATER                                             |  0.0327| 0.05         |
| ConstructioClassWOOD FRAME:HeatinSystemSPACE HEATERS                                             |  0.0457| 0.05         |
| BuildinQualityGOOD:ConstructioClassWOOD FRAME:EffYear:HeatinSystemFORCED AIR UNIT:NumberofFloors |  0.0458| 0.05         |
| ConstructioClassWOOD FRAME:EffYear:HeatinSystemSPACE HEATERS                                     |  0.0463| 0.05         |
| ConstructioClassREINFORCED CONCRETE:HeatinSystemWARMED AND COOLED AIR:NumberofFloors             |  0.0508| 0.1          |
| BuildinQualityGOOD:EffYear:HeatinSystemPACKAGE UNIT                                              |  0.0519| 0.1          |
| ConstructioClassWOOD FRAME:HeatinSystemHEAT PUMP:NumberofFloors                                  |  0.0567| 0.1          |
| BuildinQualityGOOD:EffYear:HeatinSystemELECTRIC                                                  |  0.0601| 0.1          |
| ConstructioClassREINFORCED CONCRETE:HeatinSystemSTEAM                                            |  0.0641| 0.1          |
| BuildinQualityGOOD:EffYear:HeatinSystemELECTRIC:NumberofFloors                                   |  0.0641| 0.1          |
| BuildinQualityLOW COST:ConstructioClassWOOD FRAME:EffYear                                        |  0.0646| 0.1          |
| BuildinQualityAVERAGE/GOOD:EffYear:SprinklersYes                                                 |  0.0664| 0.1          |
| ConstructioClassREINFORCED CONCRETE:EffYear:HeatinSystemSTEAM                                    |  0.0678| 0.1          |
| BuildinQualityLOW COST:EffYear:SprinklersYes                                                     |  0.0717| 0.1          |
| BuildinQualityGOOD:ConstructioClassWOOD FRAME:HeatinSystemSPACE HEATERS                          |  0.0733| 0.1          |
| BuildinQualityGOOD/EXCELLENT:HeatinSystemWARMED AND COOLED AIR:NumberofFloors                    |  0.0741| 0.1          |
| ConstructioClassSTRUCTURAL STEEL:HeatinSystemHOT WATER                                           |  0.0760| 0.1          |
| BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:EffYear:HeatinSystemELECTRIC              |  0.0771| 0.1          |
| ConstructioClassWOOD FRAME:HeatinSystemHOT WATER                                                 |  0.0801| 0.1          |
| ConstructioClassWOOD FRAME                                                                       |  0.0822| 0.1          |
| ConstructioClassREINFORCED CONCRETE:HeatinSystemWARMED AND COOLED AIR                            |  0.0858| 0.1          |
| ConstructioClassWOOD FRAME:EffYear:HeatinSystemPACKAGE UNIT                                      |  0.0887| 0.1          |
| BuildinQualityGOOD:HeatinSystemSPACE HEATERS:SprinklersYes                                       |  0.0910| 0.1          |
| BuildinQualityGOOD/EXCELLENT:HeatinSystemSTEAM                                                   |  0.0922| 0.1          |
| ConstructioClassWOOD FRAME:HeatinSystemPACKAGE UNIT                                              |  0.0954| 0.1          |
| HeatinSystemELECTRIC WALL:SprinklersUnknown                                                      |  0.0961| 0.1          |
| BuildinQualityGOOD:ConstructioClassWOOD FRAME:HeatinSystemFORCED AIR UNIT:NumberofFloors         |  0.0985| 0.1          |
| ConstructioClassWOOD FRAME:EffYear:HeatinSystemHOT WATER:SprinklersYes                           |  0.0987| 0.1          |
| BuildinQualityLOW COST:HeatinSystemHOT WATER                                                     |  0.0998| 0.1          |
| ConstructioClassWOOD FRAME:EffYear:HeatinSystemHEAT PUMP:NumberofFloors                          |  0.0998| 0.1          |

2C. Using SiteEnergyUse and NOT scaling all variables
-----------------------------------------------------

``` r
knitr::kable(m2c.coef1)
```

| Variable                                                                                                        |    pVal| Significance |
|:----------------------------------------------------------------------------------------------------------------|-------:|:-------------|
| BuildinQualityGOOD/EXCELLENT:EffYear:HeatinSystemWARMED AND COOLED AIR                                          |  0.0000| 0.001        |
| BuildinQualityGOOD/EXCELLENT:HeatinSystemWARMED AND COOLED AIR:NumberofFloors                                   |  0.0001| 0.001        |
| ConstructioClassREINFORCED CONCRETE:HeatinSystemWARMED AND COOLED AIR:NumberofFloors                            |  0.0001| 0.001        |
| ConstructioClassREINFORCED CONCRETE:HeatinSystemWARMED AND COOLED AIR                                           |  0.0006| 0.001        |
| ConstructioClassREINFORCED CONCRETE:EffYear:HeatinSystemWARMED AND COOLED AIR:NumberofFloors                    |  0.0006| 0.001        |
| HeatinSystemWARMED AND COOLED AIR:NumberofFloors                                                                |  0.0007| 0.001        |
| EffYear:HeatinSystemWARMED AND COOLED AIR:NumberofFloors                                                        |  0.0007| 0.001        |
| BuildinQualityGOOD:EffYear:HeatinSystemWARMED AND COOLED AIR:SprinklersYes                                      |  0.0008| 0.001        |
| ConstructioClassWOOD FRAME:SprinklersUnknown:NumberofFloors                                                     |  0.0027| 0.01         |
| BuildinQualityAVERAGE/GOOD:HeatinSystemHOT WATER                                                                |  0.0033| 0.01         |
| BuildinQualityGOOD:HeatinSystemWARMED AND COOLED AIR:NumberofFloors                                             |  0.0046| 0.01         |
| BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:EffYear:HeatinSystemWARMED AND COOLED AIR                |  0.0052| 0.01         |
| BuildinQualityAVERAGE/GOOD:EffYear:HeatinSystemHOT WATER                                                        |  0.0057| 0.01         |
| ConstructioClassREINFORCED CONCRETE:EffYear:HeatinSystemWARMED AND COOLED AIR                                   |  0.0069| 0.01         |
| BuildinQualityAVERAGE/GOOD                                                                                      |  0.0079| 0.01         |
| ConstructioClassREINFORCED CONCRETE:EffYear:HeatinSystemSTEAM                                                   |  0.0081| 0.01         |
| BuildinQualityAVERAGE/GOOD:EffYear                                                                              |  0.0094| 0.01         |
| BuildinQualityGOOD/EXCELLENT:HeatinSystemWARMED AND COOLED AIR                                                  |  0.0096| 0.01         |
| BuildinQualityGOOD:HeatinSystemWARMED AND COOLED AIR:SprinklersYes:NumberofFloors                               |  0.0102| 0.05         |
| ConstructioClassREINFORCED CONCRETE:HeatinSystemSTEAM                                                           |  0.0103| 0.05         |
| BuildinQualityAVERAGE/GOOD:EffYear:SprinklersYes                                                                |  0.0117| 0.05         |
| ConstructioClassREINFORCED CONCRETE:SprinklersUnknown                                                           |  0.0122| 0.05         |
| BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:EffYear:NumberofFloors                                   |  0.0122| 0.05         |
| BuildinQualityAVERAGE/GOOD:ConstructioClassREINFORCED CONCRETE:HeatinSystemHEAT PUMP:NumberofFloors             |  0.0154| 0.05         |
| BuildinQualityGOOD/EXCELLENT:ConstructioClassREINFORCED CONCRETE                                                |  0.0160| 0.05         |
| HeatinSystemELECTRIC WALL:SprinklersUnknown                                                                     |  0.0177| 0.05         |
| BuildinQualityGOOD:EffYear:HeatinSystemSTEAM WITHOUT BOILER                                                     |  0.0178| 0.05         |
| BuildinQualityAVERAGE/GOOD:SprinklersYes                                                                        |  0.0180| 0.05         |
| BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:HeatinSystemWARMED AND COOLED AIR                        |  0.0221| 0.05         |
| BuildinQualityGOOD:HeatinSystemSPACE HEATERS                                                                    |  0.0230| 0.05         |
| BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:EffYear:HeatinSystemELECTRIC                             |  0.0237| 0.05         |
| SprinklersUnknown                                                                                               |  0.0270| 0.05         |
| BuildinQualityGOOD:HeatinSystemELECTRIC:SprinklersYes                                                           |  0.0276| 0.05         |
| BuildinQualityAVERAGE/GOOD:ConstructioClassWOOD FRAME:HeatinSystemHEAT PUMP:NumberofFloors                      |  0.0291| 0.05         |
| BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:EffYear:HeatinSystemWARMED AND COOLED AIR:NumberofFloors |  0.0315| 0.05         |
| BuildinQualityGOOD/EXCELLENT:HeatinSystemSTEAM                                                                  |  0.0316| 0.05         |
| BuildinQualityGOOD:HeatinSystemELECTRIC:SprinklersUnknown                                                       |  0.0320| 0.05         |
| BuildinQualityAVERAGE/GOOD:ConstructioClassSTRUCTURAL STEEL:EffYear:HeatinSystemHEAT PUMP                       |  0.0331| 0.05         |
| BuildinQualityAVERAGE/GOOD:ConstructioClassREINFORCED CONCRETE:EffYear                                          |  0.0361| 0.05         |
| BuildinQualityGOOD:HeatinSystemELECTRIC                                                                         |  0.0392| 0.05         |
| BuildinQualityGOOD:EffYear:HeatinSystemWARMED AND COOLED AIR:NumberofFloors                                     |  0.0399| 0.05         |
| BuildinQualityGOOD:ConstructioClassSTRUCTURAL STEEL:HeatinSystemSPACE HEATERS                                   |  0.0405| 0.05         |
| BuildinQualityGOOD:EffYear:HeatinSystemELECTRIC                                                                 |  0.0409| 0.05         |
| ConstructioClassREINFORCED CONCRETE:HeatinSystemWARMED AND COOLED AIR:SprinklersYes                             |  0.0417| 0.05         |
| BuildinQualityGOOD:ConstructioClassREINFORCED CONCRETE:HeatinSystemWARMED AND COOLED AIR:NumberofFloors         |  0.0423| 0.05         |
| BuildinQualityGOOD:ConstructioClassWOOD FRAME:HeatinSystemSPACE HEATERS                                         |  0.0426| 0.05         |
| BuildinQualityGOOD:ConstructioClassWOOD FRAME:EffYear:HeatinSystemFORCED AIR UNIT:NumberofFloors                |  0.0444| 0.05         |
| BuildinQualityEXCELLENT:ConstructioClassREINFORCED CONCRETE                                                     |  0.0459| 0.05         |
| BuildinQualityGOOD:EffYear:HeatinSystemELECTRIC:NumberofFloors                                                  |  0.0513| 0.1          |
| BuildinQualityEXCELLENT:EffYear:NumberofFloors                                                                  |  0.0529| 0.1          |
| BuildinQualityGOOD/EXCELLENT:ConstructioClassREINFORCED CONCRETE:EffYear:NumberofFloors                         |  0.0552| 0.1          |
| ConstructioClassWOOD FRAME:EffYear:HeatinSystemFORCED AIR UNIT:SprinklersYes:NumberofFloors                     |  0.0597| 0.1          |
| ConstructioClassWOOD FRAME:EffYear                                                                              |  0.0649| 0.1          |
| BuildinQualityGOOD:NumberofFloors                                                                               |  0.0663| 0.1          |
| BuildinQualityAVERAGE/GOOD:ConstructioClassSTRUCTURAL STEEL:HeatinSystemHEAT PUMP                               |  0.0665| 0.1          |
| BuildinQualityAVERAGE/GOOD:EffYear:HeatinSystemSTEAM WITHOUT BOILER                                             |  0.0698| 0.1          |
| ConstructioClassWOOD FRAME:EffYear:HeatinSystemHOT WATER                                                        |  0.0702| 0.1          |
| EffYear:SprinklersUnknown                                                                                       |  0.0714| 0.1          |
| BuildinQualityGOOD:EffYear:HeatinSystemPACKAGE UNIT                                                             |  0.0760| 0.1          |
| BuildinQualityGOOD:HeatinSystemELECTRIC:NumberofFloors                                                          |  0.0760| 0.1          |
| BuildinQualityGOOD:HeatinSystemFORCED AIR UNIT:NumberofFloors                                                   |  0.0761| 0.1          |
| ConstructioClassWOOD FRAME:EffYear:HeatinSystemHEAT PUMP                                                        |  0.0766| 0.1          |
| ConstructioClassREINFORCED CONCRETE:HeatinSystemSPACE HEATERS:SprinklersYes:NumberofFloors                      |  0.0776| 0.1          |
| ConstructioClassREINFORCED CONCRETE:EffYear:HeatinSystemHEAT PUMP:SprinklersYes:NumberofFloors                  |  0.0787| 0.1          |
| BuildinQualityGOOD:HeatinSystemWARMED AND COOLED AIR:SprinklersYes                                              |  0.0801| 0.1          |
| BuildinQualityAVERAGE/GOOD:ConstructioClassREINFORCED CONCRETE:EffYear:HeatinSystemHEAT PUMP:NumberofFloors     |  0.0813| 0.1          |
| BuildinQualityGOOD:ConstructioClassWOOD FRAME:HeatinSystemFORCED AIR UNIT:NumberofFloors                        |  0.0814| 0.1          |
| BuildinQualityGOOD/EXCELLENT:EffYear:NumberofFloors                                                             |  0.0837| 0.1          |
| ConstructioClassWOOD FRAME:HeatinSystemPACKAGE UNIT                                                             |  0.0838| 0.1          |
| BuildinQualityGOOD:HeatinSystemPACKAGE UNIT                                                                     |  0.0864| 0.1          |
| ConstructioClassSTRUCTURAL STEEL:EffYear:NumberofFloors                                                         |  0.0871| 0.1          |
| ConstructioClassWOOD FRAME:EffYear:HeatinSystemPACKAGE UNIT                                                     |  0.0877| 0.1          |
| BuildinQualityAVERAGE/GOOD:EffYear:HeatinSystemHEAT PUMP:NumberofFloors                                         |  0.0881| 0.1          |
| BuildinQualityEXCELLENT:ConstructioClassREINFORCED CONCRETE:EffYear                                             |  0.0882| 0.1          |
| ConstructioClassREINFORCED CONCRETE:EffYear:HeatinSystemELECTRIC WALL:SprinklersYes:NumberofFloors              |  0.0894| 0.1          |
| ConstructioClassWOOD FRAME:EffYear:HeatinSystemHEAT PUMP:NumberofFloors                                         |  0.0895| 0.1          |
| BuildinQualityGOOD:ConstructioClassWOOD FRAME:HeatinSystemHOT WATER-RADIANT                                     |  0.0916| 0.1          |
| ConstructioClassWOOD FRAME:HeatinSystemPACKAGE UNIT:NumberofFloors                                              |  0.0928| 0.1          |
| BuildinQualityGOOD/EXCELLENT:ConstructioClassREINFORCED CONCRETE:NumberofFloors                                 |  0.0932| 0.1          |
| BuildinQualityGOOD:EffYear:HeatinSystemHOT WATER:SprinklersYes                                                  |  0.0943| 0.1          |
| ConstructioClassWOOD FRAME:EffYear:HeatinSystemPACKAGE UNIT:NumberofFloors                                      |  0.0946| 0.1          |

Ranking using EER calculation - using model 1A. log(SourceEUI) and scaling all variables
----------------------------------------------------------------------------------------

``` r
step.model = m2a
dat0 = step.model$model
pre0 = predict(step.model, dat0)
```

    ## Warning in predict.lm(step.model, dat0): prediction from a rank-deficient
    ## fit may be misleading

``` r
pre = unscale(pre0, dat0$SourceEUILog)

dat = unscale_data(dat0)

eer = dat$SourceEUILog / pre
dat["eer"] = round(eer,2)

eer_sorted = sort(eer)
     #plot(eer_sorted, xlab = "Building Id", ylab = "Energy Efficiency Ratio")
     
     # Plot cumaltive percentage for energy efficiency ratio
eer_cs = cumsum(eer_sorted)
eer_pr = cumsum(eer_sorted) / sum(eer_sorted)

summary(eer_sorted)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.4244  0.9708  1.0000  1.0000  1.0249  1.5012

``` r
hist(eer_sorted)
```

![](experiment_results_files/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
#plot(eer_sorted)

plot(eer_sorted,eer_pr, main = "Cumalativer percentile of EER",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     #cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0,
     xlab = "Energy Efficiency Ratio", 
     ylab = "Cumalative percentile")


abline(h = 0.8, col = "red", lwd = 2)
abline(v = 1.6, col = "blue", lwd = 2)
```

![](experiment_results_files/figure-markdown_github/unnamed-chunk-16-2.png)

Ranking using EER and clustering - using model 1A. log(SourceEUI) and scaling all variables
-------------------------------------------------------------------------------------------

``` r
library(Ckmeans.1d.dp)

k = 5
result <- Ckmeans.1d.dp(eer_sorted, k, y=eer_sorted)
plot(result)
```

![](experiment_results_files/figure-markdown_github/unnamed-chunk-17-1.png)

``` r
result <- Ckmeans.1d.dp(eer_sorted, y=eer_sorted)
```

    ## Warning in cluster.1d.dp(x, k, y, method, estimate.k, "L2", deparse(substitute(x)), : Max number of clusters used. Consider increasing k!

``` r
plot(result)
```

![](experiment_results_files/figure-markdown_github/unnamed-chunk-17-2.png)

``` r
k = 5
result <- Ckmedian.1d.dp(eer_sorted, k)
plot(result)
```

![](experiment_results_files/figure-markdown_github/unnamed-chunk-17-3.png)

``` r
result <- Ckmedian.1d.dp(eer_sorted)
```

    ## Warning in cluster.1d.dp(x, k, y = 1, method, estimate.k, "L1", deparse(substitute(x)), : Max number of clusters used. Consider increasing k!

``` r
plot(result)
```

![](experiment_results_files/figure-markdown_github/unnamed-chunk-17-4.png)
