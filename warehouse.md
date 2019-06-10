Benchmarking Warehouses
================
Pandarasamy Arjunan
3 June 2019

-   [Load dataset](#load-dataset)
-   [Apply filters](#apply-filters)
-   [Prepare features](#prepare-features)
-   [Descriptive statistics](#descriptive-statistics)
    -   [Data Frame Summary](#data-frame-summary)
-   [Build predictive models](#build-predictive-models)
    -   [Multiple Linear Regression (MLR)](#multiple-linear-regression-mlr)
    -   [Multiple Linear Regression (MLR) with Interaction terms](#multiple-linear-regression-mlr-with-interaction-terms)
    -   [Comparision of MLR models](#comparision-of-mlr-models)
    -   [Gradient Boosted Trees (XGBoost)](#gradient-boosted-trees-xgboost)
    -   [Comparision of XGB models](#comparision-of-xgb-models)
    -   [Comparision between MLR and XGB models](#comparision-between-mlr-and-xgb-models)

Load dataset
------------

``` r
building_type = "warehouse"

filtered_dir = './data/cbecs/filtered/'
dir.create(filtered_dir, recursive = T, showWarnings = F)

features_dir = './data/cbecs/features/'
dir.create(features_dir, recursive = T, showWarnings = F)

results_dir = './results/cbecs/'
dir.create(results_dir, recursive = T, showWarnings = F)
```

``` r
cbecs = read.csv("data/cbecs/2012_public_use_data_aug2016.csv")

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
          "CWUSED", "WOUSED", "COUSED", "SOUSED", "PRUSED",
          "RFGSTP")

warehouses = cbecs[, c(var1, var2)]
```

Apply filters
-------------

As per Energy Star's technical document [ENERGY STAR Score for Warehouses](https://www.energystar.gov/buildings/tools-and-resources/energy-star-score-warehouses), following filters are applied to define the peer group and to remove any outliers.

After applying each filter, the number of remaining buildings in the dataset (*Number Remaining: X*) and any difference (*Difference: X*) in count from the original Energy Star's technical documentation is also given.

1.  **Calculate source energy and source EUI**

    ``` r
    ## convert electricity, natural gas, fuel oil, and district heat to source energy
    w0 = warehouses %>% 
      mutate(ELBTU0 = ELBTU*2.80) %>%
      mutate(NGBTU0 = NGBTU*1.05) %>%
      mutate(FKBTU0 = FKBTU*1.01) %>%
      mutate(DHBTU0 = DHBTU*1.20) %>%
      mutate(SOURCE_ENERGY = rowSums(dplyr::select(., c(ELBTU0,NGBTU0,FKBTU0,DHBTU0)), na.rm = T)) %>% 
      mutate(SOURCE_EUI = round(SOURCE_ENERGY/SQFT, 2)) %>%
      mutate(SITE_EUI = round(MFBTU/SQFT, 2)) %>%
      mutate(NGBTU_PERCENT = round(NGBTU / SOURCE_ENERGY * 100, 2)) %>% 
      mutate(SUMBTU = rowSums(dplyr::select(., c(ELBTU,NGBTU,FKBTU,DHBTU)), na.rm = T))

    #Is MFBTU the sum of ELBTU,NGBTU,FKBTU,DHBTU? YES.
    #summary(o14$MFBTU - o14$SUMBTU)
    ```

2.  **PBAPLUS = 9, 10, or 20** Building Type Filter â€“ CBECS defines building types according to the variable â€œPBAPLUS.â€ Distribution/Shipping Centers are coded as PBAPLUS=9; Non-Refrigerated Warehouses are coded as PBAPLUS=10; and Refrigerated Warehouses are coded as PBAPLUS=20. <br/>Number Remaining: 678. <br/>Difference: 0.

    ``` r
    w1 = w0 %>% filter(PBAPLUS %in% c(9, 10, 20))
    ```

3.  **Must operate for at least 30 hours per week** <br/>EPA Program Filter â€“ Baseline condition for being a full time warehouse. <br/>Number Remaining: 621. <br/>Difference: 0.

    ``` r
    w2 = w1 %>% filter(WKHRS >= 30)
    ```

4.  **Must have at least 1 worker** <br/>EPA Program Filter â€“ Baseline condition for being a full time warehouse <br/>Number Remaining: 597. <br/>Difference: 0.

    ``` r
    w3 = w2 %>% filter(NWKER >= 1)
    ```

5.  **Must operate for at least 10 months per year** <br/>EPA Program Filter â€“ Baseline condition for being a full time warehouse. <br/>Number Remaining: 580. <br/>Difference: 0.

    ``` r
    w4 = w3 %>% filter(MONUSE >= 10)
    ```

6.  **A single activity must characterize greater than 50% of the floor space** <br/>EPA Program Filter â€“ In order to be considered part of the warehouse peer group, more than 50% of the building must be defined as distribution/shipping center, non-refrigerated warehouse, or refrigerated warehouse. <br/>Number Remaining: 568. <br/>Difference: +25

    ``` r
    w5 = w4 %>% 
      filter( (ONEACT == 1) |
                (ONEACT == 2 & 
                   ((ACT1 %in% c(13) & ACT1PCT > 50) | 
                    (ACT2 %in% c(13) & ACT2PCT > 50) | 
                    (ACT3 %in% c(13) & ACT3PCT > 50) )))

    # test code to verify the above filter
    # w51 = w4 %>% filter(ONEACT == 1)
    # w5a = w4 %>% filter(ONEACT == 2 & ACT1 == 13 & ACT1PCT > 50)
    # w5b = w4 %>% filter(ONEACT == 2 & ACT2 == 13 & ACT2PCT > 50)
    # w5c = w4 %>% filter(ONEACT == 2 & ACT3 == 13 & ACT3PCT > 50)
    # #select(c(ONEACT, ACT1, ACT2, ACT3, ACT1PCT, ACT2PCT, ACT3PCT)) %>% 
    # print(nrow(w51) + nrow(w5a) + nrow(w5b) + nrow(w5c))
    ```

7.  **Must report energy usage** <br/>EPA Program Filter â€“ Baseline condition for being a full time warehouse. <br/>Number Remaining: 562. <br/>Difference: +19.

    ``` r
    w6 = w5 %>% filter(!is.na(MFBTU))
    ```

8.  **Must be less than or equal to 1,000,000 square feet** <br/>Data Limitation Filter â€“ CBECS masks surveyed properties above 1,000,000 square feet by applying regional averages. <br/>Number Remaining: 555. <br/>Difference: +19.

    ``` r
    w7 = w6 %>% filter(SQFT <= 1000000)
    ```

9.  **If propane is used, the amount category (PRAMTC) must equal 1, 2, or 3** <br/>Data Limitation Filter â€“ Cannot estimate propane use if the quantity is â€œgreater than 1000â€ or unknown. <br/>Number Remaining: 530. <br/>Difference: +19.

    ``` r
    w8 = w7 %>% filter(is.na(PRAMTC) | PRAMTC %in% c(1,2,3))
    ```

10. **If propane is used, the unit (PRUNIT) must be known** <br/>Data Limitation Filter â€“ Cannot estimate propane use if the unit is unknown. <br/>Number Remaining: 526. <br/>Difference: +19.

    ``` r
    w9 = w8 %>% filter(is.na(PRUNIT) | PRUNIT %in% c(1,2))
    ```

11. **If propane is used, the maximum estimated propane amount must be 10% or less of the total source energy** <br/>Data Limitation Filter â€“ Because propane values are estimated from a range, propane is restricted to 10% of the total source energy. <br/>Number Remaining: 510. <br/>Difference: +6.

    ``` r
    w10 = w9 %>% 
      filter(PRUSED == 2 | is.na(NGBTU_PERCENT) == T | 
            (PRUSED == 1 & NGBTU_PERCENT <= 10))
    ```

12. **Must not use chilled water, wood, coal, or solar** <br/>Data Limitation Filter â€“ CBECS does not collect quantities of chilled water, wood, coal, or solar. <br/>Number Remaining: 499. <br/>Difference: +6.

    ``` r
    w11 = w10 %>% 
      filter(CWUSED == 2) %>%
      filter(WOUSED == 2) %>% 
      filter(COUSED == 2) %>% 
      filter(SOUSED == 2)
    ```

13. **Must have no more than 2.8 workers per 1,000 square feet** <br/>Analytical Filter â€“ Values determined to be statistical outliers. <br/>Number Remaining: 487. <br/>Difference: +4.

    ``` r
    w12 = w11 %>% filter(NWKER  / SQFT * 1000 <= 2.8)
    ```

14. **Source EUI cannot be greater than 500 kBtu/ft2** <br/>Analytical Filter â€“ Values determined to be statistical outliers. <br/>Number Remaining: 483. <br/>Difference: +3.

    ``` r
    w13 = w12 %>% filter(SOURCE_EUI <= 500)
    ```

15. **Percent Cooled plus Percent Cold Storage must be less than or equal to 100%** <br/>Analytical Filter â€“ Values exceed what is physically expected to be possible. <br/>Number Remaining: 477. <br/>Difference: +3.

    ``` r
    w14 = w13 %>% 
      dplyr::mutate(COLD_SUM = 
                      rowSums(dplyr::select(., c(COOLP,RFGSTP)), na.rm = T)) %>%
      filter(COLD_SUM <= 100)
    ```

16. **Percent Heated plus Percent Cold Storage must be less than or equal to 100%** <br/>Analytical Filter â€“ Values exceed what is physically expected to be possible. <br/>Number Remaining: 475. <br/>Difference: +3.

    ``` r
    w15 = w14 %>% 
      dplyr::mutate(HEAT_SUM = 
                      rowSums(dplyr::select(., c(HEATP,RFGSTP)), na.rm = T)) %>%
      filter(HEAT_SUM <= 100)
    ```

**Save the filtered dataset**

``` r
write.csv(w15, paste0(filtered_dir, building_type, ".csv"), row.names = F)
```

Prepare features
----------------

The final regression equation includes the following variables: ï‚·

-   Weekly Operating Hours
-   Number of Workers per 1,000 Square Feet
-   Percent Cold Storage
-   Heating Degree Days times Percent of the Building that is Heated
-   Cooling Degree Days times (Percent of the Building that is Cooled plus Percent Cold Storage)

``` r
warehouse = read.csv(paste0(filtered_dir, building_type, ".csv"))

data = warehouse %>%
  mutate(NWKER_SQFT = NWKER/SQFT * 1000) %>%
  mutate(HDD65_HEATP = HDD65 * HEATP / 100) %>%
  mutate(CDD65_COLD_SUM = CDD65 * COLD_SUM / 100) %>%
  mutate_if(is.numeric, round, 3)

ivars = c(
  #"RFGSTP", ## too many missing values
  "COLD_SUM", 
  "SQFT", 
  "WKHRS", "NWKER_SQFT", 
  "HDD65_HEATP", "CDD65_COLD_SUM")

dvars  = c("SOURCE_EUI", "SOURCE_ENERGY", "FINALWT")

features = data[, c(ivars, dvars)]
#summary(features)
features = features %>% na.omit()

write.csv(features, 
          paste0(features_dir, building_type, ".csv"), 
          row.names = F)
```

Descriptive statistics
----------------------

``` r
features1 = features
features1 = features1 %>% dplyr::select(-one_of('SOURCE_ENERGY', 'FINALWT'))

summarytools::descr(features1, stats = "common", 
                    transpose = TRUE, 
                    headings = FALSE)
```

|                      |       Mean|    Std.Dev|      Min|    Median|         Max|  N.Valid|
|---------------------:|----------:|----------:|--------:|---------:|-----------:|--------:|
|  **CDD65\_COLD\_SUM**|     611.66|     812.69|     0.00|    347.45|     5283.90|   404.00|
|         **COLD\_SUM**|      32.88|      32.48|     0.00|     24.50|      100.00|   404.00|
|      **HDD65\_HEATP**|    2101.00|    2141.40|     0.00|   1147.38|     8532.00|   404.00|
|       **NWKER\_SQFT**|       0.66|       0.59|     0.01|      0.47|        2.67|   404.00|
|       **SOURCE\_EUI**|      74.98|      66.28|     4.04|     58.17|      473.16|   404.00|
|              **SQFT**|  120074.64|  167568.49|  1100.00|  44750.00|  1000000.00|   404.00|
|             **WKHRS**|      67.72|      37.64|    32.00|     50.00|      168.00|   404.00|

|                      |  Pct.Valid|
|---------------------:|----------:|
|  **CDD65\_COLD\_SUM**|     100.00|
|         **COLD\_SUM**|     100.00|
|      **HDD65\_HEATP**|     100.00|
|       **NWKER\_SQFT**|     100.00|
|       **SOURCE\_EUI**|     100.00|
|              **SQFT**|     100.00|
|             **WKHRS**|     100.00|

``` r
dfSummary(features1, plain.ascii = FALSE, style = "grid", 
          graph.magnif = 0.75, valid.col = FALSE)
```

    text graphs are displayed; set 'tmp.img.dir' parameter to activate png graphs

### Data Frame Summary

**features1**
**Dimensions:** 404 x 7
**Duplicates:** 0

<table>
<colgroup>
<col width="4%" />
<col width="14%" />
<col width="28%" />
<col width="17%" />
<col width="28%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th>No</th>
<th>Variable</th>
<th>Stats / Values</th>
<th>Freqs (% of Valid)</th>
<th>Graph</th>
<th>Missing</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><p>1</p></td>
<td><p>COLD_SUM<br />
[numeric]</p></td>
<td><p>Mean (sd) : 32.9 (32.5)<br />
min &lt; med &lt; max:<br />
0 &lt; 24.5 &lt; 100<br />
IQR (CV) : 30.5 (1)</p></td>
<td><p>42 distinct values</p></td>
<td><p><br />
:<br />
:<br />
: . .<br />
: : :             :<br />
: : : : . .   .   :</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="even">
<td><p>2</p></td>
<td><p>SQFT<br />
[numeric]</p></td>
<td><p>Mean (sd) : 120074.6 (167568.5)<br />
min &lt; med &lt; max:<br />
1100 &lt; 44750 &lt; 1e+06<br />
IQR (CV) : 168500 (1.4)</p></td>
<td><p>192 distinct values</p></td>
<td><p><br />
:<br />
:<br />
:<br />
: .<br />
: : :</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="odd">
<td><p>3</p></td>
<td><p>WKHRS<br />
[numeric]</p></td>
<td><p>Mean (sd) : 67.7 (37.6)<br />
min &lt; med &lt; max:<br />
32 &lt; 50 &lt; 168<br />
IQR (CV) : 23.5 (0.6)</p></td>
<td><p>55 distinct values</p></td>
<td><dl>
<dt></dt>
<dd>.<br />

</dd>
<dd>:<br />

</dd>
<dd>: :<br />

</dd>
<dd>: :             .<br />

</dd>
<dd>: : . .   .     :
</dd>
</dl></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="even">
<td><p>4</p></td>
<td><p>NWKER_SQFT<br />
[numeric]</p></td>
<td><p>Mean (sd) : 0.7 (0.6)<br />
min &lt; med &lt; max:<br />
0 &lt; 0.5 &lt; 2.7<br />
IQR (CV) : 0.7 (0.9)</p></td>
<td><p>250 distinct values</p></td>
<td><p><br />
:<br />
: :<br />
: : .<br />
: : : .<br />
: : : : : . . .   .</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="odd">
<td><p>5</p></td>
<td><p>HDD65_HEATP<br />
[numeric]</p></td>
<td><p>Mean (sd) : 2101 (2141.4)<br />
min &lt; med &lt; max:<br />
0 &lt; 1147.4 &lt; 8532<br />
IQR (CV) : 3799.1 (1)</p></td>
<td><p>399 distinct values</p></td>
<td><p><br />
:<br />
:<br />
:<br />
:<br />
: : : . : : .</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="even">
<td><p>6</p></td>
<td><p>CDD65_COLD_SUM<br />
[numeric]</p></td>
<td><p>Mean (sd) : 611.7 (812.7)<br />
min &lt; med &lt; max:<br />
0 &lt; 347.4 &lt; 5283.9<br />
IQR (CV) : 637.4 (1.3)</p></td>
<td><p>380 distinct values</p></td>
<td><p><br />
:<br />
:<br />
:<br />
: .<br />
: : . .</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="odd">
<td><p>7</p></td>
<td><p>SOURCE_EUI<br />
[numeric]</p></td>
<td><p>Mean (sd) : 75 (66.3)<br />
min &lt; med &lt; max:<br />
4 &lt; 58.2 &lt; 473.2<br />
IQR (CV) : 62.2 (0.9)</p></td>
<td><p>397 distinct values</p></td>
<td><p><br />
:<br />
: .<br />
: :<br />
: : .<br />
: : : .</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
</tbody>
</table>

**Extract R code from Rmd document**

``` r
#knitr::purl("warehouse.Rmd", output = "warehouse.R", documentation = 2)
```

Build predictive models
-----------------------

``` r
#source("models.R")
source("metrics.R")

data = read.csv(paste0(features_dir, building_type, ".csv"))

allMetrics = NULL

sourceEUI_attributes = setdiff(colnames(data),
                               c("SQFT", "FINALWT", "SOURCE_EUI", "SOURCE_ENERGY"))

sourceEnergy_attributes = setdiff(colnames(data), 
                               c("FINALWT", "SOURCE_EUI", "SOURCE_ENERGY"))
#cat(colnames(data))
knitr::kable(colnames(data), col.names = NULL)
```

    Warning in kable_markdown(x, padding = padding, ...): The table should have
    a header (column names)

|                  |
|:-----------------|
| COLD\_SUM        |
| SQFT             |
| WKHRS            |
| NWKER\_SQFT      |
| HDD65\_HEATP     |
| CDD65\_COLD\_SUM |
| SOURCE\_EUI      |
| SOURCE\_ENERGY   |
| FINALWT          |

### Multiple Linear Regression (MLR)

``` r
MLR.fit <- function(data, 
                    x, 
                    y, 
                    w, 
                    interaction,
                    centering = TRUE 
                    ) {
  
  if(centering == TRUE){
    data = mean_center_data(data, x)  
  }
  
  if(interaction == 1) {  ### ordinary model
    model = paste(y, "~", paste(x, collapse = " + "))  
  } else {  ### interaction model
    allvars = paste(x, collapse = " + ")
    model = paste(y, "~ (", allvars, ") ^", interaction )
  }
  
  fit = lm(model, data = data, weights = data[, w])
  return (fit)
}

MLR.predict <- function(data, x, y, w, i) {
  
  mlrFit = MLR.fit(data, x, y, w, i)
  
  wt   = data[, w]
  obs  = data[, y]
  pred = as.numeric(predict(mlrFit))
  
  mlrMetrics = getMLRmetrics(mlrFit, obs, pred, wt)
  mlrMetrics = data.frame(
    "model" = "MLR",
    "dependent" = y,
    "interaction" = i,
    "transform" = "meanCent",
    mlrMetrics)
  
  return(mlrMetrics)
}
```

#### Using SOURCE\_EUI as dependent variable

``` r
x = sourceEUI_attributes
y = "SOURCE_EUI"
w = "FINALWT"
interaction = 1

mlrMetrics = MLR.predict(data, x, y, w, interaction)

allMetrics = rbind(allMetrics, mlrMetrics)

knitr::kable(allMetrics, row.names = F)
```

| model | dependent   |  interaction| transform |  obs|  rank|  coef|    R.2|  Adj.R.2|       mse|    rmse|     mae|   mape|  nrmse\_iqr|  nrmse\_range|  nrmse\_mean|  nrmse\_sd|
|:------|:------------|------------:|:----------|----:|-----:|-----:|------:|--------:|---------:|-------:|-------:|------:|-----------:|-------------:|------------:|----------:|
| MLR   | SOURCE\_EUI |            1| meanCent  |  404|     6|     6|  0.362|    0.354|  3287.568|  57.337|  37.043|  1.025|      92.163|        12.222|       76.471|      86.51|

#### Using SOURCE\_ENERGY as dependent variable

``` r
x = sourceEnergy_attributes
y = "SOURCE_ENERGY"
w = "FINALWT"
interaction = 1

mlrMetrics = MLR.predict(data, x, y, w, interaction)

allMetrics = rbind(allMetrics, mlrMetrics)

knitr::kable(allMetrics, row.names = F)
```

| model | dependent      |  interaction| transform |  obs|  rank|  coef|    R.2|  Adj.R.2|           mse|          rmse|          mae|   mape|  nrmse\_iqr|  nrmse\_range|  nrmse\_mean|  nrmse\_sd|
|:------|:---------------|------------:|:----------|----:|-----:|-----:|------:|--------:|-------------:|-------------:|------------:|------:|-----------:|-------------:|------------:|----------:|
| MLR   | SOURCE\_EUI    |            1| meanCent  |  404|     6|     6|  0.362|    0.354|  3.287568e+03|        57.337|       37.043|  1.025|      92.163|        12.222|       76.471|     86.510|
| MLR   | SOURCE\_ENERGY |            1| meanCent  |  404|     7|     7|  0.510|    0.502|  1.495968e+14|  12230977.252|  5951650.641|  2.070|     140.289|         8.564|      141.960|     73.879|

### Multiple Linear Regression (MLR) with Interaction terms

#### Using SOURCE\_EUI as dependent variable

``` r
x = sourceEUI_attributes
y = "SOURCE_EUI"
w = "FINALWT"

intr_depth = length(x)

for (interaction in 2:intr_depth) {
  mlrMetrics = MLR.predict(data, x, y, w, interaction)
  allMetrics = rbind(allMetrics, mlrMetrics)
}

write.csv(allMetrics, 
          paste0(results_dir, building_type, ".csv"), 
          row.names = F)

allMetrics0 = allMetrics %>% filter(dependent == y)
knitr::kable(allMetrics0, row.names = F)
```

| model | dependent   |  interaction| transform |  obs|  rank|  coef|    R.2|  Adj.R.2|       mse|    rmse|     mae|   mape|  nrmse\_iqr|  nrmse\_range|  nrmse\_mean|  nrmse\_sd|
|:------|:------------|------------:|:----------|----:|-----:|-----:|------:|--------:|---------:|-------:|-------:|------:|-----------:|-------------:|------------:|----------:|
| MLR   | SOURCE\_EUI |            1| meanCent  |  404|     6|     6|  0.362|    0.354|  3287.568|  57.337|  37.043|  1.025|      92.163|        12.222|       76.471|     86.510|
| MLR   | SOURCE\_EUI |            2| meanCent  |  404|    16|    16|  0.440|    0.418|  2956.825|  54.377|  36.162|  0.977|      87.405|        11.591|       72.523|     82.044|
| MLR   | SOURCE\_EUI |            3| meanCent  |  404|    26|    26|  0.475|    0.440|  2956.973|  54.378|  35.776|  0.953|      87.407|        11.591|       72.525|     82.045|
| MLR   | SOURCE\_EUI |            4| meanCent  |  404|    31|    31|  0.480|    0.439|  3005.242|  54.820|  35.816|  0.972|      88.117|        11.686|       73.114|     82.712|
| MLR   | SOURCE\_EUI |            5| meanCent  |  404|    32|    32|  0.480|    0.437|  3006.293|  54.830|  35.826|  0.972|      88.133|        11.688|       73.127|     82.727|

#### Using SOURCE\_ENERGY as dependent variable\*\*

``` r
x = sourceEnergy_attributes
y = "SOURCE_ENERGY"
w = "FINALWT"

intr_depth = length(x)

for (interaction in 2:intr_depth) {
  mlrMetrics = MLR.predict(data, x, y, w, interaction)
  allMetrics = rbind(allMetrics, mlrMetrics)
}

write.csv(allMetrics, 
          paste0(results_dir, building_type, ".csv"), 
          row.names = F)

allMetrics0 = allMetrics %>% filter(dependent == y)
knitr::kable(allMetrics0, row.names = F)
```

| model | dependent      |  interaction| transform |  obs|  rank|  coef|    R.2|  Adj.R.2|           mse|      rmse|      mae|   mape|  nrmse\_iqr|  nrmse\_range|  nrmse\_mean|  nrmse\_sd|
|:------|:---------------|------------:|:----------|----:|-----:|-----:|------:|--------:|-------------:|---------:|--------:|------:|-----------:|-------------:|------------:|----------:|
| MLR   | SOURCE\_ENERGY |            1| meanCent  |  404|     7|     7|  0.510|    0.502|  1.495968e+14|  12230977|  5951651|  2.070|     140.289|         8.564|      141.960|     73.879|
| MLR   | SOURCE\_ENERGY |            2| meanCent  |  404|    22|    22|  0.715|    0.699|  1.020236e+14|  10100671|  4497670|  1.411|     115.854|         7.072|      117.234|     61.011|
| MLR   | SOURCE\_ENERGY |            3| meanCent  |  404|    42|    42|  0.810|    0.789|  7.883690e+13|   8879015|  4054083|  1.028|     101.842|         6.217|      103.055|     53.632|
| MLR   | SOURCE\_ENERGY |            4| meanCent  |  404|    57|    57|  0.846|    0.821|  7.247630e+13|   8513301|  3786385|  0.975|      97.647|         5.961|       98.810|     51.423|
| MLR   | SOURCE\_ENERGY |            5| meanCent  |  404|    63|    63|  0.851|    0.824|  7.209354e+13|   8490791|  3806019|  0.940|      97.389|         5.945|       98.549|     51.287|
| MLR   | SOURCE\_ENERGY |            6| meanCent  |  404|    64|    64|  0.852|    0.825|  7.265773e+13|   8523950|  3815729|  0.934|      97.769|         5.968|       98.934|     51.487|

### Comparision of MLR models

#### MLR plots using Source EUI

``` r
mytheme = theme(legend.title = element_blank(),
           legend.text=element_text(size=12),
           axis.text=element_text(size=12),
           text=element_text(size=12))

plotR2 <- function(df, titl) {
  
  df1 = melt(df, measure.vars = c("R.2", "Adj.R.2"))
  
  plot <- ggplot(df1, aes(x = interaction, y=value, 
                          group=variable, col=variable)) + 
  geom_point(size=2) + geom_line(size=1) +
    ggtitle(titl) + 
    theme_pubr(base_size=12) +
    theme(legend.position="top", legend.title = element_blank())
  
  return(plot)
}

plotNRMSE <- function(df, titl) {
  
  df1 = melt(df, measure.vars = c("nrmse_iqr", "nrmse_mean", 
                                        "nrmse_sd"))
  df1$variable = toupper(df1$variable)
  
  plot <- ggplot(df1, aes(x = interaction, y=value, 
                          group=variable, col=variable)) + 
  geom_point(size=2) + geom_line(size=1) +
    ggtitle(titl) + 
    theme_pubr(base_size=12) +
    theme(legend.position="top", legend.title = element_blank())
    
  
  return(plot)
}  
```

``` r
allMetrics0 = allMetrics %>%
  filter(stringr::str_detect(model, "MLR")) %>%
  filter(dependent == "SOURCE_EUI")

plot1 = plotR2(allMetrics0, "MLR models using source EUI")
plot2 = plotNRMSE(allMetrics0, "MLR models using source EUI")

print(plot1)
```

![](warehouse_files/figure-markdown_github/unnamed-chunk-32-1.png)

``` r
print(plot2)
```

![](warehouse_files/figure-markdown_github/unnamed-chunk-32-2.png)

#### MLR plots using Source Energy

``` r
allMetrics0 = allMetrics %>%
  filter(stringr::str_detect(model, "MLR")) %>%
  filter(dependent == "SOURCE_ENERGY")

plot1 = plotR2(allMetrics0, "MLR models using source energy")
plot2 = plotNRMSE(allMetrics0, "MLR models using source energy")

print(plot1)
```

![](warehouse_files/figure-markdown_github/unnamed-chunk-33-1.png)

``` r
print(plot2)
```

![](warehouse_files/figure-markdown_github/unnamed-chunk-33-2.png)

### Gradient Boosted Trees (XGBoost)

``` r
tuneXGBoost <- function(x,
                        y,
                        sample_weights,
                        search = "default",
                        tree_height = 2
                        ) {
  
  N = 10  # N-fold CV
  R = 2   # and R repeats
  
  tcDefault  <- trainControl(method = "repeatedcv", 
                            number = N, 
                            repeats = R)
  
  tcRandom   <- trainControl(method = "repeatedcv", 
                            search = "random",
                            number = N, 
                            repeats = R)
  
  tcAdaptive <- trainControl(method = "adaptive_cv", 
                            search = "random",
                            number = N, 
                            repeats = R,
                            adaptive = list(min = 5, 
                                            alpha = 0.05, 
                                            method = "gls",
                                            complete = TRUE))
  
  default_param = expand.grid(
    nrounds = 100,
    max_depth = tree_height,
    eta = 0.3,
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1)
  
  # from https://github.com/topepo/caret/blob/master/models/files/xgbTree.R
  len = 10
  grid_param <- expand.grid(
    nrounds = floor((1:len) * 10),
    max_depth = tree_height,
    eta = c(.3, .4),
    gamma = 0,
    colsample_bytree = c(.6, .8),
    min_child_weight = c(1),
    subsample = seq(.25, 1, length = len))
  
  tuned = switch(search,
                 "default" = train(x, y,
                                   weights = sample_weights,
                                   method = "xgbTree", 
                                   tuneGrid = default_param,
                                   trControl = tcDefault,
                                   verbose = TRUE),
                 
                 "grid"     = train(x, y, 
                                   weights = sample_weights,
                                   method = "xgbTree", 
                                   tuneGrid = grid_param,
                                   trControl = tcDefault,
                                   verbose = TRUE),

                 "random"  = train(x, y, 
                                   weights = sample_weights,
                                   method = "xgbTree", 
                                   trControl = tcRandom,
                                   verbose = TRUE),
                 
                 "adaptive" = train(x, y, 
                                   weights = sample_weights,
                                   method = "xgbTree", 
                                   trControl = tcAdaptive,
                                   verbose = TRUE)
                 )
  
  return(tuned$finalModel)
}

XGBoost <- function( xdata, 
                     ydata,
                     sample_weights,
                     search = "default",
                     interaction = 2
                     ) {
  
  model = paste(y, "~", paste(x, collapse = " + "))
  dummy = dummyVars(model, data = data, fullRank = T)
  
  xdata = as.data.frame(predict(dummy, data))
  ydata = data[, y]
  
  xgfit = tuneXGBoost(xdata, ydata, 
                      sample_weights,
                      search, 
                      tree_height = interaction)
  return(xgfit)
} 


XGBoost.predict <- function(data, x, y, w, search) {

  model = paste(y, "~", paste(x, collapse = " + "))
  dummy = dummyVars(model, data = data, fullRank = T)
  xdata = as.data.frame(predict(dummy, data))
  ydata = data[, y]
  wt = data[, w]
  
  intr_depth = 3
  
  xgbMetricsAll = NULL
  
  for (intr in 1:intr_depth) {
    
    cat(paste(Sys.time(), "xgboost", search, y, intr, "\n"))
    
    xgfit = XGBoost(xdata, ydata, wt, 
                    search = search, 
                    interaction = intr)
    
    pred = as.numeric(predict(xgfit, as.matrix(xdata)))
    
    xgbMetrics = getXgboostmetrics(xgfit, xdata, ydata, pred, wt)
    
    xgbMetrics = data.frame(
      "model" = paste0("XGB", substr(search, 1,1), intr),
      "dependent" = y, 
      "interaction" = intr, 
      "transform" = "None",
      xgbMetrics)
    
    xgbMetricsAll = rbind(xgbMetricsAll, xgbMetrics)
  }
  
  return(xgbMetricsAll)
}
```

``` r
# y = "SOURCE_EUI"
# w = "FINALWT"
# o = c("SOURCE_ENERGY", "SQFT")
# x = setdiff(colnames(data), c(y, w, o))
# wt = data[, w]
# 
# model = paste(y, "~", paste(x, collapse = " + "))
# dummy = dummyVars(model, data = data, fullRank = T)
# xdata = as.data.frame(predict(dummy, data))
# ydata = data[, y]
# intr_depth = 3
# 
# for (intr in 1:intr_depth) {
#   
#   print(paste(Sys.time(), "xgboost default search", y, intr, "\n"))
#   
#   xgfit = XGBoost(xdata, ydata, wt, 
#                   search = "default", 
#                   interaction = intr)
#   
#   pred = as.numeric(predict(xgfit, as.matrix(xdata)))
#   
#   xgbMetrics = getXgboostmetrics(xgfit, xdata, ydata, pred, wt)
#   
#   xgbMetrics = data.frame(
#     "model" = paste0("XGBd", intr),
#     "dependent" = y, 
#     "interaction" = intr, 
#     "transform" = "None",
#     xgbMetrics)
#   
#   allMetrics = rbind(allMetrics, xgbMetrics)
# }
# knitr::kable(allMetrics, row.names = F)
```

#### Using SOURCE\_EUI as dependent variable

``` r
library(doParallel)
ncore = 4
registerDoParallel(cores = ncore)
```

##### Using default search

``` r
x = sourceEUI_attributes
y = "SOURCE_EUI"
w = "FINALWT"
search = "default"

xgbMetrics = XGBoost.predict(data, x, y, w, search)
```

2019-06-10 16:26:34 xgboost default SOURCE\_EUI 1 2019-06-10 16:26:51 xgboost default SOURCE\_EUI 2 2019-06-10 16:26:54 xgboost default SOURCE\_EUI 3

``` r
allMetrics = rbind(allMetrics, xgbMetrics)

knitr::kable(xgbMetrics, row.names = F)
```

| model | dependent   |  interaction| transform |  obs|  rank|  coef|    R.2|  Adj.R.2|       mse|    rmse|     mae|   mape|  nrmse\_iqr|  nrmse\_range|  nrmse\_mean|  nrmse\_sd|
|:------|:------------|------------:|:----------|----:|-----:|-----:|------:|--------:|---------:|-------:|-------:|------:|-----------:|-------------:|------------:|----------:|
| XGBd1 | SOURCE\_EUI |            1| None      |  404|     5|     5|  0.465|    0.459|  3073.895|  55.443|  35.551|  0.974|      89.119|        11.819|       73.945|     83.652|
| XGBd2 | SOURCE\_EUI |            2| None      |  404|     5|     5|  0.826|    0.824|  1717.801|  41.446|  24.915|  0.668|      66.620|         8.835|       55.277|     62.533|
| XGBd3 | SOURCE\_EUI |            3| None      |  404|     5|     5|  0.961|    0.961|   546.776|  23.383|  14.259|  0.406|      37.586|         4.984|       31.186|     35.280|

##### Using grid search

``` r
x = sourceEUI_attributes
y = "SOURCE_EUI"
w = "FINALWT"
search = "grid"

xgbMetrics = XGBoost.predict(data, x, y, w, search)
```

2019-06-10 16:26:59 xgboost grid SOURCE\_EUI 1 2019-06-10 16:27:39 xgboost grid SOURCE\_EUI 2 2019-06-10 16:28:42 xgboost grid SOURCE\_EUI 3

``` r
allMetrics = rbind(allMetrics, xgbMetrics)

knitr::kable(xgbMetrics, row.names = F)
```

| model | dependent   |  interaction| transform |  obs|  rank|  coef|    R.2|  Adj.R.2|       mse|    rmse|     mae|   mape|  nrmse\_iqr|  nrmse\_range|  nrmse\_mean|  nrmse\_sd|
|:------|:------------|------------:|:----------|----:|-----:|-----:|------:|--------:|---------:|-------:|-------:|------:|-----------:|-------------:|------------:|----------:|
| XGBg1 | SOURCE\_EUI |            1| None      |  404|     5|     5|  0.374|    0.368|  3264.880|  57.139|  37.142|  1.094|      91.845|        12.180|       76.207|     86.211|
| XGBg2 | SOURCE\_EUI |            2| None      |  404|     5|     5|  0.478|    0.473|  2774.557|  52.674|  33.486|  0.939|      84.668|        11.228|       70.252|     79.474|
| XGBg3 | SOURCE\_EUI |            3| None      |  404|     5|     5|  0.556|    0.552|  2178.476|  46.674|  30.853|  0.854|      75.024|         9.949|       62.250|     70.421|

##### Using adaptive search

``` r
x = sourceEUI_attributes
y = "SOURCE_EUI"
w = "FINALWT"
search = "adaptive"

xgbMetrics = XGBoost.predict(data, x, y, w, search)
```

2019-06-10 16:30:05 xgboost adaptive SOURCE\_EUI 1 2019-06-10 16:30:21 xgboost adaptive SOURCE\_EUI 2 2019-06-10 16:31:25 xgboost adaptive SOURCE\_EUI 3

``` r
allMetrics = rbind(allMetrics, xgbMetrics)

knitr::kable(xgbMetrics, row.names = F)
```

| model | dependent   |  interaction| transform |  obs|  rank|  coef|    R.2|  Adj.R.2|       mse|    rmse|     mae|   mape|  nrmse\_iqr|  nrmse\_range|  nrmse\_mean|  nrmse\_sd|
|:------|:------------|------------:|:----------|----:|-----:|-----:|------:|--------:|---------:|-------:|-------:|------:|-----------:|-------------:|------------:|----------:|
| XGBa1 | SOURCE\_EUI |            1| None      |  404|     5|     5|  0.766|    0.764|  1694.498|  41.164|  27.764|  0.808|      66.167|         8.775|       54.901|     62.108|
| XGBa2 | SOURCE\_EUI |            2| None      |  404|     5|     5|  1.000|    1.000|     0.017|   0.131|   0.078|  0.002|       0.211|         0.028|        0.175|      0.198|
| XGBa3 | SOURCE\_EUI |            3| None      |  404|     5|     5|  0.885|    0.884|   890.311|  29.838|  20.142|  0.575|      47.961|         6.360|       39.795|     45.019|

##### Using random search

``` r
x = sourceEUI_attributes
y = "SOURCE_EUI"
w = "FINALWT"
search = "random"

xgbMetrics = XGBoost.predict(data, x, y, w, search)
```

2019-06-10 16:33:09 xgboost random SOURCE\_EUI 1 2019-06-10 16:34:22 xgboost random SOURCE\_EUI 2 2019-06-10 16:34:47 xgboost random SOURCE\_EUI 3

``` r
allMetrics = rbind(allMetrics, xgbMetrics)

write.csv(allMetrics, 
          paste0(results_dir, building_type, ".csv"), 
          row.names = F)

allMetrics0 = allMetrics %>% filter(dependent == y)
knitr::kable(allMetrics0, row.names = F)
```

| model | dependent   |  interaction| transform |  obs|  rank|  coef|    R.2|  Adj.R.2|       mse|    rmse|     mae|   mape|  nrmse\_iqr|  nrmse\_range|  nrmse\_mean|  nrmse\_sd|
|:------|:------------|------------:|:----------|----:|-----:|-----:|------:|--------:|---------:|-------:|-------:|------:|-----------:|-------------:|------------:|----------:|
| MLR   | SOURCE\_EUI |            1| meanCent  |  404|     6|     6|  0.362|    0.354|  3287.568|  57.337|  37.043|  1.025|      92.163|        12.222|       76.471|     86.510|
| MLR   | SOURCE\_EUI |            2| meanCent  |  404|    16|    16|  0.440|    0.418|  2956.825|  54.377|  36.162|  0.977|      87.405|        11.591|       72.523|     82.044|
| MLR   | SOURCE\_EUI |            3| meanCent  |  404|    26|    26|  0.475|    0.440|  2956.973|  54.378|  35.776|  0.953|      87.407|        11.591|       72.525|     82.045|
| MLR   | SOURCE\_EUI |            4| meanCent  |  404|    31|    31|  0.480|    0.439|  3005.242|  54.820|  35.816|  0.972|      88.117|        11.686|       73.114|     82.712|
| MLR   | SOURCE\_EUI |            5| meanCent  |  404|    32|    32|  0.480|    0.437|  3006.293|  54.830|  35.826|  0.972|      88.133|        11.688|       73.127|     82.727|
| XGBd1 | SOURCE\_EUI |            1| None      |  404|     5|     5|  0.465|    0.459|  3073.895|  55.443|  35.551|  0.974|      89.119|        11.819|       73.945|     83.652|
| XGBd2 | SOURCE\_EUI |            2| None      |  404|     5|     5|  0.826|    0.824|  1717.801|  41.446|  24.915|  0.668|      66.620|         8.835|       55.277|     62.533|
| XGBd3 | SOURCE\_EUI |            3| None      |  404|     5|     5|  0.961|    0.961|   546.776|  23.383|  14.259|  0.406|      37.586|         4.984|       31.186|     35.280|
| XGBg1 | SOURCE\_EUI |            1| None      |  404|     5|     5|  0.374|    0.368|  3264.880|  57.139|  37.142|  1.094|      91.845|        12.180|       76.207|     86.211|
| XGBg2 | SOURCE\_EUI |            2| None      |  404|     5|     5|  0.478|    0.473|  2774.557|  52.674|  33.486|  0.939|      84.668|        11.228|       70.252|     79.474|
| XGBg3 | SOURCE\_EUI |            3| None      |  404|     5|     5|  0.556|    0.552|  2178.476|  46.674|  30.853|  0.854|      75.024|         9.949|       62.250|     70.421|
| XGBa1 | SOURCE\_EUI |            1| None      |  404|     5|     5|  0.766|    0.764|  1694.498|  41.164|  27.764|  0.808|      66.167|         8.775|       54.901|     62.108|
| XGBa2 | SOURCE\_EUI |            2| None      |  404|     5|     5|  1.000|    1.000|     0.017|   0.131|   0.078|  0.002|       0.211|         0.028|        0.175|      0.198|
| XGBa3 | SOURCE\_EUI |            3| None      |  404|     5|     5|  0.885|    0.884|   890.311|  29.838|  20.142|  0.575|      47.961|         6.360|       39.795|     45.019|
| XGBr1 | SOURCE\_EUI |            1| None      |  404|     5|     5|  1.000|    1.000|     0.008|   0.087|   0.045|  0.001|       0.140|         0.019|        0.116|      0.131|
| XGBr2 | SOURCE\_EUI |            2| None      |  404|     5|     5|  0.949|    0.948|   381.554|  19.533|  12.952|  0.375|      31.397|         4.164|       26.051|     29.471|
| XGBr3 | SOURCE\_EUI |            3| None      |  404|     5|     5|  1.000|    1.000|     0.022|   0.147|   0.086|  0.002|       0.236|         0.031|        0.196|      0.222|

#### Using SOURCE\_ENERGY as dependent variable

##### Using default search

``` r
x = sourceEnergy_attributes
y = "SOURCE_ENERGY"
w = "FINALWT"
search = "default"

xgbMetrics = XGBoost.predict(data, x, y, w, search)
```

2019-06-10 16:35:15 xgboost default SOURCE\_ENERGY 1 2019-06-10 16:35:17 xgboost default SOURCE\_ENERGY 2 2019-06-10 16:35:20 xgboost default SOURCE\_ENERGY 3

``` r
allMetrics = rbind(allMetrics, xgbMetrics)

knitr::kable(xgbMetrics, row.names = F)
```

| model | dependent      |  interaction| transform |  obs|  rank|  coef|    R.2|  Adj.R.2|           mse|      rmse|        mae|   mape|  nrmse\_iqr|  nrmse\_range|  nrmse\_mean|  nrmse\_sd|
|:------|:---------------|------------:|:----------|----:|-----:|-----:|------:|--------:|-------------:|---------:|----------:|------:|-----------:|-------------:|------------:|----------:|
| XGBd1 | SOURCE\_ENERGY |            1| None      |  404|     5|     5|  0.644|    0.641|  1.048294e+14|  10238621|  5013635.9|  1.380|     117.437|         7.169|      118.836|     61.844|
| XGBd2 | SOURCE\_ENERGY |            2| None      |  404|     6|     6|  0.960|    0.960|  1.949666e+13|   4415502|  2266657.1|  0.725|      50.646|         3.092|       51.249|     26.671|
| XGBd3 | SOURCE\_ENERGY |            3| None      |  404|     6|     6|  0.995|    0.995|  2.390148e+12|   1546010|   838758.4|  0.337|      17.733|         1.083|       17.944|      9.338|

##### Using grid search

``` r
x = sourceEnergy_attributes
y = "SOURCE_ENERGY"
w = "FINALWT"
search = "grid"

xgbMetrics = XGBoost.predict(data, x, y, w, search)
```

2019-06-10 16:35:23 xgboost grid SOURCE\_ENERGY 1 2019-06-10 16:36:09 xgboost grid SOURCE\_ENERGY 2 2019-06-10 16:37:11 xgboost grid SOURCE\_ENERGY 3

``` r
allMetrics = rbind(allMetrics, xgbMetrics)

knitr::kable(xgbMetrics, row.names = F)
```

| model | dependent      |  interaction| transform |  obs|  rank|  coef|    R.2|  Adj.R.2|           mse|      rmse|      mae|   mape|  nrmse\_iqr|  nrmse\_range|  nrmse\_mean|  nrmse\_sd|
|:------|:---------------|------------:|:----------|----:|-----:|-----:|------:|--------:|-------------:|---------:|--------:|------:|-----------:|-------------:|------------:|----------:|
| XGBg1 | SOURCE\_ENERGY |            1| None      |  404|     4|     4|  0.414|    0.409|  1.420698e+14|  11919304|  5444108|  1.677|     136.714|         8.346|      138.343|     71.996|
| XGBg2 | SOURCE\_ENERGY |            2| None      |  404|     6|     6|  0.885|    0.884|  5.255715e+13|   7249631|  3473355|  1.004|      83.153|         5.076|       84.144|     43.790|
| XGBg3 | SOURCE\_ENERGY |            3| None      |  404|     6|     6|  0.914|    0.913|  3.333815e+13|   5773920|  2944379|  0.864|      66.227|         4.043|       67.016|     34.876|

##### Using adaptive search

``` r
x = sourceEnergy_attributes
y = "SOURCE_ENERGY"
w = "FINALWT"
search = "adaptive"

xgbMetrics = XGBoost.predict(data, x, y, w, search)
```

2019-06-10 16:38:35 xgboost adaptive SOURCE\_ENERGY 1 2019-06-10 16:38:57 xgboost adaptive SOURCE\_ENERGY 2 2019-06-10 16:39:21 xgboost adaptive SOURCE\_ENERGY 3

``` r
allMetrics = rbind(allMetrics, xgbMetrics)

knitr::kable(xgbMetrics, row.names = F)
```

| model | dependent      |  interaction| transform |  obs|  rank|  coef|    R.2|  Adj.R.2|           mse|       rmse|        mae|   mape|  nrmse\_iqr|  nrmse\_range|  nrmse\_mean|  nrmse\_sd|
|:------|:---------------|------------:|:----------|----:|-----:|-----:|------:|--------:|-------------:|----------:|----------:|------:|-----------:|-------------:|------------:|----------:|
| XGBa1 | SOURCE\_ENERGY |            1| None      |  404|     6|     6|  1.000|    1.000|  1.091854e+10|   104491.8|   61305.36|  0.035|       1.199|         0.073|        1.213|      0.631|
| XGBa2 | SOURCE\_ENERGY |            2| None      |  404|     6|     6|  0.994|    0.994|  2.748174e+12|  1657761.6|  886783.47|  0.343|      19.014|         1.161|       19.241|     10.013|
| XGBa3 | SOURCE\_ENERGY |            3| None      |  404|     6|     6|  0.993|    0.993|  1.663746e+12|  1289862.6|  842614.04|  0.474|      14.795|         0.903|       14.971|      7.791|

##### Using random search

``` r
x = sourceEnergy_attributes
y = "SOURCE_ENERGY"
w = "FINALWT"
search = "random"

xgbMetrics = XGBoost.predict(data, x, y, w, search)
```

2019-06-10 16:39:51 xgboost random SOURCE\_ENERGY 1 2019-06-10 16:40:13 xgboost random SOURCE\_ENERGY 2 2019-06-10 16:40:32 xgboost random SOURCE\_ENERGY 3

``` r
allMetrics = rbind(allMetrics, xgbMetrics)

write.csv(allMetrics, 
          paste0(results_dir, building_type, ".csv"), 
          row.names = F)

allMetrics0 = allMetrics %>% filter(dependent == y)
knitr::kable(allMetrics0, row.names = F)
```

| model | dependent      |  interaction| transform |  obs|  rank|  coef|    R.2|  Adj.R.2|           mse|          rmse|          mae|   mape|  nrmse\_iqr|  nrmse\_range|  nrmse\_mean|  nrmse\_sd|
|:------|:---------------|------------:|:----------|----:|-----:|-----:|------:|--------:|-------------:|-------------:|------------:|------:|-----------:|-------------:|------------:|----------:|
| MLR   | SOURCE\_ENERGY |            1| meanCent  |  404|     7|     7|  0.510|    0.502|  1.495968e+14|  12230977.252|  5951650.641|  2.070|     140.289|         8.564|      141.960|     73.879|
| MLR   | SOURCE\_ENERGY |            2| meanCent  |  404|    22|    22|  0.715|    0.699|  1.020236e+14|  10100671.029|  4497670.365|  1.411|     115.854|         7.072|      117.234|     61.011|
| MLR   | SOURCE\_ENERGY |            3| meanCent  |  404|    42|    42|  0.810|    0.789|  7.883690e+13|   8879014.686|  4054082.540|  1.028|     101.842|         6.217|      103.055|     53.632|
| MLR   | SOURCE\_ENERGY |            4| meanCent  |  404|    57|    57|  0.846|    0.821|  7.247630e+13|   8513301.140|  3786385.203|  0.975|      97.647|         5.961|       98.810|     51.423|
| MLR   | SOURCE\_ENERGY |            5| meanCent  |  404|    63|    63|  0.851|    0.824|  7.209354e+13|   8490791.340|  3806018.981|  0.940|      97.389|         5.945|       98.549|     51.287|
| MLR   | SOURCE\_ENERGY |            6| meanCent  |  404|    64|    64|  0.852|    0.825|  7.265773e+13|   8523950.098|  3815728.765|  0.934|      97.769|         5.968|       98.934|     51.487|
| XGBd1 | SOURCE\_ENERGY |            1| None      |  404|     5|     5|  0.644|    0.641|  1.048294e+14|  10238621.121|  5013635.919|  1.380|     117.437|         7.169|      118.836|     61.844|
| XGBd2 | SOURCE\_ENERGY |            2| None      |  404|     6|     6|  0.960|    0.960|  1.949666e+13|   4415501.921|  2266657.125|  0.725|      50.646|         3.092|       51.249|     26.671|
| XGBd3 | SOURCE\_ENERGY |            3| None      |  404|     6|     6|  0.995|    0.995|  2.390148e+12|   1546010.427|   838758.384|  0.337|      17.733|         1.083|       17.944|      9.338|
| XGBg1 | SOURCE\_ENERGY |            1| None      |  404|     4|     4|  0.414|    0.409|  1.420698e+14|  11919304.268|  5444107.590|  1.677|     136.714|         8.346|      138.343|     71.996|
| XGBg2 | SOURCE\_ENERGY |            2| None      |  404|     6|     6|  0.885|    0.884|  5.255715e+13|   7249630.908|  3473354.667|  1.004|      83.153|         5.076|       84.144|     43.790|
| XGBg3 | SOURCE\_ENERGY |            3| None      |  404|     6|     6|  0.914|    0.913|  3.333815e+13|   5773919.780|  2944378.833|  0.864|      66.227|         4.043|       67.016|     34.876|
| XGBa1 | SOURCE\_ENERGY |            1| None      |  404|     6|     6|  1.000|    1.000|  1.091854e+10|    104491.832|    61305.357|  0.035|       1.199|         0.073|        1.213|      0.631|
| XGBa2 | SOURCE\_ENERGY |            2| None      |  404|     6|     6|  0.994|    0.994|  2.748174e+12|   1657761.643|   886783.471|  0.343|      19.014|         1.161|       19.241|     10.013|
| XGBa3 | SOURCE\_ENERGY |            3| None      |  404|     6|     6|  0.993|    0.993|  1.663746e+12|   1289862.630|   842614.037|  0.474|      14.795|         0.903|       14.971|      7.791|
| XGBr1 | SOURCE\_ENERGY |            1| None      |  404|     6|     6|  0.988|    0.988|  4.266727e+12|   2065605.722|  1145546.014|  0.385|      23.692|         1.446|       23.975|     12.477|
| XGBr2 | SOURCE\_ENERGY |            2| None      |  404|     6|     6|  0.999|    0.999|  8.887648e+11|    942743.256|   401515.321|  0.149|      10.813|         0.660|       10.942|      5.694|
| XGBr3 | SOURCE\_ENERGY |            3| None      |  404|     6|     6|  1.000|    1.000|  2.692300e+01|         5.189|        3.071|  0.000|       0.000|         0.000|        0.000|      0.000|

### Comparision of XGB models

``` r
plotXgbR2 <- function(df, titl) {
  
  plot <- ggplot(df, aes(x = interaction, y=Adj.R.2, 
                          group=model, col=model)) + 
    geom_point(size=2) + geom_line(size=1) +  
    ggtitle(titl) + 
    theme_pubr(base_size=12) +
    theme(legend.position="top", legend.title = element_blank()) 

  return(plot)
}

plotXgbNRMSE <- function(df, titl) {
  
  df1 = melt(df, measure.vars = c("nrmse_iqr", "nrmse_mean", 
                                        "nrmse_sd"))
  df1$variable = toupper(df1$variable)
  plot <- ggplot(df1, aes(x = interaction, y=value, 
                          group=variable, col=variable)) + 
  geom_point(size=2) + geom_line(size=1) +
    facet_wrap(. ~ model, scales = "fixed", nrow=2) + 
    ggtitle(titl) + 
    theme_pubr(base_size=12) +
    theme(legend.position="top", legend.title = element_blank()) + 
    theme(strip.placement = "outside", strip.background = element_blank())
  
  return(plot)
}
```

#### XGB plots using Source EUI

``` r
allMetrics0 = allMetrics %>%
  mutate(model = substr(model, 1, 4)) %>%
  filter(stringr::str_detect(model, "XGB")) %>%
  filter(dependent == "SOURCE_EUI")

plot1 = plotXgbR2(allMetrics0, "XGB models using source EUI")
plot2 = plotXgbNRMSE(allMetrics0, "XGB models using source EUI")

print(plot1)
```

![](warehouse_files/figure-markdown_github/unnamed-chunk-46-1.png)

``` r
print(plot2)
```

![](warehouse_files/figure-markdown_github/unnamed-chunk-46-2.png)

#### XGB plots using Source Energy

``` r
allMetrics0 = allMetrics %>%
  mutate(model = substr(model, 1, 4)) %>%
  filter(stringr::str_detect(model, "XGB")) %>%
  filter(dependent == "SOURCE_ENERGY")

plot1 = plotXgbR2(allMetrics0, "XGB models using source energy")
plot2 = plotXgbNRMSE(allMetrics0, "XGB models using source energy")

print(plot1)
```

![](warehouse_files/figure-markdown_github/unnamed-chunk-47-1.png)

``` r
print(plot2)
```

![](warehouse_files/figure-markdown_github/unnamed-chunk-47-2.png)

### Comparision between MLR and XGB models

#### MLR and XGB plots using Source EUI

``` r
allMetrics0 = allMetrics %>%
  mutate(model = substr(model, 1, 4)) %>%
  #filter(stringr::str_detect(model, "XGB")) %>%
  filter(dependent == "SOURCE_EUI")

plot1 = plotXgbR2(allMetrics0, "Comparison MLR and XGB models using source EUI")
plot2 = plotXgbNRMSE(allMetrics0, "Comparison MLR and XGB models using source EUI")

print(plot1)
```

![](warehouse_files/figure-markdown_github/unnamed-chunk-48-1.png)

``` r
print(plot2)
```

![](warehouse_files/figure-markdown_github/unnamed-chunk-48-2.png)

#### MLR and XGB plots using Source Energy

``` r
allMetrics0 = allMetrics %>%
  mutate(model = substr(model, 1, 4)) %>%
  #filter(stringr::str_detect(model, "XGB")) %>%
  filter(dependent == "SOURCE_ENERGY")

plot1 = plotXgbR2(allMetrics0, "Comparison MLR and XGB models using source energy")
plot2 = plotXgbNRMSE(allMetrics0, "Comparison MLR and XGB models using source energy")

print(plot1)
```

![](warehouse_files/figure-markdown_github/unnamed-chunk-49-1.png)

``` r
print(plot2)
```

![](warehouse_files/figure-markdown_github/unnamed-chunk-49-2.png)
