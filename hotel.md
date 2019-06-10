Benchmarking Hotels
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
building_type = "hotel"

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
          "CWUSED", "WOUSED", "COUSED", "SOUSED",  "PRUSED",
          "RFGSTP")

var3 = c( "RFGRSN", "RFGCOMPN", "RFGWIN", "RFGOPN", "RFGCLN", "RFGVNN")
var4 = c("LODGRM", "OCCUPYP", "LODOCCP")

hotels = cbecs[, c(var1, var2, var3, var4)]
```

Apply filters
-------------

As per Energy Star's technical document [ENERGY STAR Score for Hotels](https://www.energystar.gov/buildings/tools-and-resources/energy-star-score-hotels), following filters are applied to define the peer group and to remove any outliers.

After applying each filter, the number of remaining buildings in the dataset (*Number Remaining: X*) and any difference (*Difference: X*) in count from the original Energy Star's technical documentation is also given.

1.  **Calculate source energy and source EUI**

    ``` r
    ## convert electricity, natural gas, fuel oil, and district heat to source energy
    h0 = hotels %>% 
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

2.  **PBAPLUS = 38 or 39** <br/>Building Filter â€“ CBECS defines building types according to the variable â€œPBAPLUS.â€ Hotels are coded as PBAPLUS = 38 and Motels/Inns are coded as PBAPLUS = 39. <br/>Number Remaining: 220. <br/>Difference: 0.

    ``` r
    h1 = h0 %>% filter(PBAPLUS %in% c(38, 39))
    ```

3.  **Must have at least 1 room** <br/>EPA Program Filter â€“ Baseline condition for being a full time hotel. <br/>Number Remaining: 220. <br/>Difference: 0.

    ``` r
    h2 = h1 %>% filter( LODGRM >= 1)
    ```

4.  **Must operate for 168 hours per week** <br/>EPA Program Filter â€“ Baseline condition for being a full time hotel. <br/>Number Remaining: 218. <br/>Difference: 0.

    ``` r
    h3 = h2 %>% filter(WKHRS == 168)
    ```

5.  **Must have at least 1 worker** <br/>EPA Program Filter â€“ Baseline condition for being a full time hotel. <br/>Number Remaining: 215. <br/>Difference: 0.

    ``` r
    h4 = h3 %>% filter(NWKER >= 1)
    ```

6.  **Must operate for at least 10 months per year** <br/>EPA Program Filter â€“ Baseline condition for being a functioning office building. <br/>Number Remaining: 209. <br/>Difference: 0.

    ``` r
    h5 = h4 %>% filter(MONUSE >= 10)
    ```

7.  **A single activity must characterize greater than 50% of the floor space** <br/>EPA Program Filter â€“ In order to be considered part of the hotel peer group, more than 50% of the building must be defined as a hotel. <br/>This filter is applied by a set of screens. If the variable ONEACT=1, then one activity occupies 75% or more of the building. If the variable ONEACT=2, then the activities in the building are defined by ACT1, ACT2, and ACT3. One of these activities must be coded as lodging (PBAX=22), with a corresponding percent (ACT1PCT, ACT2PCT, ACT3PCT) that is greater than 50. <br/>Number Remaining: 205. <br/>Difference: +1.

    ``` r
    h6 = h5 %>% 
      filter( (ONEACT == 1) |
            (ONEACT == 2 & 
               ((ACT1 %in% c(22) & ACT1PCT > 50) | 
                  (ACT2 %in% c(22) & ACT2PCT > 50) | 
                  (ACT3 %in% c(22) & ACT3PCT > 50) )))
    ```

8.  **Must report energy usage** <br/>EPA Program Filter â€“ Baseline condition for being a functioning office building. <br/>Number Remaining: 205. <br/>Difference: +1.

    ``` r
    h7 = h6 %>% filter(!is.na(MFBTU))
    ```

9.  **Must be less than or equal to 1,000,000 square feet** <br/>Data Limitation Filter â€“ CBECS masks surveyed properties above 1,000,000 square feet by applying regional averages. <br/>Number Remaining: 197. <br/>Difference: +1.

    ``` r
    h8 = h7 %>% filter(SQFT <= 1000000)
    ```

10. **If propane is used, the amount category (PRAMTC) must equal 1, 2, or 3** <br/>Data Limitation Filter â€“ Cannot estimate propane use if the quantity is â€œgreater than 1000â€ or unknown. <br/>Number Remaining: 186. <br/>Difference: +1.

    ``` r
    h9 = h8 %>% filter(is.na(PRAMTC) | PRAMTC %in% c(1,2,3))
    ```

11. **If propane is used, the unit (PRUNIT) must be known** <br/>Data Limitation Filter â€“ Cannot estimate propane use if the unit is unknown. <br/>Number Remaining: 184. <br/>Difference: +1.

    ``` r
    h10 = h9 %>% filter(is.na(PRUNIT) | PRUNIT %in% c(1,2))
    ```

12. **If propane is used, the maximum estimated propane amount must be 10% or less of the total source energy** <br/>Data Limitation Filter â€“ Because propane values are estimated from a range, propane is restricted to 10% of the total source energy. <br/>Number Remaining: 176. <br/>Difference: -7.

    ``` r
    h11 = h10 %>% 
      filter( PRUSED == 2 | is.na(NGBTU_PERCENT) == T | 
            ( PRUSED == 1 & NGBTU_PERCENT <= 10))
    ```

13. **must not use chilled water, wood, coal, or solar** <br/>Data Limitation Filter â€“ Because propane values are estimated from a range, propane is restricted to 10% of the total source energy. <br/>Number Remaining: 171. <br/>Difference: -6.

    ``` r
    h12 = h11 %>% 
      filter(CWUSED == 2 & WOUSED == 2 & COUSED == 2 & SOUSED == 2)
    ```

14. **Must be at least 5,000 square feet** <br/>Data Limitation Filter â€“ Because propane values are estimated from a range, propane is restricted to 10% of the total source energy. <br/>Number Remaining: 162. <br/>Difference: -7.

    ``` r
    h13 = h12 %>% filter(SQFT >= 5000)
    ```

15. **Must have Source EUI less than or equal to 400 kBtu/ft2** <br/>Data Limitation Filter â€“ Because propane values are estimated from a range, propane is restricted to 10% of the total source energy. <br/>Number Remaining: 156. <br/>Difference: -7.

    ``` r
    h14 = h13 %>% filter(SOURCE_EUI <= 400)
    ```

16. **Must have no more than 4 rooms per 1,000 square feet** <br/>Data Limitation Filter â€“ Because propane values are estimated from a range, propane is restricted to 10% of the total source energy. <br/>Number Remaining: 146. <br/>Difference: -6.

    ``` r
    h15 = h14 %>% filter(LODGRM/SQFT *1000 <= 4)
    ```

17. **Must have average occupancy greater than 40%** <br/>Data Limitation Filter â€“ Because propane values are estimated from a range, propane is restricted to 10% of the total source energy. <br/>Number Remaining: 133. <br/>Difference: -6.

    ``` r
    h16 = h15 %>% filter(LODOCCP > 40)
    ```

**Save the filtered dataset**

``` r
write.csv(h16, paste0(filtered_dir, building_type, ".csv"), row.names = F)
```

Prepare features
----------------

The final regression equation includes the following variables:

-   Number of Guest Rooms per 1,000 Square Feet
-   Number of Workers per 1,000 Square Feet
-   Number of Commercial Refrigeration/Freezer Units (walk-in, open, and closed) per 1,000 Square Feet
-   Heating Degree Days times Percent of the Building that is Heated
-   Cooling Degree Days times Percent of the Building that is Cooled
-   Presence of a Commercial/Large Kitchen (1 = yes, 0 = no)

``` r
hotel = read.csv(paste0(filtered_dir, building_type, ".csv"))

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

    Non-numerical variable(s) ignored: Kitchen

|                  |       Mean|    Std.Dev|      Min|     Median|        Max|  N.Valid|  Pct.Valid|
|-----------------:|----------:|----------:|--------:|----------:|----------:|--------:|----------:|
|  **CDD65\_COOLP**|    1731.33|     983.44|    77.00|    1508.00|    4608.00|    98.00|     100.00|
|  **HDD65\_HEATP**|    3397.90|    2049.32|    76.00|    3267.00|    8180.00|    98.00|     100.00|
|  **LODGRM\_SQFT**|       1.38|       0.51|     0.44|       1.33|       2.96|    98.00|     100.00|
|   **NWKER\_SQFT**|       0.38|       0.33|     0.07|       0.28|       2.00|    98.00|     100.00|
|     **RFG\_SQFT**|       0.76|       0.75|     0.01|       0.46|       3.33|    98.00|     100.00|
|   **SOURCE\_EUI**|     183.25|      71.24|    60.80|     172.32|     381.12|    98.00|     100.00|
|          **SQFT**|  217646.99|  200186.47|  7900.00|  132000.00|  900000.00|    98.00|     100.00|

``` r
dfSummary(features1, plain.ascii = FALSE, style = "grid", 
          graph.magnif = 0.75, valid.col = FALSE)
```

    text graphs are displayed; set 'tmp.img.dir' parameter to activate png graphs

### Data Frame Summary

**features1**
**Dimensions:** 98 x 8
**Duplicates:** 0

<table>
<colgroup>
<col width="4%" />
<col width="13%" />
<col width="30%" />
<col width="19%" />
<col width="22%" />
<col width="9%" />
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
<td><p>SQFT<br />
[numeric]</p></td>
<td><p>Mean (sd) : 217647 (200186.5)<br />
min &lt; med &lt; max:<br />
7900 &lt; 132000 &lt; 9e+05<br />
IQR (CV) : 222250 (0.9)</p></td>
<td><p>65 distinct values</p></td>
<td><dl>
<dt></dt>
<dd>.<br />

</dd>
<dd>:<br />

</dd>
<dd>:<br />

</dd>
<dd>: :<br />

</dd>
<dd>: : : : : .
</dd>
</dl></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="even">
<td><p>2</p></td>
<td><p>LODGRM_SQFT<br />
[numeric]</p></td>
<td><p>Mean (sd) : 1.4 (0.5)<br />
min &lt; med &lt; max:<br />
0.4 &lt; 1.3 &lt; 3<br />
IQR (CV) : 0.7 (0.4)</p></td>
<td><p>81 distinct values</p></td>
<td><p><br />
    :<br />
    : .<br />
  : : :<br />
  : : :<br />
. : : : : .</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="odd">
<td><p>3</p></td>
<td><p>NWKER_SQFT<br />
[numeric]</p></td>
<td><p>Mean (sd) : 0.4 (0.3)<br />
min &lt; med &lt; max:<br />
0.1 &lt; 0.3 &lt; 2<br />
IQR (CV) : 0.3 (0.9)</p></td>
<td><p>85 distinct values</p></td>
<td><p><br />
:<br />
:<br />
: :<br />
: : .<br />
: : : .</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="even">
<td><p>4</p></td>
<td><p>RFG_SQFT<br />
[numeric]</p></td>
<td><p>Mean (sd) : 0.8 (0.8)<br />
min &lt; med &lt; max:<br />
0 &lt; 0.5 &lt; 3.3<br />
IQR (CV) : 1.2 (1)</p></td>
<td><p>92 distinct values</p></td>
<td><p><br />
:<br />
:<br />
:<br />
:   . .<br />
: : : : .</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="odd">
<td><p>5</p></td>
<td><p>HDD65_HEATP<br />
[numeric]</p></td>
<td><p>Mean (sd) : 3397.9 (2049.3)<br />
min &lt; med &lt; max:<br />
76 &lt; 3267 &lt; 8180<br />
IQR (CV) : 2935.8 (0.6)</p></td>
<td><p>97 distinct values</p></td>
<td><p><br />
    :<br />
  . : : :<br />
: : : : : :<br />
: : : : : :<br />
: : : : : : : :</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="even">
<td><p>6</p></td>
<td><p>CDD65_COOLP<br />
[numeric]</p></td>
<td><p>Mean (sd) : 1731.3 (983.4)<br />
min &lt; med &lt; max:<br />
77 &lt; 1508 &lt; 4608<br />
IQR (CV) : 1125.4 (0.6)</p></td>
<td><p>98 distinct values</p></td>
<td><p><br />
    . :<br />
  : : :<br />
  : : :<br />
  : : : .     .<br />
: : : : : : : : . .</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="odd">
<td><p>7</p></td>
<td><p>Kitchen<br />
[character]</p></td>
<td><p>1. No<br />
2. Yes</p></td>
<td><p>42 (42.9%)<br />
56 (57.1%)</p></td>
<td><p>IIIIIIII<br />
IIIIIIIIIII</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="even">
<td><p>8</p></td>
<td><p>SOURCE_EUI<br />
[numeric]</p></td>
<td><p>Mean (sd) : 183.3 (71.2)<br />
min &lt; med &lt; max:<br />
60.8 &lt; 172.3 &lt; 381.1<br />
IQR (CV) : 109.4 (0.4)</p></td>
<td><p>98 distinct values</p></td>
<td><p><br />
  : :<br />
  : :<br />
  : : : :<br />
: : : : :<br />
: : : : : . .</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
</tbody>
</table>

**Extract R code from Rmd document**

``` r
#knitr::purl("office.Rmd", output = "office.R", documentation = 2)
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

|                |
|:---------------|
| SQFT           |
| LODGRM\_SQFT   |
| NWKER\_SQFT    |
| RFG\_SQFT      |
| HDD65\_HEATP   |
| CDD65\_COOLP   |
| Kitchen        |
| SOURCE\_EUI    |
| SOURCE\_ENERGY |
| FINALWT        |

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
| MLR   | SOURCE\_EUI |            1| meanCent  |   98|     7|     7|  0.519|    0.487|  3225.436|  56.793|  43.441|  0.242|       51.91|         17.73|       30.992|     79.723|

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
| MLR   | SOURCE\_EUI    |            1| meanCent  |   98|     7|     7|  0.519|    0.487|  3.225436e+03|        56.793|       43.441|  0.242|      51.910|        17.730|       30.992|     79.723|
| MLR   | SOURCE\_ENERGY |            1| meanCent  |   98|     8|     8|  0.895|    0.887|  1.803708e+14|  13430219.878|  9080424.454|  0.383|      27.788|         9.234|       33.515|     35.980|

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
| MLR   | SOURCE\_EUI |            1| meanCent  |   98|     7|     7|  0.519|    0.487|  3225.436|  56.793|  43.441|  0.242|      51.910|        17.730|       30.992|     79.723|
| MLR   | SOURCE\_EUI |            2| meanCent  |   98|    22|    22|  0.620|    0.515|  3037.103|  55.110|  40.682|  0.229|      50.371|        17.205|       30.073|     77.360|
| MLR   | SOURCE\_EUI |            3| meanCent  |   98|    42|    42|  0.797|    0.648|  2346.616|  48.442|  34.816|  0.200|      44.277|        15.123|       26.435|     68.000|
| MLR   | SOURCE\_EUI |            4| meanCent  |   98|    57|    57|  0.874|    0.703|  2006.153|  44.790|  30.259|  0.177|      40.939|        13.983|       24.442|     62.873|
| MLR   | SOURCE\_EUI |            5| meanCent  |   98|    63|    63|  0.895|    0.708|  1702.391|  41.260|  28.187|  0.165|      37.712|        12.881|       22.515|     57.918|
| MLR   | SOURCE\_EUI |            6| meanCent  |   98|    64|    64|  0.895|    0.699|  1698.416|  41.212|  28.204|  0.165|      37.668|        12.866|       22.489|     57.851|

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
| MLR   | SOURCE\_ENERGY |            1| meanCent  |   98|     8|     8|  0.895|    0.887|  1.803708e+14|  13430220|  9080424|  0.383|      27.788|         9.234|       33.515|     35.980|
| MLR   | SOURCE\_ENERGY |            2| meanCent  |   98|    29|    29|  0.953|    0.934|  1.293115e+14|  11371523|  7485864|  0.237|      23.528|         7.819|       28.378|     30.465|
| MLR   | SOURCE\_ENERGY |            3| meanCent  |   98|    64|    64|  0.977|    0.934|  5.039182e+13|   7098720|  4217495|  0.149|      14.688|         4.881|       17.715|     19.018|
| MLR   | SOURCE\_ENERGY |            4| meanCent  |   98|    98|    99|  1.000|      NaN|  0.000000e+00|         0|        0|  0.000|       0.000|         0.000|        0.000|      0.000|
| MLR   | SOURCE\_ENERGY |            5| meanCent  |   98|    98|   120|  1.000|      NaN|  0.000000e+00|         0|        0|  0.000|       0.000|         0.000|        0.000|      0.000|
| MLR   | SOURCE\_ENERGY |            6| meanCent  |   98|    98|   127|  1.000|      NaN|  0.000000e+00|         0|        0|  0.000|       0.000|         0.000|        0.000|      0.000|
| MLR   | SOURCE\_ENERGY |            7| meanCent  |   98|    98|   128|  1.000|      NaN|  0.000000e+00|         0|        0|  0.000|       0.000|         0.000|        0.000|      0.000|

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

![](hotel_files/figure-markdown_github/unnamed-chunk-358-1.png)

``` r
print(plot2)
```

![](hotel_files/figure-markdown_github/unnamed-chunk-358-2.png)

#### MLR plots using Source Energy

``` r
allMetrics0 = allMetrics %>%
  filter(stringr::str_detect(model, "MLR")) %>%
  filter(dependent == "SOURCE_ENERGY")

plot1 = plotR2(allMetrics0, "MLR models using source energy")
plot2 = plotNRMSE(allMetrics0, "MLR models using source energy")

print(plot1)
```

    Warning: Removed 4 rows containing missing values (geom_point).

    Warning: Removed 4 rows containing missing values (geom_path).

![](hotel_files/figure-markdown_github/unnamed-chunk-359-1.png)

``` r
print(plot2)
```

![](hotel_files/figure-markdown_github/unnamed-chunk-359-2.png)

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

2019-06-10 15:12:39 xgboost default SOURCE\_EUI 1 2019-06-10 15:12:55 xgboost default SOURCE\_EUI 2 2019-06-10 15:12:58 xgboost default SOURCE\_EUI 3

``` r
allMetrics = rbind(allMetrics, xgbMetrics)

knitr::kable(xgbMetrics, row.names = F)
```

| model | dependent   |  interaction| transform |  obs|  rank|  coef|    R.2|  Adj.R.2|       mse|    rmse|     mae|   mape|  nrmse\_iqr|  nrmse\_range|  nrmse\_mean|  nrmse\_sd|
|:------|:------------|------------:|:----------|----:|-----:|-----:|------:|--------:|---------:|-------:|-------:|------:|-----------:|-------------:|------------:|----------:|
| XGBd1 | SOURCE\_EUI |            1| None      |   98|     6|     6|  0.811|    0.801|  1690.512|  41.116|  28.498|  0.160|      37.581|        12.836|       22.437|     57.716|
| XGBd2 | SOURCE\_EUI |            2| None      |   98|     6|     6|  0.990|    0.990|   238.550|  15.445|   9.238|  0.050|      14.117|         4.822|        8.428|     21.681|
| XGBd3 | SOURCE\_EUI |            3| None      |   98|     6|     6|  1.000|    1.000|     9.348|   3.057|   1.637|  0.009|       2.794|         0.954|        1.668|      4.291|

##### Using grid search

``` r
x = sourceEUI_attributes
y = "SOURCE_EUI"
w = "FINALWT"
search = "grid"

xgbMetrics = XGBoost.predict(data, x, y, w, search)
```

2019-06-10 15:13:01 xgboost grid SOURCE\_EUI 1 2019-06-10 15:13:36 xgboost grid SOURCE\_EUI 2 2019-06-10 15:14:33 xgboost grid SOURCE\_EUI 3

``` r
allMetrics = rbind(allMetrics, xgbMetrics)

knitr::kable(xgbMetrics, row.names = F)
```

| model | dependent   |  interaction| transform |  obs|  rank|  coef|    R.2|  Adj.R.2|       mse|    rmse|     mae|   mape|  nrmse\_iqr|  nrmse\_range|  nrmse\_mean|  nrmse\_sd|
|:------|:------------|------------:|:----------|----:|-----:|-----:|------:|--------:|---------:|-------:|-------:|------:|-----------:|-------------:|------------:|----------:|
| XGBg1 | SOURCE\_EUI |            1| None      |   98|     6|     6|  0.845|    0.836|  1576.696|  39.708|  27.577|  0.157|      36.294|        12.396|       21.668|     55.740|
| XGBg2 | SOURCE\_EUI |            2| None      |   98|     6|     6|  0.946|    0.943|   927.479|  30.455|  20.277|  0.112|      27.836|         9.508|       16.619|     42.751|
| XGBg3 | SOURCE\_EUI |            3| None      |   98|     6|     6|  0.969|    0.967|   570.730|  23.890|  15.395|  0.087|      21.836|         7.458|       13.037|     33.535|

##### Using adaptive search

``` r
x = sourceEUI_attributes
y = "SOURCE_EUI"
w = "FINALWT"
search = "adaptive"

xgbMetrics = XGBoost.predict(data, x, y, w, search)
```

2019-06-10 15:15:42 xgboost adaptive SOURCE\_EUI 1 2019-06-10 15:16:09 xgboost adaptive SOURCE\_EUI 2 2019-06-10 15:16:49 xgboost adaptive SOURCE\_EUI 3

``` r
allMetrics = rbind(allMetrics, xgbMetrics)

knitr::kable(xgbMetrics, row.names = F)
```

| model | dependent   |  interaction| transform |  obs|  rank|  coef|    R.2|  Adj.R.2|       mse|    rmse|     mae|   mape|  nrmse\_iqr|  nrmse\_range|  nrmse\_mean|  nrmse\_sd|
|:------|:------------|------------:|:----------|----:|-----:|-----:|------:|--------:|---------:|-------:|-------:|------:|-----------:|-------------:|------------:|----------:|
| XGBa1 | SOURCE\_EUI |            1| None      |   98|     6|     6|  0.921|    0.917|  1141.108|  33.780|  21.710|  0.123|      30.875|        10.546|       18.434|     47.418|
| XGBa2 | SOURCE\_EUI |            2| None      |   98|     6|     6|  1.000|    1.000|     0.019|   0.137|   0.094|  0.001|       0.125|         0.043|        0.075|      0.192|
| XGBa3 | SOURCE\_EUI |            3| None      |   98|     6|     6|  0.985|    0.985|    75.619|   8.696|   6.042|  0.039|       7.948|         2.715|        4.745|     12.207|

##### Using random search

``` r
x = sourceEUI_attributes
y = "SOURCE_EUI"
w = "FINALWT"
search = "random"

xgbMetrics = XGBoost.predict(data, x, y, w, search)
```

2019-06-10 15:17:12 xgboost random SOURCE\_EUI 1 2019-06-10 15:17:24 xgboost random SOURCE\_EUI 2 2019-06-10 15:18:15 xgboost random SOURCE\_EUI 3

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
| MLR   | SOURCE\_EUI |            1| meanCent  |   98|     7|     7|  0.519|    0.487|  3225.436|  56.793|  43.441|  0.242|      51.910|        17.730|       30.992|     79.723|
| MLR   | SOURCE\_EUI |            2| meanCent  |   98|    22|    22|  0.620|    0.515|  3037.103|  55.110|  40.682|  0.229|      50.371|        17.205|       30.073|     77.360|
| MLR   | SOURCE\_EUI |            3| meanCent  |   98|    42|    42|  0.797|    0.648|  2346.616|  48.442|  34.816|  0.200|      44.277|        15.123|       26.435|     68.000|
| MLR   | SOURCE\_EUI |            4| meanCent  |   98|    57|    57|  0.874|    0.703|  2006.153|  44.790|  30.259|  0.177|      40.939|        13.983|       24.442|     62.873|
| MLR   | SOURCE\_EUI |            5| meanCent  |   98|    63|    63|  0.895|    0.708|  1702.391|  41.260|  28.187|  0.165|      37.712|        12.881|       22.515|     57.918|
| MLR   | SOURCE\_EUI |            6| meanCent  |   98|    64|    64|  0.895|    0.699|  1698.416|  41.212|  28.204|  0.165|      37.668|        12.866|       22.489|     57.851|
| XGBd1 | SOURCE\_EUI |            1| None      |   98|     6|     6|  0.811|    0.801|  1690.512|  41.116|  28.498|  0.160|      37.581|        12.836|       22.437|     57.716|
| XGBd2 | SOURCE\_EUI |            2| None      |   98|     6|     6|  0.990|    0.990|   238.550|  15.445|   9.238|  0.050|      14.117|         4.822|        8.428|     21.681|
| XGBd3 | SOURCE\_EUI |            3| None      |   98|     6|     6|  1.000|    1.000|     9.348|   3.057|   1.637|  0.009|       2.794|         0.954|        1.668|      4.291|
| XGBg1 | SOURCE\_EUI |            1| None      |   98|     6|     6|  0.845|    0.836|  1576.696|  39.708|  27.577|  0.157|      36.294|        12.396|       21.668|     55.740|
| XGBg2 | SOURCE\_EUI |            2| None      |   98|     6|     6|  0.946|    0.943|   927.479|  30.455|  20.277|  0.112|      27.836|         9.508|       16.619|     42.751|
| XGBg3 | SOURCE\_EUI |            3| None      |   98|     6|     6|  0.969|    0.967|   570.730|  23.890|  15.395|  0.087|      21.836|         7.458|       13.037|     33.535|
| XGBa1 | SOURCE\_EUI |            1| None      |   98|     6|     6|  0.921|    0.917|  1141.108|  33.780|  21.710|  0.123|      30.875|        10.546|       18.434|     47.418|
| XGBa2 | SOURCE\_EUI |            2| None      |   98|     6|     6|  1.000|    1.000|     0.019|   0.137|   0.094|  0.001|       0.125|         0.043|        0.075|      0.192|
| XGBa3 | SOURCE\_EUI |            3| None      |   98|     6|     6|  0.985|    0.985|    75.619|   8.696|   6.042|  0.039|       7.948|         2.715|        4.745|     12.207|
| XGBr1 | SOURCE\_EUI |            1| None      |   98|     6|     6|  0.963|    0.961|    95.891|   9.792|   7.250|  0.047|       8.950|         3.057|        5.343|     13.745|
| XGBr2 | SOURCE\_EUI |            2| None      |   98|     6|     6|  0.854|    0.846|  1519.672|  38.983|  26.680|  0.150|      35.631|        12.170|       21.273|     54.722|
| XGBr3 | SOURCE\_EUI |            3| None      |   98|     6|     6|  0.975|    0.974|   471.710|  21.719|  13.262|  0.076|      19.851|         6.780|       11.852|     30.488|

#### Using SOURCE\_ENERGY as dependent variable

##### Using default search

``` r
x = sourceEnergy_attributes
y = "SOURCE_ENERGY"
w = "FINALWT"
search = "default"

xgbMetrics = XGBoost.predict(data, x, y, w, search)
```

2019-06-10 15:19:14 xgboost default SOURCE\_ENERGY 1 2019-06-10 15:19:16 xgboost default SOURCE\_ENERGY 2 2019-06-10 15:19:19 xgboost default SOURCE\_ENERGY 3

``` r
allMetrics = rbind(allMetrics, xgbMetrics)

knitr::kable(xgbMetrics, row.names = F)
```

| model | dependent      |  interaction| transform |  obs|  rank|  coef|    R.2|  Adj.R.2|           mse|        rmse|        mae|   mape|  nrmse\_iqr|  nrmse\_range|  nrmse\_mean|  nrmse\_sd|
|:------|:---------------|------------:|:----------|----:|-----:|-----:|------:|--------:|-------------:|-----------:|----------:|------:|-----------:|-------------:|------------:|----------:|
| XGBd1 | SOURCE\_ENERGY |            1| None      |   98|     7|     7|  0.957|    0.954|  1.137787e+14|  10666710.8|  6782657.4|  0.217|      22.070|         7.334|       26.619|     28.576|
| XGBd2 | SOURCE\_ENERGY |            2| None      |   98|     7|     7|  0.998|    0.998|  8.479356e+12|   2911933.4|  1684005.8|  0.061|       6.025|         2.002|        7.267|      7.801|
| XGBd3 | SOURCE\_ENERGY |            3| None      |   98|     7|     7|  1.000|    1.000|  2.345787e+11|    484333.3|   263625.9|  0.009|       1.002|         0.333|        1.209|      1.298|

##### Using grid search

``` r
x = sourceEnergy_attributes
y = "SOURCE_ENERGY"
w = "FINALWT"
search = "grid"

xgbMetrics = XGBoost.predict(data, x, y, w, search)
```

2019-06-10 15:19:22 xgboost grid SOURCE\_ENERGY 1 2019-06-10 15:20:03 xgboost grid SOURCE\_ENERGY 2 2019-06-10 15:21:04 xgboost grid SOURCE\_ENERGY 3

``` r
allMetrics = rbind(allMetrics, xgbMetrics)

knitr::kable(xgbMetrics, row.names = F)
```

| model | dependent      |  interaction| transform |  obs|  rank|  coef|    R.2|  Adj.R.2|           mse|      rmse|      mae|   mape|  nrmse\_iqr|  nrmse\_range|  nrmse\_mean|  nrmse\_sd|
|:------|:---------------|------------:|:----------|----:|-----:|-----:|------:|--------:|-------------:|---------:|--------:|------:|-----------:|-------------:|------------:|----------:|
| XGBg1 | SOURCE\_ENERGY |            1| None      |   98|     7|     7|  0.957|    0.954|  1.006812e+14|  10034003|  6556843|  0.261|      20.761|         6.899|       25.040|     26.881|
| XGBg2 | SOURCE\_ENERGY |            2| None      |   98|     7|     7|  0.981|    0.980|  3.412971e+13|   5842064|  4326996|  0.219|      12.088|         4.017|       14.579|     15.651|
| XGBg3 | SOURCE\_ENERGY |            3| None      |   98|     7|     7|  0.999|    0.999|  3.932741e+12|   1983114|  1247867|  0.044|       4.103|         1.364|        4.949|      5.313|

##### Using adaptive search

``` r
x = sourceEnergy_attributes
y = "SOURCE_ENERGY"
w = "FINALWT"
search = "adaptive"

xgbMetrics = XGBoost.predict(data, x, y, w, search)
```

2019-06-10 15:22:16 xgboost adaptive SOURCE\_ENERGY 1 2019-06-10 15:22:40 xgboost adaptive SOURCE\_ENERGY 2 2019-06-10 15:22:52 xgboost adaptive SOURCE\_ENERGY 3

``` r
allMetrics = rbind(allMetrics, xgbMetrics)

knitr::kable(xgbMetrics, row.names = F)
```

| model | dependent      |  interaction| transform |  obs|  rank|  coef|    R.2|  Adj.R.2|           mse|          rmse|          mae|   mape|  nrmse\_iqr|  nrmse\_range|  nrmse\_mean|  nrmse\_sd|
|:------|:---------------|------------:|:----------|----:|-----:|-----:|------:|--------:|-------------:|-------------:|------------:|------:|-----------:|-------------:|------------:|----------:|
| XGBa1 | SOURCE\_ENERGY |            1| None      |   98|     7|     7|  0.951|    0.948|  7.903738e+13|   8890297.048|  5639783.269|  0.266|      18.395|         6.113|       22.186|     23.817|
| XGBa2 | SOURCE\_ENERGY |            2| None      |   98|     7|     7|  0.959|    0.956|  1.027859e+14|  10138338.489|  6394248.818|  0.267|      20.977|         6.971|       25.300|     27.161|
| XGBa3 | SOURCE\_ENERGY |            3| None      |   98|     7|     7|  1.000|    1.000|  3.775100e+01|         6.144|        4.013|  0.000|       0.000|         0.000|        0.000|      0.000|

##### Using random search

``` r
x = sourceEnergy_attributes
y = "SOURCE_ENERGY"
w = "FINALWT"
search = "random"

xgbMetrics = XGBoost.predict(data, x, y, w, search)
```

2019-06-10 15:23:10 xgboost random SOURCE\_ENERGY 1 2019-06-10 15:24:04 xgboost random SOURCE\_ENERGY 2 2019-06-10 15:24:59 xgboost random SOURCE\_ENERGY 3

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
| MLR   | SOURCE\_ENERGY |            1| meanCent  |   98|     8|     8|  0.895|    0.887|  1.803708e+14|  13430219.878|  9080424.454|  0.383|      27.788|         9.234|       33.515|     35.980|
| MLR   | SOURCE\_ENERGY |            2| meanCent  |   98|    29|    29|  0.953|    0.934|  1.293115e+14|  11371523.147|  7485864.471|  0.237|      23.528|         7.819|       28.378|     30.465|
| MLR   | SOURCE\_ENERGY |            3| meanCent  |   98|    64|    64|  0.977|    0.934|  5.039182e+13|   7098719.730|  4217495.354|  0.149|      14.688|         4.881|       17.715|     19.018|
| MLR   | SOURCE\_ENERGY |            4| meanCent  |   98|    98|    99|  1.000|      NaN|  0.000000e+00|         0.000|        0.000|  0.000|       0.000|         0.000|        0.000|      0.000|
| MLR   | SOURCE\_ENERGY |            5| meanCent  |   98|    98|   120|  1.000|      NaN|  0.000000e+00|         0.000|        0.000|  0.000|       0.000|         0.000|        0.000|      0.000|
| MLR   | SOURCE\_ENERGY |            6| meanCent  |   98|    98|   127|  1.000|      NaN|  0.000000e+00|         0.000|        0.000|  0.000|       0.000|         0.000|        0.000|      0.000|
| MLR   | SOURCE\_ENERGY |            7| meanCent  |   98|    98|   128|  1.000|      NaN|  0.000000e+00|         0.000|        0.000|  0.000|       0.000|         0.000|        0.000|      0.000|
| XGBd1 | SOURCE\_ENERGY |            1| None      |   98|     7|     7|  0.957|    0.954|  1.137787e+14|  10666710.789|  6782657.395|  0.217|      22.070|         7.334|       26.619|     28.576|
| XGBd2 | SOURCE\_ENERGY |            2| None      |   98|     7|     7|  0.998|    0.998|  8.479356e+12|   2911933.406|  1684005.844|  0.061|       6.025|         2.002|        7.267|      7.801|
| XGBd3 | SOURCE\_ENERGY |            3| None      |   98|     7|     7|  1.000|    1.000|  2.345787e+11|    484333.273|   263625.922|  0.009|       1.002|         0.333|        1.209|      1.298|
| XGBg1 | SOURCE\_ENERGY |            1| None      |   98|     7|     7|  0.957|    0.954|  1.006812e+14|  10034002.895|  6556843.155|  0.261|      20.761|         6.899|       25.040|     26.881|
| XGBg2 | SOURCE\_ENERGY |            2| None      |   98|     7|     7|  0.981|    0.980|  3.412971e+13|   5842063.790|  4326995.890|  0.219|      12.088|         4.017|       14.579|     15.651|
| XGBg3 | SOURCE\_ENERGY |            3| None      |   98|     7|     7|  0.999|    0.999|  3.932741e+12|   1983114.049|  1247867.311|  0.044|       4.103|         1.364|        4.949|      5.313|
| XGBa1 | SOURCE\_ENERGY |            1| None      |   98|     7|     7|  0.951|    0.948|  7.903738e+13|   8890297.048|  5639783.269|  0.266|      18.395|         6.113|       22.186|     23.817|
| XGBa2 | SOURCE\_ENERGY |            2| None      |   98|     7|     7|  0.959|    0.956|  1.027859e+14|  10138338.489|  6394248.818|  0.267|      20.977|         6.971|       25.300|     27.161|
| XGBa3 | SOURCE\_ENERGY |            3| None      |   98|     7|     7|  1.000|    1.000|  3.775100e+01|         6.144|        4.013|  0.000|       0.000|         0.000|        0.000|      0.000|
| XGBr1 | SOURCE\_ENERGY |            1| None      |   98|     7|     7|  1.000|    1.000|  1.712140e+02|        13.085|        8.245|  0.000|       0.000|         0.000|        0.000|      0.000|
| XGBr2 | SOURCE\_ENERGY |            2| None      |   98|     7|     7|  1.000|    1.000|  1.936149e+08|     13914.558|     9067.557|  0.000|       0.029|         0.010|        0.035|      0.037|
| XGBr3 | SOURCE\_ENERGY |            3| None      |   98|     7|     7|  0.985|    0.984|  3.229860e+13|   5683185.490|  3965112.052|  0.180|      11.759|         3.908|       14.182|     15.225|

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

![](hotel_files/figure-markdown_github/unnamed-chunk-372-1.png)

``` r
print(plot2)
```

![](hotel_files/figure-markdown_github/unnamed-chunk-372-2.png)

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

![](hotel_files/figure-markdown_github/unnamed-chunk-373-1.png)

``` r
print(plot2)
```

![](hotel_files/figure-markdown_github/unnamed-chunk-373-2.png)

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

![](hotel_files/figure-markdown_github/unnamed-chunk-374-1.png)

``` r
print(plot2)
```

![](hotel_files/figure-markdown_github/unnamed-chunk-374-2.png)

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

    Warning: Removed 4 rows containing missing values (geom_point).

    Warning: Removed 4 rows containing missing values (geom_path).

![](hotel_files/figure-markdown_github/unnamed-chunk-375-1.png)

``` r
print(plot2)
```

![](hotel_files/figure-markdown_github/unnamed-chunk-375-2.png)
