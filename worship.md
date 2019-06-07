CBECS Worship Facilities - Data filteration
================
Pandarasamy Arjunan
3 June 2019

-   [Load dataset](#load-dataset)
-   [Apply filters](#apply-filters)
-   [Prepare features](#prepare-features)
-   [Descriptive statistics](#descriptive-statistics)
    -   [Data Frame Summary](#data-frame-summary)

Load dataset
------------

``` r
library(dplyr)

save_dir1 = './data/filtered/'
dir.create(save_dir1, showWarnings = F)

save_dir2 = './data/features/'
dir.create(save_dir2, showWarnings = F)
```

``` r
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
          "CWUSED", "WOUSED", "COUSED", "SOUSED", "PRUSED", 
          "RFGSTP")

var3 = c("RWSEAT")

worships = cbecs[, c(var1, var2, var3)]
```

Apply filters
-------------

As per Energy Star's technical document [ENERGY STAR Score for Worship Facilities](https://www.energystar.gov/buildings/tools-and-resources/energy-star-score-worship-facilities), following filters are applied to define the peer group and to remove any outliers.

After applying each filter, the number of remaining buildings in the dataset (*Number Remaining: X*) and any difference (*Difference: X*) in count from the original Energy Star's technical documentation is also given.

1.  **Calculate source energy and source EUI**

    ``` r
    ## convert electricity, natural gas, fuel oil, and district heat to source energy
    w0 = worships %>% 
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

2.  **PBAPLUS = 21** <br/>Building Type Filter – CBECS defines building types according to the variable “PBAPLUS.” Religious Worship is coded as PBAPBLUS= 21. <br/>Number Remaining: 352. <br/>Difference: 0.

    ``` r
    w1 = w0 %>% filter(PBAPLUS == 21)
    ```

3.  **Must operate for at least 30 hours per week** <br/>EPA Program Filter – Baseline condition for being a full time worship facility. <br/>Number Remaining: 351. <br/>Difference: 0.

    ``` r
    w2 = w1 %>% filter(WKHRS >= 1)
    ```

4.  **Must have at least 1 seat** <br/>EPA Program Filter – Baseline condition for being a full time worship facility. <br/>Number Remaining: 351. <br/>Difference: 0.

    ``` r
    w3 = w2 %>% filter(RWSEAT >= 1)
    ```

5.  **Must operate for at least 10 months per year** <br/>EPA Program Filter – Baseline condition for being a full time worship facility. <br/>Number Remaining: 339. <br/>Difference: 0.

    ``` r
    w4 = w3 %>% filter(MONUSE >= 10)
    ```

6.  **A single activity must characterize greater than 50% of the floor space** <br/>EPA Program Filter – In order to be considered part of the worship facility peer group, more than 50% of the building must be defined as religious worship. <br/>This filter is applied by a set of screens. If the variable ONEACT=1, then one activity occupies 75% or more of the building. If the variable ONEACT=2, then the activities in the building are defined by ACT1, ACT2, and ACT3. One of these activities must be coded as religious worship (PBA=18), with a corresponding percent (ACT1PCT, ACT2PCT, ACT3PCT) that is greater than 50. <br/>Number Remaining: 329. <br/>Difference: +2.

    ``` r
    w5 = w4 %>% 
      filter( (ONEACT == 1) |
            (ONEACT == 2 & 
               ((ACT1 %in% c(18) & ACT1PCT > 50) | 
                  (ACT2 %in% c(18) & ACT2PCT > 50) | 
                  (ACT3 %in% c(18) & ACT3PCT > 50) )))
    ```

7.  **Must report energy usage** <br/>EPA Program Filter – Baseline condition for being a full time worship facility. <br/>Number Remaining: 329. <br/>Difference: +2.

    ``` r
    w6 = w5 %>% filter(!is.na(MFBTU))
    ```

8.  **Must be less than or equal to 1,000,000 square feet** <br/>Data Limitation Filter – CBECS masks surveyed properties above 1,000,000 square feet by applying regional averages. <br/>Number Remaining: 329. <br/>Difference: +2.

    ``` r
    w7 = w6 %>% filter(SQFT <= 1000000)
    ```

9.  **If propane is used, the amount category (PRAMTC) must equal 1, 2, or 3** <br/>Data Limitation Filter – Cannot estimate propane use if the quantity is “greater than 1000” or unknown. <br/>Number Remaining: 314. <br/>Difference: +2.

    ``` r
    w8 = w7 %>% filter(is.na(PRAMTC) | PRAMTC %in% c(1,2,3))
    ```

10. **If propane is used, the unit (PRUNIT) must be known** <br/>Data Limitation Filter – Cannot estimate propane use if the unit is unknown. <br/>Number Remaining: 314. <br/>Difference: +2.

    ``` r
    w9 = w8 %>% filter(is.na(PRUNIT) | PRUNIT %in% c(1,2))
    ```

11. **If propane is used, the maximum estimated propane amount must be 10% or less of the total source energy** <br/>Data Limitation Filter – Because propane values are estimated from a range, propane is restricted to 10% of the total source energy. <br/>Number Remaining: 314. <br/>Difference: +22.

    ``` r
    w10 = w9 %>% 
      filter( PRUSED == 2 | is.na(NGBTU_PERCENT) == T | 
            (PRUSED == 1 & NGBTU_PERCENT <= 10))
    ```

12. **must not use chilled water, wood, coal, or solar** <br/>Data Limitation Filter – CBECS does not collect quantities of chilled water, wood, coal, or solar. <br/>Number Remaining: 310. <br/>Difference: +22.

    ``` r
    w11 = w10 %>% 
      filter(CWUSED == 2 & WOUSED == 2 & COUSED == 2 & SOUSED == 2)
    ```

13. **If space within the building is used for food preparation, then square footage used for this purpose (FDPREPSFR) must be reported.** <br/>Data Limitation Filter – Cannot calculate percentage of square footage used for food preparation if square footage value is not reported. <br/>Number Remaining: 310. <br/>Difference: +22. <br/>**TODO: FDPREPSFR is not available in the dataset**

    ``` r
    w12 = w11 
    ```

14. **Must have no more than 250 seats per 1,000 square feet** <br/>Analytical Filter – Values determined to be statistical outliers. <br/>Number Remaining: 303. <br/>Difference: +32.

    ``` r
    w13 = w12 %>% filter(RWSEAT / SQFT * 1000 <= 250)
    ```

15. **Must have no more than 2.5 workers per 1,000 square feet** <br/>Analytical Filter – Values determined to be statistical outliers. <br/>Number Remaining: 292. <br/>Difference: +29.

    ``` r
    w14 = w13 %>% filter(NWKER  / SQFT * 1000 <= 2.5)
    ```

16. **Must not operate 168 hours/week** <br/>Analytical Filter – Values determined to be statistical outliers. <br/>Number Remaining: 279. <br/>Difference: +26.

    ``` r
    w15 = w14 %>% filter(WKHRS != 168)
    ```

17. **Must have Source EUI less than or equal to 250 kBtu/ft2** <br/>Analytical Filter – Values determined to be statistical outliers. <br/>Number Remaining: 273. <br/>Difference: +26.

    ``` r
    w16 = w15 %>% filter(SOURCE_EUI <= 250)
    ```

18. **Must have Source EUI greater than or equal to 10 kBtu/ft2** <br/>Analytical Filter – Values determined to be statistical outliers. <br/>Number Remaining: 265. <br/>Difference: +22.

    ``` r
    w17 = w16 %>% filter(SOURCE_EUI >= 10)
    ```

**Save the filtered dataset**

``` r
write.csv(w17, paste0(save_dir1, "worship.csv"), row.names = F)
```

Prepare features
----------------

The final regression equation includes the following variables: 

-   Weekly Operating Hours
-   Number of Religious Worship Seats per 1,000 Square Feet
-   Percent of Square Footage Used for Food Preparation
-   Heating Degree Days times Percent of the Building that is Heated
-   Cooling Degree Days times Percent of the Building that is Cooled

``` r
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
#summary(features)

features = features %>% na.omit()
#write.csv(features, paste0(save_dir2, "worship.csv"), row.names = F)
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

|                  |      Mean|   Std.Dev|      Min|    Median|        Max|  N.Valid|  Pct.Valid|
|-----------------:|---------:|---------:|--------:|---------:|----------:|--------:|----------:|
|  **CDD65\_COOLP**|   1474.41|    925.94|    31.89|   1348.00|    5221.00|   249.00|     100.00|
|  **HDD65\_HEATP**|   3224.50|   1836.81|   191.00|   3215.00|    7355.00|   249.00|     100.00|
|  **RWSEAT\_SQFT**|     33.72|     24.14|     2.50|     26.47|     140.00|   249.00|     100.00|
|   **SOURCE\_EUI**|     71.03|     47.54|    10.32|     58.88|     237.81|   249.00|     100.00|
|          **SQFT**|  18246.80|  21635.00|  1200.00|  11500.00|  120000.00|   249.00|     100.00|
|         **WKHRS**|     30.57|     24.98|     1.00|     20.00|     105.00|   249.00|     100.00|

``` r
dfSummary(features1, plain.ascii = FALSE, style = "grid", 
          graph.magnif = 0.75, valid.col = FALSE)
```

    text graphs are displayed; set 'tmp.img.dir' parameter to activate png graphs

### Data Frame Summary

**features1**
**Dimensions:** 249 x 6
**Duplicates:** 0

<table>
<colgroup>
<col width="4%" />
<col width="14%" />
<col width="28%" />
<col width="20%" />
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
<td><p>Mean (sd) : 18246.8 (21635)<br />
min &lt; med &lt; max:<br />
1200 &lt; 11500 &lt; 120000<br />
IQR (CV) : 18000 (1.2)</p></td>
<td><p>120 distinct values</p></td>
<td><p><br />
:<br />
:<br />
:<br />
: :<br />
: : :</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="even">
<td><p>2</p></td>
<td><p>WKHRS<br />
[numeric]</p></td>
<td><p>Mean (sd) : 30.6 (25)<br />
min &lt; med &lt; max:<br />
1 &lt; 20 &lt; 105<br />
IQR (CV) : 40 (0.8)</p></td>
<td><p>59 distinct values</p></td>
<td><dl>
<dt></dt>
<dd>.<br />

</dd>
<dd>:<br />

</dd>
<dd>:<br />

</dd>
<dd>: : . :<br />

</dd>
<dd>: : : : : : :   .
</dd>
</dl></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="odd">
<td><p>3</p></td>
<td><p>RWSEAT_SQFT<br />
[numeric]</p></td>
<td><p>Mean (sd) : 33.7 (24.1)<br />
min &lt; med &lt; max:<br />
2.5 &lt; 26.5 &lt; 140<br />
IQR (CV) : 28.9 (0.7)</p></td>
<td><p>159 distinct values</p></td>
<td><p><br />
. :<br />
: :<br />
: : .<br />
: : :<br />
: : : : .   .</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="even">
<td><p>4</p></td>
<td><p>HDD65_HEATP<br />
[numeric]</p></td>
<td><p>Mean (sd) : 3224.5 (1836.8)<br />
min &lt; med &lt; max:<br />
191 &lt; 3215 &lt; 7355<br />
IQR (CV) : 2906 (0.6)</p></td>
<td><p>247 distinct values</p></td>
<td><p><br />
  . : : :<br />
: : : : :<br />
: : : : : :<br />
: : : : : : :<br />
: : : : : : : .</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="odd">
<td><p>5</p></td>
<td><p>CDD65_COOLP<br />
[numeric]</p></td>
<td><p>Mean (sd) : 1474.4 (925.9)<br />
min &lt; med &lt; max:<br />
31.9 &lt; 1348 &lt; 5221<br />
IQR (CV) : 1125 (0.6)</p></td>
<td><p>246 distinct values</p></td>
<td><p><br />
  :<br />
  : . :<br />
. : : :<br />
: : : : .<br />
: : : : : : :</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="even">
<td><p>6</p></td>
<td><p>SOURCE_EUI<br />
[numeric]</p></td>
<td><p>Mean (sd) : 71 (47.5)<br />
min &lt; med &lt; max:<br />
10.3 &lt; 58.9 &lt; 237.8<br />
IQR (CV) : 57.3 (0.7)</p></td>
<td><p>248 distinct values</p></td>
<td><p><br />
  :<br />
: : :<br />
: : : .<br />
: : : :<br />
: : : : : : . .</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
</tbody>
</table>

**Extract R code from Rmd document**

``` r
#knitr::purl("worship.Rmd", output = "worship.R", documentation = 2)
```
