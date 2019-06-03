CBECS - Filtering offices
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
library(readr)
library(dplyr)

save_dir1 = './data/filtered/'
dir.create(save_dir1, showWarnings = F)

save_dir2 = './data/features/'
dir.create(save_dir2, showWarnings = F)

save_dir3 = './data/features_all/'
dir.create(save_dir3, showWarnings = F)
```

``` r
cbecs = read.csv("data/2012_public_use_data_aug2016.csv")

## list of building attributes relevant to office buildings 
columns = c( 'PBAPLUS', 'PBA', 'FINALWT', 
             'MFBTU', 'ELBTU', 'NGBTU', 'FKBTU', 'DHBTU',
             'ONEACT', 'ACT1', 'ACT2', 'ACT3', 'ACT1PCT', 'ACT2PCT', 'ACT3PCT',
             'PRAMTC', 'PRUNIT',
             'CWUSED', 'WOUSED', 'COUSED', 'SOUSED', 'PRUSED',
             'SQFT', 'NFLOOR', 'NELVTR', 'NESLTR', 'COURT', 
             'MONUSE', 'OPNWE',  'WKHRS', 'NWKER', 'COOK', 
             'MANU', 'HEATP',  'COOLP',  'SNACK', 'FASTFD', 'CAF',
             'FDPREP', 'KITCHN', 'BREAKRM', 'OTFDRM', 'LABEQP', 'MCHEQP',
             'POOL', 'HTPOOL', 'RFGWIN', 'RFGOPN', 'RFGCLN', 'RFGVNN',
             'RFGICN', 'PCTERMN', 'LAPTPN', 'PRNTRN', 'SERVERN', 'TVVIDEON',
             'RGSTRN', 'COPIERN', 'HDD65','CDD65')

offices = cbecs[, columns]
```

Apply filters
-------------

As per Energy Star's technical document [ENERGY STAR Score for Offices](https://www.energystar.gov/buildings/tools-and-resources/energy-star-score-offices), following filters applied to define the peer group and to remove any outliers.

After applying each filter, the number of remaining buildings in the dataset and any difference in count from the original technical documentation is also given.

1.  **Calculate source energy and source EUI**

    ``` r
    ## convert electricity, natural gas, fuel oil, and district heat to source energy
    o0 = offices %>% 
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

2.  **PBAPLUS = 2, 3, 4 or 52** <br/>Building Type Filter – CBECS defines building types according to the variable “PBAPLUS.” Offices are coded as PBAPLUS=2 and 4; Bank/Financial Institutions are coded as PBAPLUS=3; and Courthouses are coded as PBAPLUS=52. <br/>Number Remaining: 1076.

    ``` r
    o1 = o0 %>% filter(PBAPLUS %in% c(2, 3, 4, 52))
    ```

3.  **Must have at least 1 computer** <br/>EPA Program Filter – Baseline condition for being a functioning office building. <br/>Number Remaining: 1072.

    ``` r
    o2 = o1 %>% 
      mutate(PC_TOT = rowSums(dplyr::select(., c(PCTERMN,SERVERN,LAPTPN)), na.rm = T)) %>% 
      filter(PC_TOT >= 1)
    ```

4.  **Must have at least 1 worker** <br/>EPA Program Filter – Baseline condition for being a full time office building. <br/>Number Remaining: 1072.

    ``` r
    o3 = o2 %>% filter(NWKER >= 1)
    ```

5.  **Must operate for at least 30 hours per week** <br/>EPA Program Filter – Baseline condition for being a full time office building. <br/>Number Remaining: 1065.

    ``` r
    o4 = o3 %>% filter(WKHRS >= 30)
    ```

6.  **Must operate for at least 10 months per year** <br/>EPA Program Filter – Baseline condition for being a full time office building. <br/>Number Remaining: 1046.

    ``` r
    o5 = o4 %>% filter(MONUSE >= 10)
    ```

7.  **A single activity must characterize greater than 50% of the floor space** <br/>EPA Program Filter – In order to be considered part of the office peer group, more than 50% of the building must be defined as an office, bank/financial institution, or courthouse. <br/>This filter is applied by a set of screens. If the variable ONEACT=1, then one activity occupies 75% or more of the building. If the variable ONEACT=2, then the activities in the building are defined by ACT1, ACT2, and ACT3. One of these activities must be coded as Office/Professional (PBAX=11) or Public Order and Safety (PBAX=23), with a corresponding percent (ACT1PCT, ACT2PCT, ACT3PCT) that is greater than 50. <br/>Number Remaining: 1003.

    ``` r
    o6 = o5 %>% 
      filter( (ONEACT == 1) |
            (ONEACT == 2 & 
               ((ACT1 %in% c(11,23) & ACT1PCT > 50) | 
                  (ACT2 %in% c(11,23) & ACT2PCT > 50) | 
                  (ACT3 %in% c(11,23) & ACT3PCT > 50) )))
    ```

8.  **Must report energy usage** <br/>EPA Program Filter – Baseline condition for being a full time office building. <br/>Number Remaining: 1003.

    ``` r
    o7 = o6 %>% filter(!is.na(MFBTU))
    ```

9.  **Must be less than or equal to 1,000,000 square feet** <br/>Data Limitation Filter – CBECS masks surveyed properties above 1,000,000 square feet by applying regional averages. <br/>Number Remaining: 972.

    ``` r
    o8 = o7 %>% filter(SQFT <= 1000000)
    ```

10. **If propane is used, the amount category (PRAMTC) must equal 1, 2, or 3** Data Limitation Filter – Cannot estimate propane use if the quantity is “greater than 1000” or unknown. <br/>Number Remaining: 959.

    ``` r
    o9 = o8 %>% filter(is.na(PRAMTC) | PRAMTC %in% c(1,2,3))
    ```

11. **If propane is used, the unit (PRUNIT) must be known** <br/>Data Limitation Filter – Cannot estimate propane use if the unit is unknown. <br/>Number Remaining: 959.

    ``` r
    o10 = o9 %>% filter(is.na(PRUNIT) | PRUNIT %in% c(1,2))
    ```

12. **If propane is used, the maximum estimated propane amount must be 10% or less of the total source energy** <br/>Data Limitation Filter – Because propane values are estimated from a range, propane is restricted to 10% of the total source energy. <br/>Number Remaining: 957.

    ``` r
    o11 = o10 %>% 
      filter( PRUSED == 2 | is.na(NGBTU_PERCENT) == T | 
            (PRUSED == 1 & NGBTU_PERCENT <= 10))
    ```

13. **must not use chilled water, wood, coal, or solar** <br/>Data Limitation Filter – CBECS does not collect quantities of chilled water, wood, coal, or solar. <br/>Number Remaining: 897. Difference: +1.

    ``` r
    o12 = o11 %>% 
      filter(CWUSED == 2 & WOUSED == 2 & COUSED == 2 & SOUSED == 2)
    ```

14. **Server count must be known** <br/>Data Limitation Filter – CBECS codes missing responses for number of servers as ‘9995.’ <br/>Number Remaining: 893. Difference: +1.

    ``` r
    o13 = o12 %>% filter(SERVERN != 9995)
    ```

15. **Must have no more than 8 workers per 1,000 square feet** <br/>Analytical Filter – Values determined to be statistical outliers. <br/>Number Remaining: 889. Difference: +1.

    ``` r
    o14 = o13 %>% filter(NWKER  / SQFT * 1000 <= 8)
    ```

16. **Banks must have Source EUI greater than 50 kBtu/ft2** <br/>Analytical Filter – Values determined to be statistical outliers. <br/>Number Remaining: 887. Difference: +1.

    ``` r
    o15 = o14 %>% 
      filter( PBAPLUS != 3 | (PBAPLUS == 3 & SOURCE_EUI > 50))
    ```

**Save the filtered dataset**

``` r
write.csv(o15, paste0(save_dir1, "office.csv"), row.names = F)
```

Prepare features
----------------

``` r
office = read.csv(paste0(save_dir1, "office.csv"))

data = office %>%
  mutate(NWKER_SQFT = NWKER/SQFT * 1000) %>%
  mutate(PCTERMN_TOT = rowSums(dplyr::select(., c(PCTERMN,SERVERN,LAPTPN)), na.rm = T)) %>% 
  mutate(PCTERMN_SQFT = PCTERMN_TOT/SQFT * 1000) %>%
  mutate(CDD65_COOLP = log(CDD65) * COOLP / 100) %>%
  mutate(IsBank = ifelse(PBAPLUS == 3, "Yes", "No")) %>%
  mutate_if(is.numeric, round, 3)

#data = data %>% filter(SOURCE_EUI <= 500)

ivars = c("SQFT", "WKHRS", "NWKER_SQFT", "PCTERMN_SQFT", 
          "CDD65_COOLP", "IsBank")
dvars  = c("SOURCE_EUI", "SOURCE_ENERGY", "FINALWT")

features = data[, c(ivars, dvars)]

write.csv(features, paste0(save_dir2, "office.csv"), row.names = F)
```

Descriptive statistics
----------------------

``` r
features1 = features
features1[features1$SQFT >= 100000, ]$SQFT = NA
features1 = features1 %>% dplyr::select(-one_of('SOURCE_ENERGY', 'FINALWT'))

summarytools::descr(features1, stats = "common", 
                    transpose = TRUE, 
                    headings = FALSE)
```

    Non-numerical variable(s) ignored: IsBank

|                   |      Mean|   Std.Dev|      Min|   Median|       Max|  N.Valid|  Pct.Valid|
|------------------:|---------:|---------:|--------:|--------:|---------:|--------:|----------:|
|   **CDD65\_COOLP**|      6.51|      1.47|     0.72|     6.94|      8.53|   882.00|      99.44|
|    **NWKER\_SQFT**|      2.16|      1.27|     0.08|     1.95|      7.99|   887.00|     100.00|
|  **PCTERMN\_SQFT**|      3.04|      2.05|     0.03|     2.57|     14.06|   887.00|     100.00|
|    **SOURCE\_EUI**|    181.75|    129.04|     4.74|   153.69|   1245.06|   887.00|     100.00|
|           **SQFT**|  20978.37|  25111.09|  1001.00|  7500.00|  99000.00|   640.00|      72.15|
|          **WKHRS**|     62.01|     33.21|    32.00|    50.00|    168.00|   887.00|     100.00|

``` r
dfSummary(features1, plain.ascii = FALSE, style = "grid", 
          graph.magnif = 0.75, valid.col = FALSE)
```

    text graphs are displayed; set 'tmp.img.dir' parameter to activate png graphs

### Data Frame Summary

**features1**
**Dimensions:** 887 x 7
**Duplicates:** 0

<table>
<colgroup>
<col width="4%" />
<col width="13%" />
<col width="27%" />
<col width="18%" />
<col width="27%" />
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
<td><p>Mean (sd) : 20978.4 (25111.1)<br />
min &lt; med &lt; max:<br />
1001 &lt; 7500 &lt; 99000<br />
IQR (CV) : 29512.5 (1.2)</p></td>
<td><p>209 distinct values</p></td>
<td><p><br />
:<br />
:<br />
:<br />
:<br />
: : . . . . . .</p></td>
<td><p>247<br />
(27.85%)</p></td>
</tr>
<tr class="even">
<td><p>2</p></td>
<td><p>WKHRS<br />
[numeric]</p></td>
<td><p>Mean (sd) : 62 (33.2)<br />
min &lt; med &lt; max:<br />
32 &lt; 50 &lt; 168<br />
IQR (CV) : 16 (0.5)</p></td>
<td><p>56 distinct values</p></td>
<td><p><br />
:<br />
: : .<br />
: : :<br />
: : :<br />
: : : . .         :</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="odd">
<td><p>3</p></td>
<td><p>NWKER_SQFT<br />
[numeric]</p></td>
<td><p>Mean (sd) : 2.2 (1.3)<br />
min &lt; med &lt; max:<br />
0.1 &lt; 2 &lt; 8<br />
IQR (CV) : 1.7 (0.6)</p></td>
<td><p>501 distinct values</p></td>
<td><p><br />
  :<br />
  : .<br />
: : :<br />
: : : :<br />
: : : : :</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="even">
<td><p>4</p></td>
<td><p>PCTERMN_SQFT<br />
[numeric]</p></td>
<td><p>Mean (sd) : 3 (2.1)<br />
min &lt; med &lt; max:<br />
0 &lt; 2.6 &lt; 14.1<br />
IQR (CV) : 2.3 (0.7)</p></td>
<td><p>658 distinct values</p></td>
<td><p><br />
  :<br />
  :<br />
: : :<br />
: : : .<br />
: : : : . .</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="odd">
<td><p>5</p></td>
<td><p>CDD65_COOLP<br />
[numeric]</p></td>
<td><p>Mean (sd) : 6.5 (1.5)<br />
min &lt; med &lt; max:<br />
0.7 &lt; 6.9 &lt; 8.5<br />
IQR (CV) : 1.6 (0.2)</p></td>
<td><p>771 distinct values</p></td>
<td><p><br />
              :<br />
            . :<br />
            : :<br />
          . : : .<br />
    . . : : : : :</p></td>
<td><p>5<br />
(0.56%)</p></td>
</tr>
<tr class="even">
<td><p>6</p></td>
<td><p>IsBank<br />
[character]</p></td>
<td><p>1. No<br />
2. Yes</p></td>
<td><p>815 (91.9%)<br />
72 ( 8.1%)</p></td>
<td><p>IIIIIIIIIIIIIIIIII<br />
I</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
<tr class="odd">
<td><p>7</p></td>
<td><p>SOURCE_EUI<br />
[numeric]</p></td>
<td><p>Mean (sd) : 181.7 (129)<br />
min &lt; med &lt; max:<br />
4.7 &lt; 153.7 &lt; 1245.1<br />
IQR (CV) : 120.6 (0.7)</p></td>
<td><p>870 distinct values</p></td>
<td><p><br />
  :<br />
: :<br />
: :<br />
: : .<br />
: : : .</p></td>
<td><p>0<br />
(0%)</p></td>
</tr>
</tbody>
</table>
