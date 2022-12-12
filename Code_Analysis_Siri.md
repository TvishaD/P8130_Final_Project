Code and Analysis for Siri
================
2022-12-11

``` r
library(tidyverse)
```

    ## Warning: 套件 'ggplot2' 是用 R 版本 4.2.2 來建造的

    ## Warning: 套件 'tidyr' 是用 R 版本 4.2.2 來建造的

    ## Warning: 套件 'readr' 是用 R 版本 4.2.2 來建造的

    ## Warning: 套件 'purrr' 是用 R 版本 4.2.2 來建造的

    ## Warning: 套件 'stringr' 是用 R 版本 4.2.2 來建造的

``` r
library(ggplot2)
library(patchwork)
library(GGally)
```

    ## Warning: 套件 'GGally' 是用 R 版本 4.2.2 來建造的

``` r
library(leaps)
```

    ## Warning: 套件 'leaps' 是用 R 版本 4.2.2 來建造的

``` r
library(caret)
```

    ## Warning: 套件 'caret' 是用 R 版本 4.2.2 來建造的

``` r
bodyfat_df = 
  readxl::read_excel("body_density_data.xlsx") %>%
  janitor::clean_names() 

head(bodyfat_df)
```

    ## # A tibble: 6 × 17
    ##      id bodyfat_…¹ bodyf…² body_…³   age weight height  neck chest abdomen   hip
    ##   <dbl>      <dbl>   <dbl>   <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>   <dbl> <dbl>
    ## 1     1       12.6    12.3    1.07    23   154.   67.8  36.2  93.1    85.2  94.5
    ## 2     2        6.9     6.1    1.09    22   173.   72.2  38.5  93.6    83    98.7
    ## 3     3       24.6    25.3    1.04    22   154    66.2  34    95.8    87.9  99.2
    ## 4     4       10.9    10.4    1.08    26   185.   72.2  37.4 102.     86.4 101. 
    ## 5     5       27.8    28.7    1.03    24   184.   71.2  34.4  97.3   100   102. 
    ## 6     6       20.6    20.9    1.05    24   210.   74.8  39   104.     94.4 108. 
    ## # … with 6 more variables: thigh <dbl>, knee <dbl>, ankle <dbl>, bicep <dbl>,
    ## #   forearm <dbl>, wrist <dbl>, and abbreviated variable names ¹​bodyfat_brozek,
    ## #   ²​bodyfat_siri, ³​body_density

## Exploratory Analysis and Transformations

``` r
par(mfrow=c(1,3))

boxplot(bodyfat_df$bodyfat_brozek, main='bodyfat_brozek')
boxplot(bodyfat_df$bodyfat_siri, main='bodyfat_siri')
boxplot(bodyfat_df$body_density, main='bodyfat_density')
```

![](Code_Analysis_Siri_files/figure-gfm/Y%20distribution%20exploration-1.png)<!-- -->

``` r
brozek = 
bodyfat_df %>%
  ggplot(aes(x = bodyfat_brozek)) +
  geom_histogram()

siri = 
bodyfat_df %>%
  ggplot(aes(x = bodyfat_siri)) +
  geom_histogram()

density =
bodyfat_df %>%
  ggplot(aes(x = body_density)) +
  geom_histogram()

brozek + siri + density
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Code_Analysis_Siri_files/figure-gfm/Y%20distribution%20exploration-2.png)<!-- -->

NOTE: All are approximately normal \* Probably going with Brozek.
(60-92)

``` r
bodyfat_df %>%
  ggplot(aes(x = age)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Code_Analysis_Siri_files/figure-gfm/checking%20validity%20in%20y%20choice-1.png)<!-- -->

``` r
bodyfat_df %>% 
  select(-id, -bodyfat_brozek, -body_density) %>% 
  gtsummary::tbl_summary() %>% 
  gtsummary::bold_labels()
```

<div id="fhpgicbxzu" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#fhpgicbxzu .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#fhpgicbxzu .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fhpgicbxzu .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#fhpgicbxzu .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#fhpgicbxzu .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#fhpgicbxzu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fhpgicbxzu .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fhpgicbxzu .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#fhpgicbxzu .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#fhpgicbxzu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#fhpgicbxzu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#fhpgicbxzu .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#fhpgicbxzu .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#fhpgicbxzu .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#fhpgicbxzu .gt_from_md > :first-child {
  margin-top: 0;
}

#fhpgicbxzu .gt_from_md > :last-child {
  margin-bottom: 0;
}

#fhpgicbxzu .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#fhpgicbxzu .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#fhpgicbxzu .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#fhpgicbxzu .gt_row_group_first td {
  border-top-width: 2px;
}

#fhpgicbxzu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fhpgicbxzu .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#fhpgicbxzu .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#fhpgicbxzu .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fhpgicbxzu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fhpgicbxzu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#fhpgicbxzu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#fhpgicbxzu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fhpgicbxzu .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fhpgicbxzu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#fhpgicbxzu .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fhpgicbxzu .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#fhpgicbxzu .gt_left {
  text-align: left;
}

#fhpgicbxzu .gt_center {
  text-align: center;
}

#fhpgicbxzu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#fhpgicbxzu .gt_font_normal {
  font-weight: normal;
}

#fhpgicbxzu .gt_font_bold {
  font-weight: bold;
}

#fhpgicbxzu .gt_font_italic {
  font-style: italic;
}

#fhpgicbxzu .gt_super {
  font-size: 65%;
}

#fhpgicbxzu .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#fhpgicbxzu .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#fhpgicbxzu .gt_indent_1 {
  text-indent: 5px;
}

#fhpgicbxzu .gt_indent_2 {
  text-indent: 10px;
}

#fhpgicbxzu .gt_indent_3 {
  text-indent: 15px;
}

#fhpgicbxzu .gt_indent_4 {
  text-indent: 20px;
}

#fhpgicbxzu .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;N = 252&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>N = 252</strong><sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">bodyfat_siri</td>
<td headers="stat_0" class="gt_row gt_center">19 (12, 25)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">age</td>
<td headers="stat_0" class="gt_row gt_center">43 (36, 54)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">weight</td>
<td headers="stat_0" class="gt_row gt_center">176 (159, 197)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">height</td>
<td headers="stat_0" class="gt_row gt_center">70.00 (68.25, 72.25)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">neck</td>
<td headers="stat_0" class="gt_row gt_center">38.00 (36.40, 39.42)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">chest</td>
<td headers="stat_0" class="gt_row gt_center">100 (94, 105)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">abdomen</td>
<td headers="stat_0" class="gt_row gt_center">91 (85, 99)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">hip</td>
<td headers="stat_0" class="gt_row gt_center">99 (96, 104)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">thigh</td>
<td headers="stat_0" class="gt_row gt_center">59.0 (56.0, 62.3)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">knee</td>
<td headers="stat_0" class="gt_row gt_center">38.50 (36.98, 39.92)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">ankle</td>
<td headers="stat_0" class="gt_row gt_center">22.80 (22.00, 24.00)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">bicep</td>
<td headers="stat_0" class="gt_row gt_center">32.05 (30.20, 34.32)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">forearm</td>
<td headers="stat_0" class="gt_row gt_center">28.70 (27.30, 30.00)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">wrist</td>
<td headers="stat_0" class="gt_row gt_center">18.30 (17.60, 18.80)</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="2"><sup class="gt_footnote_marks">1</sup> Median (IQR)</td>
    </tr>
  </tfoot>
</table>
</div>

``` r
bodyfat_df %>% 
  select(-bodyfat_brozek, -body_density, -id) %>% 
  relocate("bodyfat_siri") %>% 
  ggpairs()
```

![](Code_Analysis_Siri_files/figure-gfm/normality,%20y-x%20rels,%20collinearity%20in%20covariates%20(xs)-1.png)<!-- -->

``` r
before = 
bodyfat_df %>%
  ggplot(aes(x = weight)) +
  geom_density()

after = 
bodyfat_df %>% 
  mutate(inv_weight = 1/(weight)) %>% 
  ggplot(aes(x = inv_weight)) +
  geom_density()

before + after
```

![](Code_Analysis_Siri_files/figure-gfm/inv_transformation%20for%20weight-1.png)<!-- -->

NOTE: For the almost normal but right skewed distributions, inverse
transformation is making a considerable difference. From this point,
inversely transforming (weight and neck — bicep).

``` r
bodyfat_df = 
  bodyfat_df %>%
  mutate(i_weight = 1/weight,
         i_neck = 1/neck,
         i_chest = 1/chest,
         i_abdomen = 1/abdomen,
         i_hip = 1/hip,
         i_thigh = 1/thigh,
         i_knee = 1/knee,
         i_ankle = 1/ankle,
         i_bicep = 1/bicep) %>% 
  select(-id, -bodyfat_brozek, -body_density, -weight, -neck, -chest, -abdomen,
         -hip, -thigh, -knee, -ankle, -bicep)
```

NOTE: Collinearity continues though. Weight with all others. Just fyi.
Not manually removing any covariates as they would be removed during the
model selection processes?

## Model Selection

``` r
mult.fit = lm(bodyfat_siri ~ ., data = bodyfat_df)

#Backward
step(mult.fit, direction = 'backward')
```

    ## Start:  AIC=742.36
    ## bodyfat_siri ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_chest + i_abdomen + i_hip + i_thigh + i_knee + i_ankle + 
    ##     i_bicep
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_ankle    1      2.10 4292.7 740.48
    ## - i_thigh    1      4.96 4295.6 740.65
    ## - i_hip      1      5.98 4296.6 740.71
    ## - i_bicep    1      9.94 4300.6 740.94
    ## - i_knee     1     10.18 4300.8 740.96
    ## - forearm    1     21.88 4312.5 741.64
    ## - i_weight   1     26.52 4317.2 741.91
    ## - i_chest    1     29.76 4320.4 742.10
    ## <none>                   4290.6 742.36
    ## - i_neck     1    106.34 4397.0 746.53
    ## - age        1    117.85 4408.5 747.19
    ## - height     1    150.27 4440.9 749.03
    ## - wrist      1    237.29 4527.9 753.92
    ## - i_abdomen  1   1406.09 5696.7 811.79
    ## 
    ## Step:  AIC=740.48
    ## bodyfat_siri ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_chest + i_abdomen + i_hip + i_thigh + i_knee + i_bicep
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_thigh    1      4.82 4297.6 738.77
    ## - i_hip      1      6.35 4299.1 738.86
    ## - i_knee     1      8.63 4301.4 738.99
    ## - i_bicep    1      9.35 4302.1 739.03
    ## - forearm    1     21.46 4314.2 739.74
    ## - i_chest    1     30.31 4323.1 740.26
    ## - i_weight   1     31.00 4323.7 740.30
    ## <none>                   4292.7 740.48
    ## - i_neck     1    111.09 4403.8 744.92
    ## - age        1    115.79 4408.5 745.19
    ## - height     1    156.40 4449.1 747.50
    ## - wrist      1    245.30 4538.0 752.49
    ## - i_abdomen  1   1416.93 5709.7 810.36
    ## 
    ## Step:  AIC=738.77
    ## bodyfat_siri ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_chest + i_abdomen + i_hip + i_knee + i_bicep
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_hip      1      3.59 4301.2 736.98
    ## - i_knee     1      5.77 4303.3 737.10
    ## - i_bicep    1     13.31 4310.9 737.55
    ## - forearm    1     20.48 4318.1 737.96
    ## <none>                   4297.6 738.77
    ## - i_chest    1     37.97 4335.5 738.98
    ## - i_weight   1     39.45 4337.0 739.07
    ## - i_neck     1    107.83 4405.4 743.01
    ## - age        1    113.46 4411.0 743.33
    ## - height     1    196.04 4493.6 748.01
    ## - wrist      1    257.88 4555.5 751.45
    ## - i_abdomen  1   1421.87 5719.4 808.79
    ## 
    ## Step:  AIC=736.98
    ## bodyfat_siri ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_chest + i_abdomen + i_knee + i_bicep
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_knee     1      8.79 4310.0 735.49
    ## - i_bicep    1     13.06 4314.2 735.74
    ## - forearm    1     23.56 4324.7 736.35
    ## <none>                   4301.2 736.98
    ## - i_chest    1     35.02 4336.2 737.02
    ## - i_weight   1     37.10 4338.3 737.14
    ## - i_neck     1    105.49 4406.6 741.08
    ## - age        1    133.13 4434.3 742.66
    ## - height     1    195.68 4496.8 746.19
    ## - wrist      1    257.84 4559.0 749.65
    ## - i_abdomen  1   1473.75 5774.9 809.23
    ## 
    ## Step:  AIC=735.49
    ## bodyfat_siri ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_chest + i_abdomen + i_bicep
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_bicep    1     12.90 4322.8 734.24
    ## - forearm    1     23.70 4333.6 734.87
    ## - i_weight   1     28.81 4338.8 735.17
    ## - i_chest    1     30.64 4340.6 735.28
    ## <none>                   4310.0 735.49
    ## - i_neck     1     99.28 4409.2 739.23
    ## - age        1    128.78 4438.7 740.91
    ## - height     1    199.88 4509.8 744.92
    ## - wrist      1    274.25 4584.2 749.04
    ## - i_abdomen  1   1466.61 5776.6 807.30
    ## 
    ## Step:  AIC=734.24
    ## bodyfat_siri ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_chest + i_abdomen
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_chest    1     29.88 4352.7 733.98
    ## <none>                   4322.8 734.24
    ## - forearm    1     36.04 4358.9 734.34
    ## - i_weight   1     44.74 4367.6 734.84
    ## - i_neck     1     92.92 4415.8 737.60
    ## - age        1    125.40 4448.3 739.45
    ## - height     1    231.10 4553.9 745.37
    ## - wrist      1    271.19 4594.0 747.58
    ## - i_abdomen  1   1454.17 5777.0 805.32
    ## 
    ## Step:  AIC=733.98
    ## bodyfat_siri ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_abdomen
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_weight   1     22.75 4375.5 733.29
    ## - forearm    1     33.91 4386.6 733.94
    ## <none>                   4352.7 733.98
    ## - i_neck     1    106.84 4459.6 738.09
    ## - age        1    109.63 4462.4 738.25
    ## - height     1    201.33 4554.1 743.38
    ## - wrist      1    265.96 4618.7 746.93
    ## - i_abdomen  1   1437.91 5790.6 803.91
    ## 
    ## Step:  AIC=733.29
    ## bodyfat_siri ~ age + height + forearm + wrist + i_neck + i_abdomen
    ## 
    ##             Df Sum of Sq     RSS    AIC
    ## <none>                    4375.5 733.29
    ## - forearm    1      58.0  4433.5 734.61
    ## - age        1      86.9  4462.4 736.25
    ## - i_neck     1      89.8  4465.3 736.41
    ## - height     1     209.4  4584.8 743.07
    ## - wrist      1     243.4  4618.8 744.93
    ## - i_abdomen  1    7393.4 11768.9 980.63

    ## 
    ## Call:
    ## lm(formula = bodyfat_siri ~ age + height + forearm + wrist + 
    ##     i_neck + i_abdomen, data = bodyfat_df)
    ## 
    ## Coefficients:
    ## (Intercept)          age       height      forearm        wrist       i_neck  
    ##   1.257e+02    5.425e-02   -4.106e-01    3.315e-01   -1.741e+00    6.568e+02  
    ##   i_abdomen  
    ##  -6.876e+03

``` r
#Forward
intercept_only = lm(bodyfat_siri ~ 1, data = bodyfat_df)
step(intercept_only, direction = "forward", scope = formula(mult.fit))
```

    ## Start:  AIC=1071.75
    ## bodyfat_siri ~ 1
    ## 
    ##             Df Sum of Sq     RSS     AIC
    ## + i_abdomen  1   11877.9  5701.1  789.98
    ## + i_chest    1    8733.3  8845.7  900.68
    ## + i_hip      1    7264.8 10314.2  939.39
    ## + i_weight   1    7006.0 10573.0  945.63
    ## + i_thigh    1    5775.7 11803.3  973.37
    ## + i_knee     1    4584.2 12994.8  997.60
    ## + i_bicep    1    4467.2 13111.8  999.86
    ## + i_neck     1    4188.7 13390.2 1005.16
    ## + forearm    1    2295.8 15283.2 1038.48
    ## + wrist      1    2111.5 15467.5 1041.50
    ## + age        1    1493.3 16085.7 1051.38
    ## + i_ankle    1    1471.0 16108.0 1051.73
    ## <none>                   17579.0 1071.75
    ## + height     1      11.2 17567.7 1073.59
    ## 
    ## Step:  AIC=789.98
    ## bodyfat_siri ~ i_abdomen
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## + wrist     1    764.63 4936.4 755.69
    ## + height    1    757.42 4943.6 756.06
    ## + i_weight  1    658.41 5042.7 761.06
    ## + i_neck    1    551.36 5149.7 766.35
    ## + i_knee    1    382.74 5318.3 774.47
    ## + i_hip     1    291.86 5409.2 778.74
    ## + i_ankle   1    290.87 5410.2 778.79
    ## + i_chest   1    171.97 5529.1 784.26
    ## + age       1    162.42 5538.6 784.70
    ## + i_thigh   1    127.66 5573.4 786.28
    ## + forearm   1    117.10 5584.0 786.75
    ## + i_bicep   1    103.74 5597.3 787.36
    ## <none>                  5701.1 789.98
    ## 
    ## Step:  AIC=755.69
    ## bodyfat_siri ~ i_abdomen + wrist
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## + height    1    367.34 4569.1 738.21
    ## + age       1    228.63 4707.8 745.74
    ## + i_weight  1    185.28 4751.2 748.05
    ## + i_neck    1    110.63 4825.8 751.98
    ## + i_hip     1    102.13 4834.3 752.42
    ## + i_knee    1     87.68 4848.8 753.18
    ## + i_thigh   1     44.12 4892.3 755.43
    ## <none>                  4936.4 755.69
    ## + i_chest   1     23.73 4912.7 756.48
    ## + i_ankle   1     23.66 4912.8 756.48
    ## + i_bicep   1      0.23 4936.2 757.68
    ## + forearm   1      0.00 4936.4 757.69
    ## 
    ## Step:  AIC=738.21
    ## bodyfat_siri ~ i_abdomen + wrist + height
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## + i_neck    1    77.991 4491.1 735.87
    ## + age       1    75.788 4493.3 735.99
    ## <none>                  4569.1 738.21
    ## + i_chest   1    21.435 4547.7 739.02
    ## + i_hip     1     9.035 4560.1 739.71
    ## + forearm   1     6.597 4562.5 739.84
    ## + i_bicep   1     3.677 4565.4 740.00
    ## + i_thigh   1     3.109 4566.0 740.03
    ## + i_weight  1     0.971 4568.1 740.15
    ## + i_knee    1     0.353 4568.7 740.19
    ## + i_ankle   1     0.165 4568.9 740.20
    ## 
    ## Step:  AIC=735.87
    ## bodyfat_siri ~ i_abdomen + wrist + height + i_neck
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## + age       1    57.617 4433.5 734.61
    ## <none>                  4491.1 735.87
    ## + forearm   1    28.742 4462.4 736.25
    ## + i_bicep   1    22.106 4469.0 736.62
    ## + i_chest   1     5.413 4485.7 737.56
    ## + i_hip     1     4.821 4486.3 737.60
    ## + i_weight  1     4.199 4486.9 737.63
    ## + i_knee    1     0.379 4490.7 737.85
    ## + i_ankle   1     0.029 4491.1 737.87
    ## + i_thigh   1     0.001 4491.1 737.87
    ## 
    ## Step:  AIC=734.61
    ## bodyfat_siri ~ i_abdomen + wrist + height + i_neck + age
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## + forearm   1    58.002 4375.5 733.29
    ## + i_bicep   1    51.709 4381.8 733.66
    ## + i_weight  1    46.845 4386.6 733.94
    ## <none>                  4433.5 734.61
    ## + i_thigh   1    29.276 4404.2 734.94
    ## + i_ankle   1     6.495 4427.0 736.24
    ## + i_hip     1     3.117 4430.4 736.44
    ## + i_chest   1     2.214 4431.3 736.49
    ## + i_knee    1     0.461 4433.0 736.59
    ## 
    ## Step:  AIC=733.29
    ## bodyfat_siri ~ i_abdomen + wrist + height + i_neck + age + forearm
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## <none>                  4375.5 733.29
    ## + i_bicep   1   23.5255 4352.0 733.94
    ## + i_weight  1   22.7538 4352.7 733.98
    ## + i_thigh   1   21.1466 4354.3 734.07
    ## + i_chest   1    7.8937 4367.6 734.84
    ## + i_ankle   1    4.8019 4370.7 735.02
    ## + i_hip     1    2.1619 4373.3 735.17
    ## + i_knee    1    0.0245 4375.5 735.29

    ## 
    ## Call:
    ## lm(formula = bodyfat_siri ~ i_abdomen + wrist + height + i_neck + 
    ##     age + forearm, data = bodyfat_df)
    ## 
    ## Coefficients:
    ## (Intercept)    i_abdomen        wrist       height       i_neck          age  
    ##   1.257e+02   -6.876e+03   -1.741e+00   -4.106e-01    6.568e+02    5.425e-02  
    ##     forearm  
    ##   3.315e-01

``` r
#Stepwise
step(mult.fit, direction = 'both')
```

    ## Start:  AIC=742.36
    ## bodyfat_siri ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_chest + i_abdomen + i_hip + i_thigh + i_knee + i_ankle + 
    ##     i_bicep
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_ankle    1      2.10 4292.7 740.48
    ## - i_thigh    1      4.96 4295.6 740.65
    ## - i_hip      1      5.98 4296.6 740.71
    ## - i_bicep    1      9.94 4300.6 740.94
    ## - i_knee     1     10.18 4300.8 740.96
    ## - forearm    1     21.88 4312.5 741.64
    ## - i_weight   1     26.52 4317.2 741.91
    ## - i_chest    1     29.76 4320.4 742.10
    ## <none>                   4290.6 742.36
    ## - i_neck     1    106.34 4397.0 746.53
    ## - age        1    117.85 4408.5 747.19
    ## - height     1    150.27 4440.9 749.03
    ## - wrist      1    237.29 4527.9 753.92
    ## - i_abdomen  1   1406.09 5696.7 811.79
    ## 
    ## Step:  AIC=740.48
    ## bodyfat_siri ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_chest + i_abdomen + i_hip + i_thigh + i_knee + i_bicep
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_thigh    1      4.82 4297.6 738.77
    ## - i_hip      1      6.35 4299.1 738.86
    ## - i_knee     1      8.63 4301.4 738.99
    ## - i_bicep    1      9.35 4302.1 739.03
    ## - forearm    1     21.46 4314.2 739.74
    ## - i_chest    1     30.31 4323.1 740.26
    ## - i_weight   1     31.00 4323.7 740.30
    ## <none>                   4292.7 740.48
    ## + i_ankle    1      2.10 4290.6 742.36
    ## - i_neck     1    111.09 4403.8 744.92
    ## - age        1    115.79 4408.5 745.19
    ## - height     1    156.40 4449.1 747.50
    ## - wrist      1    245.30 4538.0 752.49
    ## - i_abdomen  1   1416.93 5709.7 810.36
    ## 
    ## Step:  AIC=738.77
    ## bodyfat_siri ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_chest + i_abdomen + i_hip + i_knee + i_bicep
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_hip      1      3.59 4301.2 736.98
    ## - i_knee     1      5.77 4303.3 737.10
    ## - i_bicep    1     13.31 4310.9 737.55
    ## - forearm    1     20.48 4318.1 737.96
    ## <none>                   4297.6 738.77
    ## - i_chest    1     37.97 4335.5 738.98
    ## - i_weight   1     39.45 4337.0 739.07
    ## + i_thigh    1      4.82 4292.7 740.48
    ## + i_ankle    1      1.96 4295.6 740.65
    ## - i_neck     1    107.83 4405.4 743.01
    ## - age        1    113.46 4411.0 743.33
    ## - height     1    196.04 4493.6 748.01
    ## - wrist      1    257.88 4555.5 751.45
    ## - i_abdomen  1   1421.87 5719.4 808.79
    ## 
    ## Step:  AIC=736.98
    ## bodyfat_siri ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_chest + i_abdomen + i_knee + i_bicep
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_knee     1      8.79 4310.0 735.49
    ## - i_bicep    1     13.06 4314.2 735.74
    ## - forearm    1     23.56 4324.7 736.35
    ## <none>                   4301.2 736.98
    ## - i_chest    1     35.02 4336.2 737.02
    ## - i_weight   1     37.10 4338.3 737.14
    ## + i_hip      1      3.59 4297.6 738.77
    ## + i_ankle    1      2.29 4298.9 738.84
    ## + i_thigh    1      2.06 4299.1 738.86
    ## - i_neck     1    105.49 4406.6 741.08
    ## - age        1    133.13 4434.3 742.66
    ## - height     1    195.68 4496.8 746.19
    ## - wrist      1    257.84 4559.0 749.65
    ## - i_abdomen  1   1473.75 5774.9 809.23
    ## 
    ## Step:  AIC=735.49
    ## bodyfat_siri ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_chest + i_abdomen + i_bicep
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_bicep    1     12.90 4322.8 734.24
    ## - forearm    1     23.70 4333.6 734.87
    ## - i_weight   1     28.81 4338.8 735.17
    ## - i_chest    1     30.64 4340.6 735.28
    ## <none>                   4310.0 735.49
    ## + i_knee     1      8.79 4301.2 736.98
    ## + i_hip      1      6.61 4303.3 737.10
    ## + i_ankle    1      0.66 4309.3 737.45
    ## + i_thigh    1      0.08 4309.9 737.49
    ## - i_neck     1     99.28 4409.2 739.23
    ## - age        1    128.78 4438.7 740.91
    ## - height     1    199.88 4509.8 744.92
    ## - wrist      1    274.25 4584.2 749.04
    ## - i_abdomen  1   1466.61 5776.6 807.30
    ## 
    ## Step:  AIC=734.24
    ## bodyfat_siri ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_chest + i_abdomen
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_chest    1     29.88 4352.7 733.98
    ## <none>                   4322.8 734.24
    ## - forearm    1     36.04 4358.9 734.34
    ## - i_weight   1     44.74 4367.6 734.84
    ## + i_bicep    1     12.90 4310.0 735.49
    ## + i_knee     1      8.63 4314.2 735.74
    ## + i_hip      1      6.25 4316.6 735.88
    ## + i_thigh    1      1.05 4321.8 736.18
    ## + i_ankle    1      0.29 4322.6 736.23
    ## - i_neck     1     92.92 4415.8 737.60
    ## - age        1    125.40 4448.3 739.45
    ## - height     1    231.10 4553.9 745.37
    ## - wrist      1    271.19 4594.0 747.58
    ## - i_abdomen  1   1454.17 5777.0 805.32
    ## 
    ## Step:  AIC=733.98
    ## bodyfat_siri ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_abdomen
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_weight   1     22.75 4375.5 733.29
    ## - forearm    1     33.91 4386.6 733.94
    ## <none>                   4352.7 733.98
    ## + i_chest    1     29.88 4322.8 734.24
    ## + i_bicep    1     12.14 4340.6 735.28
    ## + i_thigh    1      6.77 4346.0 735.59
    ## + i_knee     1      4.34 4348.4 735.73
    ## + i_hip      1      1.66 4351.1 735.88
    ## + i_ankle    1      0.68 4352.0 735.94
    ## - i_neck     1    106.84 4459.6 738.09
    ## - age        1    109.63 4462.4 738.25
    ## - height     1    201.33 4554.1 743.38
    ## - wrist      1    265.96 4618.7 746.93
    ## - i_abdomen  1   1437.91 5790.6 803.91
    ## 
    ## Step:  AIC=733.29
    ## bodyfat_siri ~ age + height + forearm + wrist + i_neck + i_abdomen
    ## 
    ##             Df Sum of Sq     RSS    AIC
    ## <none>                    4375.5 733.29
    ## + i_bicep    1      23.5  4352.0 733.94
    ## + i_weight   1      22.8  4352.7 733.98
    ## + i_thigh    1      21.1  4354.3 734.07
    ## - forearm    1      58.0  4433.5 734.61
    ## + i_chest    1       7.9  4367.6 734.84
    ## + i_ankle    1       4.8  4370.7 735.02
    ## + i_hip      1       2.2  4373.3 735.17
    ## + i_knee     1       0.0  4375.5 735.29
    ## - age        1      86.9  4462.4 736.25
    ## - i_neck     1      89.8  4465.3 736.41
    ## - height     1     209.4  4584.8 743.07
    ## - wrist      1     243.4  4618.8 744.93
    ## - i_abdomen  1    7393.4 11768.9 980.63

    ## 
    ## Call:
    ## lm(formula = bodyfat_siri ~ age + height + forearm + wrist + 
    ##     i_neck + i_abdomen, data = bodyfat_df)
    ## 
    ## Coefficients:
    ## (Intercept)          age       height      forearm        wrist       i_neck  
    ##   1.257e+02    5.425e-02   -4.106e-01    3.315e-01   -1.741e+00    6.568e+02  
    ##   i_abdomen  
    ##  -6.876e+03

Based on Backward: 6 predictors - age, height, forearm, wrist, i_neck,
i_abdomen Based on Forward: 6 predictors - age, height, forearm, wrist,
i_neck, i_abdomen Based on Stepwise: 6 predictors - age, height,
forearm, wrist, i_neck, i_abdomen

``` r
bodyfat_df =
  bodyfat_df %>%
  select(bodyfat_siri, everything())

mat = as.matrix(bodyfat_df)
# Printing the 2 best models of each size, using the Cp criterion:
leaps(x = mat[,2:14], y = mat[,1], nbest = 2, method = "Cp")
```

    ## $which
    ##        1     2     3     4     5     6     7     8     9     A     B     C
    ## 1  FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 1  FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
    ## 2  FALSE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 2  FALSE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 3  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 3  FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 4  FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 4   TRUE  TRUE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 5   TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 5  FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 6   TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 6   TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 7   TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 7   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 8   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## 8   TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## 9   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## 9   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE
    ## 10  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE
    ## 10  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
    ## 11  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE
    ## 11  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE
    ## 12  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
    ## 12  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE
    ## 13  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ##        D
    ## 1  FALSE
    ## 1  FALSE
    ## 2  FALSE
    ## 2  FALSE
    ## 3  FALSE
    ## 3  FALSE
    ## 4  FALSE
    ## 4  FALSE
    ## 5  FALSE
    ## 5  FALSE
    ## 6  FALSE
    ## 6   TRUE
    ## 7   TRUE
    ## 7  FALSE
    ## 8  FALSE
    ## 8   TRUE
    ## 9   TRUE
    ## 9  FALSE
    ## 10  TRUE
    ## 10  TRUE
    ## 11  TRUE
    ## 11  TRUE
    ## 12  TRUE
    ## 12  TRUE
    ## 13  TRUE
    ## 
    ## $label
    ##  [1] "(Intercept)" "1"           "2"           "3"           "4"          
    ##  [6] "5"           "6"           "7"           "8"           "9"          
    ## [11] "A"           "B"           "C"           "D"          
    ## 
    ## $size
    ##  [1]  2  2  3  3  4  4  5  5  6  6  7  7  8  8  9  9 10 10 11 11 12 12 13 13 14
    ## 
    ## $Cp
    ##  [1]  68.235578 242.664819  27.821853  28.221892   9.445519  14.047015
    ##  [7]   7.119409   7.241597   5.923439   7.525108   4.706110   5.055157
    ## [13]   5.401159   5.443970   5.786470   6.385474   7.071089   7.307826
    ## [19]   8.583472   8.704279  10.384203  10.456311  12.116617  12.275234
    ## [25]  14.000000

``` r
# Printing the 2 best models of each size, using the adjusted Rˆ2 criterion:
leaps(x = mat[,2:14], y = mat[,1], nbest = 2, method = "adjr2")
```

    ## $which
    ##        1     2     3     4     5     6     7     8     9     A     B     C
    ## 1  FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 1  FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
    ## 2  FALSE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 2  FALSE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 3  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 3  FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 4  FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 4   TRUE  TRUE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 5   TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 5  FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 6   TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 6   TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 7   TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 7   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 8   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## 8   TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## 9   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## 9   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE
    ## 10  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE
    ## 10  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
    ## 11  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE
    ## 11  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE
    ## 12  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
    ## 12  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE
    ## 13  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ##        D
    ## 1  FALSE
    ## 1  FALSE
    ## 2  FALSE
    ## 2  FALSE
    ## 3  FALSE
    ## 3  FALSE
    ## 4  FALSE
    ## 4  FALSE
    ## 5  FALSE
    ## 5  FALSE
    ## 6  FALSE
    ## 6   TRUE
    ## 7   TRUE
    ## 7  FALSE
    ## 8  FALSE
    ## 8   TRUE
    ## 9   TRUE
    ## 9  FALSE
    ## 10  TRUE
    ## 10  TRUE
    ## 11  TRUE
    ## 11  TRUE
    ## 12  TRUE
    ## 12  TRUE
    ## 13  TRUE
    ## 
    ## $label
    ##  [1] "(Intercept)" "1"           "2"           "3"           "4"          
    ##  [6] "5"           "6"           "7"           "8"           "9"          
    ## [11] "A"           "B"           "C"           "D"          
    ## 
    ## $size
    ##  [1]  2  2  3  3  4  4  5  5  6  6  7  7  8  8  9  9 10 10 11 11 12 12 13 13 14
    ## 
    ## $adjr2
    ##  [1] 0.6743914 0.4947922 0.7169300 0.7165164 0.7369380 0.7321619 0.7403814
    ##  [8] 0.7402541 0.7426703 0.7409943 0.7450002 0.7446335 0.7453318 0.7452867
    ## [15] 0.7459943 0.7453597 0.7457056 0.7454538 0.7451712 0.7450422 0.7443232
    ## [22] 0.7442458 0.7435416 0.7433708 0.7425902

``` r
b = regsubsets(bodyfat_siri ~ ., data = bodyfat_df)
rs = summary(b)

par(mfrow = c(1,2))
plot(1:8, rs$cp, xlab = "No of predictors", ylab = "Cp Statistic")
abline(0,1)
plot(1:8, rs$adjr2, xlab = "No of predictors", ylab = "Adj R2")
```

![](Code_Analysis_Siri_files/figure-gfm/automatic%20criterion%20output-1.png)<!-- -->

Based on Cp: 6 predictors (age, height, forearm, wrist, i_neck,
i_abdomen)

Based on Adj R^2: 8 predictors (age, height, forearm, wrist, i_neck,
i_abdomen, i_weight, i_chest)

NOTE: Some of these are highly correlated (DISCUSSION POINT!). Would use
shrinkage method (LASSO) which would factor the matter of collinearity
into the process). If we were to just abide by this though, we would
have to use Vif values etc and drop the super highly correlated
covariates. (Must ensure that the predictive ability is retained though
by actually fitting the model and keeping track of the adj. r^2 or
whatever parameter.)

``` r
six_pred = lm(bodyfat_siri ~ age + height + forearm + wrist + i_neck + i_abdomen, data = bodyfat_df)
summary(six_pred)
```

    ## 
    ## Call:
    ## lm(formula = bodyfat_siri ~ age + height + forearm + wrist + 
    ##     i_neck + i_abdomen, data = bodyfat_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -10.7876  -2.9024  -0.1935   2.6852  10.3786 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.257e+02  1.567e+01   8.025 4.23e-14 ***
    ## age          5.425e-02  2.460e-02   2.206 0.028343 *  
    ## height      -4.106e-01  1.199e-01  -3.424 0.000724 ***
    ## forearm      3.315e-01  1.839e-01   1.802 0.072751 .  
    ## wrist       -1.741e+00  4.717e-01  -3.691 0.000275 ***
    ## i_neck       6.568e+02  2.929e+02   2.242 0.025830 *  
    ## i_abdomen   -6.876e+03  3.379e+02 -20.347  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.226 on 245 degrees of freedom
    ## Multiple R-squared:  0.7511, Adjusted R-squared:  0.745 
    ## F-statistic: 123.2 on 6 and 245 DF,  p-value: < 2.2e-16

``` r
eight_pred = lm(bodyfat_siri ~ age + height + forearm + wrist + 
                  i_neck + i_abdomen + i_weight + i_chest, data = bodyfat_df)
summary(eight_pred)
```

    ## 
    ## Call:
    ## lm(formula = bodyfat_siri ~ age + height + forearm + wrist + 
    ##     i_neck + i_abdomen + i_weight + i_chest, data = bodyfat_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -10.6009  -3.0874  -0.1126   2.6700  11.0310 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.372e+02  1.840e+01   7.456 1.57e-12 ***
    ## age          7.416e-02  2.793e-02   2.655 0.008453 ** 
    ## height      -5.949e-01  1.651e-01  -3.604 0.000380 ***
    ## forearm      2.742e-01  1.926e-01   1.423 0.155943    
    ## wrist       -1.937e+00  4.962e-01  -3.904 0.000122 ***
    ## i_neck       6.922e+02  3.029e+02   2.285 0.023151 *  
    ## i_abdomen   -6.414e+03  7.095e+02  -9.041  < 2e-16 ***
    ## i_weight    -2.235e+03  1.410e+03  -1.586 0.114065    
    ## i_chest      1.263e+03  9.747e+02   1.296 0.196192    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.218 on 243 degrees of freedom
    ## Multiple R-squared:  0.7541, Adjusted R-squared:  0.746 
    ## F-statistic: 93.15 on 8 and 243 DF,  p-value: < 2.2e-16

``` r
anova(six_pred, eight_pred)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: bodyfat_siri ~ age + height + forearm + wrist + i_neck + i_abdomen
    ## Model 2: bodyfat_siri ~ age + height + forearm + wrist + i_neck + i_abdomen + 
    ##     i_weight + i_chest
    ##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
    ## 1    245 4375.5                           
    ## 2    243 4322.8  2    52.635 1.4794 0.2298

DISCUSSION: Smaller is better. Can discuss anova hypotheses and results
as proof.

``` r
# supply sequence of lambda values for the lasso cross validation for lambda
lambda_seq <- 10^seq(-3, 0, by = .1)
set.seed(2022)

# save matrix of predictors to pass to the lasso function
predictors_dat.state =
  bodyfat_df %>% 
  select(age, height, forearm, wrist, i_weight, i_neck, 
         i_chest, i_abdomen, i_hip, i_thigh, i_knee, i_ankle, i_bicep) %>% 
    as.matrix()

response_dat.state =
  bodyfat_df %>% 
  select(bodyfat_siri) %>% 
  as.matrix()

cv_lasso_fit <- glmnet::cv.glmnet(x = predictors_dat.state,
                                    y = response_dat.state,
                                    lambda = lambda_seq,
                                    nfolds = 10)
cv_lasso_fit
```

    ## 
    ## Call:  glmnet::cv.glmnet(x = predictors_dat.state, y = response_dat.state,      lambda = lambda_seq, nfolds = 10) 
    ## 
    ## Measure: Mean-Squared Error 
    ## 
    ##      Lambda Index Measure     SE Nonzero
    ## min 0.07943    12   18.35 0.9685       8
    ## 1se 0.31623     6   19.01 1.2366       4

Min lambda is 0.07943.

``` r
lasso_fit = glmnet::glmnet(x = predictors_dat.state,
                           y = response_dat.state,
                           lambda = cv_lasso_fit$lambda.min)
coef(lasso_fit)
```

    ## 14 x 1 sparse Matrix of class "dgCMatrix"
    ##                        s0
    ## (Intercept)  1.326566e+02
    ## age          5.490337e-02
    ## height      -4.040322e-01
    ## forearm      1.513398e-01
    ## wrist       -1.604327e+00
    ## i_weight     .           
    ## i_neck       5.102076e+02
    ## i_chest      .           
    ## i_abdomen   -6.465985e+03
    ## i_hip        .           
    ## i_thigh     -1.444694e+02
    ## i_knee       .           
    ## i_ankle      .           
    ## i_bicep     -9.356241e+01

Based on LASSO: 8 predictors (different though) - age, height, forearm,
wrist, i_neck, i_abdomen, i_thigh, i_bicep.

``` r
eight_pred_lasso = lm(bodyfat_siri ~ age + height + forearm + wrist + 
                  i_neck + i_abdomen + i_thigh + i_bicep, data = bodyfat_df)
summary(eight_pred_lasso)
```

    ## 
    ## Call:
    ## lm(formula = bodyfat_siri ~ age + height + forearm + wrist + 
    ##     i_neck + i_abdomen + i_thigh + i_bicep, data = bodyfat_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -10.9777  -3.0223  -0.0985   2.6728  10.4405 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.327e+02  1.651e+01   8.038 3.98e-14 ***
    ## age          7.211e-02  2.958e-02   2.438 0.015488 *  
    ## height      -4.124e-01  1.201e-01  -3.435 0.000696 ***
    ## forearm      2.544e-01  1.962e-01   1.297 0.195902    
    ## wrist       -1.843e+00  4.775e-01  -3.859 0.000146 ***
    ## i_neck       7.321e+02  2.984e+02   2.453 0.014859 *  
    ## i_abdomen   -6.521e+03  4.555e+02 -14.316  < 2e-16 ***
    ## i_thigh     -3.048e+02  3.998e+02  -0.762 0.446558    
    ## i_bicep     -1.390e+02  1.644e+02  -0.845 0.398815    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.227 on 243 degrees of freedom
    ## Multiple R-squared:  0.753,  Adjusted R-squared:  0.7449 
    ## F-statistic: 92.61 on 8 and 243 DF,  p-value: < 2.2e-16

DISCUSSION: Adj R^2 only SLIIIIGHTLY better than six_pred model.
Principle of parsimony? Smaller model is better? Not much predictive
capacity is lost.

## Diagnostics

``` r
par(mfrow = c(2,2))
plot(six_pred)
```

![](Code_Analysis_Siri_files/figure-gfm/diagnostic%20plots-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(eight_pred)
```

![](Code_Analysis_Siri_files/figure-gfm/diagnostic%20plots-2.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(eight_pred_lasso)
```

![](Code_Analysis_Siri_files/figure-gfm/diagnostic%20plots-3.png)<!-- -->

Looks good. Super slight differences. For six_pred, things look pretty
great.

## Validation

``` r
set.seed(2022)

# use 10-fold validation and create the training sets
train = trainControl(method = "cv", number = 10)

# fit the 6-variables model that we selected as our final model
model_caret = train(bodyfat_siri ~ age + height + forearm + wrist + i_neck + i_abdomen,
                    data = bodyfat_df,
                    trControl = train,
                    method = 'lm',
                    na.action = na.pass)
                    
                    
model_caret
```

    ## Linear Regression 
    ## 
    ## 252 samples
    ##   6 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 227, 227, 226, 227, 227, 227, ... 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared   MAE     
    ##   4.241122  0.7481343  3.460785
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

RMSE Rsquared MAE  
4.241122 0.7481343 3.460785

``` r
model_caret$resample
```

    ##        RMSE  Rsquared      MAE Resample
    ## 1  4.100914 0.8401044 3.382587   Fold01
    ## 2  3.819520 0.7783916 3.290048   Fold02
    ## 3  4.982825 0.6506995 3.999370   Fold03
    ## 4  4.357821 0.7407724 3.571123   Fold04
    ## 5  4.608197 0.6214811 3.623799   Fold05
    ## 6  4.209324 0.7926481 3.169832   Fold06
    ## 7  4.156964 0.7790983 3.417540   Fold07
    ## 8  3.676108 0.8287296 3.046982   Fold08
    ## 9  4.278965 0.6933328 3.605827   Fold09
    ## 10 4.220576 0.7560854 3.500740   Fold10

From the output above, the overall RMSE (root mean squared error) is
4.241122.
