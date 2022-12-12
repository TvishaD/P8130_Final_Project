Code_Analysis_Density
================
Tvisha R. Devavarapu
2022-12-12

``` r
library(tidyverse)
library(ggplot2)
library(patchwork)
library(GGally)
library(leaps)
library(caret)
```

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

![](Code_Analysis_Density_files/figure-gfm/Y%20distribution%20exploration-1.png)<!-- -->

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

![](Code_Analysis_Density_files/figure-gfm/Y%20distribution%20exploration-2.png)<!-- -->

NOTE: All are approximately normal DENSITY

``` r
bodyfat_df %>%
  ggplot(aes(x = age)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Code_Analysis_Density_files/figure-gfm/checking%20validity%20in%20y%20choice-1.png)<!-- -->

``` r
bodyfat_df %>% 
  select(-id, -bodyfat_siri, -bodyfat_brozek) %>% 
  gtsummary::tbl_summary() %>% 
  gtsummary::bold_labels()
```

<div id="tqycakdhgz" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#tqycakdhgz .gt_table {
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

#tqycakdhgz .gt_heading {
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

#tqycakdhgz .gt_title {
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

#tqycakdhgz .gt_subtitle {
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

#tqycakdhgz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tqycakdhgz .gt_col_headings {
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

#tqycakdhgz .gt_col_heading {
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

#tqycakdhgz .gt_column_spanner_outer {
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

#tqycakdhgz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#tqycakdhgz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#tqycakdhgz .gt_column_spanner {
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

#tqycakdhgz .gt_group_heading {
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
}

#tqycakdhgz .gt_empty_group_heading {
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

#tqycakdhgz .gt_from_md > :first-child {
  margin-top: 0;
}

#tqycakdhgz .gt_from_md > :last-child {
  margin-bottom: 0;
}

#tqycakdhgz .gt_row {
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

#tqycakdhgz .gt_stub {
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

#tqycakdhgz .gt_stub_row_group {
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

#tqycakdhgz .gt_row_group_first td {
  border-top-width: 2px;
}

#tqycakdhgz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tqycakdhgz .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#tqycakdhgz .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#tqycakdhgz .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tqycakdhgz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tqycakdhgz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#tqycakdhgz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#tqycakdhgz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tqycakdhgz .gt_footnotes {
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

#tqycakdhgz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#tqycakdhgz .gt_sourcenotes {
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

#tqycakdhgz .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#tqycakdhgz .gt_left {
  text-align: left;
}

#tqycakdhgz .gt_center {
  text-align: center;
}

#tqycakdhgz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#tqycakdhgz .gt_font_normal {
  font-weight: normal;
}

#tqycakdhgz .gt_font_bold {
  font-weight: bold;
}

#tqycakdhgz .gt_font_italic {
  font-style: italic;
}

#tqycakdhgz .gt_super {
  font-size: 65%;
}

#tqycakdhgz .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#tqycakdhgz .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#tqycakdhgz .gt_indent_1 {
  text-indent: 5px;
}

#tqycakdhgz .gt_indent_2 {
  text-indent: 10px;
}

#tqycakdhgz .gt_indent_3 {
  text-indent: 15px;
}

#tqycakdhgz .gt_indent_4 {
  text-indent: 20px;
}

#tqycakdhgz .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col"><strong>N = 252</strong><sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left" style="font-weight: bold;">body_density</td>
<td class="gt_row gt_center">1.055 (1.041, 1.070)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">age</td>
<td class="gt_row gt_center">43 (36, 54)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">weight</td>
<td class="gt_row gt_center">176 (159, 197)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">height</td>
<td class="gt_row gt_center">70.00 (68.25, 72.25)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">neck</td>
<td class="gt_row gt_center">38.00 (36.40, 39.42)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">chest</td>
<td class="gt_row gt_center">100 (94, 105)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">abdomen</td>
<td class="gt_row gt_center">91 (85, 99)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">hip</td>
<td class="gt_row gt_center">99 (96, 104)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">thigh</td>
<td class="gt_row gt_center">59.0 (56.0, 62.3)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">knee</td>
<td class="gt_row gt_center">38.50 (36.98, 39.92)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">ankle</td>
<td class="gt_row gt_center">22.80 (22.00, 24.00)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">bicep</td>
<td class="gt_row gt_center">32.05 (30.20, 34.32)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">forearm</td>
<td class="gt_row gt_center">28.70 (27.30, 30.00)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">wrist</td>
<td class="gt_row gt_center">18.30 (17.60, 18.80)</td></tr>
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
  select(-bodyfat_siri, -bodyfat_brozek, -id) %>% 
  relocate("body_density") %>% 
  ggpairs()
```

![](Code_Analysis_Density_files/figure-gfm/normality,%20y-x%20rels,%20collinearity%20in%20covariates%20(xs)-1.png)<!-- -->

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

![](Code_Analysis_Density_files/figure-gfm/inv_transformation%20for%20weight-1.png)<!-- -->

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
  select(-id, -bodyfat_siri, -bodyfat_brozek, -weight, -neck, -chest, -abdomen,
         -hip, -thigh, -knee, -ankle, -bicep)
```

NOTE: Collinearity continues though. Weight with all others. Just fyi.
Not manually removing any covariates as they would be removed during the
model selection processes?

## Model Selection

``` r
mult.fit = lm(body_density ~ ., data = bodyfat_df)

#Backward
step(mult.fit, direction = 'backward')
```

    ## Start:  AIC=-2316.36
    ## body_density ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_chest + i_abdomen + i_hip + i_thigh + i_knee + i_ankle + 
    ##     i_bicep
    ## 
    ##             Df Sum of Sq      RSS     AIC
    ## - i_thigh    1 0.0000324 0.023002 -2318.0
    ## - i_knee     1 0.0000474 0.023017 -2317.8
    ## - i_ankle    1 0.0000511 0.023020 -2317.8
    ## - forearm    1 0.0000933 0.023063 -2317.3
    ## - i_bicep    1 0.0000938 0.023063 -2317.3
    ## - i_hip      1 0.0001105 0.023080 -2317.2
    ## <none>                   0.022969 -2316.4
    ## - i_weight   1 0.0002142 0.023183 -2316.0
    ## - i_chest    1 0.0002984 0.023268 -2315.1
    ## - age        1 0.0004975 0.023467 -2313.0
    ## - i_neck     1 0.0006133 0.023583 -2311.7
    ## - height     1 0.0010232 0.023992 -2307.4
    ## - wrist      1 0.0014811 0.024450 -2302.6
    ## - i_abdomen  1 0.0077464 0.030716 -2245.1
    ## 
    ## Step:  AIC=-2318.01
    ## body_density ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_chest + i_abdomen + i_hip + i_knee + i_ankle + i_bicep
    ## 
    ##             Df Sum of Sq      RSS     AIC
    ## - i_knee     1 0.0000300 0.023032 -2319.7
    ## - i_ankle    1 0.0000493 0.023051 -2319.5
    ## - i_hip      1 0.0000836 0.023085 -2319.1
    ## - forearm    1 0.0000879 0.023089 -2319.1
    ## - i_bicep    1 0.0001264 0.023128 -2318.6
    ## <none>                   0.023002 -2318.0
    ## - i_weight   1 0.0002716 0.023273 -2317.1
    ## - i_chest    1 0.0003633 0.023365 -2316.1
    ## - age        1 0.0004693 0.023471 -2314.9
    ## - i_neck     1 0.0005935 0.023595 -2313.6
    ## - height     1 0.0012846 0.024286 -2306.3
    ## - wrist      1 0.0015502 0.024552 -2303.6
    ## - i_abdomen  1 0.0077715 0.030773 -2246.7
    ## 
    ## Step:  AIC=-2319.68
    ## body_density ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_chest + i_abdomen + i_hip + i_ankle + i_bicep
    ## 
    ##             Df Sum of Sq      RSS     AIC
    ## - i_ankle    1 0.0000350 0.023067 -2321.3
    ## - forearm    1 0.0000841 0.023116 -2320.8
    ## - i_hip      1 0.0001188 0.023150 -2320.4
    ## - i_bicep    1 0.0001239 0.023156 -2320.3
    ## <none>                   0.023032 -2319.7
    ## - i_weight   1 0.0002504 0.023282 -2318.9
    ## - i_chest    1 0.0003477 0.023379 -2317.9
    ## - age        1 0.0004431 0.023475 -2316.9
    ## - i_neck     1 0.0005760 0.023608 -2315.4
    ## - height     1 0.0013588 0.024390 -2307.2
    ## - wrist      1 0.0015727 0.024604 -2305.0
    ## - i_abdomen  1 0.0077602 0.030792 -2248.5
    ## 
    ## Step:  AIC=-2321.3
    ## body_density ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_chest + i_abdomen + i_hip + i_bicep
    ## 
    ##             Df Sum of Sq      RSS     AIC
    ## - forearm    1 0.0000817 0.023148 -2322.4
    ## - i_bicep    1 0.0001149 0.023182 -2322.0
    ## - i_hip      1 0.0001190 0.023186 -2322.0
    ## <none>                   0.023067 -2321.3
    ## - i_weight   1 0.0003203 0.023387 -2319.8
    ## - i_chest    1 0.0003598 0.023426 -2319.4
    ## - age        1 0.0004199 0.023487 -2318.8
    ## - i_neck     1 0.0006271 0.023694 -2316.5
    ## - height     1 0.0014072 0.024474 -2308.4
    ## - wrist      1 0.0015858 0.024653 -2306.5
    ## - i_abdomen  1 0.0077616 0.030828 -2250.2
    ## 
    ## Step:  AIC=-2322.41
    ## body_density ~ age + height + wrist + i_weight + i_neck + i_chest + 
    ##     i_abdomen + i_hip + i_bicep
    ## 
    ##             Df Sum of Sq      RSS     AIC
    ## - i_hip      1 0.0001518 0.023300 -2322.8
    ## - i_bicep    1 0.0001829 0.023331 -2322.4
    ## <none>                   0.023148 -2322.4
    ## - i_chest    1 0.0003611 0.023509 -2320.5
    ## - age        1 0.0003862 0.023534 -2320.2
    ## - i_weight   1 0.0004109 0.023559 -2320.0
    ## - i_neck     1 0.0005830 0.023731 -2318.1
    ## - height     1 0.0015207 0.024669 -2308.4
    ## - wrist      1 0.0015245 0.024673 -2308.3
    ## - i_abdomen  1 0.0076846 0.030833 -2252.2
    ## 
    ## Step:  AIC=-2322.76
    ## body_density ~ age + height + wrist + i_weight + i_neck + i_chest + 
    ##     i_abdomen + i_bicep
    ## 
    ##             Df Sum of Sq      RSS     AIC
    ## <none>                   0.023300 -2322.8
    ## - i_bicep    1 0.0001890 0.023489 -2322.7
    ## - i_weight   1 0.0002627 0.023563 -2321.9
    ## - i_chest    1 0.0002808 0.023581 -2321.7
    ## - i_neck     1 0.0005189 0.023819 -2319.2
    ## - age        1 0.0005288 0.023829 -2319.1
    ## - height     1 0.0013814 0.024681 -2310.2
    ## - wrist      1 0.0015389 0.024839 -2308.6
    ## - i_abdomen  1 0.0076672 0.030967 -2253.1

    ## 
    ## Call:
    ## lm(formula = body_density ~ age + height + wrist + i_weight + 
    ##     i_neck + i_chest + i_abdomen + i_bicep, data = bodyfat_df)
    ## 
    ## Coefficients:
    ## (Intercept)          age       height        wrist     i_weight       i_neck  
    ##    0.748436    -0.000152     0.001479     0.004588     5.583112    -1.629852  
    ##     i_chest    i_abdomen      i_bicep  
    ##   -3.872077    14.608371     0.518141

``` r
#Forward
intercept_only = lm(body_density ~ 1, data = bodyfat_df)
step(intercept_only, direction = "forward", scope = formula(mult.fit))
```

    ## Start:  AIC=-1995.68
    ## body_density ~ 1
    ## 
    ##             Df Sum of Sq      RSS     AIC
    ## + i_abdomen  1  0.059582 0.031329 -2262.2
    ## + i_chest    1  0.042951 0.047960 -2154.8
    ## + i_hip      1  0.035849 0.055062 -2120.0
    ## + i_weight   1  0.034634 0.056277 -2114.5
    ## + i_thigh    1  0.029240 0.061671 -2091.5
    ## + i_knee     1  0.022608 0.068304 -2065.7
    ## + i_bicep    1  0.022530 0.068381 -2065.4
    ## + i_neck     1  0.020177 0.070734 -2056.9
    ## + forearm    1  0.011242 0.079669 -2026.9
    ## + wrist      1  0.009645 0.081266 -2021.9
    ## + i_ankle    1  0.007553 0.083358 -2015.5
    ## + age        1  0.007008 0.083903 -2013.9
    ## <none>                   0.090911 -1995.7
    ## + height     1  0.000140 0.090771 -1994.1
    ## 
    ## Step:  AIC=-2262.15
    ## body_density ~ i_abdomen
    ## 
    ##            Df Sum of Sq      RSS     AIC
    ## + wrist     1 0.0046153 0.026713 -2300.3
    ## + height    1 0.0043664 0.026962 -2298.0
    ## + i_weight  1 0.0036408 0.027688 -2291.3
    ## + i_neck    1 0.0032382 0.028090 -2287.6
    ## + i_knee    1 0.0020914 0.029237 -2277.6
    ## + i_hip     1 0.0017084 0.029620 -2274.3
    ## + i_ankle   1 0.0013725 0.029956 -2271.4
    ## + i_chest   1 0.0011753 0.030153 -2269.8
    ## + forearm   1 0.0006630 0.030666 -2265.5
    ## + age       1 0.0006564 0.030672 -2265.5
    ## + i_thigh   1 0.0005802 0.030748 -2264.9
    ## + i_bicep   1 0.0004955 0.030833 -2264.2
    ## <none>                  0.031329 -2262.2
    ## 
    ## Step:  AIC=-2300.31
    ## body_density ~ i_abdomen + wrist
    ## 
    ##            Df  Sum of Sq      RSS     AIC
    ## + height    1 0.00206588 0.024647 -2318.6
    ## + age       1 0.00098831 0.025725 -2307.8
    ## + i_weight  1 0.00091909 0.025794 -2307.1
    ## + i_neck    1 0.00062102 0.026092 -2304.2
    ## + i_hip     1 0.00058392 0.026129 -2303.9
    ## + i_knee    1 0.00041882 0.026294 -2302.3
    ## <none>                   0.026713 -2300.3
    ## + i_chest   1 0.00019986 0.026513 -2300.2
    ## + i_thigh   1 0.00015853 0.026555 -2299.8
    ## + i_ankle   1 0.00004203 0.026671 -2298.7
    ## + i_bicep   1 0.00000311 0.026710 -2298.3
    ## + forearm   1 0.00000063 0.026713 -2298.3
    ## 
    ## Step:  AIC=-2318.59
    ## body_density ~ i_abdomen + wrist + height
    ## 
    ##            Df  Sum of Sq      RSS     AIC
    ## + i_neck    1 0.00043765 0.024210 -2321.1
    ## + age       1 0.00025264 0.024395 -2319.2
    ## <none>                   0.024647 -2318.6
    ## + i_chest   1 0.00018401 0.024463 -2318.5
    ## + i_bicep   1 0.00005597 0.024591 -2317.2
    ## + i_hip     1 0.00005393 0.024594 -2317.1
    ## + forearm   1 0.00004907 0.024598 -2317.1
    ## + i_ankle   1 0.00003865 0.024609 -2317.0
    ## + i_thigh   1 0.00000083 0.024647 -2316.6
    ## + i_knee    1 0.00000031 0.024647 -2316.6
    ## + i_weight  1 0.00000010 0.024647 -2316.6
    ## 
    ## Step:  AIC=-2321.11
    ## body_density ~ i_abdomen + wrist + height + i_neck
    ## 
    ##            Df  Sum of Sq      RSS     AIC
    ## + i_bicep   1 2.0194e-04 0.024008 -2321.2
    ## <none>                   0.024210 -2321.1
    ## + forearm   1 1.8666e-04 0.024023 -2321.1
    ## + age       1 1.7451e-04 0.024035 -2320.9
    ## + i_chest   1 6.7400e-05 0.024142 -2319.8
    ## + i_weight  1 5.8689e-05 0.024151 -2319.7
    ## + i_ankle   1 3.2051e-05 0.024178 -2319.4
    ## + i_hip     1 2.9434e-05 0.024180 -2319.4
    ## + i_thigh   1 1.0644e-05 0.024199 -2319.2
    ## + i_knee    1 2.5200e-07 0.024210 -2319.1
    ## 
    ## Step:  AIC=-2321.22
    ## body_density ~ i_abdomen + wrist + height + i_neck + i_bicep
    ## 
    ##            Df  Sum of Sq      RSS     AIC
    ## + age       1 0.00032635 0.023682 -2322.7
    ## <none>                   0.024008 -2321.2
    ## + i_chest   1 0.00013853 0.023869 -2320.7
    ## + i_hip     1 0.00010280 0.023905 -2320.3
    ## + forearm   1 0.00007656 0.023931 -2320.0
    ## + i_ankle   1 0.00001542 0.023992 -2319.4
    ## + i_thigh   1 0.00000978 0.023998 -2319.3
    ## + i_knee    1 0.00000537 0.024002 -2319.3
    ## + i_weight  1 0.00000113 0.024007 -2319.2
    ## 
    ## Step:  AIC=-2322.67
    ## body_density ~ i_abdomen + wrist + height + i_neck + i_bicep + 
    ##     age
    ## 
    ##            Df  Sum of Sq      RSS     AIC
    ## <none>                   0.023682 -2322.7
    ## + forearm   1 1.4307e-04 0.023539 -2322.2
    ## + i_chest   1 1.1867e-04 0.023563 -2321.9
    ## + i_weight  1 1.0054e-04 0.023581 -2321.7
    ## + i_ankle   1 8.5990e-05 0.023596 -2321.6
    ## + i_thigh   1 5.8307e-05 0.023623 -2321.3
    ## + i_hip     1 5.5940e-06 0.023676 -2320.7
    ## + i_knee    1 2.0000e-08 0.023682 -2320.7

    ## 
    ## Call:
    ## lm(formula = body_density ~ i_abdomen + wrist + height + i_neck + 
    ##     i_bicep + age, data = bodyfat_df)
    ## 
    ## Coefficients:
    ## (Intercept)    i_abdomen        wrist       height       i_neck      i_bicep  
    ##   0.7679086   15.3285984    0.0041606    0.0010116   -1.6136150    0.6505710  
    ##         age  
    ##  -0.0001059

``` r
#Stepwise
step(mult.fit, direction = 'both')
```

    ## Start:  AIC=-2316.36
    ## body_density ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_chest + i_abdomen + i_hip + i_thigh + i_knee + i_ankle + 
    ##     i_bicep
    ## 
    ##             Df Sum of Sq      RSS     AIC
    ## - i_thigh    1 0.0000324 0.023002 -2318.0
    ## - i_knee     1 0.0000474 0.023017 -2317.8
    ## - i_ankle    1 0.0000511 0.023020 -2317.8
    ## - forearm    1 0.0000933 0.023063 -2317.3
    ## - i_bicep    1 0.0000938 0.023063 -2317.3
    ## - i_hip      1 0.0001105 0.023080 -2317.2
    ## <none>                   0.022969 -2316.4
    ## - i_weight   1 0.0002142 0.023183 -2316.0
    ## - i_chest    1 0.0002984 0.023268 -2315.1
    ## - age        1 0.0004975 0.023467 -2313.0
    ## - i_neck     1 0.0006133 0.023583 -2311.7
    ## - height     1 0.0010232 0.023992 -2307.4
    ## - wrist      1 0.0014811 0.024450 -2302.6
    ## - i_abdomen  1 0.0077464 0.030716 -2245.1
    ## 
    ## Step:  AIC=-2318.01
    ## body_density ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_chest + i_abdomen + i_hip + i_knee + i_ankle + i_bicep
    ## 
    ##             Df Sum of Sq      RSS     AIC
    ## - i_knee     1 0.0000300 0.023032 -2319.7
    ## - i_ankle    1 0.0000493 0.023051 -2319.5
    ## - i_hip      1 0.0000836 0.023085 -2319.1
    ## - forearm    1 0.0000879 0.023089 -2319.1
    ## - i_bicep    1 0.0001264 0.023128 -2318.6
    ## <none>                   0.023002 -2318.0
    ## - i_weight   1 0.0002716 0.023273 -2317.1
    ## + i_thigh    1 0.0000324 0.022969 -2316.4
    ## - i_chest    1 0.0003633 0.023365 -2316.1
    ## - age        1 0.0004693 0.023471 -2314.9
    ## - i_neck     1 0.0005935 0.023595 -2313.6
    ## - height     1 0.0012846 0.024286 -2306.3
    ## - wrist      1 0.0015502 0.024552 -2303.6
    ## - i_abdomen  1 0.0077715 0.030773 -2246.7
    ## 
    ## Step:  AIC=-2319.68
    ## body_density ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_chest + i_abdomen + i_hip + i_ankle + i_bicep
    ## 
    ##             Df Sum of Sq      RSS     AIC
    ## - i_ankle    1 0.0000350 0.023067 -2321.3
    ## - forearm    1 0.0000841 0.023116 -2320.8
    ## - i_hip      1 0.0001188 0.023150 -2320.4
    ## - i_bicep    1 0.0001239 0.023156 -2320.3
    ## <none>                   0.023032 -2319.7
    ## - i_weight   1 0.0002504 0.023282 -2318.9
    ## + i_knee     1 0.0000300 0.023002 -2318.0
    ## - i_chest    1 0.0003477 0.023379 -2317.9
    ## + i_thigh    1 0.0000149 0.023017 -2317.8
    ## - age        1 0.0004431 0.023475 -2316.9
    ## - i_neck     1 0.0005760 0.023608 -2315.4
    ## - height     1 0.0013588 0.024390 -2307.2
    ## - wrist      1 0.0015727 0.024604 -2305.0
    ## - i_abdomen  1 0.0077602 0.030792 -2248.5
    ## 
    ## Step:  AIC=-2321.3
    ## body_density ~ age + height + forearm + wrist + i_weight + i_neck + 
    ##     i_chest + i_abdomen + i_hip + i_bicep
    ## 
    ##             Df Sum of Sq      RSS     AIC
    ## - forearm    1 0.0000817 0.023148 -2322.4
    ## - i_bicep    1 0.0001149 0.023182 -2322.0
    ## - i_hip      1 0.0001190 0.023186 -2322.0
    ## <none>                   0.023067 -2321.3
    ## - i_weight   1 0.0003203 0.023387 -2319.8
    ## + i_ankle    1 0.0000350 0.023032 -2319.7
    ## + i_thigh    1 0.0000171 0.023050 -2319.5
    ## + i_knee     1 0.0000157 0.023051 -2319.5
    ## - i_chest    1 0.0003598 0.023426 -2319.4
    ## - age        1 0.0004199 0.023487 -2318.8
    ## - i_neck     1 0.0006271 0.023694 -2316.5
    ## - height     1 0.0014072 0.024474 -2308.4
    ## - wrist      1 0.0015858 0.024653 -2306.5
    ## - i_abdomen  1 0.0077616 0.030828 -2250.2
    ## 
    ## Step:  AIC=-2322.41
    ## body_density ~ age + height + wrist + i_weight + i_neck + i_chest + 
    ##     i_abdomen + i_hip + i_bicep
    ## 
    ##             Df Sum of Sq      RSS     AIC
    ## - i_hip      1 0.0001518 0.023300 -2322.8
    ## - i_bicep    1 0.0001829 0.023331 -2322.4
    ## <none>                   0.023148 -2322.4
    ## + forearm    1 0.0000817 0.023067 -2321.3
    ## + i_ankle    1 0.0000326 0.023116 -2320.8
    ## + i_thigh    1 0.0000142 0.023134 -2320.6
    ## + i_knee     1 0.0000135 0.023135 -2320.6
    ## - i_chest    1 0.0003611 0.023509 -2320.5
    ## - age        1 0.0003862 0.023534 -2320.2
    ## - i_weight   1 0.0004109 0.023559 -2320.0
    ## - i_neck     1 0.0005830 0.023731 -2318.1
    ## - height     1 0.0015207 0.024669 -2308.4
    ## - wrist      1 0.0015245 0.024673 -2308.3
    ## - i_abdomen  1 0.0076846 0.030833 -2252.2
    ## 
    ## Step:  AIC=-2322.76
    ## body_density ~ age + height + wrist + i_weight + i_neck + i_chest + 
    ##     i_abdomen + i_bicep
    ## 
    ##             Df Sum of Sq      RSS     AIC
    ## <none>                   0.023300 -2322.8
    ## - i_bicep    1 0.0001890 0.023489 -2322.7
    ## + i_hip      1 0.0001518 0.023148 -2322.4
    ## + forearm    1 0.0001145 0.023186 -2322.0
    ## - i_weight   1 0.0002627 0.023563 -2321.9
    ## - i_chest    1 0.0002808 0.023581 -2321.7
    ## + i_knee     1 0.0000434 0.023257 -2321.2
    ## + i_ankle    1 0.0000323 0.023268 -2321.1
    ## + i_thigh    1 0.0000021 0.023298 -2320.8
    ## - i_neck     1 0.0005189 0.023819 -2319.2
    ## - age        1 0.0005288 0.023829 -2319.1
    ## - height     1 0.0013814 0.024681 -2310.2
    ## - wrist      1 0.0015389 0.024839 -2308.6
    ## - i_abdomen  1 0.0076672 0.030967 -2253.1

    ## 
    ## Call:
    ## lm(formula = body_density ~ age + height + wrist + i_weight + 
    ##     i_neck + i_chest + i_abdomen + i_bicep, data = bodyfat_df)
    ## 
    ## Coefficients:
    ## (Intercept)          age       height        wrist     i_weight       i_neck  
    ##    0.748436    -0.000152     0.001479     0.004588     5.583112    -1.629852  
    ##     i_chest    i_abdomen      i_bicep  
    ##   -3.872077    14.608371     0.518141

Based on Backward: 8 predictors - age, height, wrist, i_neck, i_abdomen,
i_chest, i_weight, i_bicep Based on Forward: 6 predictors - age, height,
i_bicep, wrist, i_neck, i_abdomen Based on Stepwise: 8 predictors - age,
height, wrist, i_neck, i_abdomen, i_chest, i_weight, i_bicep

``` r
six_pred = lm(body_density ~ age + height + i_bicep + wrist + i_neck + i_abdomen, data = bodyfat_df)
summary(six_pred)
```

    ## 
    ## Call:
    ## lm(formula = body_density ~ age + height + i_bicep + wrist + 
    ##     i_neck + i_abdomen, data = bodyfat_df)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.022888 -0.006533  0.000392  0.006822  0.038239 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  7.679e-01  3.425e-02  22.418  < 2e-16 ***
    ## age         -1.060e-04  5.766e-05  -1.837 0.067354 .  
    ## height       1.012e-03  2.790e-04   3.626 0.000350 ***
    ## i_bicep      6.506e-01  3.401e-01   1.913 0.056899 .  
    ## wrist        4.161e-03  1.090e-03   3.817 0.000171 ***
    ## i_neck      -1.614e+00  6.815e-01  -2.368 0.018677 *  
    ## i_abdomen    1.533e+01  8.378e-01  18.297  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.009832 on 245 degrees of freedom
    ## Multiple R-squared:  0.7395, Adjusted R-squared:  0.7331 
    ## F-statistic: 115.9 on 6 and 245 DF,  p-value: < 2.2e-16

``` r
eight_pred = lm(body_density ~ age + height + wrist + i_neck + i_abdomen + i_chest + i_weight + i_bicep, 
                data = bodyfat_df)
summary(eight_pred)
```

    ## 
    ## Call:
    ## lm(formula = body_density ~ age + height + wrist + i_neck + i_abdomen + 
    ##     i_chest + i_weight + i_bicep, data = bodyfat_df)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.024475 -0.006358 -0.000040  0.007376  0.036098 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  7.484e-01  3.788e-02  19.757  < 2e-16 ***
    ## age         -1.520e-04  6.473e-05  -2.348 0.019654 *  
    ## height       1.479e-03  3.897e-04   3.796 0.000186 ***
    ## wrist        4.588e-03  1.145e-03   4.006 8.21e-05 ***
    ## i_neck      -1.630e+00  7.006e-01  -2.326 0.020825 *  
    ## i_abdomen    1.461e+01  1.634e+00   8.942  < 2e-16 ***
    ## i_chest     -3.872e+00  2.263e+00  -1.711 0.088281 .  
    ## i_weight     5.583e+00  3.373e+00   1.655 0.099164 .  
    ## i_bicep      5.181e-01  3.691e-01   1.404 0.161619    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.009792 on 243 degrees of freedom
    ## Multiple R-squared:  0.7437, Adjusted R-squared:  0.7353 
    ## F-statistic: 88.14 on 8 and 243 DF,  p-value: < 2.2e-16

``` r
anova(six_pred, eight_pred)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: body_density ~ age + height + i_bicep + wrist + i_neck + i_abdomen
    ## Model 2: body_density ~ age + height + wrist + i_neck + i_abdomen + i_chest + 
    ##     i_weight + i_bicep
    ##   Res.Df      RSS Df  Sum of Sq      F Pr(>F)
    ## 1    245 0.023682                            
    ## 2    243 0.023300  2 0.00038139 1.9888 0.1391

``` r
bodyfat_df =
  bodyfat_df %>%
  select(body_density, everything())

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
    ## 5  FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 5  FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 6   TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 6   TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 7   TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## 7   TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 8   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## 8   TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## 9   TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
    ## 9   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
    ## 10  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
    ## 10  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE
    ## 11  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE
    ## 11  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE
    ## 12  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE
    ## 12  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE
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
    ## 5   TRUE
    ## 5  FALSE
    ## 6   TRUE
    ## 6  FALSE
    ## 7  FALSE
    ## 7   TRUE
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
    ##  [1]  76.616334 248.948859  30.794538  33.372861  11.388598  17.488615
    ##  [7]   8.853828  10.770843   8.761439   8.919778   7.379958   7.783662
    ## [13]   7.386423   7.897480   7.385538   7.428176   7.855258   8.199717
    ## [19]   9.008912   9.517907  10.646364  10.831300  12.335422  12.491552
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
    ## 5  FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 5  FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 6   TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 6   TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 7   TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## 7   TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 8   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## 8   TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## 9   TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
    ## 9   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
    ## 10  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
    ## 10  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE
    ## 11  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE
    ## 11  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE
    ## 12  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE
    ## 12  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE
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
    ## 5   TRUE
    ## 5  FALSE
    ## 6   TRUE
    ## 6  FALSE
    ## 7  FALSE
    ## 7   TRUE
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
    ##  [1] 0.6540141 0.4703370 0.7037992 0.7010402 0.7256040 0.7190500 0.7293851
    ##  [8] 0.7273170 0.7305514 0.7303799 0.7331292 0.7326902 0.7342125 0.7336544
    ## [15] 0.7353128 0.7352660 0.7359040 0.7355247 0.7357439 0.7351811 0.7350453
    ## [22] 0.7348400 0.7342834 0.7341093 0.7335425

``` r
b = regsubsets(body_density ~ ., data = bodyfat_df)
rs = summary(b)

par(mfrow = c(1,2))
plot(1:8, rs$cp, xlab = "No of predictors", ylab = "Cp Statistic")
abline(0,1)
plot(1:8, rs$adjr2, xlab = "No of predictors", ylab = "Adj R2")
```

![](Code_Analysis_Density_files/figure-gfm/automatic%20criterion%20output-1.png)<!-- -->

``` r
rs
```

    ## Subset selection object
    ## Call: regsubsets.formula(body_density ~ ., data = bodyfat_df)
    ## 13 Variables  (and intercept)
    ##           Forced in Forced out
    ## age           FALSE      FALSE
    ## height        FALSE      FALSE
    ## forearm       FALSE      FALSE
    ## wrist         FALSE      FALSE
    ## i_weight      FALSE      FALSE
    ## i_neck        FALSE      FALSE
    ## i_chest       FALSE      FALSE
    ## i_abdomen     FALSE      FALSE
    ## i_hip         FALSE      FALSE
    ## i_thigh       FALSE      FALSE
    ## i_knee        FALSE      FALSE
    ## i_ankle       FALSE      FALSE
    ## i_bicep       FALSE      FALSE
    ## 1 subsets of each size up to 8
    ## Selection Algorithm: exhaustive
    ##          age height forearm wrist i_weight i_neck i_chest i_abdomen i_hip
    ## 1  ( 1 ) " " " "    " "     " "   " "      " "    " "     "*"       " "  
    ## 2  ( 1 ) " " " "    " "     "*"   " "      " "    " "     "*"       " "  
    ## 3  ( 1 ) " " "*"    " "     "*"   " "      " "    " "     "*"       " "  
    ## 4  ( 1 ) " " "*"    " "     "*"   " "      "*"    " "     "*"       " "  
    ## 5  ( 1 ) " " "*"    " "     "*"   " "      "*"    " "     "*"       " "  
    ## 6  ( 1 ) "*" "*"    " "     "*"   " "      "*"    " "     "*"       " "  
    ## 7  ( 1 ) "*" "*"    " "     "*"   "*"      "*"    "*"     "*"       " "  
    ## 8  ( 1 ) "*" "*"    "*"     "*"   "*"      "*"    "*"     "*"       " "  
    ##          i_thigh i_knee i_ankle i_bicep
    ## 1  ( 1 ) " "     " "    " "     " "    
    ## 2  ( 1 ) " "     " "    " "     " "    
    ## 3  ( 1 ) " "     " "    " "     " "    
    ## 4  ( 1 ) " "     " "    " "     " "    
    ## 5  ( 1 ) " "     " "    " "     "*"    
    ## 6  ( 1 ) " "     " "    " "     "*"    
    ## 7  ( 1 ) " "     " "    " "     " "    
    ## 8  ( 1 ) " "     " "    " "     " "

Based on Cp: 8 predictors (age height forearm wrist i_weight i_neck
i_chest i_abdomen)

Based on Adj R^2: 8 predictors (age height forearm wrist i_weight i_neck
i_chest i_abdomen)

NOTE: Some of these are highly correlated (DISCUSSION POINT!). Would use
shrinkage method (LASSO) which would factor the matter of collinearity
into the process). If we were to just abide by this though, we would
have to use Vif values etc and drop the super highly correlated
covariates. (Must ensure that the predictive ability is retained though
by actually fitting the model and keeping track of the adj. r^2 or
whatever parameter.)

``` r
eight_pred_auto = lm(body_density ~ age + height + wrist + i_neck + i_abdomen + i_chest + i_weight + forearm, 
                data = bodyfat_df)
summary(eight_pred_auto)
```

    ## 
    ## Call:
    ## lm(formula = body_density ~ age + height + wrist + i_neck + i_abdomen + 
    ##     i_chest + i_weight + forearm, data = bodyfat_df)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.024644 -0.006605 -0.000212  0.006806  0.037061 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  7.749e-01  4.272e-02  18.139  < 2e-16 ***
    ## age         -1.542e-04  6.484e-05  -2.377   0.0182 *  
    ## height       1.524e-03  3.832e-04   3.977 9.23e-05 ***
    ## wrist        4.694e-03  1.152e-03   4.075 6.23e-05 ***
    ## i_neck      -1.654e+00  7.031e-01  -2.353   0.0194 *  
    ## i_abdomen    1.472e+01  1.647e+00   8.935  < 2e-16 ***
    ## i_chest     -3.892e+00  2.263e+00  -1.720   0.0867 .  
    ## i_weight     6.038e+00  3.272e+00   1.845   0.0662 .  
    ## forearm     -6.347e-04  4.472e-04  -1.419   0.1571    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.009791 on 243 degrees of freedom
    ## Multiple R-squared:  0.7437, Adjusted R-squared:  0.7353 
    ## F-statistic: 88.16 on 8 and 243 DF,  p-value: < 2.2e-16

``` r
seven_pred_sig = lm(body_density ~ age + height + wrist + i_neck + i_abdomen + i_chest + i_weight,
                    data = bodyfat_df)
summary(seven_pred_sig)
```

    ## 
    ## Call:
    ## lm(formula = body_density ~ age + height + wrist + i_neck + i_abdomen + 
    ##     i_chest + i_weight, data = bodyfat_df)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.024919 -0.006629  0.000204  0.006868  0.037004 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  7.468e-01  3.794e-02  19.685  < 2e-16 ***
    ## age         -1.460e-04  6.472e-05  -2.256 0.024982 *  
    ## height       1.611e-03  3.791e-04   4.249 3.05e-05 ***
    ## wrist        4.494e-03  1.146e-03   3.923 0.000114 ***
    ## i_neck      -1.486e+00  6.944e-01  -2.139 0.033393 *  
    ## i_abdomen    1.423e+01  1.615e+00   8.814 2.32e-16 ***
    ## i_chest     -3.785e+00  2.266e+00  -1.670 0.096169 .  
    ## i_weight     7.226e+00  3.170e+00   2.280 0.023497 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.009812 on 244 degrees of freedom
    ## Multiple R-squared:  0.7416, Adjusted R-squared:  0.7342 
    ## F-statistic: 100.1 on 7 and 244 DF,  p-value: < 2.2e-16

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
  select(body_density) %>% 
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
    ##       Lambda Index   Measure        SE Nonzero
    ## min 0.001000    31 0.0001062 8.019e-06       4
    ## 1se 0.001259    30 0.0001095 8.537e-06       3

Min lambda is 0.001000.

``` r
lasso_fit = glmnet::glmnet(x = predictors_dat.state,
                           y = response_dat.state,
                           lambda = cv_lasso_fit$lambda.min)
coef(lasso_fit)
```

    ## 14 x 1 sparse Matrix of class "dgCMatrix"
    ##                        s0
    ## (Intercept)  0.8125761573
    ## age         -0.0000171612
    ## height       0.0009295641
    ## forearm      .           
    ## wrist        0.0019373760
    ## i_weight     .           
    ## i_neck       .           
    ## i_chest      .           
    ## i_abdomen   13.0751558004
    ## i_hip        .           
    ## i_thigh      .           
    ## i_knee       .           
    ## i_ankle      .           
    ## i_bicep      .

Based on LASSO: 4 predictors (different though) - age, height, wrist,
i_abdomen.

``` r
four_pred_lasso = lm(body_density ~ age + height + wrist + i_abdomen, 
                      data = bodyfat_df)
summary(four_pred_lasso)
```

    ## 
    ## Call:
    ## lm(formula = body_density ~ age + height + wrist + i_abdomen, 
    ##     data = bodyfat_df)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.023202 -0.006432  0.000619  0.006849  0.039288 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  7.352e-01  2.525e-02  29.117  < 2e-16 ***
    ## age         -8.847e-05  5.531e-05  -1.599 0.111016    
    ## height       1.034e-03  2.818e-04   3.670 0.000297 ***
    ## wrist        4.741e-03  9.347e-04   5.072 7.73e-07 ***
    ## i_abdomen    1.509e+01  6.605e-01  22.852  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.009938 on 247 degrees of freedom
    ## Multiple R-squared:  0.7317, Adjusted R-squared:  0.7273 
    ## F-statistic: 168.4 on 4 and 247 DF,  p-value: < 2.2e-16

Small model! About the same adjusted R^2. Predictive capacity retained.

## Diagnostics

``` r
par(mfrow = c(2,2))
plot(six_pred)
```

![](Code_Analysis_Density_files/figure-gfm/diagnostic%20plots-1.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(eight_pred)
```

![](Code_Analysis_Density_files/figure-gfm/diagnostic%20plots-2.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(eight_pred_auto)
```

![](Code_Analysis_Density_files/figure-gfm/diagnostic%20plots-3.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(seven_pred_sig)
```

![](Code_Analysis_Density_files/figure-gfm/diagnostic%20plots-4.png)<!-- -->

``` r
par(mfrow = c(2,2))
plot(four_pred_lasso)
```

![](Code_Analysis_Density_files/figure-gfm/diagnostic%20plots-5.png)<!-- -->

## Validation

``` r
set.seed(2022)

# use 10-fold validation and create the training sets
train = trainControl(method = "cv", number = 10)

# fit the 6-variables model that we selected as our final model
model_caret = train(body_density ~ age + height + wrist + i_abdomen,
                    data = bodyfat_df,
                    trControl = train,
                    method = 'lm',
                    na.action = na.pass)
                    
                    
model_caret
```

    ## Linear Regression 
    ## 
    ## 252 samples
    ##   4 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 228, 227, 226, 225, 228, 226, ... 
    ## Resampling results:
    ## 
    ##   RMSE         Rsquared   MAE        
    ##   0.009892999  0.7339559  0.008172341
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

RMSE Rsquared MAE  
0.009797114 0.7384043 0.008104832 : six_pred

RMSE Rsquared MAE  
0.00981521 0.7354601 0.008116662 : eight_pred

RMSE Rsquared MAE  
0.009784504 0.7335926 0.008047442 : eight_pred_auto

RMSE Rsquared MAE  
0.00981479 0.7344385 0.008082829 : seven_pred

RMSE Rsquared MAE  
0.009892999 0.7339559 0.008172341 : four_pred_lasso

WE PICK THE 4 MODEL. USING LASSO. WITH DENSITY AS THE Y (AS RMSE IS THE
LOWEST HERE). – NO COLLINEARITY WITHIN COVARIATES IN THIS MODEL. –
NORMALITY ASSUMPTIONS ARE MET (ONLY ONE TRANSFORMED VARIABLE). – NO
INFLUENCE OF OUTLIERS. – HOMOSCEDASTIC. – MEANINGFUL VARIABLES. THEY ALL
MAKE SENSE.

– STRENGTHEN THIS ARGUMENT BY POINTING OUT THAT THE COVARIATES IN OTHER
MODELS ARE CORRELATED.

``` r
model_caret$resample
```

    ##           RMSE  Rsquared         MAE Resample
    ## 1  0.007489805 0.8864315 0.006346002   Fold01
    ## 2  0.008477103 0.7695404 0.007156762   Fold02
    ## 3  0.013669514 0.5328213 0.010895287   Fold03
    ## 4  0.008863450 0.7912088 0.007453438   Fold04
    ## 5  0.011833889 0.6489546 0.010104886   Fold05
    ## 6  0.010682963 0.6522852 0.008906822   Fold06
    ## 7  0.008709408 0.7506146 0.007049602   Fold07
    ## 8  0.011372452 0.6360131 0.009179398   Fold08
    ## 9  0.009836959 0.8206014 0.008460619   Fold09
    ## 10 0.007994449 0.8510879 0.006170594   Fold10
