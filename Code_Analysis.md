Code and Analysis
================
2022-12-11

``` r
library(tidyverse)
library(ggplot2)
library(patchwork)
library(GGally)
library(leaps)
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

![](Code_Analysis_files/figure-gfm/Y%20distribution%20exploration-1.png)<!-- -->

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

![](Code_Analysis_files/figure-gfm/Y%20distribution%20exploration-2.png)<!-- -->

NOTE: All are approximately normal \* Probably going with Brozek.
(60-92)

``` r
bodyfat_df %>%
  ggplot(aes(x = age)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Code_Analysis_files/figure-gfm/checking%20validity%20in%20y%20choice-1.png)<!-- -->

``` r
bodyfat_df %>% 
  select(-id, -bodyfat_siri, -body_density) %>% 
  gtsummary::tbl_summary() %>% 
  gtsummary::bold_labels()
```

<div id="xufzvojddw" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#xufzvojddw .gt_table {
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

#xufzvojddw .gt_heading {
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

#xufzvojddw .gt_title {
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

#xufzvojddw .gt_subtitle {
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

#xufzvojddw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xufzvojddw .gt_col_headings {
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

#xufzvojddw .gt_col_heading {
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

#xufzvojddw .gt_column_spanner_outer {
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

#xufzvojddw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#xufzvojddw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#xufzvojddw .gt_column_spanner {
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

#xufzvojddw .gt_group_heading {
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

#xufzvojddw .gt_empty_group_heading {
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

#xufzvojddw .gt_from_md > :first-child {
  margin-top: 0;
}

#xufzvojddw .gt_from_md > :last-child {
  margin-bottom: 0;
}

#xufzvojddw .gt_row {
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

#xufzvojddw .gt_stub {
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

#xufzvojddw .gt_stub_row_group {
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

#xufzvojddw .gt_row_group_first td {
  border-top-width: 2px;
}

#xufzvojddw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xufzvojddw .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#xufzvojddw .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#xufzvojddw .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xufzvojddw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xufzvojddw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#xufzvojddw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#xufzvojddw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xufzvojddw .gt_footnotes {
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

#xufzvojddw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#xufzvojddw .gt_sourcenotes {
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

#xufzvojddw .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#xufzvojddw .gt_left {
  text-align: left;
}

#xufzvojddw .gt_center {
  text-align: center;
}

#xufzvojddw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#xufzvojddw .gt_font_normal {
  font-weight: normal;
}

#xufzvojddw .gt_font_bold {
  font-weight: bold;
}

#xufzvojddw .gt_font_italic {
  font-style: italic;
}

#xufzvojddw .gt_super {
  font-size: 65%;
}

#xufzvojddw .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#xufzvojddw .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#xufzvojddw .gt_indent_1 {
  text-indent: 5px;
}

#xufzvojddw .gt_indent_2 {
  text-indent: 10px;
}

#xufzvojddw .gt_indent_3 {
  text-indent: 15px;
}

#xufzvojddw .gt_indent_4 {
  text-indent: 20px;
}

#xufzvojddw .gt_indent_5 {
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
    <tr><td class="gt_row gt_left" style="font-weight: bold;">bodyfat_brozek</td>
<td class="gt_row gt_center">19 (13, 25)</td></tr>
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
  select(-bodyfat_siri, -body_density, -id) %>% 
  relocate("bodyfat_brozek") %>% 
  ggpairs()
```

![](Code_Analysis_files/figure-gfm/normality,%20y-x%20rels,%20collinearity%20in%20covariates%20(xs)-1.png)<!-- -->

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

![](Code_Analysis_files/figure-gfm/inv_transformation%20for%20weight-1.png)<!-- -->

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
  select(-id, -bodyfat_siri, -body_density, -weight, -neck, -chest, -abdomen,
         -hip, -thigh, -knee, -ankle, -bicep)
```

NOTE: Collinearity continues though. Weight with all others. Just fyi.
Not manually removing any covariates as they would be removed during the
model selection processes?

## Model Selection

``` r
mult.fit = lm(bodyfat_brozek ~ ., data = bodyfat_df)

#Backward
step(mult.fit, direction = 'backward')
```

    ## Start:  AIC=703.89
    ## bodyfat_brozek ~ age + height + forearm + wrist + i_weight + 
    ##     i_neck + i_chest + i_abdomen + i_hip + i_thigh + i_knee + 
    ##     i_ankle + i_bicep
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_ankle    1      1.88 3685.1 702.02
    ## - i_thigh    1      5.64 3688.9 702.28
    ## - i_hip      1      5.89 3689.1 702.29
    ## - i_bicep    1      6.20 3689.4 702.32
    ## - i_knee     1     11.70 3694.9 702.69
    ## - forearm    1     20.58 3703.8 703.30
    ## - i_weight   1     24.71 3707.9 703.58
    ## - i_chest    1     25.99 3709.2 703.66
    ## <none>                   3683.2 703.89
    ## - i_neck     1     91.59 3774.8 708.08
    ## - age        1     99.44 3782.7 708.61
    ## - height     1    129.78 3813.0 710.62
    ## - wrist      1    199.06 3882.3 715.16
    ## - i_abdomen  1   1203.44 4886.6 773.14
    ## 
    ## Step:  AIC=702.02
    ## bodyfat_brozek ~ age + height + forearm + wrist + i_weight + 
    ##     i_neck + i_chest + i_abdomen + i_hip + i_thigh + i_knee + 
    ##     i_bicep
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_thigh    1      5.50 3690.6 700.40
    ## - i_bicep    1      5.76 3690.9 700.41
    ## - i_hip      1      6.25 3691.3 700.45
    ## - i_knee     1     10.19 3695.3 700.72
    ## - forearm    1     20.20 3705.3 701.40
    ## - i_chest    1     26.48 3711.6 701.82
    ## - i_weight   1     28.83 3713.9 701.98
    ## <none>                   3685.1 702.02
    ## - i_neck     1     95.73 3780.8 706.48
    ## - age        1     97.58 3782.7 706.61
    ## - height     1    135.14 3820.2 709.10
    ## - wrist      1    205.39 3890.5 713.69
    ## - i_abdomen  1   1212.42 4897.5 771.70
    ## 
    ## Step:  AIC=700.4
    ## bodyfat_brozek ~ age + height + forearm + wrist + i_weight + 
    ##     i_neck + i_chest + i_abdomen + i_hip + i_knee + i_bicep
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_hip      1      3.31 3693.9 698.62
    ## - i_knee     1      6.87 3697.5 698.87
    ## - i_bicep    1      9.04 3699.6 699.01
    ## - forearm    1     19.18 3709.8 699.70
    ## <none>                   3690.6 700.40
    ## - i_chest    1     33.96 3724.6 700.70
    ## - i_weight   1     37.37 3728.0 700.94
    ## - i_neck     1     92.39 3783.0 704.63
    ## - age        1     93.28 3783.9 704.69
    ## - height     1    172.15 3862.8 709.89
    ## - wrist      1    217.17 3907.8 712.81
    ## - i_abdomen  1   1217.19 4907.8 770.23
    ## 
    ## Step:  AIC=698.62
    ## bodyfat_brozek ~ age + height + forearm + wrist + i_weight + 
    ##     i_neck + i_chest + i_abdomen + i_knee + i_bicep
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_bicep    1      8.84 3702.8 697.23
    ## - i_knee     1     10.07 3704.0 697.31
    ## - forearm    1     22.04 3715.9 698.12
    ## <none>                   3693.9 698.62
    ## - i_chest    1     31.26 3725.2 698.75
    ## - i_weight   1     35.30 3729.2 699.02
    ## - i_neck     1     90.29 3784.2 702.71
    ## - age        1    110.03 3803.9 704.02
    ## - height     1    171.53 3865.4 708.06
    ## - wrist      1    217.13 3911.0 711.02
    ## - i_abdomen  1   1260.42 4954.3 770.60
    ## 
    ## Step:  AIC=697.23
    ## bodyfat_brozek ~ age + height + forearm + wrist + i_weight + 
    ##     i_neck + i_chest + i_abdomen + i_knee
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_knee     1      9.92 3712.7 695.90
    ## <none>                   3702.8 697.23
    ## - i_chest    1     30.60 3733.4 697.30
    ## - forearm    1     32.01 3734.8 697.39
    ## - i_weight   1     48.64 3751.4 698.51
    ## - i_neck     1     85.38 3788.1 700.97
    ## - age        1    107.44 3810.2 702.43
    ## - height     1    196.26 3899.0 708.24
    ## - wrist      1    214.97 3917.7 709.45
    ## - i_abdomen  1   1252.57 4955.3 768.65
    ## 
    ## Step:  AIC=695.9
    ## bodyfat_brozek ~ age + height + forearm + wrist + i_weight + 
    ##     i_neck + i_chest + i_abdomen
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_chest    1     26.18 3738.9 695.67
    ## <none>                   3712.7 695.90
    ## - forearm    1     32.11 3744.8 696.07
    ## - i_weight   1     38.90 3751.6 696.53
    ## - i_neck     1     79.19 3791.9 699.22
    ## - age        1    103.20 3815.9 700.81
    ## - height     1    200.63 3913.3 707.16
    ## - wrist      1    230.39 3943.1 709.07
    ## - i_abdomen  1   1245.27 4957.9 766.79
    ## 
    ## Step:  AIC=695.67
    ## bodyfat_brozek ~ age + height + forearm + wrist + i_weight + 
    ##     i_neck + i_abdomen
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_weight   1     19.70 3758.6 694.99
    ## <none>                   3738.9 695.67
    ## - forearm    1     30.24 3769.1 695.70
    ## - age        1     89.74 3828.6 699.65
    ## - i_neck     1     91.22 3830.1 699.75
    ## - height     1    174.57 3913.4 705.17
    ## - wrist      1    225.88 3964.7 708.45
    ## - i_abdomen  1   1230.34 4969.2 765.36
    ## 
    ## Step:  AIC=694.99
    ## bodyfat_brozek ~ age + height + forearm + wrist + i_neck + i_abdomen
    ## 
    ##             Df Sum of Sq     RSS    AIC
    ## <none>                    3758.6 694.99
    ## - forearm    1      51.4  3810.0 696.42
    ## - age        1      70.1  3828.6 697.65
    ## - i_neck     1      76.5  3835.1 698.08
    ## - height     1     181.6  3940.2 704.89
    ## - wrist      1     206.3  3964.8 706.46
    ## - i_abdomen  1    6333.4 10091.9 941.90

    ## 
    ## Call:
    ## lm(formula = bodyfat_brozek ~ age + height + forearm + wrist + 
    ##     i_neck + i_abdomen, data = bodyfat_df)
    ## 
    ## Coefficients:
    ## (Intercept)          age       height      forearm        wrist       i_neck  
    ##   1.175e+02    4.871e-02   -3.824e-01    3.121e-01   -1.603e+00    6.063e+02  
    ##   i_abdomen  
    ##  -6.364e+03

``` r
#Forward
intercept_only = lm(bodyfat_brozek ~ 1, data = bodyfat_df)
step(intercept_only, direction = "forward", scope = formula(mult.fit))
```

    ## Start:  AIC=1033.09
    ## bodyfat_brozek ~ 1
    ## 
    ##             Df Sum of Sq     RSS     AIC
    ## + i_abdomen  1   10195.8  4883.2  750.96
    ## + i_chest    1    7502.6  7576.4  861.65
    ## + i_hip      1    6244.4  8834.7  900.37
    ## + i_weight   1    6031.7  9047.3  906.36
    ## + i_thigh    1    4981.7 10097.3  934.03
    ## + i_knee     1    3920.4 11158.6  959.21
    ## + i_bicep    1    3826.9 11252.2  961.32
    ## + i_neck     1    3608.9 11470.1  966.15
    ## + forearm    1    1990.0 13089.0  999.43
    ## + wrist      1    1821.6 13257.4 1002.65
    ## + i_ankle    1    1270.8 13808.2 1012.90
    ## + age        1    1260.9 13818.1 1013.08
    ## <none>                   15079.0 1033.09
    ## + height     1       9.1 15069.9 1034.94
    ## 
    ## Step:  AIC=750.96
    ## bodyfat_brozek ~ i_abdomen
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## + wrist     1    649.32 4233.9 717.01
    ## + height    1    645.22 4238.0 717.25
    ## + i_weight  1    553.68 4329.5 722.63
    ## + i_neck    1    466.09 4417.1 727.68
    ## + i_knee    1    334.85 4548.4 735.06
    ## + i_hip     1    247.22 4636.0 739.87
    ## + i_ankle   1    245.57 4637.6 739.96
    ## + i_chest   1    145.58 4737.6 745.33
    ## + age       1    132.38 4750.8 746.04
    ## + i_thigh   1    104.14 4779.1 747.53
    ## + forearm   1     95.48 4787.7 747.98
    ## + i_bicep   1     90.67 4792.6 748.24
    ## <none>                  4883.2 750.96
    ## 
    ## Step:  AIC=717.01
    ## bodyfat_brozek ~ i_abdomen + wrist
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## + height    1   313.447 3920.5 699.62
    ## + age       1   187.536 4046.4 707.59
    ## + i_weight  1   154.006 4079.9 709.67
    ## + i_neck    1    92.832 4141.1 713.42
    ## + i_hip     1    86.339 4147.6 713.81
    ## + i_knee    1    79.637 4154.3 714.22
    ## + i_thigh   1    34.934 4199.0 716.92
    ## <none>                  4233.9 717.01
    ## + i_chest   1    19.974 4213.9 717.81
    ## + i_ankle   1    19.637 4214.3 717.83
    ## + i_bicep   1     0.349 4233.5 718.98
    ## + forearm   1     0.031 4233.9 719.00
    ## 
    ## Step:  AIC=699.62
    ## bodyfat_brozek ~ i_abdomen + wrist + height
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## + i_neck    1    65.230 3855.2 697.39
    ## + age       1    60.038 3860.4 697.73
    ## <none>                  3920.5 699.62
    ## + i_chest   1    18.032 3902.4 700.46
    ## + i_hip     1     7.452 3913.0 701.14
    ## + forearm   1     6.722 3913.7 701.19
    ## + i_bicep   1     2.643 3917.8 701.45
    ## + i_thigh   1     1.949 3918.5 701.50
    ## + i_knee    1     0.736 3919.7 701.57
    ## + i_weight  1     0.475 3920.0 701.59
    ## + i_ankle   1     0.193 3920.3 701.61
    ## 
    ## Step:  AIC=697.39
    ## bodyfat_brozek ~ i_abdomen + wrist + height + i_neck
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## + age       1    45.248 3810.0 696.42
    ## <none>                  3855.2 697.39
    ## + forearm   1    26.601 3828.6 697.65
    ## + i_bicep   1    17.358 3837.9 698.26
    ## + i_chest   1     4.582 3850.6 699.09
    ## + i_weight  1     4.404 3850.8 699.11
    ## + i_hip     1     3.956 3851.3 699.14
    ## + i_knee    1     0.770 3854.4 699.34
    ## + i_ankle   1     0.050 3855.2 699.39
    ## + i_thigh   1     0.038 3855.2 699.39
    ## 
    ## Step:  AIC=696.42
    ## bodyfat_brozek ~ i_abdomen + wrist + height + i_neck + age
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## + forearm   1    51.416 3758.6 694.99
    ## + i_weight  1    40.875 3769.1 695.70
    ## + i_bicep   1    40.604 3769.4 695.72
    ## <none>                  3810.0 696.42
    ## + i_thigh   1    25.663 3784.3 696.72
    ## + i_ankle   1     5.454 3804.5 698.06
    ## + i_hip     1     2.295 3807.7 698.27
    ## + i_chest   1     1.954 3808.0 698.29
    ## + i_knee    1     0.070 3809.9 698.41
    ## 
    ## Step:  AIC=694.99
    ## bodyfat_brozek ~ i_abdomen + wrist + height + i_neck + age + 
    ##     forearm
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## <none>                  3758.6 694.99
    ## + i_weight  1   19.6954 3738.9 695.67
    ## + i_thigh   1   18.4986 3740.1 695.75
    ## + i_bicep   1   17.1568 3741.4 695.84
    ## + i_chest   1    6.9805 3751.6 696.53
    ## + i_ankle   1    3.9956 3754.6 696.73
    ## + i_hip     1    1.5305 3757.0 696.89
    ## + i_knee    1    0.2744 3758.3 696.98

    ## 
    ## Call:
    ## lm(formula = bodyfat_brozek ~ i_abdomen + wrist + height + i_neck + 
    ##     age + forearm, data = bodyfat_df)
    ## 
    ## Coefficients:
    ## (Intercept)    i_abdomen        wrist       height       i_neck          age  
    ##   1.175e+02   -6.364e+03   -1.603e+00   -3.824e-01    6.063e+02    4.871e-02  
    ##     forearm  
    ##   3.121e-01

``` r
#Stepwise
step(mult.fit, direction = 'both')
```

    ## Start:  AIC=703.89
    ## bodyfat_brozek ~ age + height + forearm + wrist + i_weight + 
    ##     i_neck + i_chest + i_abdomen + i_hip + i_thigh + i_knee + 
    ##     i_ankle + i_bicep
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_ankle    1      1.88 3685.1 702.02
    ## - i_thigh    1      5.64 3688.9 702.28
    ## - i_hip      1      5.89 3689.1 702.29
    ## - i_bicep    1      6.20 3689.4 702.32
    ## - i_knee     1     11.70 3694.9 702.69
    ## - forearm    1     20.58 3703.8 703.30
    ## - i_weight   1     24.71 3707.9 703.58
    ## - i_chest    1     25.99 3709.2 703.66
    ## <none>                   3683.2 703.89
    ## - i_neck     1     91.59 3774.8 708.08
    ## - age        1     99.44 3782.7 708.61
    ## - height     1    129.78 3813.0 710.62
    ## - wrist      1    199.06 3882.3 715.16
    ## - i_abdomen  1   1203.44 4886.6 773.14
    ## 
    ## Step:  AIC=702.02
    ## bodyfat_brozek ~ age + height + forearm + wrist + i_weight + 
    ##     i_neck + i_chest + i_abdomen + i_hip + i_thigh + i_knee + 
    ##     i_bicep
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_thigh    1      5.50 3690.6 700.40
    ## - i_bicep    1      5.76 3690.9 700.41
    ## - i_hip      1      6.25 3691.3 700.45
    ## - i_knee     1     10.19 3695.3 700.72
    ## - forearm    1     20.20 3705.3 701.40
    ## - i_chest    1     26.48 3711.6 701.82
    ## - i_weight   1     28.83 3713.9 701.98
    ## <none>                   3685.1 702.02
    ## + i_ankle    1      1.88 3683.2 703.89
    ## - i_neck     1     95.73 3780.8 706.48
    ## - age        1     97.58 3782.7 706.61
    ## - height     1    135.14 3820.2 709.10
    ## - wrist      1    205.39 3890.5 713.69
    ## - i_abdomen  1   1212.42 4897.5 771.70
    ## 
    ## Step:  AIC=700.4
    ## bodyfat_brozek ~ age + height + forearm + wrist + i_weight + 
    ##     i_neck + i_chest + i_abdomen + i_hip + i_knee + i_bicep
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_hip      1      3.31 3693.9 698.62
    ## - i_knee     1      6.87 3697.5 698.87
    ## - i_bicep    1      9.04 3699.6 699.01
    ## - forearm    1     19.18 3709.8 699.70
    ## <none>                   3690.6 700.40
    ## - i_chest    1     33.96 3724.6 700.70
    ## - i_weight   1     37.37 3728.0 700.94
    ## + i_thigh    1      5.50 3685.1 702.02
    ## + i_ankle    1      1.74 3688.9 702.28
    ## - i_neck     1     92.39 3783.0 704.63
    ## - age        1     93.28 3783.9 704.69
    ## - height     1    172.15 3862.8 709.89
    ## - wrist      1    217.17 3907.8 712.81
    ## - i_abdomen  1   1217.19 4907.8 770.23
    ## 
    ## Step:  AIC=698.62
    ## bodyfat_brozek ~ age + height + forearm + wrist + i_weight + 
    ##     i_neck + i_chest + i_abdomen + i_knee + i_bicep
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_bicep    1      8.84 3702.8 697.23
    ## - i_knee     1     10.07 3704.0 697.31
    ## - forearm    1     22.04 3715.9 698.12
    ## <none>                   3693.9 698.62
    ## - i_chest    1     31.26 3725.2 698.75
    ## - i_weight   1     35.30 3729.2 699.02
    ## + i_hip      1      3.31 3690.6 700.40
    ## + i_thigh    1      2.57 3691.3 700.45
    ## + i_ankle    1      2.04 3691.9 700.48
    ## - i_neck     1     90.29 3784.2 702.71
    ## - age        1    110.03 3803.9 704.02
    ## - height     1    171.53 3865.4 708.06
    ## - wrist      1    217.13 3911.0 711.02
    ## - i_abdomen  1   1260.42 4954.3 770.60
    ## 
    ## Step:  AIC=697.23
    ## bodyfat_brozek ~ age + height + forearm + wrist + i_weight + 
    ##     i_neck + i_chest + i_abdomen + i_knee
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_knee     1      9.92 3712.7 695.90
    ## <none>                   3702.8 697.23
    ## - i_chest    1     30.60 3733.4 697.30
    ## - forearm    1     32.01 3734.8 697.39
    ## - i_weight   1     48.64 3751.4 698.51
    ## + i_bicep    1      8.84 3693.9 698.62
    ## + i_thigh    1      4.91 3697.8 698.89
    ## + i_hip      1      3.11 3699.6 699.01
    ## + i_ankle    1      1.42 3701.3 699.13
    ## - i_neck     1     85.38 3788.1 700.97
    ## - age        1    107.44 3810.2 702.43
    ## - height     1    196.26 3899.0 708.24
    ## - wrist      1    214.97 3917.7 709.45
    ## - i_abdomen  1   1252.57 4955.3 768.65
    ## 
    ## Step:  AIC=695.9
    ## bodyfat_brozek ~ age + height + forearm + wrist + i_weight + 
    ##     i_neck + i_chest + i_abdomen
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_chest    1     26.18 3738.9 695.67
    ## <none>                   3712.7 695.90
    ## - forearm    1     32.11 3744.8 696.07
    ## - i_weight   1     38.90 3751.6 696.53
    ## + i_knee     1      9.92 3702.8 697.23
    ## + i_bicep    1      8.70 3704.0 697.31
    ## + i_hip      1      6.21 3706.5 697.48
    ## + i_thigh    1      0.94 3711.7 697.84
    ## + i_ankle    1      0.21 3712.5 697.89
    ## - i_neck     1     79.19 3791.9 699.22
    ## - age        1    103.20 3815.9 700.81
    ## - height     1    200.63 3913.3 707.16
    ## - wrist      1    230.39 3943.1 709.07
    ## - i_abdomen  1   1245.27 4957.9 766.79
    ## 
    ## Step:  AIC=695.67
    ## bodyfat_brozek ~ age + height + forearm + wrist + i_weight + 
    ##     i_neck + i_abdomen
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - i_weight   1     19.70 3758.6 694.99
    ## <none>                   3738.9 695.67
    ## - forearm    1     30.24 3769.1 695.70
    ## + i_chest    1     26.18 3712.7 695.90
    ## + i_bicep    1      8.12 3730.7 697.12
    ## + i_thigh    1      5.99 3732.9 697.27
    ## + i_knee     1      5.51 3733.4 697.30
    ## + i_hip      1      1.83 3737.0 697.55
    ## + i_ankle    1      0.53 3738.3 697.64
    ## - age        1     89.74 3828.6 699.65
    ## - i_neck     1     91.22 3830.1 699.75
    ## - height     1    174.57 3913.4 705.17
    ## - wrist      1    225.88 3964.7 708.45
    ## - i_abdomen  1   1230.34 4969.2 765.36
    ## 
    ## Step:  AIC=694.99
    ## bodyfat_brozek ~ age + height + forearm + wrist + i_neck + i_abdomen
    ## 
    ##             Df Sum of Sq     RSS    AIC
    ## <none>                    3758.6 694.99
    ## + i_weight   1      19.7  3738.9 695.67
    ## + i_thigh    1      18.5  3740.1 695.75
    ## + i_bicep    1      17.2  3741.4 695.84
    ## - forearm    1      51.4  3810.0 696.42
    ## + i_chest    1       7.0  3751.6 696.53
    ## + i_ankle    1       4.0  3754.6 696.73
    ## + i_hip      1       1.5  3757.0 696.89
    ## + i_knee     1       0.3  3758.3 696.98
    ## - age        1      70.1  3828.6 697.65
    ## - i_neck     1      76.5  3835.1 698.08
    ## - height     1     181.6  3940.2 704.89
    ## - wrist      1     206.3  3964.8 706.46
    ## - i_abdomen  1    6333.4 10091.9 941.90

    ## 
    ## Call:
    ## lm(formula = bodyfat_brozek ~ age + height + forearm + wrist + 
    ##     i_neck + i_abdomen, data = bodyfat_df)
    ## 
    ## Coefficients:
    ## (Intercept)          age       height      forearm        wrist       i_neck  
    ##   1.175e+02    4.871e-02   -3.824e-01    3.121e-01   -1.603e+00    6.063e+02  
    ##   i_abdomen  
    ##  -6.364e+03

Based on Backward: 6 predictors - age, height, forearm, wrist, i_neck,
i_abdomen Based on Forward: 6 predictors - age, height, forearm, wrist,
i_neck, i_abdomen Based on Stepwise: 6 predictors - age, height,
forearm, wrist, i_neck, i_abdomen

``` r
bodyfat_df =
  bodyfat_df %>%
  select(bodyfat_brozek, everything())

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
    ## 6   TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 7   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 7   TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE
    ## 8   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## 8   TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## 9   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE
    ## 9   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## 10  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE
    ## 10  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
    ## 11  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE
    ## 11  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
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
    ## 6  FALSE
    ## 7  FALSE
    ## 7  FALSE
    ## 8  FALSE
    ## 8   TRUE
    ## 9  FALSE
    ## 9   TRUE
    ## 10  TRUE
    ## 10  TRUE
    ## 11  TRUE
    ## 11 FALSE
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
    ##  [1]  67.541747 241.568578  27.584094  27.849208   9.329922  13.972183
    ##  [7]   7.114933   7.450427   6.191088   7.396023   4.868716   5.549818
    ## [13]   5.596044   5.673383   5.904074   6.774978   7.262894   7.341849
    ## [19]   8.691463   8.921633  10.477402  10.493845  12.121731  12.364653
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
    ## 6   TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 7   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
    ## 7   TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE
    ## 8   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## 8   TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## 9   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE
    ## 9   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## 10  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE
    ## 10  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
    ## 11  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE
    ## 11  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
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
    ## 6  FALSE
    ## 7  FALSE
    ## 7  FALSE
    ## 8  FALSE
    ## 8   TRUE
    ## 9  FALSE
    ## 9   TRUE
    ## 10  TRUE
    ## 10  TRUE
    ## 11  TRUE
    ## 11 FALSE
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
    ##  [1] 0.6748626 0.4955436 0.7169640 0.7166898 0.7368612 0.7320392 0.7401917
    ##  [8] 0.7398418 0.7421974 0.7409356 0.7446384 0.7439222 0.7449354 0.7448538
    ## [15] 0.7456794 0.7447562 0.7453110 0.7452270 0.7448650 0.7446190 0.7440317
    ## [22] 0.7440141 0.7433441 0.7430823 0.7423975

``` r
b = regsubsets(bodyfat_brozek ~ ., data = bodyfat_df)
rs = summary(b)

par(mfrow = c(1,2))
plot(1:8, rs$cp, xlab = "No of predictors", ylab = "Cp Statistic")
abline(0,1)
plot(1:8, rs$adjr2, xlab = "No of predictors", ylab = "Adj R2")
```

![](Code_Analysis_files/figure-gfm/automatic%20criterion%20output-1.png)<!-- -->

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
six_pred = lm(bodyfat_brozek ~ age + height + forearm + wrist + i_neck + i_abdomen, data = bodyfat_df)
summary(six_pred)
```

    ## 
    ## Call:
    ## lm(formula = bodyfat_brozek ~ age + height + forearm + wrist + 
    ##     i_neck + i_abdomen, data = bodyfat_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.8798 -2.7447 -0.2029  2.4801  9.6070 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.175e+02  1.452e+01   8.095 2.68e-14 ***
    ## age          4.871e-02  2.280e-02   2.137 0.033584 *  
    ## height      -3.824e-01  1.111e-01  -3.441 0.000682 ***
    ## forearm      3.121e-01  1.705e-01   1.831 0.068357 .  
    ## wrist       -1.603e+00  4.372e-01  -3.667 0.000301 ***
    ## i_neck       6.063e+02  2.715e+02   2.234 0.026407 *  
    ## i_abdomen   -6.364e+03  3.132e+02 -20.318  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.917 on 245 degrees of freedom
    ## Multiple R-squared:  0.7507, Adjusted R-squared:  0.7446 
    ## F-statistic:   123 on 6 and 245 DF,  p-value: < 2.2e-16

``` r
eight_pred = lm(bodyfat_brozek ~ age + height + forearm + wrist + 
                  i_neck + i_abdomen + i_weight + i_chest, data = bodyfat_df)
summary(eight_pred)
```

    ## 
    ## Call:
    ## lm(formula = bodyfat_brozek ~ age + height + forearm + wrist + 
    ##     i_neck + i_abdomen + i_weight + i_chest, data = bodyfat_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.7056 -2.8734 -0.1633  2.4322 10.2151 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.282e+02  1.706e+01   7.519 1.06e-12 ***
    ## age          6.728e-02  2.589e-02   2.599 0.009922 ** 
    ## height      -5.544e-01  1.530e-01  -3.624 0.000354 ***
    ## forearm      2.589e-01  1.785e-01   1.450 0.148404    
    ## wrist       -1.786e+00  4.598e-01  -3.883 0.000133 ***
    ## i_neck       6.390e+02  2.807e+02   2.277 0.023674 *  
    ## i_abdomen   -5.936e+03  6.575e+02  -9.028  < 2e-16 ***
    ## i_weight    -2.084e+03  1.306e+03  -1.596 0.111872    
    ## i_chest      1.183e+03  9.033e+02   1.309 0.191729    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.909 on 243 degrees of freedom
    ## Multiple R-squared:  0.7538, Adjusted R-squared:  0.7457 
    ## F-statistic: 92.99 on 8 and 243 DF,  p-value: < 2.2e-16

``` r
anova(six_pred, eight_pred)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: bodyfat_brozek ~ age + height + forearm + wrist + i_neck + i_abdomen
    ## Model 2: bodyfat_brozek ~ age + height + forearm + wrist + i_neck + i_abdomen + 
    ##     i_weight + i_chest
    ##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
    ## 1    245 3758.6                           
    ## 2    243 3712.7  2     45.88 1.5014 0.2249

DISCUSSION: Smaller is better. Can discuss anova hypotheses and results
as proof.
