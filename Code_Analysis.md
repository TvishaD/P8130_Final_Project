Code and Analysis
================
2022-12-11

``` r
library(tidyverse)
library(ggplot2)
library(patchwork)
library(GGally)
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

<div id="djbvqzcxjt" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#djbvqzcxjt .gt_table {
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

#djbvqzcxjt .gt_heading {
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

#djbvqzcxjt .gt_title {
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

#djbvqzcxjt .gt_subtitle {
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

#djbvqzcxjt .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#djbvqzcxjt .gt_col_headings {
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

#djbvqzcxjt .gt_col_heading {
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

#djbvqzcxjt .gt_column_spanner_outer {
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

#djbvqzcxjt .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#djbvqzcxjt .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#djbvqzcxjt .gt_column_spanner {
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

#djbvqzcxjt .gt_group_heading {
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

#djbvqzcxjt .gt_empty_group_heading {
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

#djbvqzcxjt .gt_from_md > :first-child {
  margin-top: 0;
}

#djbvqzcxjt .gt_from_md > :last-child {
  margin-bottom: 0;
}

#djbvqzcxjt .gt_row {
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

#djbvqzcxjt .gt_stub {
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

#djbvqzcxjt .gt_stub_row_group {
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

#djbvqzcxjt .gt_row_group_first td {
  border-top-width: 2px;
}

#djbvqzcxjt .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#djbvqzcxjt .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#djbvqzcxjt .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#djbvqzcxjt .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#djbvqzcxjt .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#djbvqzcxjt .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#djbvqzcxjt .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#djbvqzcxjt .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#djbvqzcxjt .gt_footnotes {
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

#djbvqzcxjt .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#djbvqzcxjt .gt_sourcenotes {
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

#djbvqzcxjt .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#djbvqzcxjt .gt_left {
  text-align: left;
}

#djbvqzcxjt .gt_center {
  text-align: center;
}

#djbvqzcxjt .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#djbvqzcxjt .gt_font_normal {
  font-weight: normal;
}

#djbvqzcxjt .gt_font_bold {
  font-weight: bold;
}

#djbvqzcxjt .gt_font_italic {
  font-style: italic;
}

#djbvqzcxjt .gt_super {
  font-size: 65%;
}

#djbvqzcxjt .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#djbvqzcxjt .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#djbvqzcxjt .gt_indent_1 {
  text-indent: 5px;
}

#djbvqzcxjt .gt_indent_2 {
  text-indent: 10px;
}

#djbvqzcxjt .gt_indent_3 {
  text-indent: 15px;
}

#djbvqzcxjt .gt_indent_4 {
  text-indent: 20px;
}

#djbvqzcxjt .gt_indent_5 {
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
