R Notebook - data from MSSE Weng et al., 2015
================
Michelle Voss
Nov 2018

-   [setup](#setup)
-   [demographics in Table 1](#demographics-in-table-1)
-   [paired t-tests for exercise conditions](#paired-t-tests-for-exercise-conditions)
-   [Cognitive outcomes](#cognitive-outcomes)

**Citation**: Weng, T. B., Pierce, G. L., Darling, W. G., & Voss, M. W. (2015). Differential effects of acute exercise on distinct aspects of executive function. Medicine and science in sports and exercise, 47(7), 1460-1469.

setup
=====

Clear previous, load packages

Load data from an spss file
---------------------------

Do not show in knit

demographics in Table 1
=======================

``` r
demogs <- data %>% 
        select(age,gender,edu,height,weight,bmi,godin_score) 

pander(describe(demogs))
```

<table>
<caption>Table continues below</caption>
<colgroup>
<col width="23%" />
<col width="9%" />
<col width="6%" />
<col width="10%" />
<col width="12%" />
<col width="11%" />
<col width="12%" />
<col width="12%" />
</colgroup>
<thead>
<tr class="header">
<th align="center"> </th>
<th align="center">vars</th>
<th align="center">n</th>
<th align="center">mean</th>
<th align="center">sd</th>
<th align="center">median</th>
<th align="center">trimmed</th>
<th align="center">mad</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center"><strong>age</strong></td>
<td align="center">1</td>
<td align="center">26</td>
<td align="center">25.23</td>
<td align="center">2.847</td>
<td align="center">26</td>
<td align="center">25.32</td>
<td align="center">2.965</td>
</tr>
<tr class="even">
<td align="center"><strong>gender</strong>*</td>
<td align="center">2</td>
<td align="center">26</td>
<td align="center">1.538</td>
<td align="center">0.5084</td>
<td align="center">2</td>
<td align="center">1.545</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center"><strong>edu</strong></td>
<td align="center">3</td>
<td align="center">26</td>
<td align="center">17.77</td>
<td align="center">2.736</td>
<td align="center">17.75</td>
<td align="center">17.73</td>
<td align="center">1.853</td>
</tr>
<tr class="even">
<td align="center"><strong>height</strong></td>
<td align="center">4</td>
<td align="center">26</td>
<td align="center">1.74</td>
<td align="center">0.08227</td>
<td align="center">1.753</td>
<td align="center">1.744</td>
<td align="center">0.07532</td>
</tr>
<tr class="odd">
<td align="center"><strong>weight</strong></td>
<td align="center">5</td>
<td align="center">26</td>
<td align="center">70.88</td>
<td align="center">10.1</td>
<td align="center">69.4</td>
<td align="center">70.51</td>
<td align="center">10.96</td>
</tr>
<tr class="even">
<td align="center"><strong>bmi</strong></td>
<td align="center">6</td>
<td align="center">26</td>
<td align="center">23.35</td>
<td align="center">2.528</td>
<td align="center">22.64</td>
<td align="center">23.19</td>
<td align="center">2.501</td>
</tr>
<tr class="odd">
<td align="center"><strong>godin_score</strong></td>
<td align="center">7</td>
<td align="center">25</td>
<td align="center">53.26</td>
<td align="center">23.23</td>
<td align="center">54</td>
<td align="center">53.17</td>
<td align="center">26.69</td>
</tr>
</tbody>
</table>

<table>
<colgroup>
<col width="24%" />
<col width="10%" />
<col width="10%" />
<col width="12%" />
<col width="13%" />
<col width="14%" />
<col width="14%" />
</colgroup>
<thead>
<tr class="header">
<th align="center"> </th>
<th align="center">min</th>
<th align="center">max</th>
<th align="center">range</th>
<th align="center">skew</th>
<th align="center">kurtosis</th>
<th align="center">se</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center"><strong>age</strong></td>
<td align="center">20</td>
<td align="center">30</td>
<td align="center">10</td>
<td align="center">-0.2844</td>
<td align="center">-1.266</td>
<td align="center">0.5583</td>
</tr>
<tr class="even">
<td align="center"><strong>gender</strong>*</td>
<td align="center">1</td>
<td align="center">2</td>
<td align="center">1</td>
<td align="center">-0.1455</td>
<td align="center">-2.053</td>
<td align="center">0.0997</td>
</tr>
<tr class="odd">
<td align="center"><strong>edu</strong></td>
<td align="center">13</td>
<td align="center">23</td>
<td align="center">10</td>
<td align="center">0.04106</td>
<td align="center">-0.929</td>
<td align="center">0.5365</td>
</tr>
<tr class="even">
<td align="center"><strong>height</strong></td>
<td align="center">1.549</td>
<td align="center">1.88</td>
<td align="center">0.3302</td>
<td align="center">-0.5204</td>
<td align="center">-0.5734</td>
<td align="center">0.01613</td>
</tr>
<tr class="odd">
<td align="center"><strong>weight</strong></td>
<td align="center">48.53</td>
<td align="center">94.35</td>
<td align="center">45.81</td>
<td align="center">0.2599</td>
<td align="center">-0.1714</td>
<td align="center">1.981</td>
</tr>
<tr class="even">
<td align="center"><strong>bmi</strong></td>
<td align="center">19.94</td>
<td align="center">28.61</td>
<td align="center">8.67</td>
<td align="center">0.6501</td>
<td align="center">-0.6785</td>
<td align="center">0.4958</td>
</tr>
<tr class="odd">
<td align="center"><strong>godin_score</strong></td>
<td align="center">12</td>
<td align="center">93</td>
<td align="center">81</td>
<td align="center">0.04111</td>
<td align="center">-1.218</td>
<td align="center">4.647</td>
</tr>
</tbody>
</table>

``` r
pander(describeBy(demogs,group="gender"))
```

    ## Warning in pander.default(describeBy(demogs, group = "gender")): No
    ## pander.method for "psych", reverting to default.No pander.method for
    ## "describeBy", reverting to default.

-   **male**:

    <table>
    <caption>Table continues below</caption>
    <colgroup>
    <col width="23%" />
    <col width="9%" />
    <col width="6%" />
    <col width="10%" />
    <col width="12%" />
    <col width="11%" />
    <col width="12%" />
    <col width="12%" />
    </colgroup>
    <thead>
    <tr class="header">
    <th align="center"> </th>
    <th align="center">vars</th>
    <th align="center">n</th>
    <th align="center">mean</th>
    <th align="center">sd</th>
    <th align="center">median</th>
    <th align="center">trimmed</th>
    <th align="center">mad</th>
    </tr>
    </thead>
    <tbody>
    <tr class="odd">
    <td align="center"><strong>age</strong></td>
    <td align="center">1</td>
    <td align="center">12</td>
    <td align="center">25.83</td>
    <td align="center">2.588</td>
    <td align="center">26</td>
    <td align="center">25.9</td>
    <td align="center">2.965</td>
    </tr>
    <tr class="even">
    <td align="center"><strong>gender</strong>*</td>
    <td align="center">2</td>
    <td align="center">12</td>
    <td align="center">1</td>
    <td align="center">0</td>
    <td align="center">1</td>
    <td align="center">1</td>
    <td align="center">0</td>
    </tr>
    <tr class="odd">
    <td align="center"><strong>edu</strong></td>
    <td align="center">3</td>
    <td align="center">12</td>
    <td align="center">17.96</td>
    <td align="center">3.122</td>
    <td align="center">17</td>
    <td align="center">17.85</td>
    <td align="center">3.706</td>
    </tr>
    <tr class="even">
    <td align="center"><strong>height</strong></td>
    <td align="center">4</td>
    <td align="center">12</td>
    <td align="center">1.779</td>
    <td align="center">0.05594</td>
    <td align="center">1.778</td>
    <td align="center">1.779</td>
    <td align="center">0.03766</td>
    </tr>
    <tr class="odd">
    <td align="center"><strong>weight</strong></td>
    <td align="center">5</td>
    <td align="center">12</td>
    <td align="center">74.49</td>
    <td align="center">7.48</td>
    <td align="center">74.03</td>
    <td align="center">74.23</td>
    <td align="center">7.935</td>
    </tr>
    <tr class="even">
    <td align="center"><strong>bmi</strong></td>
    <td align="center">6</td>
    <td align="center">12</td>
    <td align="center">23.57</td>
    <td align="center">2.465</td>
    <td align="center">22.51</td>
    <td align="center">23.41</td>
    <td align="center">0.9867</td>
    </tr>
    <tr class="odd">
    <td align="center"><strong>godin_score</strong></td>
    <td align="center">7</td>
    <td align="center">11</td>
    <td align="center">54.95</td>
    <td align="center">26.57</td>
    <td align="center">49</td>
    <td align="center">55.5</td>
    <td align="center">33.36</td>
    </tr>
    </tbody>
    </table>

    <table>
    <colgroup>
    <col width="23%" />
    <col width="10%" />
    <col width="10%" />
    <col width="11%" />
    <col width="14%" />
    <col width="14%" />
    <col width="14%" />
    </colgroup>
    <thead>
    <tr class="header">
    <th align="center"> </th>
    <th align="center">min</th>
    <th align="center">max</th>
    <th align="center">range</th>
    <th align="center">skew</th>
    <th align="center">kurtosis</th>
    <th align="center">se</th>
    </tr>
    </thead>
    <tbody>
    <tr class="odd">
    <td align="center"><strong>age</strong></td>
    <td align="center">21</td>
    <td align="center">30</td>
    <td align="center">9</td>
    <td align="center">-0.3515</td>
    <td align="center">-0.88</td>
    <td align="center">0.747</td>
    </tr>
    <tr class="even">
    <td align="center"><strong>gender</strong>*</td>
    <td align="center">1</td>
    <td align="center">1</td>
    <td align="center">0</td>
    <td align="center">NA</td>
    <td align="center">NA</td>
    <td align="center">0</td>
    </tr>
    <tr class="odd">
    <td align="center"><strong>edu</strong></td>
    <td align="center">14</td>
    <td align="center">23</td>
    <td align="center">9</td>
    <td align="center">0.2479</td>
    <td align="center">-1.536</td>
    <td align="center">0.9013</td>
    </tr>
    <tr class="even">
    <td align="center"><strong>height</strong></td>
    <td align="center">1.676</td>
    <td align="center">1.88</td>
    <td align="center">0.2032</td>
    <td align="center">0.04312</td>
    <td align="center">-0.7343</td>
    <td align="center">0.01615</td>
    </tr>
    <tr class="odd">
    <td align="center"><strong>weight</strong></td>
    <td align="center">62.32</td>
    <td align="center">89.36</td>
    <td align="center">27.03</td>
    <td align="center">0.231</td>
    <td align="center">-0.9043</td>
    <td align="center">2.159</td>
    </tr>
    <tr class="even">
    <td align="center"><strong>bmi</strong></td>
    <td align="center">20.59</td>
    <td align="center">28.25</td>
    <td align="center">7.658</td>
    <td align="center">0.7234</td>
    <td align="center">-1.08</td>
    <td align="center">0.7116</td>
    </tr>
    <tr class="odd">
    <td align="center"><strong>godin_score</strong></td>
    <td align="center">12</td>
    <td align="center">93</td>
    <td align="center">81</td>
    <td align="center">-0.04331</td>
    <td align="center">-1.614</td>
    <td align="center">8.01</td>
    </tr>
    </tbody>
    </table>

-   **female**:

    <table>
    <caption>Table continues below</caption>
    <colgroup>
    <col width="24%" />
    <col width="9%" />
    <col width="6%" />
    <col width="10%" />
    <col width="13%" />
    <col width="12%" />
    <col width="13%" />
    <col width="9%" />
    </colgroup>
    <thead>
    <tr class="header">
    <th align="center"> </th>
    <th align="center">vars</th>
    <th align="center">n</th>
    <th align="center">mean</th>
    <th align="center">sd</th>
    <th align="center">median</th>
    <th align="center">trimmed</th>
    <th align="center">mad</th>
    </tr>
    </thead>
    <tbody>
    <tr class="odd">
    <td align="center"><strong>age</strong></td>
    <td align="center">1</td>
    <td align="center">14</td>
    <td align="center">24.71</td>
    <td align="center">3.049</td>
    <td align="center">24</td>
    <td align="center">24.83</td>
    <td align="center">4.448</td>
    </tr>
    <tr class="even">
    <td align="center"><strong>gender</strong>*</td>
    <td align="center">2</td>
    <td align="center">14</td>
    <td align="center">2</td>
    <td align="center">0</td>
    <td align="center">2</td>
    <td align="center">2</td>
    <td align="center">0</td>
    </tr>
    <tr class="odd">
    <td align="center"><strong>edu</strong></td>
    <td align="center">3</td>
    <td align="center">14</td>
    <td align="center">17.61</td>
    <td align="center">2.467</td>
    <td align="center">18.5</td>
    <td align="center">17.62</td>
    <td align="center">1.112</td>
    </tr>
    <tr class="even">
    <td align="center"><strong>height</strong></td>
    <td align="center">4</td>
    <td align="center">14</td>
    <td align="center">1.708</td>
    <td align="center">0.08866</td>
    <td align="center">1.715</td>
    <td align="center">1.711</td>
    <td align="center">0.113</td>
    </tr>
    <tr class="odd">
    <td align="center"><strong>weight</strong></td>
    <td align="center">5</td>
    <td align="center">14</td>
    <td align="center">67.78</td>
    <td align="center">11.24</td>
    <td align="center">64.86</td>
    <td align="center">67.17</td>
    <td align="center">6.725</td>
    </tr>
    <tr class="even">
    <td align="center"><strong>bmi</strong></td>
    <td align="center">6</td>
    <td align="center">14</td>
    <td align="center">23.16</td>
    <td align="center">2.657</td>
    <td align="center">23.15</td>
    <td align="center">22.97</td>
    <td align="center">2.285</td>
    </tr>
    <tr class="odd">
    <td align="center"><strong>godin_score</strong></td>
    <td align="center">7</td>
    <td align="center">14</td>
    <td align="center">51.93</td>
    <td align="center">21.2</td>
    <td align="center">54.5</td>
    <td align="center">51.42</td>
    <td align="center">20.02</td>
    </tr>
    </tbody>
    </table>

    <table>
    <colgroup>
    <col width="25%" />
    <col width="11%" />
    <col width="11%" />
    <col width="12%" />
    <col width="13%" />
    <col width="15%" />
    <col width="11%" />
    </colgroup>
    <thead>
    <tr class="header">
    <th align="center"> </th>
    <th align="center">min</th>
    <th align="center">max</th>
    <th align="center">range</th>
    <th align="center">skew</th>
    <th align="center">kurtosis</th>
    <th align="center">se</th>
    </tr>
    </thead>
    <tbody>
    <tr class="odd">
    <td align="center"><strong>age</strong></td>
    <td align="center">20</td>
    <td align="center">28</td>
    <td align="center">8</td>
    <td align="center">-0.1111</td>
    <td align="center">-1.72</td>
    <td align="center">0.8149</td>
    </tr>
    <tr class="even">
    <td align="center"><strong>gender</strong>*</td>
    <td align="center">2</td>
    <td align="center">2</td>
    <td align="center">0</td>
    <td align="center">NA</td>
    <td align="center">NA</td>
    <td align="center">0</td>
    </tr>
    <tr class="odd">
    <td align="center"><strong>edu</strong></td>
    <td align="center">13</td>
    <td align="center">22</td>
    <td align="center">9</td>
    <td align="center">-0.4335</td>
    <td align="center">-0.7143</td>
    <td align="center">0.6592</td>
    </tr>
    <tr class="even">
    <td align="center"><strong>height</strong></td>
    <td align="center">1.549</td>
    <td align="center">1.829</td>
    <td align="center">0.2794</td>
    <td align="center">-0.1746</td>
    <td align="center">-1.4</td>
    <td align="center">0.0237</td>
    </tr>
    <tr class="odd">
    <td align="center"><strong>weight</strong></td>
    <td align="center">48.53</td>
    <td align="center">94.35</td>
    <td align="center">45.81</td>
    <td align="center">0.7164</td>
    <td align="center">0.1294</td>
    <td align="center">3.005</td>
    </tr>
    <tr class="even">
    <td align="center"><strong>bmi</strong></td>
    <td align="center">19.94</td>
    <td align="center">28.61</td>
    <td align="center">8.67</td>
    <td align="center">0.5747</td>
    <td align="center">-0.7468</td>
    <td align="center">0.7102</td>
    </tr>
    <tr class="odd">
    <td align="center"><strong>godin_score</strong></td>
    <td align="center">19</td>
    <td align="center">91</td>
    <td align="center">72</td>
    <td align="center">0.06941</td>
    <td align="center">-1.121</td>
    <td align="center">5.665</td>
    </tr>
    </tbody>
    </table>

<!-- end of list -->
paired t-tests for exercise conditions
======================================

``` r
# get columns that match this pattern (| means "or")
exercise_vars <- grep(names(data), pattern = "^sub$|^exclude$|^countergroup$|active$|passive$|act$|pass$")
exercise_data <- select(data, exercise_vars)

# function for doing the ttest
desttest <- function(cols, data) {
    data[, cols] %>%
    describe() %>%
    pander()
  
  t.test(data[, cols[1]], 
         data[, cols[2]], 
         paired=TRUE, 
         conf.level=0.95)
}
```

Exercise HR
-----------

``` r
desttest(c("avghr_passive", "avghr_active"), exercise_data)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  data[, cols[1]] and data[, cols[2]]
    ## t = -22.102, df = 25, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -53.70215 -44.54708
    ## sample estimates:
    ## mean of the differences 
    ##               -49.12462

Exercise %HRmax
---------------

``` r
desttest(c("hrmax_pass", "hrmax_act"), exercise_data)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  data[, cols[1]] and data[, cols[2]]
    ## t = -21.709, df = 25, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.2763739 -0.2284776
    ## sample estimates:
    ## mean of the differences 
    ##              -0.2524257

RPE
---

``` r
desttest(c("avgrpe_passive", "avgrpe_active"), exercise_data)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  data[, cols[1]] and data[, cols[2]]
    ## t = -14.652, df = 25, p-value = 8.898e-14
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -5.687447 -4.285630
    ## sample estimates:
    ## mean of the differences 
    ##               -4.986538

FAS
---

``` r
desttest(c("avgfas_passive", "avgfas_active"), exercise_data)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  data[, cols[1]] and data[, cols[2]]
    ## t = -8.1776, df = 25, p-value = 1.571e-08
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -1.5094445 -0.9020939
    ## sample estimates:
    ## mean of the differences 
    ##               -1.205769

FS
--

``` r
desttest(c("avgfs_passive", "avgfs_active"), exercise_data)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  data[, cols[1]] and data[, cols[2]]
    ## t = 2.3705, df = 25, p-value = 0.02579
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  0.08727932 1.24348991
    ## sample estimates:
    ## mean of the differences 
    ##               0.6653846

Cognitive outcomes
==================

-   percdiff=((post-pre)/pre)\*100

n-back
------

``` r
nback_prepost <- data %>%
        filter(exclude == "include") %>%
        select(sub, countergroup, pre_passive_1b_acc:percdiff_active_2b_rt) %>%
        select(sub, countergroup, matches("pre|post")) %>%
        gather(key=condition, value=perf, -sub, -countergroup)

# now split condition column up to it's factors
nback_prepost <- separate(nback_prepost, "condition", c("time", "intensity", "load", "dvtype"), "_")

write.csv(nback_prepost, "acute_nback-prepost_weng-etal-2015.csv", row.names=FALSE)
```

``` r
nback_percdiff <- data %>%
        filter(exclude == "include") %>%
        select(sub, countergroup, pre_passive_1b_acc:percdiff_active_2b_rt) %>%
        select(sub, countergroup, matches("percdiff")) %>%
        gather(key=condition, value=perf, -sub, -countergroup)

# now split condition column up to it's factors
nback_percdiff <- separate(nback_percdiff, "condition", c("drop", "intensity", "load", "dvtype"), "_")
nback_percdiff$drop <- NULL
write.csv(nback_percdiff, "acute_nback-percdiff_weng-etal-2015.csv", row.names=FALSE)
```

### set factors

``` r
nback_prepost$time <- as.factor(nback_prepost$time)
nback_prepost$time <- relevel(nback_prepost$time, "pre")
nback_prepost$intensity <- as.factor(nback_prepost$intensity)
nback_prepost$intensity <- relevel(nback_prepost$intensity, "passive")
nback_prepost$load <- as.factor(nback_prepost$load)

nback_percdiff$intensity <- as.factor(nback_percdiff$intensity)
nback_percdiff$intensity <- relevel(nback_percdiff$intensity, "passive")
nback_percdiff$load <- as.factor(nback_percdiff$load)
```

### plot nback

``` r
ggplot(subset(nback_prepost, dvtype == "acc"), aes(x=time, y=perf, group=sub)) + 
  geom_line() + 
  geom_point() +
  facet_grid(~intensity * load) +
  labs(title = "Accuracy", y = "Accuracy") + 
  theme(strip.text = element_text(face = "bold", size = 15, lineheight = 5.0), 
  strip.background = element_rect(colour = "black", size = 1))
```

![](analysis-notebook_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
ggplot(subset(nback_prepost, dvtype == "rt"), aes(x = time, y = perf, group = sub)) + 
  geom_line() + 
  geom_point() +
  facet_grid(~intensity * load) +
  labs(title = "Reaction Time", y = "RT") + 
  theme(strip.text = element_text(face = "bold", size = 15, lineheight = 5.0), 
  strip.background = element_rect(colour = "black", size = 1))
```

![](analysis-notebook_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
ggplot(subset(nback_percdiff,dvtype == "rt"), aes(x = intensity, y = perf, fill = load)) + 
  scale_fill_manual(values = c("#E3871C", "#F5BD78")) +
  geom_boxplot() + 
  labs(title = "Acute RT %change on N-back", y = "%Diff", x= "", fill = "load") +
  theme(title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))
```

![](analysis-notebook_files/figure-markdown_github/unnamed-chunk-15-1.png)

``` r
ggplot(subset(nback_percdiff, dvtype == "rt"), aes(x = intensity, y = perf)) + 
  stat_summary(aes(y = perf), size = .5, fun.y = mean, geom = "bar", size = 1) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", size = .5, width = .3) + 
        facet_grid(~load) +
        labs(title = "Acute RT %change on N-back", y = "%Diff", x = "") +
        theme(title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))
```

    ## Warning: The plyr::rename operation has created duplicates for the
    ## following name(s): (`size`)

![](analysis-notebook_files/figure-markdown_github/unnamed-chunk-15-2.png)

``` r
ggplot(subset(nback_percdiff, dvtype == "rt"), aes(x = load,y = perf)) + 
  stat_summary(aes(y = perf), size = .5, fun.y = mean, geom = "bar", size = 1) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", size = .5, width = .3) + 
        facet_grid(~intensity) +
        labs(title = "Acute RT %change on N-back", y = "%Diff", x= "") +
        theme(title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))
```

    ## Warning: The plyr::rename operation has created duplicates for the
    ## following name(s): (`size`)

![](analysis-notebook_files/figure-markdown_github/unnamed-chunk-15-3.png)

``` r
ggplot(subset(nback_percdiff,dvtype=="acc"), aes(x=intensity,y=perf,fill=load)) + 
  scale_fill_manual(values=c("#E3871C", "#F5BD78")) +
  geom_boxplot() + 
  labs(title="Acute ACC %change on N-back",y="%Diff",x="",fill="load") +
  theme(title=element_text(size=16, face='bold'),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=15),
        axis.title.y = element_text(size=16),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))
```

![](analysis-notebook_files/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
ggplot(subset(nback_percdiff,dvtype=="acc"), aes(x=intensity,y=perf)) + 
  stat_summary(aes(y = perf), size=.5, fun.y = mean, geom="bar",size=1) + 
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", size=.5,width=.3) + 
        facet_grid(~load) +
        labs(title="Acute ACC %change on N-back",y="%Diff",x="") +
        theme(title=element_text(size=16, face='bold'),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=15),
        axis.title.y = element_text(size=16),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))
```

    ## Warning: The plyr::rename operation has created duplicates for the
    ## following name(s): (`size`)

![](analysis-notebook_files/figure-markdown_github/unnamed-chunk-16-2.png)

``` r
ggplot(subset(nback_percdiff,dvtype=="acc"), aes(x=load,y=perf)) + 
  stat_summary(aes(y = perf), size=.5, fun.y = mean, geom="bar",size=1) + 
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", size=.5,width=.3) + 
        facet_grid(~intensity) +
        labs(title="Acute ACC %change on N-back",y="%Diff",x="") +
        theme(title=element_text(size=16, face='bold'),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=15),
        axis.title.y = element_text(size=16),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))
```

    ## Warning: The plyr::rename operation has created duplicates for the
    ## following name(s): (`size`)

![](analysis-notebook_files/figure-markdown_github/unnamed-chunk-16-3.png)

### nback stats

#### repeated measures anova with time as a factor

``` r
aovACC <- aov_car(perf ~ time * intensity * load + Error(sub / time * intensity * load), subset(nback_prepost, dvtype == "acc"))
nice(aovACC)
```

    ## Anova Table (Type 3 tests)
    ## 
    ## Response: perf
    ##                Effect    df  MSE         F    ges p.value
    ## 1                time 1, 24 0.00 28.85 ***    .05  <.0001
    ## 2           intensity 1, 24 0.00      0.00 <.0001     .96
    ## 3                load 1, 24 0.01    7.63 *    .05     .01
    ## 4      time:intensity 1, 24 0.00      2.63   .006     .12
    ## 5           time:load 1, 24 0.00      1.88   .002     .18
    ## 6      intensity:load 1, 24 0.00      1.75   .002     .20
    ## 7 time:intensity:load 1, 24 0.00      2.90   .006     .10
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

#### repeated measures anova with percentdiff

**reaction time**

``` r
aovRT <- aov_car(perf ~ intensity * load + Error(sub / intensity * load), subset(nback_percdiff, dvtype == "rt"))
nice(aovRT)
```

    ## Anova Table (Type 3 tests)
    ## 
    ## Response: perf
    ##           Effect    df  MSE    F   ges p.value
    ## 1      intensity 1, 24 0.01 0.62  .007     .44
    ## 2           load 1, 24 0.00 1.53  .009     .23
    ## 3 intensity:load 1, 24 0.00 0.19 .0006     .66
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

``` r
aovRT_fitted <- lsmeans(aovRT, ~load|intensity)
aovRT_fitted
```

    ## intensity = passive:
    ##  load      lsmean         SE    df    lower.CL    upper.CL
    ##  X1b  -0.08154058 0.01658916 66.16 -0.11466039 -0.04842076
    ##  X2b  -0.06997877 0.01658916 66.16 -0.10309859 -0.03685895
    ## 
    ## intensity = active:
    ##  load      lsmean         SE    df    lower.CL    upper.CL
    ##  X1b  -0.07249350 0.01658916 66.16 -0.10561332 -0.03937369
    ##  X2b  -0.05266686 0.01658916 66.16 -0.08578667 -0.01954704
    ## 
    ## Confidence level used: 0.95

``` r
pairs(aovRT_fitted)
```

    ## intensity = passive:
    ##  contrast     estimate         SE    df t.ratio p.value
    ##  X1b - X2b -0.01156181 0.01577587 44.14  -0.733  0.4675
    ## 
    ## intensity = active:
    ##  contrast     estimate         SE    df t.ratio p.value
    ##  X1b - X2b -0.01982665 0.01577587 44.14  -1.257  0.2154

**accuracy**

``` r
aovACC <- aov_car(perf ~ intensity * load + Error(sub / intensity * load), subset(nback_percdiff, dvtype == "acc"))
nice(aovACC)
```

    ## Anova Table (Type 3 tests)
    ## 
    ## Response: perf
    ##           Effect    df   MSE      F ges p.value
    ## 1      intensity 1, 24 43.40   2.00 .02     .17
    ## 2           load 1, 24 24.99 3.32 + .02     .08
    ## 3 intensity:load 1, 24 38.94   2.59 .03     .12
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

``` r
aovACC_fitted <- lsmeans(aovACC, ~load|intensity)
aovACC_fitted
```

    ## intensity = passive:
    ##  load   lsmean       SE    df  lower.CL upper.CL
    ##  X1b  2.711715 1.197933 92.68 0.3327485 5.090682
    ##  X2b  2.524510 1.197933 92.68 0.1455435 4.903477
    ## 
    ## intensity = active:
    ##  load   lsmean       SE    df  lower.CL upper.CL
    ##  X1b  2.565791 1.197933 92.68 0.1868247 4.944758
    ##  X2b  6.397401 1.197933 92.68 4.0184347 8.776368
    ## 
    ## Confidence level used: 0.95

``` r
pairs(aovACC_fitted)
```

    ## intensity = passive:
    ##  contrast   estimate       SE    df t.ratio p.value
    ##  X1b - X2b  0.187205 1.599024 45.82   0.117  0.9073
    ## 
    ## intensity = active:
    ##  contrast   estimate       SE    df t.ratio p.value
    ##  X1b - X2b -3.831610 1.599024 45.82  -2.396  0.0207

**Accuracy with glm and covariates **

Set to effect coding

``` r
contrasts(nback_prepost$time) <- c(-.5, .5)
contrasts(nback_prepost$time)
```

    ##      [,1]
    ## pre  -0.5
    ## post  0.5

``` r
contrasts(nback_prepost$intensity) <- c(-.5, .5)
contrasts(nback_prepost$intensity)
```

    ##         [,1]
    ## passive -0.5
    ## active   0.5

``` r
contrasts(nback_prepost$load) <- c(-.5, .5)
contrasts(nback_prepost$load)
```

    ##    [,1]
    ## 1b -0.5
    ## 2b  0.5

``` r
lm_acc <- lmer(perf ~ time * intensity * load + (1 + time + intensity + load | sub), data = subset(nback_prepost, dvtype == "acc"))
lm_acc2<-lmer(perf ~ time * intensity * load + (1 | sub), data = subset(nback_prepost, dvtype == "acc"))
anova(lm_acc, lm_acc2)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: subset(nback_prepost, dvtype == "acc")
    ## Models:
    ## lm_acc2: perf ~ time * intensity * load + (1 | sub)
    ## lm_acc: perf ~ time * intensity * load + (1 + time + intensity + load | 
    ## lm_acc:     sub)
    ##         Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
    ## lm_acc2 10 -592.69 -559.70 306.34  -612.69                             
    ## lm_acc  19 -630.62 -567.95 334.31  -668.62 55.931      9   8.09e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(lm_acc)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: perf ~ time * intensity * load + (1 + time + intensity + load |  
    ##     sub)
    ##    Data: subset(nback_prepost, dvtype == "acc")
    ## 
    ## REML criterion at convergence: -608.3
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6774 -0.4954 -0.0280  0.6455  1.8458 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr             
    ##  sub      (Intercept) 0.001952 0.04418                   
    ##           time1       0.000348 0.01865   0.34            
    ##           intensity1  0.001018 0.03191  -0.57 -0.82      
    ##           load1       0.002180 0.04669   0.45 -0.68  0.29
    ##  Residual             0.001198 0.03461                   
    ## Number of obs: 200, groups:  sub, 25
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error         df t value Pr(>|t|)
    ## (Intercept)             9.263e-01  9.168e-03  2.400e+01 101.031  < 2e-16
    ## time1                   2.904e-02  6.154e-03  3.350e+01   4.719 4.09e-05
    ## intensity1              4.274e-04  8.042e-03  2.404e+01   0.053   0.9581
    ## load1                  -2.896e-02  1.054e-02  2.413e+01  -2.747   0.0112
    ## time1:intensity1        1.891e-02  9.788e-03  1.200e+02   1.932   0.0557
    ## time1:load1             1.192e-02  9.788e-03  1.200e+02   1.218   0.2256
    ## intensity1:load1       -1.197e-02  9.788e-03  1.200e+02  -1.222   0.2239
    ## time1:intensity1:load1  3.885e-02  1.958e-02  1.200e+02   1.984   0.0495
    ##                           
    ## (Intercept)            ***
    ## time1                  ***
    ## intensity1                
    ## load1                  *  
    ## time1:intensity1       .  
    ## time1:load1               
    ## intensity1:load1          
    ## time1:intensity1:load1 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) time1  intns1 load1  tm1:n1 tm1:l1 int1:1
    ## time1        0.200                                          
    ## intensity1  -0.436 -0.394                                   
    ## load1        0.381 -0.366  0.203                            
    ## tm1:ntnsty1  0.000  0.000  0.000  0.000                     
    ## time1:load1  0.000  0.000  0.000  0.000  0.000              
    ## intnsty1:l1  0.000  0.000  0.000  0.000  0.000  0.000       
    ## tm1:ntns1:1  0.000  0.000  0.000  0.000  0.000  0.000  0.000

``` r
cat_plot(lm_acc, pred = time, modx = intensity, mod2 = load, geom = "line")
```

    ## Confidence intervals for merMod models is an experimental feature. The
    ## intervals reflect only the variance of the fixed effects, not the
    ## random effects.

![](analysis-notebook_files/figure-markdown_github/unnamed-chunk-22-1.png)

flanker
-------

``` r
flanker_prepost <- data %>%
        filter(exclude=="include") %>%
        select(sub,countergroup,pre_passive_inc_acc:percdiff_passive_con_rt) %>%
        select(sub,countergroup,matches("pre|post")) %>%
        gather(key=condition,value=perf,-sub,-countergroup)

# now split condition column up to it's factors
flanker_prepost <- separate(flanker_prepost, "condition", c("time", "intensity", "load", "dvtype"), "_")

write.csv(flanker_prepost, "acute_flanker-prepost_weng-etal-2015.csv", row.names=FALSE)
```

``` r
flanker_percdiff <- data %>%
        filter(exclude=="include") %>%
        select(sub,countergroup,pre_passive_inc_acc:percdiff_passive_con_rt) %>%
        select(sub,countergroup,matches("percdiff")) %>%
        gather(key=condition,value=perf,-sub,-countergroup)

# now split condition column up to it's factors
flanker_percdiff <- separate(flanker_percdiff, "condition", c("drop", "intensity", "load", "dvtype"), "_")
flanker_percdiff$drop <- NULL

write.csv(flanker_percdiff,"acute_flanker-percdiff_weng-etal-2015.csv",row.names=FALSE)
```

### set factors

``` r
flanker_prepost$time <- as.factor(flanker_prepost$time)
flanker_prepost$time <- relevel(flanker_prepost$time,"pre")
flanker_prepost$intensity <- as.factor(flanker_prepost$intensity)
flanker_prepost$intensity <- relevel(flanker_prepost$intensity,"passive")
flanker_prepost$load <- as.factor(flanker_prepost$load)

flanker_percdiff$intensity <- as.factor(flanker_percdiff$intensity)
flanker_percdiff$intensity <- relevel(flanker_percdiff$intensity,"passive")
flanker_percdiff$load <- as.factor(flanker_percdiff$load)
```

### plot flanker

``` r
ggplot(subset(flanker_prepost,dvtype=="acc"), aes(x=time,y=perf,group=sub)) + 
  geom_line() + 
  geom_point() +
        facet_grid(~intensity*load) +
        labs(title="Accuracy",y="Accuracy") + 
        theme(strip.text = element_text(face="bold", size=15,lineheight=5.0), 
        strip.background = element_rect(colour="black", size=1))
```

![](analysis-notebook_files/figure-markdown_github/unnamed-chunk-26-1.png)

``` r
ggplot(subset(flanker_prepost,dvtype=="rt"), aes(x=time,y=perf,group=sub)) + 
  geom_line() + 
  geom_point() +
        facet_grid(~intensity*load) +
        labs(title="Reaction Time",y="RT") + 
        theme(strip.text = element_text(face="bold", size=15,lineheight=5.0), 
        strip.background = element_rect(colour="black", size=1))
```

![](analysis-notebook_files/figure-markdown_github/unnamed-chunk-27-1.png)

``` r
ggplot(subset(flanker_percdiff,dvtype=="rt"), aes(x=intensity,y=perf,fill=load)) + 
  scale_fill_manual(values=c("#E3871C", "#F5BD78","#CC9966")) +
  geom_boxplot() + 
  labs(title="Acute RT %change on flanker",y="%Diff",x="",fill="load") +
  theme(title=element_text(size=16, face='bold'),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=15),
        axis.title.y = element_text(size=16),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))
```

![](analysis-notebook_files/figure-markdown_github/unnamed-chunk-28-1.png)

``` r
ggplot(subset(flanker_percdiff,dvtype=="rt"), aes(x=intensity,y=perf)) + 
  stat_summary(aes(y = perf), size=.5, fun.y = mean, geom="bar",size=1) + 
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", size=.5,width=.3) + 
        facet_grid(~load) +
        labs(title="Acute RT %change on flanker",y="%Diff",x="") +
        theme(title=element_text(size=16, face='bold'),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=15),
        axis.title.y = element_text(size=16),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))
```

    ## Warning: The plyr::rename operation has created duplicates for the
    ## following name(s): (`size`)

![](analysis-notebook_files/figure-markdown_github/unnamed-chunk-28-2.png)

``` r
ggplot(subset(flanker_percdiff,dvtype=="rt"), aes(x=load,y=perf)) + 
  stat_summary(aes(y = perf), size=.5, fun.y = mean, geom="bar",size=1) + 
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", size=.5,width=.3) + 
        facet_grid(~intensity) +
        labs(title="Acute RT %change on flanker",y="%Diff",x="") +
        theme(title=element_text(size=16, face='bold'),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=15),
        axis.title.y = element_text(size=16),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))
```

    ## Warning: The plyr::rename operation has created duplicates for the
    ## following name(s): (`size`)

![](analysis-notebook_files/figure-markdown_github/unnamed-chunk-28-3.png)

``` r
ggplot(subset(flanker_percdiff,dvtype=="acc"), aes(x=intensity,y=perf,fill=load)) + 
  scale_fill_manual(values=c("#E3871C", "#F5BD78","#CC9966")) +
  geom_boxplot() + 
  labs(title="Acute ACC %change on flanker",y="%Diff",x="",fill="load") +
  theme(title=element_text(size=16, face='bold'),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=15),
        axis.title.y = element_text(size=16),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))
```

![](analysis-notebook_files/figure-markdown_github/unnamed-chunk-29-1.png)

``` r
ggplot(subset(flanker_percdiff,dvtype=="acc"), aes(x=intensity,y=perf)) + 
  stat_summary(aes(y = perf), size=.5, fun.y = mean, geom="bar",size=1) + 
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", size=.5,width=.3) + 
        facet_grid(~load) +
        labs(title="Acute ACC %change on flanker",y="%Diff",x="") +
        theme(title=element_text(size=16, face='bold'),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=15),
        axis.title.y = element_text(size=16),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))
```

    ## Warning: The plyr::rename operation has created duplicates for the
    ## following name(s): (`size`)

![](analysis-notebook_files/figure-markdown_github/unnamed-chunk-29-2.png)

``` r
ggplot(subset(flanker_percdiff,dvtype=="acc"), aes(x=load,y=perf)) + 
  stat_summary(aes(y = perf), size=.5, fun.y = mean, geom="bar",size=1) + 
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", size=.5,width=.3) + 
        facet_grid(~intensity) +
        labs(title="Acute ACC %change on flanker",y="%Diff",x="") +
        theme(title=element_text(size=16, face='bold'),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=15),
        axis.title.y = element_text(size=16),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))
```

    ## Warning: The plyr::rename operation has created duplicates for the
    ## following name(s): (`size`)

![](analysis-notebook_files/figure-markdown_github/unnamed-chunk-29-3.png)

#### repeated measures anova with time as a factor

``` r
aovACC<-aov_car(perf~time*intensity*load+Error(sub/time*intensity*load),subset(flanker_prepost,dvtype=="acc"))
nice(aovACC)
```

    ## Anova Table (Type 3 tests)
    ## 
    ## Response: perf
    ##                Effect          df  MSE         F    ges p.value
    ## 1                time       1, 24 0.00      0.69  .0009     .42
    ## 2           intensity       1, 24 0.00      0.01 <.0001     .93
    ## 3                load 1.03, 24.61 0.01 40.47 ***    .40  <.0001
    ## 4      time:intensity       1, 24 0.00    5.81 *   .010     .02
    ## 5           time:load 1.21, 29.00 0.00      0.55   .001     .49
    ## 6      intensity:load 1.22, 29.38 0.00      0.06  .0002     .85
    ## 7 time:intensity:load 1.39, 33.40 0.00      1.86   .005     .18
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
    ## 
    ## Sphericity correction method: GG

#### repeated measures anova with percentdiff

**reaction time**

``` r
aovRT<-aov_car(perf~intensity*load+Error(sub/intensity*load),subset(flanker_percdiff,dvtype=="rt"))
nice(aovRT)
```

    ## Anova Table (Type 3 tests)
    ## 
    ## Response: perf
    ##           Effect          df   MSE    F    ges p.value
    ## 1      intensity       1, 24 66.45 0.00 <.0001     .99
    ## 2           load 1.95, 46.78 15.46 0.04  .0002     .95
    ## 3 intensity:load 1.94, 46.64 16.08 0.06  .0003     .94
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
    ## 
    ## Sphericity correction method: GG

``` r
aovRT_fitted<-lsmeans(aovRT,~load|intensity)
aovRT_fitted
```

    ## intensity = passive:
    ##  load     lsmean       SE   df  lower.CL upper.CL
    ##  con  -0.7134544 1.314549 71.3 -3.334398 1.907489
    ##  inc  -0.7289983 1.314549 71.3 -3.349942 1.891946
    ##  neu  -0.4349059 1.314549 71.3 -3.055850 2.186038
    ## 
    ## intensity = active:
    ##  load     lsmean       SE   df  lower.CL upper.CL
    ##  con  -0.4088091 1.314549 71.3 -3.029753 2.212135
    ##  inc  -0.7717119 1.314549 71.3 -3.392656 1.849232
    ##  neu  -0.6499404 1.314549 71.3 -3.270884 1.971003
    ## 
    ## Confidence level used: 0.95

``` r
pairs(aovRT_fitted)
```

    ## intensity = passive:
    ##  contrast     estimate       SE    df t.ratio p.value
    ##  con - inc  0.01554387 1.108052 95.97   0.014  0.9999
    ##  con - neu -0.27854856 1.108052 95.97  -0.251  0.9658
    ##  inc - neu -0.29409242 1.108052 95.97  -0.265  0.9619
    ## 
    ## intensity = active:
    ##  contrast     estimate       SE    df t.ratio p.value
    ##  con - inc  0.36290282 1.108052 95.97   0.328  0.9426
    ##  con - neu  0.24113126 1.108052 95.97   0.218  0.9742
    ##  inc - neu -0.12177156 1.108052 95.97  -0.110  0.9934
    ## 
    ## P value adjustment: tukey method for comparing a family of 3 estimates

**accuracy**

``` r
aovACC<-aov_car(perf~intensity*load+Error(sub/intensity*load),subset(flanker_percdiff,dvtype=="acc"))
nice(aovACC)
```

    ## Anova Table (Type 3 tests)
    ## 
    ## Response: perf
    ##           Effect          df   MSE      F  ges p.value
    ## 1      intensity       1, 24 23.20 4.93 *  .04     .04
    ## 2           load 1.15, 27.63 25.14   0.37 .004     .58
    ## 3 intensity:load 1.26, 30.25 30.17   1.90  .03     .18
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
    ## 
    ## Sphericity correction method: GG

``` r
aovACC_fitted<-lsmeans(aovACC,~load|intensity)
aovACC_fitted
```

    ## intensity = passive:
    ##  load      lsmean        SE     df    lower.CL   upper.CL
    ##  con  -0.88974359 0.8470577 140.06 -2.56441583  0.7849286
    ##  inc  -1.94353489 0.8470577 140.06 -3.61820713 -0.2688627
    ##  neu  -0.19743590 0.8470577 140.06 -1.87210813  1.4772363
    ## 
    ## intensity = active:
    ##  load      lsmean        SE     df    lower.CL   upper.CL
    ##  con  -0.08448043 0.8470577 140.06 -1.75915267  1.5901918
    ##  inc   1.76328405 0.8470577 140.06  0.08861182  3.4379563
    ##  neu   0.52860999 0.8470577 140.06 -1.14606225  2.2032822
    ## 
    ## Confidence level used: 0.95

``` r
pairs(aovACC_fitted)
```

    ## intensity = passive:
    ##  contrast    estimate       SE    df t.ratio p.value
    ##  con - inc  1.0537913 1.157286 94.26   0.911  0.6350
    ##  con - neu -0.6923077 1.157286 94.26  -0.598  0.8214
    ##  inc - neu -1.7460990 1.157286 94.26  -1.509  0.2914
    ## 
    ## intensity = active:
    ##  contrast    estimate       SE    df t.ratio p.value
    ##  con - inc -1.8477645 1.157286 94.26  -1.597  0.2521
    ##  con - neu -0.6130904 1.157286 94.26  -0.530  0.8569
    ##  inc - neu  1.2346741 1.157286 94.26   1.067  0.5369
    ## 
    ## P value adjustment: tukey method for comparing a family of 3 estimates

**Accuracy with glm and covariates **

Set to effect coding

``` r
contrasts(flanker_prepost$time) <- c(-.5,.5)
contrasts(flanker_prepost$time)
```

    ##      [,1]
    ## pre  -0.5
    ## post  0.5

``` r
contrasts(flanker_prepost$intensity) <- c(-.5,.5)
contrasts(flanker_prepost$intensity)
```

    ##         [,1]
    ## passive -0.5
    ## active   0.5

``` r
flanker_prepost$load <- relevel(flanker_prepost$load,"neu")
# not sure if effect coding for 3-level variable is correct here
c<-contr.treatment(3)
my.coding<-matrix(rep(.25, 6), ncol=2)
my.simple<-c-my.coding
my.simple[my.simple==.75]<-.50
contrasts(flanker_prepost$load)<-my.simple
contrasts(flanker_prepost$load)
```

    ##         2     3
    ## neu -0.25 -0.25
    ## con  0.50 -0.25
    ## inc -0.25  0.50

``` r
lm_acc<-lmer(perf ~ time*intensity*load + (1 + time + intensity + load |sub), data=subset(flanker_prepost,dvtype=="acc"))
lm_acc2<-lmer(perf ~ time*intensity*load + (1 |sub), data=subset(flanker_prepost,dvtype=="acc"))
anova(lm_acc,lm_acc2)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: subset(flanker_prepost, dvtype == "acc")
    ## Models:
    ## lm_acc2: perf ~ time * intensity * load + (1 | sub)
    ## lm_acc: perf ~ time * intensity * load + (1 + time + intensity + load | 
    ## lm_acc:     sub)
    ##         Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
    ## lm_acc2 14 -1114.8 -1062.9 571.37  -1142.8                             
    ## lm_acc  28 -1278.7 -1175.0 667.36  -1334.7 191.97     14  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(lm_acc)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: perf ~ time * intensity * load + (1 + time + intensity + load |  
    ##     sub)
    ##    Data: subset(flanker_prepost, dvtype == "acc")
    ## 
    ## REML criterion at convergence: -1235.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.6726 -0.5943  0.1973  0.4482  3.2337 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev. Corr                   
    ##  sub      (Intercept) 4.151e-04 0.020373                        
    ##           time1       3.158e-05 0.005619 -0.01                  
    ##           intensity1  1.378e-04 0.011738  0.14 -0.75            
    ##           load2       2.179e-06 0.001476  0.58  0.20  0.44      
    ##           load3       4.593e-03 0.067770  1.00  0.03  0.11  0.58
    ##  Residual             5.156e-04 0.022706                        
    ## Number of obs: 300, groups:  sub, 25
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error         df t value Pr(>|t|)
    ## (Intercept)             9.692e-01  4.280e-03  2.411e+01 226.423  < 2e-16
    ## time1                  -2.333e-03  2.853e-03  2.533e+01  -0.818  0.42100
    ## intensity1             -3.333e-04  3.519e-03  2.416e+01  -0.095  0.92532
    ## load2                  -1.000e-03  4.292e-03  2.116e+02  -0.233  0.81598
    ## load3                  -8.933e-02  1.421e-02  2.411e+01  -6.285 1.66e-06
    ## time1:intensity1        1.533e-02  5.244e-03  2.160e+02   2.924  0.00382
    ## time1:load2            -8.667e-03  8.563e-03  2.160e+02  -1.012  0.31262
    ## time1:load3            -6.667e-03  8.563e-03  2.160e+02  -0.779  0.43710
    ## intensity1:load2        2.000e-03  8.563e-03  2.160e+02   0.234  0.81554
    ## intensity1:load3       -1.333e-03  8.563e-03  2.160e+02  -0.156  0.87641
    ## time1:intensity1:load2  1.333e-03  1.713e-02  2.160e+02   0.078  0.93802
    ## time1:intensity1:load3  3.200e-02  1.713e-02  2.160e+02   1.869  0.06304
    ##                           
    ## (Intercept)            ***
    ## time1                     
    ## intensity1                
    ## load2                     
    ## load3                  ***
    ## time1:intensity1       ** 
    ## time1:load2               
    ## time1:load3               
    ## intensity1:load2          
    ## intensity1:load3          
    ## time1:intensity1:load2    
    ## time1:intensity1:load3 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) time1  intns1 load2  load3  tm1:n1 tm1:l2 tm1:l3 int1:2
    ## time1       -0.003                                                        
    ## intensity1   0.091 -0.197                                                 
    ## load2        0.038  0.005  0.020                                          
    ## load3        0.907  0.011  0.072  0.188                                   
    ## tm1:ntnsty1  0.000  0.000  0.000  0.000  0.000                            
    ## time1:load2  0.000  0.000  0.000  0.000  0.000  0.000                     
    ## time1:load3  0.000  0.000  0.000  0.000  0.000  0.000  0.500              
    ## intnsty1:l2  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000       
    ## intnsty1:l3  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.500
    ## tm1:ntns1:2  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000
    ## tm1:ntns1:3  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000
    ##             int1:3 t1:1:2
    ## time1                    
    ## intensity1               
    ## load2                    
    ## load3                    
    ## tm1:ntnsty1              
    ## time1:load2              
    ## time1:load3              
    ## intnsty1:l2              
    ## intnsty1:l3              
    ## tm1:ntns1:2  0.000       
    ## tm1:ntns1:3  0.000  0.500

``` r
cat_plot(lm_acc, pred = time, modx = intensity, mod2 = load, geom="line")
```

    ## Confidence intervals for merMod models is an experimental feature. The
    ## intervals reflect only the variance of the fixed effects, not the
    ## random effects.

![](analysis-notebook_files/figure-markdown_github/unnamed-chunk-35-1.png)

``` r
lm_rt<-lmer(perf ~ time*intensity*load + (1 + time + intensity + load |sub), data=subset(flanker_prepost,dvtype=="rt"))
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
    ## control$checkConv, : unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
    ## control$checkConv, : Model failed to converge: degenerate Hessian with 1
    ## negative eigenvalues

    ## Warning: Model failed to converge with 1 negative eigenvalue: -2.0e+00

``` r
lm_rt2<-lmer(perf ~ time*intensity*load + (1 |sub), data=subset(flanker_prepost,dvtype=="rt"))
anova(lm_rt,lm_rt2)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: subset(flanker_prepost, dvtype == "rt")
    ## Models:
    ## lm_rt2: perf ~ time * intensity * load + (1 | sub)
    ## lm_rt: perf ~ time * intensity * load + (1 + time + intensity + load | 
    ## lm_rt:     sub)
    ##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
    ## lm_rt2 14 3067.3 3119.1 -1519.6   3039.3                             
    ## lm_rt  28 2854.7 2958.4 -1399.3   2798.7 240.59     14  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(lm_rt)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: perf ~ time * intensity * load + (1 + time + intensity + load |  
    ##     sub)
    ##    Data: subset(flanker_prepost, dvtype == "rt")
    ## 
    ## REML criterion at convergence: 2732.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.7397 -0.4689 -0.0510  0.4982  5.0140 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr                   
    ##  sub      (Intercept) 2364.33  48.624                          
    ##           time1        529.80  23.017   -0.28                  
    ##           intensity1  2436.23  49.358    0.47 -0.76            
    ##           load2         73.07   8.548    0.27  0.60 -0.72      
    ##           load3        372.42  19.298   -0.17  0.39 -0.68  0.61
    ##  Residual              317.17  17.809                          
    ## Number of obs: 300, groups:  sub, 25
    ## 
    ## Fixed effects:
    ##                        Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)            470.4523     9.7791  24.0000  48.108  < 2e-16 ***
    ## time1                   -4.6486     5.0419  23.9995  -0.922    0.366    
    ## intensity1               7.6792    10.0835  24.0011   0.762    0.454    
    ## load2                   -0.3496     3.7683  52.9978  -0.093    0.926    
    ## load3                   78.1002     5.1160  27.2633  15.266 6.99e-15 ***
    ## time1:intensity1        -0.4437     4.1128 191.8756  -0.108    0.914    
    ## time1:load2              2.2431     6.7162 191.8756   0.334    0.739    
    ## time1:load3             -0.2260     6.7162 191.8756  -0.034    0.973    
    ## intensity1:load2         5.7943     6.7162 191.8756   0.863    0.389    
    ## intensity1:load3         2.3914     6.7162 191.8756   0.356    0.722    
    ## time1:intensity1:load2   8.2715    13.4325 191.8756   0.616    0.539    
    ## time1:intensity1:load3   3.3200    13.4325 191.8756   0.247    0.805    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) time1  intns1 load2  load3  tm1:n1 tm1:l2 tm1:l3 int1:2
    ## time1       -0.257                                                        
    ## intensity1   0.456 -0.677                                                 
    ## load2        0.122  0.250 -0.322                                          
    ## load3       -0.126  0.269 -0.502  0.501                                   
    ## tm1:ntnsty1  0.000  0.000  0.000  0.000  0.000                            
    ## time1:load2  0.000  0.000  0.000  0.000  0.000  0.000                     
    ## time1:load3  0.000  0.000  0.000  0.000  0.000  0.000  0.500              
    ## intnsty1:l2  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000       
    ## intnsty1:l3  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.500
    ## tm1:ntns1:2  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000
    ## tm1:ntns1:3  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000
    ##             int1:3 t1:1:2
    ## time1                    
    ## intensity1               
    ## load2                    
    ## load3                    
    ## tm1:ntnsty1              
    ## time1:load2              
    ## time1:load3              
    ## intnsty1:l2              
    ## intnsty1:l3              
    ## tm1:ntns1:2  0.000       
    ## tm1:ntns1:3  0.000  0.500
    ## convergence code: 0
    ## unable to evaluate scaled gradient
    ## Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

``` r
cat_plot(lm_rt, pred = time, modx = intensity, mod2 = load, geom="line")
```

    ## Confidence intervals for merMod models is an experimental feature. The
    ## intervals reflect only the variance of the fixed effects, not the
    ## random effects.

![](analysis-notebook_files/figure-markdown_github/unnamed-chunk-37-1.png)
