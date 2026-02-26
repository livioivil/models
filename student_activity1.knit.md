---
title: "Statistics for Neuroscience – Lab Activity"
author: "Livio Finos"
output:
  pdf_document:
    number_sections: yes
    toc: yes
  html_document:
    number_sections: yes
    toc_float: yes
    toc: yes
---

# Part 1: Design a study

**Does hippocampal volume predict memory performance?**  

*Design a study*:  
- How should the data be collected?  
- What sample size?  
- Which variables should be recorded?  
- What statistical analysis is required?  

---

---

---

---

---

---

---

---

---

---

---

# Part 2: Analyse the data

## Age in 2 groups

Load data


``` r
library(tidyverse)
```

```
## -- Attaching core tidyverse packages ------------------------ tidyverse 2.0.0 --
## v dplyr     1.1.4     v readr     2.1.5
## v forcats   1.0.1     v stringr   1.5.2
## v ggplot2   4.0.0     v tibble    3.3.0
## v lubridate 1.9.4     v tidyr     1.3.1
## v purrr     1.1.0     
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
## i Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
data <- read.csv("./data/hippocampus_memory_age_group.csv")
head(data)
```

```
##   memory_score hippocampal_volume sex education_years      ICV     motion
## 1     91.53550           8.944114   0              12 1340.944 0.11138780
## 2     92.45135           9.420234   0              15 1630.450 0.09647092
## 3     82.95413           9.183322   0              14 1519.601 0.19102411
## 4     72.04912           9.376540   1              14 1586.066 0.23391279
## 5     84.27416           9.678588   1              13 1746.514 0.15968465
## 6     95.60113          10.016206   1              19 1713.977 0.12493657
##   sleep_hours stress_score handedness age_group
## 1    7.496961     46.60323          0    1Young
## 2    6.775125     33.35235          1    1Young
## 3    5.882857     59.28852          1    1Young
## 4    6.605005     64.16827          0    1Young
## 5    8.549830     49.37279          0    1Young
## 6    6.256486     40.19098          0    1Young
```


### Codebook

| Variable             | Type            | Description                     | Values / Units                   | Notes                                                     |
| -------------------- | --------------- | ------------------------------- | -------------------------------- | --------------------------------------------------------- |
| `memory_score`       | numeric         | Composite episodic memory score | ~50–120                          | Outcome variable; higher = better memory                  |
| `hippocampal_volume` | numeric         | Hippocampal volume from MRI     | ~6–9 mm³                         | Predictor of interest; affected by age and ICV            |
| `age_group`          | categorical     | Age stratum                     | `"Young"` (<50), `"Older"` (≥50) | Key confounder in analysis                                |
| `sex`                | binary / factor | Biological sex                  | 0 = female, 1 = male             | May have small/no effect on memory                        |
| `education_years`    | numeric         | Years of formal education       | ~10–20                           | Small positive effect on memory                           |
| `ICV`                | numeric         | Intracranial volume             | ~1300–1800 mm³                   | Covariate; larger in males; influences hippocampal volume |
| `motion`             | numeric         | Head motion during MRI          | ~0–0.4                           | Minor noise; fMRI nuisance variable                       |
| `sleep_hours`        | numeric         | Average sleep per night         | ~5–9 hours                       | Irrelevant noise variable                                 |
| `stress_score`       | numeric         | Self-reported stress            | ~30–70                           | Irrelevant noise variable                                 |
| `handedness`         | binary / factor | Dominant hand                   | 0 = right, 1 = left              | Irrelevant noise variable                                 |

### Explore

1. What variables are available?  
2. How to 

``` r
summary(data)
```

```
##   memory_score    hippocampal_volume      sex         education_years
##  Min.   : 54.38   Min.   : 8.193     Min.   :0.0000   Min.   : 9.00  
##  1st Qu.: 75.10   1st Qu.: 8.959     1st Qu.:0.0000   1st Qu.:14.00  
##  Median : 81.79   Median : 9.338     Median :0.0000   Median :15.00  
##  Mean   : 81.27   Mean   : 9.335     Mean   :0.4227   Mean   :14.99  
##  3rd Qu.: 87.64   3rd Qu.: 9.673     3rd Qu.:1.0000   3rd Qu.:16.00  
##  Max.   :100.90   Max.   :10.292     Max.   :1.0000   Max.   :20.00  
##       ICV           motion           sleep_hours     stress_score  
##  Min.   :1296   Min.   :6.357e-05   Min.   :4.060   Min.   :20.03  
##  1st Qu.:1474   1st Qu.:8.852e-02   1st Qu.:6.032   1st Qu.:42.81  
##  Median :1557   Median :1.387e-01   Median :6.821   Median :51.12  
##  Mean   :1557   Mean   :1.449e-01   Mean   :6.906   Mean   :50.18  
##  3rd Qu.:1628   3rd Qu.:2.066e-01   3rd Qu.:7.702   3rd Qu.:57.27  
##  Max.   :1864   Max.   :3.421e-01   Max.   :9.676   Max.   :80.56  
##    handedness      age_group        
##  Min.   :0.0000   Length:220        
##  1st Qu.:0.0000   Class :character  
##  Median :0.0000   Mode  :character  
##  Mean   :0.1136                     
##  3rd Qu.:0.0000                     
##  Max.   :1.0000
```

### Naive model

Fit a simple regression using only hippocampal volume.


``` r
m1 <- lm(memory_score ~ hippocampal_volume, data=data)
summary(m1)
```

```
## 
## Call:
## lm(formula = memory_score ~ hippocampal_volume, data = data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -29.9676  -5.7885   0.2131   5.0341  18.9868 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          14.388     11.114   1.295    0.197    
## hippocampal_volume    7.164      1.189   6.024 7.14e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.824 on 218 degrees of freedom
## Multiple R-squared:  0.1427,	Adjusted R-squared:  0.1388 
## F-statistic: 36.29 on 1 and 218 DF,  p-value: 7.144e-09
```

Plot:


``` r
ggplot(data, aes(hippocampal_volume, memory_score)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)+theme_minimal()
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](student_activity1_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 

Interpretation:
- What is the conclusion?


### Consider Age (categorical)

Fit a model including age group.


``` r
m2 <- lm(memory_score ~ hippocampal_volume + age_group, data=data)
summary(m2)
```

```
## 
## Call:
## lm(formula = memory_score ~ hippocampal_volume + age_group, data = data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -19.7929  -3.8493   0.0754   3.9238  13.4272 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         103.683     11.219   9.242   <2e-16 ***
## hippocampal_volume   -1.720      1.166  -1.475    0.142    
## age_group2Older     -12.718      1.034 -12.297   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.02 on 217 degrees of freedom
## Multiple R-squared:  0.4948,	Adjusted R-squared:  0.4901 
## F-statistic: 106.3 on 2 and 217 DF,  p-value: < 2.2e-16
```

Plot colored by age group:


``` r
ggplot(data, aes(hippocampal_volume, memory_score,col=age_group)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  theme_minimal()
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](student_activity1_files/figure-latex/unnamed-chunk-6-1.pdf)<!-- --> 

Questions:
- Does the association change?
- Is this confounding?
- What happens within each age group?

---

### Add other covariates

Try adding other variables (sex, education, etc.).


``` r
m3 <- lm(memory_score ~ hippocampal_volume + age_group + sex + education_years + ICV, data=data)
summary(m3)
```

```
## 
## Call:
## lm(formula = memory_score ~ hippocampal_volume + age_group + 
##     sex + education_years + ICV, data = data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -17.7373  -4.1069   0.1719   4.5166  14.2710 
## 
## Coefficients:
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         9.137e+01  1.229e+01   7.436 2.47e-12 ***
## hippocampal_volume -1.252e+00  1.475e+00  -0.849  0.39692    
## age_group2Older    -1.233e+01  1.112e+00 -11.091  < 2e-16 ***
## sex                -6.710e-01  9.545e-01  -0.703  0.48284    
## education_years     5.224e-01  2.002e-01   2.609  0.00973 ** 
## ICV                 1.334e-04  5.064e-03   0.026  0.97901    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.963 on 214 degrees of freedom
## Multiple R-squared:  0.5112,	Adjusted R-squared:  0.4998 
## F-statistic: 44.76 on 5 and 214 DF,  p-value: < 2.2e-16
```

Which variables matter? Which do not?



``` r
library(effects)
```

```
## Warning: il pacchetto 'effects' è stato creato con R versione 4.5.2
```

```
## Caricamento del pacchetto richiesto: carData
```

```
## lattice theme set by effectsTheme()
## See ?effectsTheme for details.
```

``` r
plot(Effect("age_group",m3))
```

![](student_activity1_files/figure-latex/unnamed-chunk-8-1.pdf)<!-- --> 

``` r
plot(Effect("education_years",m3))
```

![](student_activity1_files/figure-latex/unnamed-chunk-8-2.pdf)<!-- --> 

### Reflection

Discuss in your group:

- Which variable was the key confounder?
- Why did the naive model mislead us?
- How would you design the study differently?


## Age (continous)

Load data


``` r
library(tidyverse)

data <- read.csv("./data/hippocampus_memory_continuous_age.csv")
head(data)
```

```
##   memory_score hippocampal_volume      age sex education_years      ICV
## 1     84.04417           9.086692 35.93052   0              12 1340.944
## 2     77.07902           9.809754 42.32743   0              15 1630.450
## 3     68.06108           9.483574 54.37120   0              14 1519.601
## 4     45.67415           8.605894 74.49247   1              14 1586.066
## 5     88.29504          10.354951 32.10092   1              13 1746.514
## 6     58.28328           8.650676 73.90338   1              19 1713.977
##       motion sleep_hours stress_score handedness
## 1 0.11138780    7.496961     46.60323          0
## 2 0.09647092    6.775125     33.35235          1
## 3 0.19102411    5.882857     59.28852          1
## 4 0.23391279    6.605005     64.16827          0
## 5 0.15968465    8.549830     49.37279          0
## 6 0.12493657    6.256486     40.19098          0
```




``` r
summary(data)
```

```
##   memory_score    hippocampal_volume      age             sex        
##  Min.   : 33.90   Min.   : 7.547     Min.   :20.78   Min.   :0.0000  
##  1st Qu.: 60.61   1st Qu.: 8.578     1st Qu.:37.06   1st Qu.:0.0000  
##  Median : 72.11   Median : 9.130     Median :50.56   Median :0.0000  
##  Mean   : 72.54   Mean   : 9.126     Mean   :51.08   Mean   :0.4227  
##  3rd Qu.: 84.87   3rd Qu.: 9.620     3rd Qu.:64.92   3rd Qu.:1.0000  
##  Max.   :104.49   Max.   :10.838     Max.   :79.56   Max.   :1.0000  
##  education_years      ICV           motion           sleep_hours   
##  Min.   : 9.00   Min.   :1296   Min.   :6.357e-05   Min.   :4.060  
##  1st Qu.:14.00   1st Qu.:1474   1st Qu.:8.852e-02   1st Qu.:6.032  
##  Median :15.00   Median :1557   Median :1.387e-01   Median :6.821  
##  Mean   :14.99   Mean   :1557   Mean   :1.449e-01   Mean   :6.906  
##  3rd Qu.:16.00   3rd Qu.:1628   3rd Qu.:2.066e-01   3rd Qu.:7.702  
##  Max.   :20.00   Max.   :1864   Max.   :3.421e-01   Max.   :9.676  
##   stress_score     handedness    
##  Min.   :20.03   Min.   :0.0000  
##  1st Qu.:42.81   1st Qu.:0.0000  
##  Median :51.12   Median :0.0000  
##  Mean   :50.18   Mean   :0.1136  
##  3rd Qu.:57.27   3rd Qu.:0.0000  
##  Max.   :80.56   Max.   :1.0000
```

``` r
ggplot(data, aes(hippocampal_volume, memory_score)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)+theme_minimal()
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](student_activity1_files/figure-latex/unnamed-chunk-10-1.pdf)<!-- --> 

### Consider Age (continous)

Fit a model including age group.


``` r
m2 <- lm(memory_score ~ hippocampal_volume + age, data=data)
summary(m2)
```

```
## 
## Call:
## lm(formula = memory_score ~ hippocampal_volume + age, data = data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -16.0274  -4.6017  -0.5688   5.0331  19.4073 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        104.00985   14.33119   7.258 6.92e-12 ***
## hippocampal_volume   1.13975    1.30988   0.870    0.385    
## age                 -0.81963    0.05304 -15.453  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.786 on 217 degrees of freedom
## Multiple R-squared:  0.8113,	Adjusted R-squared:  0.8096 
## F-statistic: 466.6 on 2 and 217 DF,  p-value: < 2.2e-16
```

Plot colored by age :


``` r
ggplot(data, aes(hippocampal_volume, memory_score,col=age)) +
  geom_point() + theme_minimal()
```

![](student_activity1_files/figure-latex/unnamed-chunk-12-1.pdf)<!-- --> 


### Add other covariates

Try adding other variables (sex, education, etc.).


``` r
m3 <- lm(memory_score ~ hippocampal_volume + age + sex + education_years + ICV, data=data)
summary(m3)
```

```
## 
## Call:
## lm(formula = memory_score ~ hippocampal_volume + age + sex + 
##     education_years + ICV, data = data)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -16.306  -3.828  -0.375   5.026  19.589 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        95.610831  15.670696   6.101 4.87e-09 ***
## hippocampal_volume  0.307251   1.733959   0.177 0.859522    
## age                -0.844746   0.066931 -12.621  < 2e-16 ***
## sex                 0.004536   1.050952   0.004 0.996560    
## education_years     0.749366   0.222955   3.361 0.000919 ***
## ICV                 0.003883   0.005779   0.672 0.502344    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.652 on 214 degrees of freedom
## Multiple R-squared:  0.8212,	Adjusted R-squared:  0.817 
## F-statistic: 196.6 on 5 and 214 DF,  p-value: < 2.2e-16
```

Which variables matter? Which do not?



``` r
library(effects)

plot(Effect("age",m3))
```

![](student_activity1_files/figure-latex/unnamed-chunk-14-1.pdf)<!-- --> 

``` r
plot(Effect("education_years",m3))
```

![](student_activity1_files/figure-latex/unnamed-chunk-14-2.pdf)<!-- --> 

## Some readings

- Short notes covering the motivation for multiple regression as a method to control confounders, with examples and discussion of randomized trials vs observational studies. [pages.stat.wisc](https://pages.stat.wisc.edu/~karlrohe/lr333/o1_scribed_lecture_notes/multiple_regression_for_confounding.pdf)

- R tutorial on confounding/interaction and model updating, with code examples for adding covariates and visualizing changes. [mbounthavong](https://mbounthavong.com/blog/2022/1/31/r-tutorials-on-confoundinginteraction-and-linear-regression-model-updates)
