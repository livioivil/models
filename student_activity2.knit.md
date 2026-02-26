---
title: "Statistics for Neuroscience – Lab Activity 2"
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

## Multiple Linear Regression Homework: Model Selection via F-tests, Adjusted R², AIC, and Effect Sizes

**Objective**: Select and justify the best model for diabetes progression (using in-sample criteria only): F-tests, adjusted R², AIC/BIC, and standardized effect sizes.

### Part 1: Data loading and exploration (10 points)


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
diab <- read_csv("./data/diabetes_sklearn.csv")
```

```
## Rows: 442 Columns: 11
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## dbl (11): age, sex, bmi, bp, s1, s2, s3, s4, s5, s6, target
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```
| Variable | Description                         | Units (pre-standardization) | Notes                                                  |
| -------- | ----------------------------------- | --------------------------- | ------------------------------------------------------ |
| age      | Age                                 | Years                       | -                                                      |
| sex      | Sex                                 | M/F coded quantitatively    | Typically -0.0446 (female), +0.0507 (male)             |
| bmi      | Body mass index                     | kg/m²                       | Strongest univariate predictor                         |
| bp       | Average blood pressure              | mm Hg                       | -                                                      |
| s1       | Total serum cholesterol (tc)        | mg/dL                       | -                                                      |
| s2       | Low-density lipoprotein (ldl)       | mg/dL                       | "Bad" cholesterol                                      |
| s3       | High-density lipoprotein (hdl)      | mg/dL                       | "Good" cholesterol                                     |
| s4       | Total cholesterol / HDL ratio (tch) | Ratio                       | Higher = worse risk profile                            |
| s5       | Log serum triglycerides (ltg)       | log(mg/dL)                  | Often second-strongest predictor                       |
| s6       | Blood glucose level (glu)           | mg/dL                       | Fasting blood sugar                                    |
| target   | Disease progression                 | Quantitative score          | One year after baseline; mean=152, SD=77, range 25–346 |

**Tasks**:  

1. Report predictor correlations with `target`, and pairwise correlations among top 3 predictors of `target`.   
2. Scatterplot matrix of `target` vs `{bmi, s5, bp}`. Comment on linearity.

### Part 2: Baseline models (20 points)

Fit these nested models:


``` r
m_null  <- lm(target ~ 1, data = diab)                    # Intercept only
m_small <- lm(target ~ age + sex + bmi + bp, data = diab) # 4 clinical basics
m_full  <- lm(target ~ ., data = diab)                    # All 10 predictors
```

**Tasks**:

1. Compute and tabulate adjusted R² and AIC/BIC for all three.  
2. **Global F-tests**: `anova(m_null, m_small)` and `anova(m_small, m_full)`. Interpret p-values: evidence against null model? Incremental value of extra 6 predictors?    
3. **Effect size**: Report partial η² for each predictor in `m_full` (use `performance::model_performance()` or `car::Anova()`).

### Part 3: Your model selection (20 points)

**Propose `m_yours`** (4–8 predictors). Justify based on:  
- Domain (e.g., BMI central to diabetes).   
- Univariate correlations.   
- Expected confounding (e.g., control for age/sex).

Example:

``` r
m_yours <- lm(target ~ bmi + s5 + s6 + sex + age, data = diab)
```

**Tasks**:

1. Tabulate adjusted R², AIC/BIC vs `m_small` and `m_full`. Which "wins"?   
2. **Hierarchical F-test**: `anova(m_yours, m_full)`. Significant loss in fit?  
3. **Standardized effects**: Scale predictors and refit. Rank top 3 coefficients by magnitude (effect size).   

### Part 4: Model comparison table (15 points)

Create this table (using `broom::glance()` + `bind_rows()`):

| Model    | adj.R² | Δadj.R² | AIC   | ΔAIC | Global F (p) |
|----------|--------|---------|-------|------|--------------|
| m_null   |        | -       |       | -    | -            |
| m_small  |        |         |       |      |              |
| m_yours  |        |         |       |      |              |
| m_full   |        |         |       |      |              |

**Discussion** (3–4 sentences): Best model by each criterion? Why AIC/BIC favor parsimony? Trade-off with adj.R²?

### Part 5: Interpretation and effect sizes (20 points)

For your best model:


``` r
library(car)
```

```
## Caricamento del pacchetto richiesto: carData
```

```
## 
## Caricamento pacchetto: 'car'
```

```
## Il seguente oggetto è mascherato da 'package:dplyr':
## 
##     recode
```

```
## Il seguente oggetto è mascherato da 'package:purrr':
## 
##     some
```

``` r
Anova(m_yours, type="III")  # Type III SS for effect sizes
```

```
## Anova Table (Type III tests)
## 
## Response: target
##               Sum Sq  Df   F value  Pr(>F)    
## (Intercept) 10229912   1 3184.9720 < 2e-16 ***
## bmi           322025   1  100.2590 < 2e-16 ***
## s5            235764   1   73.4024 < 2e-16 ***
## s6              8786   1    2.7356 0.09886 .  
## sex             9482   1    2.9522 0.08647 .  
## age               64   1    0.0199 0.88799    
## Residuals    1400402 436                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

**Tasks**:
1. Identify 2 strongest predictors (Type III F or partial η² > 0.02).  
2. Interpret: "The effect of BMI is [X] units per SD, p=[Y]."  
3. **Discussion**: Confounding example? (Compare univariate vs multivariate coefficient for BMI.)

### Part 6: Diagnostics (10 points)


``` r
par(mfrow=c(2,2)); plot(m_yours)
```

![](student_activity2_files/figure-latex/unnamed-chunk-5-1.pdf)<!-- --> 

Comment briefly: violations? Influential cases?

### Part 7: Peer review interaction (5 points)

**In class**: Share your table and best model. Groups vote: most parsimonious? Most explanatory? Submit 1-sentence peer feedback.

**Total: 100 points**.
