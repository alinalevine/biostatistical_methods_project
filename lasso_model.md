lasso\_model
================

``` r
death_data_full = read_csv("data/Cancer_Registry.csv") %>%
    janitor::clean_names() %>%
    mutate(median_age = ifelse(median_age >60, NA, median_age)) %>%
    separate(geography, into = c("county", "state"), sep = ", ") %>%
    mutate(study_per_cap = ifelse(study_per_cap == 0,0,1)) %>%
    mutate(race_other = pct_other_race + pct_asian) %>%
    mutate(state = ifelse(state == "District of Columbia", "DC", state.abb[match(state, state.name)])) %>% 
    mutate(region_w = ifelse(state == "WA" | state == "MT" | state == "ID" | state == "OR" | state == "WY" | state == "NV" | state == "UT" | state == "CO" | state == "CA", 1, 0),
           region_sw = ifelse(state == "AZ" | state == "NM" | state == "TX" | state == "OK", 1, 0),
           region_me = ifelse(state == "ND" | state == "SD" | state == "NE" | state == "KS" | state == "MN" | state == "IA" | state == "MO" | state == "WI" | state == "IL" | state == "MI" | state == "IN" | state == "OH", 1, 0),
           region_se = ifelse(state == "AR" | state == "LA" | state == "MS" | state == "AL" | state == "GA" | state == "FL" | state == "SC" | state == "NC" | state == "TN" | state == "KY" | state == "VA" | state == "WV" | state == "MD" | state == "DC" , 1, 0)) %>% 
   unite(geography,c(county, state), sep = ", ")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   avgDeathsPerYear = col_integer(),
    ##   medIncome = col_integer(),
    ##   popEst2015 = col_integer(),
    ##   binnedInc = col_character(),
    ##   Geography = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
cancer_data = read_csv("./data/Cancer_Registry.csv") %>% 
  janitor::clean_names() %>% 
  mutate(study_per_cap = ifelse(study_per_cap == 0, 0, 1),
         mortality = avg_deaths_per_year/pop_est2015,
         geography = ifelse(str_detect(geography, "County") == TRUE,geography,str_replace(geography, ","," County,")),
         geography = str_replace(geography,"city"," City"),
         race_nonwhite = pct_asian + pct_other_race + pct_black)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   avgDeathsPerYear = col_integer(),
    ##   medIncome = col_integer(),
    ##   popEst2015 = col_integer(),
    ##   binnedInc = col_character(),
    ##   Geography = col_character()
    ## )
    ## See spec(...) for full column specifications.

``` r
         #race_nonwhite = ifelse(race_white >= 50,0,1))

sex_data = read_csv("./data/SEX01.csv") %>% 
  dplyr::select(Areaname,SEX255209D) %>% 
  separate(Areaname, c("county","state"), sep = ", ") %>% 
  na.omit() %>% 
  mutate(county = paste(county,"County", sep = " ")) %>% 
  unite(geography,c(county, state), sep = ", ")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   SEX255203F = col_integer(),
    ##   SEX255203D = col_double(),
    ##   SEX255204F = col_integer(),
    ##   SEX255204D = col_double(),
    ##   SEX255205F = col_integer(),
    ##   SEX255205D = col_double(),
    ##   SEX255206F = col_integer(),
    ##   SEX255206D = col_double(),
    ##   SEX255207F = col_integer(),
    ##   SEX255207D = col_double(),
    ##   SEX255208F = col_integer(),
    ##   SEX255208D = col_double(),
    ##   SEX255209F = col_integer(),
    ##   SEX255209D = col_double(),
    ##   SEX300200F = col_integer(),
    ##   SEX300200D = col_double(),
    ##   SEX300210F = col_integer(),
    ##   SEX300210D = col_double(),
    ##   SEX320200F = col_integer(),
    ##   SEX320200D = col_double()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 53 rows [1,
    ## 2, 70, 100, 116, 192, 251, 316, 325, 329, 330, 331, 399, 559, 565, 610,
    ## 713, 806, 906, 1012, ...].

``` r
age_data=read_csv("./data/AGE04.csv") %>% 
  dplyr::select(Areaname,AGE775208D) %>% 
  separate(Areaname, c("county","state"), sep = ", ") %>% 
  na.omit() %>% 
  mutate(county = paste(county,"County", sep = " ")) %>% 
  unite(geography,c(county, state), sep = ", ")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   AGE770209F = col_integer(),
    ##   AGE770209D = col_integer(),
    ##   AGE775200F = col_integer(),
    ##   AGE775200D = col_double(),
    ##   AGE775201F = col_integer(),
    ##   AGE775201D = col_double(),
    ##   AGE775202F = col_integer(),
    ##   AGE775202D = col_double(),
    ##   AGE775203F = col_integer(),
    ##   AGE775203D = col_double(),
    ##   AGE775204F = col_integer(),
    ##   AGE775204D = col_double(),
    ##   AGE775205F = col_integer(),
    ##   AGE775205D = col_double(),
    ##   AGE775206F = col_integer(),
    ##   AGE775206D = col_double(),
    ##   AGE775207F = col_integer(),
    ##   AGE775207D = col_double(),
    ##   AGE775208F = col_integer(),
    ##   AGE775208D = col_double()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 53 rows [1,
    ## 2, 70, 100, 116, 192, 251, 316, 325, 329, 330, 331, 399, 559, 565, 610,
    ## 713, 806, 906, 1012, ...].

``` r
cancer_1 = left_join(death_data_full,sex_data,by = "geography")
death_data_full = left_join(cancer_1,age_data,by = "geography") 

death_data_full = death_data_full %>% 
  separate(geography, c("county","state"), sep = ", ")

death_data = death_data_full %>%
    janitor::clean_names() %>%
    dplyr::select(-c(avg_ann_count,avg_deaths_per_year, pct_some_col18_24, binned_inc, county, pct_asian, pct_other_race,state))
```

``` r
d = death_data

d[is.na(d)] <- 0

cancer_x = d %>%
  select(-target_death_rate)

predictor = as.matrix(cancer_x)

target = as.matrix(d$target_death_rate)

grid <- 10^seq(2,-2, length=20)

lasso <- cv.glmnet(predictor, target, standardize = TRUE, lambda = grid)

plot(lasso$glmnet.fit, "lambda", label=TRUE)
```

![](lasso_model_files/figure-markdown_github/lasso-1.png)

``` r
colnames(predictor)
```

    ##  [1] "incidence_rate"             "med_income"                
    ##  [3] "pop_est2015"                "poverty_percent"           
    ##  [5] "study_per_cap"              "median_age"                
    ##  [7] "median_age_male"            "median_age_female"         
    ##  [9] "avg_household_size"         "percent_married"           
    ## [11] "pct_no_hs18_24"             "pct_hs18_24"               
    ## [13] "pct_bach_deg18_24"          "pct_hs25_over"             
    ## [15] "pct_bach_deg25_over"        "pct_employed16_over"       
    ## [17] "pct_unemployed16_over"      "pct_private_coverage"      
    ## [19] "pct_private_coverage_alone" "pct_emp_priv_coverage"     
    ## [21] "pct_public_coverage"        "pct_public_coverage_alone" 
    ## [23] "pct_white"                  "pct_black"                 
    ## [25] "pct_married_households"     "birth_rate"                
    ## [27] "race_other"                 "region_w"                  
    ## [29] "region_sw"                  "region_me"                 
    ## [31] "region_se"                  "sex255209d"                
    ## [33] "age775208d"

16, 21, 3, 6, 15, 17, 13, 25, 18, 23

PctBachDeg25\_Over, PctPublicCoverageAlone, incidenceRate, povertyPercent, PctHS25\_Over, PctUnemployed16\_Over, PctHS18\_24, PctMarriedHouseholds, PctPrivateCoverage, PctBlack

``` r
fit_1 = lm(target_death_rate ~ pct_bach_deg25_over + region_w + pct_public_coverage_alone + region_se + pct_private_coverage_alone + pct_emp_priv_coverage + pct_married_households + median_age_female + pct_hs18_24 + birth_rate, data = d)

summary(fit_1)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ pct_bach_deg25_over + region_w + 
    ##     pct_public_coverage_alone + region_se + pct_private_coverage_alone + 
    ##     pct_emp_priv_coverage + pct_married_households + median_age_female + 
    ##     pct_hs18_24 + birth_rate, data = d)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -102.602  -12.525    0.452   12.070  153.916 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                198.85160    8.04208  24.726  < 2e-16 ***
    ## pct_bach_deg25_over         -1.67366    0.10460 -16.000  < 2e-16 ***
    ## region_w                   -12.11827    1.35442  -8.947  < 2e-16 ***
    ## pct_public_coverage_alone    0.89668    0.11550   7.763 1.12e-14 ***
    ## region_se                    8.37050    0.92173   9.081  < 2e-16 ***
    ## pct_private_coverage_alone  -0.00434    0.01976  -0.220   0.8262    
    ## pct_emp_priv_coverage        0.45446    0.06840   6.644 3.60e-11 ***
    ## pct_married_households      -0.74180    0.07413 -10.007  < 2e-16 ***
    ## median_age_female           -0.16009    0.08354  -1.916   0.0554 .  
    ## pct_hs18_24                  0.41354    0.04933   8.384  < 2e-16 ***
    ## birth_rate                  -0.90150    0.20786  -4.337 1.49e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 21.71 on 3048 degrees of freedom
    ## Multiple R-squared:  0.3905, Adjusted R-squared:  0.3885 
    ## F-statistic: 195.3 on 10 and 3048 DF,  p-value: < 2.2e-16

``` r
fit_2 = lm(target_death_rate ~ pct_bach_deg25_over + region_w + pct_public_coverage_alone + region_se + pct_emp_priv_coverage + pct_married_households + median_age_female + pct_hs18_24 + birth_rate, data = d)

summary(fit_2)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ pct_bach_deg25_over + region_w + 
    ##     pct_public_coverage_alone + region_se + pct_emp_priv_coverage + 
    ##     pct_married_households + median_age_female + pct_hs18_24 + 
    ##     birth_rate, data = d)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -102.612  -12.571    0.476   12.037  153.866 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               198.76949    8.03213  24.747  < 2e-16 ***
    ## pct_bach_deg25_over        -1.67361    0.10458 -16.003  < 2e-16 ***
    ## region_w                  -12.12281    1.35405  -8.953  < 2e-16 ***
    ## pct_public_coverage_alone   0.89844    0.11520   7.799 8.52e-15 ***
    ## region_se                   8.36172    0.92072   9.082  < 2e-16 ***
    ## pct_emp_priv_coverage       0.45164    0.06717   6.724 2.10e-11 ***
    ## pct_married_households     -0.74179    0.07412 -10.008  < 2e-16 ***
    ## median_age_female          -0.16001    0.08352  -1.916   0.0555 .  
    ## pct_hs18_24                 0.41367    0.04932   8.388  < 2e-16 ***
    ## birth_rate                 -0.90307    0.20771  -4.348 1.42e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 21.7 on 3049 degrees of freedom
    ## Multiple R-squared:  0.3905, Adjusted R-squared:  0.3887 
    ## F-statistic: 217.1 on 9 and 3049 DF,  p-value: < 2.2e-16

``` r
fit_3 = lm(target_death_rate ~ pct_bach_deg25_over + region_w + pct_public_coverage_alone + region_se + pct_emp_priv_coverage + pct_married_households + pct_hs18_24 + birth_rate, data = d)

summary(fit_3)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ pct_bach_deg25_over + region_w + 
    ##     pct_public_coverage_alone + region_se + pct_emp_priv_coverage + 
    ##     pct_married_households + pct_hs18_24 + birth_rate, data = d)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -102.198  -12.661    0.428   12.187  154.853 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               190.68734    6.83794  27.887  < 2e-16 ***
    ## pct_bach_deg25_over        -1.66565    0.10455 -15.932  < 2e-16 ***
    ## region_w                  -11.84292    1.34674  -8.794  < 2e-16 ***
    ## pct_public_coverage_alone   0.93390    0.11376   8.210 3.24e-16 ***
    ## region_se                   8.54484    0.91615   9.327  < 2e-16 ***
    ## pct_emp_priv_coverage       0.49426    0.06340   7.795 8.74e-15 ***
    ## pct_married_households     -0.76514    0.07314 -10.461  < 2e-16 ***
    ## pct_hs18_24                 0.39805    0.04866   8.181 4.10e-16 ***
    ## birth_rate                 -0.82528    0.20379  -4.050 5.26e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 21.71 on 3050 degrees of freedom
    ## Multiple R-squared:  0.3898, Adjusted R-squared:  0.3882 
    ## F-statistic: 243.5 on 8 and 3050 DF,  p-value: < 2.2e-16
