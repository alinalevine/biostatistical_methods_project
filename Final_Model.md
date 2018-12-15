Final\_Model
================
Yue Gu
12/15/2018

Data Cleaning
-------------

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
sex_data = read_csv("./data/SEX01.csv") %>% 
  dplyr::select(Areaname,SEX255209D) %>% 
  separate(Areaname, c("county","state"), sep = ", ") %>% 
  na.omit() %>% 
  mutate(county = paste(county,"County", sep = " "),
         sex_ratio = SEX255209D) %>% 
  unite(geography,c(county, state), sep = ", ") %>% 
  dplyr::select(geography,sex_ratio)
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
  mutate(county = paste(county,"County", sep = " "),
         pct_aging = AGE775208D) %>% 
  unite(geography,c(county, state), sep = ", ") %>% 
  dplyr::select(geography,pct_aging)
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

death_data =
  death_data_full %>%
  janitor::clean_names() %>%
  dplyr::select(-c(avg_ann_count,avg_deaths_per_year, pct_some_col18_24, binned_inc, county, pct_asian, pct_other_race,state)) %>% 
  mutate(sex_ratio = replace_na(sex_ratio,50.2), pct_aging = replace_na(pct_aging,15.58), pct_private_coverage_alone = replace_na(pct_private_coverage_alone,48.48), pct_employed16_over = replace_na(pct_employed16_over,48.48), median_age = replace_na(median_age,40.82))
```

Stepwise (Automatic Procedure)
------------------------------

``` r
reg_step = lm(formula = target_death_rate ~ incidence_rate + med_income + 
    poverty_percent + median_age_male + percent_married + pct_hs18_24 + 
    pct_hs25_over + pct_bach_deg25_over + pct_employed16_over + 
    pct_unemployed16_over + pct_private_coverage + pct_emp_priv_coverage + 
    pct_white + pct_black + pct_married_households + birth_rate + 
    race_other + region_w + region_sw + region_me + region_se + 
    sex_ratio + pct_aging, data = death_data)

summary(reg_step)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ incidence_rate + med_income + 
    ##     poverty_percent + median_age_male + percent_married + pct_hs18_24 + 
    ##     pct_hs25_over + pct_bach_deg25_over + pct_employed16_over + 
    ##     pct_unemployed16_over + pct_private_coverage + pct_emp_priv_coverage + 
    ##     pct_white + pct_black + pct_married_households + birth_rate + 
    ##     race_other + region_w + region_sw + region_me + region_se + 
    ##     sex_ratio + pct_aging, data = death_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -103.940  -10.306   -0.303   10.153  133.716 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             1.169e+02  1.716e+01   6.814 1.14e-11 ***
    ## incidence_rate          1.909e-01  7.070e-03  27.000  < 2e-16 ***
    ## med_income              1.489e-04  7.469e-05   1.993  0.04632 *  
    ## poverty_percent         3.181e-01  1.523e-01   2.089  0.03680 *  
    ## median_age_male        -3.241e-01  1.284e-01  -2.525  0.01162 *  
    ## percent_married         7.928e-01  1.718e-01   4.616 4.07e-06 ***
    ## pct_hs18_24             2.709e-01  4.652e-02   5.823 6.37e-09 ***
    ## pct_hs25_over           4.353e-01  9.731e-02   4.474 7.96e-06 ***
    ## pct_bach_deg25_over    -9.296e-01  1.443e-01  -6.443 1.36e-10 ***
    ## pct_employed16_over    -5.053e-01  8.894e-02  -5.681 1.46e-08 ***
    ## pct_unemployed16_over   3.087e-01  1.699e-01   1.817  0.06932 .  
    ## pct_private_coverage   -3.928e-01  9.702e-02  -4.048 5.28e-05 ***
    ## pct_emp_priv_coverage   2.939e-01  9.708e-02   3.027  0.00249 ** 
    ## pct_white              -1.687e-01  5.489e-02  -3.073  0.00214 ** 
    ## pct_black              -2.729e-01  5.707e-02  -4.781 1.83e-06 ***
    ## pct_married_households -1.148e+00  1.542e-01  -7.446 1.25e-13 ***
    ## birth_rate             -6.234e-01  1.911e-01  -3.263  0.00111 ** 
    ## race_other             -5.705e-01  9.899e-02  -5.764 9.06e-09 ***
    ## region_w               -3.640e+00  1.823e+00  -1.997  0.04588 *  
    ## region_sw               8.913e+00  1.894e+00   4.706 2.64e-06 ***
    ## region_me               6.580e+00  1.521e+00   4.326 1.57e-05 ***
    ## region_se               1.071e+01  1.613e+00   6.637 3.78e-11 ***
    ## sex_ratio               8.527e-01  2.093e-01   4.074 4.73e-05 ***
    ## pct_aging              -4.715e-01  1.676e-01  -2.813  0.00495 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 18.89 on 3035 degrees of freedom
    ## Multiple R-squared:  0.5406, Adjusted R-squared:  0.5371 
    ## F-statistic: 155.3 on 23 and 3035 DF,  p-value: < 2.2e-16

VIF + CP
--------

``` r
death_data_vif = death_data %>% 
  dplyr::select(-c(med_income, pct_private_coverage, pct_white, median_age, percent_married, pct_public_coverage, median_age_male, pct_emp_priv_coverage, pct_bach_deg25_over))


reg_cp = lm(target_death_rate ~ incidence_rate + study_per_cap + median_age_female + 
           pct_hs18_24 + pct_bach_deg18_24 + 
           pct_hs25_over + pct_employed16_over + pct_unemployed16_over + 
           pct_public_coverage_alone + pct_black + 
           pct_married_households + birth_rate + race_other + region_w + region_sw + 
           region_me + region_se + sex_ratio + pct_aging, data = death_data_vif)

summary(reg_cp)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ incidence_rate + study_per_cap + 
    ##     median_age_female + pct_hs18_24 + pct_bach_deg18_24 + pct_hs25_over + 
    ##     pct_employed16_over + pct_unemployed16_over + pct_public_coverage_alone + 
    ##     pct_black + pct_married_households + birth_rate + race_other + 
    ##     region_w + region_sw + region_me + region_se + sex_ratio + 
    ##     pct_aging, data = death_data_vif)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -119.075  -10.771   -0.534   10.052  133.618 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               58.071248  12.146108   4.781 1.83e-06 ***
    ## incidence_rate             0.190623   0.007036  27.094  < 2e-16 ***
    ## study_per_cap             -2.152834   0.829252  -2.596 0.009474 ** 
    ## median_age_female         -0.364792   0.116936  -3.120 0.001828 ** 
    ## pct_hs18_24                0.294418   0.046807   6.290 3.63e-10 ***
    ## pct_bach_deg18_24         -0.257546   0.100563  -2.561 0.010484 *  
    ## pct_hs25_over              0.767887   0.073445  10.455  < 2e-16 ***
    ## pct_employed16_over       -0.400771   0.079459  -5.044 4.83e-07 ***
    ## pct_unemployed16_over      0.472372   0.167191   2.825 0.004754 ** 
    ## pct_public_coverage_alone  0.588113   0.096763   6.078 1.37e-09 ***
    ## pct_black                 -0.142338   0.036289  -3.922 8.96e-05 ***
    ## pct_married_households    -0.568556   0.079153  -7.183 8.54e-13 ***
    ## birth_rate                -0.368987   0.186991  -1.973 0.048554 *  
    ## race_other                -0.315699   0.088422  -3.570 0.000362 ***
    ## region_w                  -2.736680   1.797449  -1.523 0.127979    
    ## region_sw                 12.141483   1.853527   6.550 6.71e-11 ***
    ## region_me                  8.068116   1.466072   5.503 4.04e-08 ***
    ## region_se                 12.756774   1.550269   8.229 2.77e-16 ***
    ## sex_ratio                  1.094486   0.198586   5.511 3.86e-08 ***
    ## pct_aging                 -0.469054   0.157706  -2.974 0.002960 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.12 on 3039 degrees of freedom
    ## Multiple R-squared:  0.5287, Adjusted R-squared:  0.5258 
    ## F-statistic: 179.4 on 19 and 3039 DF,  p-value: < 2.2e-16

Lasso
-----

``` r
reg_lasso = lm(target_death_rate ~ pct_bach_deg25_over + region_w + poverty_percent + pct_public_coverage_alone + incidence_rate + pct_hs25_over + region_se + pct_unemployed16_over + pct_hs18_24 + sex_ratio + pct_private_coverage + race_other + pct_married_households, data = death_data)

summary(reg_lasso)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ pct_bach_deg25_over + region_w + 
    ##     poverty_percent + pct_public_coverage_alone + incidence_rate + 
    ##     pct_hs25_over + region_se + pct_unemployed16_over + pct_hs18_24 + 
    ##     sex_ratio + pct_private_coverage + race_other + pct_married_households, 
    ##     data = death_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -102.687  -11.009   -0.086   10.892  134.057 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                68.688653  14.759620   4.654 3.40e-06 ***
    ## pct_bach_deg25_over        -0.904371   0.134924  -6.703 2.43e-11 ***
    ## region_w                  -10.058182   1.251341  -8.038 1.29e-15 ***
    ## poverty_percent             0.402897   0.123366   3.266 0.001103 ** 
    ## pct_public_coverage_alone   0.032881   0.138671   0.237 0.812581    
    ## incidence_rate              0.192104   0.007116  26.998  < 2e-16 ***
    ## pct_hs25_over               0.305595   0.092676   3.297 0.000987 ***
    ## region_se                   3.835152   0.872524   4.395 1.14e-05 ***
    ## pct_unemployed16_over       0.535166   0.147178   3.636 0.000281 ***
    ## pct_hs18_24                 0.302517   0.046031   6.572 5.82e-11 ***
    ## sex_ratio                   0.716231   0.180371   3.971 7.33e-05 ***
    ## pct_private_coverage       -0.264454   0.089287  -2.962 0.003082 ** 
    ## race_other                 -0.307973   0.084341  -3.652 0.000265 ***
    ## pct_married_households     -0.295331   0.075033  -3.936 8.47e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.3 on 3045 degrees of freedom
    ## Multiple R-squared:  0.5186, Adjusted R-squared:  0.5166 
    ## F-statistic: 252.4 on 13 and 3045 DF,  p-value: < 2.2e-16
