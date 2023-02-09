Activity 4 - Day 1
================

## Libraries

``` r
library(tidyverse)
library(tidymodels)
library(GGally)
```

## Load Data

``` r
evals <- readr::read_tsv("https://www.openintro.org/data/tab-delimited/evals.txt")
```

### The Data

``` r
evals %>% 
  ggplot(aes(x=score)) + 
  geom_histogram(bins = 24) + 
  theme_bw()
```

![](activity04_files/figure-gfm/plot%20score-1.png)<!-- -->

The distribution of score appears to be left skewed.

``` r
evals %>% 
  ggplot(aes(x=age,y=bty_avg)) + 
  geom_point() + 
  theme_bw()
```

![](activity04_files/figure-gfm/plot%202%20other%20variables-1.png)<!-- -->

There does not appear to be a strong relation between `bty_avg` and
`age`. There may be a weak negative relationship.

## Pairwise Relationships

``` r
evals %>% 
  select(starts_with("bty_")) %>% 
  ggpairs() +
  theme_bw()
```

![](activity04_files/figure-gfm/pairwise-1.png)<!-- --> The relationship
between all of these variables appear to be positive. `bty_m1low` seems
to have the weakest correlations with the 3 variables starting with
`bty_f`. It does not make sense to include all of these beauty variables
in our model.

### MLR: One Quantitative predictor and One Qualitative Predictor

``` r
m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)
tidy(m_bty_gen)
```

    ## # A tibble: 3 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)   3.75      0.0847     44.3  6.23e-168
    ## 2 bty_avg       0.0742    0.0163      4.56 6.48e-  6
    ## 3 gendermale    0.172     0.0502      3.43 6.52e-  4

The predictor `bty_avg` is a significant in explaining the variance in
score. The inclusion of the variable `gender` increase the parameter
estimate for `bty_avg`.

The formula for male professors is 3.747 + .074`bty_avg` + 0.172

Therefore, if two professors have the same beauty rating, the male
professor will have a score that is 0.172 higher than the female.

``` r
m_bty_rank <- lm(score ~ bty_avg + rank, data = evals)
tidy(m_bty_rank)
```

    ## # A tibble: 4 × 5
    ##   term             estimate std.error statistic   p.value
    ##   <chr>               <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)        3.98      0.0908     43.9  2.92e-166
    ## 2 bty_avg            0.0678    0.0165      4.10 4.92e-  5
    ## 3 ranktenure track  -0.161     0.0740     -2.17 3.03e-  2
    ## 4 ranktenured       -0.126     0.0627     -2.01 4.45e-  2
