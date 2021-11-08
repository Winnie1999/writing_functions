writing\_functions
================
Yiqun Jin
11/4/2021

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)
# Z score
(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.36160307  0.39039430 -1.22687991  0.71862688 -2.32167788  0.79408496
    ##  [7]  0.08105414  0.91282061 -0.25615295  0.09674139  0.01564957  1.41887387
    ## [13] -0.66488485 -0.30008055 -0.04611658 -0.14546085 -1.34704030 -0.65814208
    ## [19]  0.37607011 -1.88679024  0.88891428  2.03254611 -0.36363294 -1.30039891
    ## [25]  1.21929314 -0.67592408 -0.41017967  0.99278657  1.17612842  0.12777438

I want a function to compute z-scores

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  z =  (x - mean(x)) / sd(x)
  
  return(z)
}
z_scores(x_vec)
```

    ##  [1]  0.36160307  0.39039430 -1.22687991  0.71862688 -2.32167788  0.79408496
    ##  [7]  0.08105414  0.91282061 -0.25615295  0.09674139  0.01564957  1.41887387
    ## [13] -0.66488485 -0.30008055 -0.04611658 -0.14546085 -1.34704030 -0.65814208
    ## [19]  0.37607011 -1.88679024  0.88891428  2.03254611 -0.36363294 -1.30039891
    ## [25]  1.21929314 -0.67592408 -0.41017967  0.99278657  1.17612842  0.12777438

Try my function on some other things. These should give errors

``` r
z_scores(3) #NA
```

    ## Error in z_scores(3): Input must have at least three numbers

``` r
z_scores("my name is jeff") #error
```

    ## Error in z_scores("my name is jeff"): Input must be numeric

``` r
z_scores(mtcars) #error
```

    ## Error in z_scores(mtcars): Input must be numeric

``` r
z_scores(c(TRUE, TRUE, FALSE,TRUE))
```

    ## Error in z_scores(c(TRUE, TRUE, FALSE, TRUE)): Input must be numeric

# Multiple outputs

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  mean_x =  mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}
```

Check tat the function works

``` r
x_vec = rnorm(1000,mean = 3, sd = 4)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.86  4.04

## Multiple inputs

I’d like to do this with a function

``` r
sim_data = 
  tibble(
    x = rnorm(100, mean = 4, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.35  2.93

``` r
sim_mean_sd = function(samp_size,mu = 3, sigma = 4){ #default setting
  sim_data = 
  tibble(
    x = rnorm(n = samp_size, mean = mu, sd = sigma)
  )

  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
}
sim_mean_sd(100, 6, 3) #position matching
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.19  2.83

``` r
sim_mean_sd(samp_size = 100, mu = 6, sigma = 3) #name matching
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.65  2.95

``` r
sim_mean_sd(samp_size = 100)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.46  3.95
