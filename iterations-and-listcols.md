iterations and listcols
================
Yiqun Jin
11/9/2021

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
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

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

## List

You can put anything in a list

``` r
l = list(
   vec_numeric = 5:8,
   vec_logical = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
   mat = matrix(1:8, nrow = 2, ncol = 4),
   summary = summary(rnorm(100))
)
```

``` r
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $vec_logical
    ## [1]  TRUE  TRUE FALSE  TRUE FALSE FALSE
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -2.03553 -0.53838  0.05542  0.11875  0.71124  2.39341

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
l[["vec_numeric"]][1:3]
```

    ## [1] 5 6 7

``` r
mean(l[["vec_numeric"]])
```

    ## [1] 6.5

## `for` loop

Create a new list

``` r
list_norm = 
  list(
    a = rnorm(20, mean = 3, sd = 1),
    b = rnorm(30, mean = 0, sd = 5),
    c = rnorm(40, mean = 10, sd = .2),
    d = rnorm(20, mean = -3, sd = 1)
  )
```

``` r
list_norm
```

    ## $a
    ##  [1] 4.0241795 3.6017103 2.3276848 2.1072515 2.2880204 2.5992312 2.8575638
    ##  [8] 2.2371495 3.3374955 2.3233239 2.2891658 4.6250973 3.3813496 2.8489016
    ## [15] 4.1302334 2.7597953 3.7365188 3.3574134 3.8410834 0.9695689
    ## 
    ## $b
    ##  [1]  -8.3958713   0.9422373  -0.9625900  -3.8167008   0.5325119   2.5757704
    ##  [7]   1.7362906  -2.8724493  -0.6098092  -6.5072922  -5.3683192   2.7281445
    ## [13]   1.2126889   4.0813338  -6.8242546   1.8492236   1.0379356   6.2653647
    ## [19]   5.8920730   7.2926871  -5.9939121   2.7233484   5.4907646  -4.0938733
    ## [25]   4.4256866 -12.2110664   8.1197822  17.4151738  -1.7047941  -1.4988072
    ## 
    ## $c
    ##  [1]  9.633256 10.310547  9.823981  9.999773 10.378005  9.854114 10.061327
    ##  [8]  9.895348 10.009325  9.944688 10.175841 10.157686  9.828604  9.992794
    ## [15]  9.855687 10.197278 10.021638 10.127124 10.107915  9.841086  9.906863
    ## [22] 10.159963 10.198311 10.011726 10.163031 10.089459 10.079122 10.041204
    ## [29] 10.080204 10.082440  9.700725 10.139682 10.164838  9.997001  9.905716
    ## [36] 10.000829 10.079792 10.015311 10.090346 10.069830
    ## 
    ## $d
    ##  [1] -2.9973365 -4.3581120 -4.1016669 -4.1226231 -3.2495330 -2.0650407
    ##  [7] -1.0176340 -1.8390925 -3.8964380 -2.8490704 -1.8309258 -2.2041393
    ## [13] -4.4894005 -2.7744203 -3.6094784 -2.7294744 -1.2349636 -0.8929491
    ## [19] -4.5640466 -1.9287618

Pause and get my old function

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

I can apply that function to each list element

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.98 0.878

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.449  5.89

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.152

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.84  1.19

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
   output[[i]] = mean_and_sd(list_norm[[i]])
}
```
