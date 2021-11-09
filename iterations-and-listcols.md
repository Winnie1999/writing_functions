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
    ## -3.04190 -0.85115 -0.12307 -0.09774  0.60598  2.18704

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
    ##  [1] 3.009971 3.398953 2.449876 3.249027 3.963611 3.127782 4.760883 1.413641
    ##  [9] 1.597731 1.026353 4.679549 3.338585 4.233608 4.050474 3.159971 4.122640
    ## [17] 1.099664 2.693351 2.667052 5.209650
    ## 
    ## $b
    ##  [1]  2.6758952  3.9505551  4.6787280  2.9450918 -1.9169743  8.0848099
    ##  [7]  2.8346680 -4.8589127 -8.5833270  4.4045540 -6.6314108  2.2924937
    ## [13]  1.7488643 -0.3579082  1.6570686  0.5357395  0.5328506 -3.2122223
    ## [19] -4.8673442 -4.5179724  0.6143035  3.6075297  2.8433683 -7.4512270
    ## [25]  3.4844099 -0.6001964 -4.2316373  3.9553460 11.2042278  6.6549588
    ## 
    ## $c
    ##  [1]  9.705631 10.137994 10.021799 10.239505 10.156522 10.042003  9.651146
    ##  [8]  9.897489 10.249023 10.063065 10.094249 10.202760 10.020267  9.922921
    ## [15]  9.690675 10.228869  9.987397 10.075584  9.978376  9.621933  9.639084
    ## [22] 10.150944 10.295929  9.750203  9.977431 10.063662 10.177691 10.175909
    ## [29] 10.420434  9.789857 10.178016  9.952256  9.853235 10.314127  9.900088
    ## [36]  9.793065 10.055819 10.128460 10.247087 10.043200
    ## 
    ## $d
    ##  [1] -3.2249636 -1.9849428 -2.9129567 -1.2806998 -3.7865833 -0.7820581
    ##  [7] -3.8544140 -2.4655373 -3.7343118 -2.8839306 -2.7302371 -4.1257624
    ## [13] -2.2004737 -1.6008374 -2.9305426 -2.2762902 -2.7097802 -2.1945934
    ## [19] -3.6625366 -2.3618584

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
    ## 1  3.16  1.21

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.716  4.68

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.203

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.69 0.895

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
   output[[i]] = mean_and_sd(list_norm[[i]])
}
```

## Let’s try map!

``` r
output = map(list_norm, mean_and_sd)
```

What if you want a different function..?

``` r
output = map(list_norm, median)
```
