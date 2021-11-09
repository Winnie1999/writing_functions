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
    ## -3.38467 -0.60540  0.01421 -0.04865  0.64779  2.12262

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
    ##  [1] 4.8622208 2.4257340 3.7134632 4.1877894 3.3932894 1.7211886 2.0019610
    ##  [8] 2.9840162 1.4334581 2.7890177 2.5765675 3.6779421 3.2673645 5.7425360
    ## [15] 4.2303353 0.7117002 3.1850147 4.8321886 2.5301911 3.0921433
    ## 
    ## $b
    ##  [1] -2.8159690  8.4755760 -0.7854571 -2.3773138  3.6618361 -0.2388125
    ##  [7]  5.5618894  6.2405050 -3.7443281  7.8982528  4.7836008 -2.0659815
    ## [13]  0.8326805 -0.6908492 -4.2822846  7.4720898 -8.8568577 -1.5818224
    ## [19] 10.0826332  5.4970892  0.7807001 -0.5447760  9.8776651  6.3172809
    ## [25] -1.0622633  8.0053173 -3.2789289  4.6359181 -0.2670467  0.5153848
    ## 
    ## $c
    ##  [1] 10.150389  9.807684  9.830921  9.583821 10.020812  9.520732  9.679717
    ##  [8] 10.089573  9.712498  9.681424 10.109499 10.030586  9.722062  9.859048
    ## [15]  9.544635 10.027255 10.099973 10.466741 10.012899  9.960131  9.864233
    ## [22] 10.401859 10.313295 10.103507  9.783209 10.131107 10.038539  9.856946
    ## [29]  9.714597  9.841445 10.178512  9.924627  9.991591 10.025922 10.373254
    ## [36]  9.877837  9.834403 10.287734  9.918305  9.926053
    ## 
    ## $d
    ##  [1] -3.0768726 -3.5569090 -0.2540200 -2.1982925 -2.3515339 -4.7237460
    ##  [7] -3.7086482 -3.0070710 -3.2211111 -2.5240823 -3.9290290 -2.1269973
    ## [13] -2.0617455 -2.4331234 -3.6464729 -0.5804602 -3.9788722 -3.3649572
    ## [19] -3.4323000 -2.2438539

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
    ## 1  3.17  1.24

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.93  4.86

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.96 0.230

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.82  1.10

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

``` r
output = map_dbl(list_norm, median)
```

``` r
output = map_df(list_norm, mean_and_sd, .id = "input")
```

## List columns!

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm
  )
```

``` r
listcol_df %>%  pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>%  pull(samp)
```

    ## $a
    ##  [1] 4.8622208 2.4257340 3.7134632 4.1877894 3.3932894 1.7211886 2.0019610
    ##  [8] 2.9840162 1.4334581 2.7890177 2.5765675 3.6779421 3.2673645 5.7425360
    ## [15] 4.2303353 0.7117002 3.1850147 4.8321886 2.5301911 3.0921433
    ## 
    ## $b
    ##  [1] -2.8159690  8.4755760 -0.7854571 -2.3773138  3.6618361 -0.2388125
    ##  [7]  5.5618894  6.2405050 -3.7443281  7.8982528  4.7836008 -2.0659815
    ## [13]  0.8326805 -0.6908492 -4.2822846  7.4720898 -8.8568577 -1.5818224
    ## [19] 10.0826332  5.4970892  0.7807001 -0.5447760  9.8776651  6.3172809
    ## [25] -1.0622633  8.0053173 -3.2789289  4.6359181 -0.2670467  0.5153848
    ## 
    ## $c
    ##  [1] 10.150389  9.807684  9.830921  9.583821 10.020812  9.520732  9.679717
    ##  [8] 10.089573  9.712498  9.681424 10.109499 10.030586  9.722062  9.859048
    ## [15]  9.544635 10.027255 10.099973 10.466741 10.012899  9.960131  9.864233
    ## [22] 10.401859 10.313295 10.103507  9.783209 10.131107 10.038539  9.856946
    ## [29]  9.714597  9.841445 10.178512  9.924627  9.991591 10.025922 10.373254
    ## [36]  9.877837  9.834403 10.287734  9.918305  9.926053
    ## 
    ## $d
    ##  [1] -3.0768726 -3.5569090 -0.2540200 -2.1982925 -2.3515339 -4.7237460
    ##  [7] -3.7086482 -3.0070710 -3.2211111 -2.5240823 -3.9290290 -2.1269973
    ## [13] -2.0617455 -2.4331234 -3.6464729 -0.5804602 -3.9788722 -3.3649572
    ## [19] -3.4323000 -2.2438539

``` r
listcol_df %>%
  filter(name == "a")
```

    ## # A tibble: 1 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

Let’s try some operations.

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.17  1.24

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.93  4.86

Can I just … map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.17  1.24
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.93  4.86
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.96 0.230
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.82  1.10

So .. can I add a list column?

``` r
listcol_df = 
  listcol_df %>% 
  mutate(summary = map(samp, mean_and_sd), medians = map_dbl(samp, median))
```
