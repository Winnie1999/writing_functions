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
    ## -3.15365 -0.79168 -0.04628 -0.07991  0.82046  1.88372

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
    ##  [1] 4.229508 2.365378 4.649019 3.191325 1.511879 2.875362 3.230320 2.776284
    ##  [9] 2.083409 3.195039 2.821900 1.885724 3.029424 4.467078 3.329090 3.025135
    ## [17] 2.558647 3.815326 2.211216 3.530382
    ## 
    ## $b
    ##  [1]  -2.97810472   1.24797406  -1.16783154  -4.52432646   2.07600674
    ##  [6]   5.59246804  -6.42044130   0.80309429   6.72374569  -2.80213672
    ## [11]  -0.92588065  -0.78627927  -6.94046834 -11.75789133  -0.09051474
    ## [16]  -0.49918817   4.78907788   4.35877946 -10.95382881  -6.43933862
    ## [21]  -1.65291489   1.10576124  -5.40647557   3.55713404  -3.52523451
    ## [26]   4.65121552   4.66163065  -2.97451831   0.92584074   8.99978019
    ## 
    ## $c
    ##  [1] 10.136546 10.118154  9.936820  9.790762 10.262087  9.917217  9.871541
    ##  [8]  9.848461  9.859674  9.723256 10.034849 10.051426 10.071857 10.178792
    ## [15] 10.037814  9.996072  9.977603  9.784916 10.221042 10.082731  9.872466
    ## [22] 10.289296 10.071853 10.003792 10.102307 10.110016 10.229947  9.818855
    ## [29]  9.502741  9.928939 10.199498 10.366237 10.193974 10.306713 10.273561
    ## [36] 10.009596 10.020632  9.845843  9.864355  9.769842
    ## 
    ## $d
    ##  [1] -1.238159 -3.287332 -2.151860 -3.172857 -1.824014 -3.062844 -4.162541
    ##  [8] -1.792428 -4.419420 -3.193829 -3.561910 -3.394324 -3.275380 -1.683053
    ## [15] -2.171650 -1.730405 -4.767455 -2.507002 -2.198511 -4.265007

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
    ## 1  3.04 0.832

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.678  5.03

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.187

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.89  1.03

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
