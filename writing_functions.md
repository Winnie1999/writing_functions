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

    ##  [1] -0.279475145 -0.265277479 -1.060857774  1.155453895  1.554848751
    ##  [6]  1.221278233 -0.236088753  1.212267123 -0.715514362  0.388728565
    ## [11] -1.606967129 -0.867995882  1.529991763  1.284900105  0.237335880
    ## [16] -1.597960442  0.633691994 -0.710063390 -0.811459426 -1.819675156
    ## [21]  1.456941230 -0.060443733 -0.722179747 -0.617428853  0.005083901
    ## [26]  1.157385317  0.752066155 -0.349193160 -0.282079651 -0.587312828

I want a function to compute z-scores

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  if (length(x) < 3){
    stop("Input must have at least three numbers")
  }
  z =  (x - mean(x)) / sd(x)
  
  return(z)
}
z_scores(x_vec)
```

    ##  [1] -0.279475145 -0.265277479 -1.060857774  1.155453895  1.554848751
    ##  [6]  1.221278233 -0.236088753  1.212267123 -0.715514362  0.388728565
    ## [11] -1.606967129 -0.867995882  1.529991763  1.284900105  0.237335880
    ## [16] -1.597960442  0.633691994 -0.710063390 -0.811459426 -1.819675156
    ## [21]  1.456941230 -0.060443733 -0.722179747 -0.617428853  0.005083901
    ## [26]  1.157385317  0.752066155 -0.349193160 -0.282079651 -0.587312828

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
