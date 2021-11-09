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

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)
# Z score
(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.09296410 -0.56702013  0.52457658 -2.03605096 -0.11157554 -1.34882297
    ##  [7]  1.09273323 -1.76756156  0.07695991  0.46075079  2.66511949  1.40666188
    ## [13]  1.02095037 -0.57922089 -0.05648116 -1.90205398  0.65505892  0.38251439
    ## [19]  0.88440481  0.06700567 -0.52718891 -0.55105947 -0.87582340 -0.03375291
    ## [25]  0.31174574  0.31050652 -0.50434018  0.57992073  0.51819531 -0.18911640

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

    ##  [1]  0.09296410 -0.56702013  0.52457658 -2.03605096 -0.11157554 -1.34882297
    ##  [7]  1.09273323 -1.76756156  0.07695991  0.46075079  2.66511949  1.40666188
    ## [13]  1.02095037 -0.57922089 -0.05648116 -1.90205398  0.65505892  0.38251439
    ## [19]  0.88440481  0.06700567 -0.52718891 -0.55105947 -0.87582340 -0.03375291
    ## [25]  0.31174574  0.31050652 -0.50434018  0.57992073  0.51819531 -0.18911640

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
    ## 1  2.94  3.99

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
    ## 1  3.38  3.10

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
    ## 1  6.22  3.32

``` r
sim_mean_sd(samp_size = 100, mu = 6, sigma = 3) #name matching
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.64  3.21

``` r
sim_mean_sd(samp_size = 100)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.78  4.11

## Let’s review Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>% # Get the first digit
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

What about the next page of reviews..

Let’s turn that code into a function

``` r
read_page_reviews = function(url){
  
  html = read_html(url)

  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()

  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>% # Get the first digit
    as.numeric()

  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()

  reviews = tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
  reviews
}
```

Let me try my function

``` r
dynamite_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

read_page_reviews(dynamite_url)
```

    ## # A tibble: 10 × 3
    ##    title                                                 stars text             
    ##    <chr>                                                 <dbl> <chr>            
    ##  1 Vintage                                                   5 Easy to order. I…
    ##  2 too many commercials                                      1 5 minutes into t…
    ##  3 this film is so good!                                     5 VOTE FOR PEDRO!  
    ##  4 Good movie                                                5 Weird story, goo…
    ##  5 I Just everyone to know this....                          5 VOTE FOR PEDRO !…
    ##  6 the cobweb in his hair during the bike ramp scene lol     5 5 stars for bein…
    ##  7 Best quirky movie ever                                    5 You all know the…
    ##  8 Classic Film                                              5 Had to order thi…
    ##  9 hehehehe                                                  5 goodjobboys      
    ## 10 Painful                                                   1 I think I sneeze…

Let’s read a few pages of reviews

``` r
dynamite_url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

dynamite_urls = str_c(dynamite_url_base, 1:5)

all_reviews = 
  bind_rows(
   read_page_reviews(dynamite_urls[1]),
   read_page_reviews(dynamite_urls[2]),
   read_page_reviews(dynamite_urls[3]),
   read_page_reviews(dynamite_urls[4]),
   read_page_reviews(dynamite_urls[5])
  )
```

## Mean scoping example

``` r
f = function(x){
  z = x + y
  z
}
# global environment
x = 1
y = 2

f(x = y)
```

    ## [1] 4

## Functions as arguments

``` r
my_summary = function(x, summ_func) {
  
  summ_func(x)
}

x_vec = rnorm(100, 3, 7)
mean(x_vec)
```

    ## [1] 2.859161

``` r
median(x_vec)
```

    ## [1] 3.061147

``` r
my_summary(x_vec, mean)
```

    ## [1] 2.859161
