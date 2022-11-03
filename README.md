Projectfinal
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

This R package is designed to provide a template for users to conduct
various statistical tests, including the two sample t-test, simple
linear regression test and chi-squared test for independence, on any
sample data set to solve the research questions of interest.

## First things first

`projectfinal` is a toy project and is still under development. You can
install the latest version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("hemishahuja140/mq-stat1378-project2022")
```

## Take it for a spin

`projectfinal` contains three wrapper functions with the three seperate
tests. For our example, we will consider `mytest_chi` which will perform
a chi-squared test for independence.

``` r
devtools::install_github("https://github.com/hemishahuja140/practice")
#> Skipping install of 'projectfinal' from a github remote, the SHA1 (8d09e37b) has not changed since last install.
#>   Use `force = TRUE` to force installation

library("projectfinal")
 
mytest_chi(project2022)
#> TESTING FOR INDEPENDENCE BETWEEN GENDER AND AMOUNT OF PHYSICAL ACTIVITY
#> 
#> NULL HYPOTHESIS
#> Gender and amount of physical activity are independent from each other.
#> 
#> ALTERNATE HYPOTHESIS
#> Geder and amount of physical relationship are not independent from each other.
#> 
#> 1. Gender and amount of physical activity are categorical
#> 2. All observations are independent from each other
#> 3. All expected frequencies are large enough (greater than 5)NULL
#> 
#> VALUES
#> $`TEST STATISTIC`
#> X-squared 
#>  3.226111 
#> 
#> $`DEGREES OF FREEDOM`
#> df 
#>  2 
#> 
#> $`P-VALUE`
#> [1] 0.1992778
#> 
#> $`TEST STATISTIC`
#> X-squared 
#>  3.226111 
#> 
#> $`DEGREES OF FREEDOM`
#> df 
#>  2 
#> 
#> $`P-VALUE`
#> [1] 0.1992778
#> 
#> DECISION
#> Since the p.value of 0.199277786323652 is greater than the significance level of 5%, we retain the null hypothesis
#> 
#> CONCLUSION
#> As we have retained the null hypothesis, there is sufficient evidence to suggest that there is no relationship between gender and amount of physical activity.
```
