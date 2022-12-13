# GenderInfer


The goal of `GenderInfer` is to analyse publishing data for gender differences.
It assigns the gender based on the first name.

It should only be used when self-reported gender is unavailable.

This package makes it possible to find if there are significant differences between 
male and female given a baseline.

## Installation

You can install the released version of GenderInfer from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("GenderInfer")
```
It is also possible to install the package directly from github:

``` r
devtools::install_github("rscapplications/genderinfer")
```


The package has the following packages as dependencies:
``` r
  binom
  ggplot2
```
  
## Example

This is a basic example which shows you how to assign gender to a data frame containing first names:

``` r
library(GenderInfer)
## assign gender
authors_df <- assign_gender(data_df = authors, first_name_col = first_name)
head(authors_df)
```

