# GenderDiversity

<!-- badges: start -->

<!-- badges: end -->

The goal of `GenderDiversity` is to analysed data for gender diversity in the editorial field.
It assigns the gender and let possible to find if there are significant differences.

## Installation

You can install the released version of GenderDiversity from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("GenderDiversity")
```

The package use the following packages as dependencies:
``` r
  binom
  dplyr
  ggplot2
  ggpubr
  magrittr
  readr
  rlang
  stats
  stringr
  tibble
  tidyr
```
  
## Example

This is a basic example which shows you how to assign gender to a data frame containing first names:

``` r
library(GenderDiversity)
## assign gender
authors_df <- assign_gender(data_df = authors, first_name_col = first_name)
head(authors_df)
```

