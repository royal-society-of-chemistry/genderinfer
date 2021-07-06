## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----setup--------------------------------------------------------------------
library(GenderDiversity)


## ---- warning=FALSE, message=FALSE--------------------------------------------
head(authors)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(dplyr)
library(ggplot2)

authors_df <- assign_gender(data_df = authors, first_name_col = first_name)
  
head(authors_df)

## ---- warning=FALSE, message=FALSE--------------------------------------------
## Count how many female, male and unknown gender there are in the data
authors_df %>% count(gender)

## per gender and country
authors_df %>% count(gender, country_code)


## ----  warning=FALSE, message=FALSE-------------------------------------------
## calculates baseline for the year range 2016-2019
baseline_female <- baseline(data_df = authors_df %>% 
                              filter(publication_years %in% seq(2016, 2019)),
                            gendercol = gender)
baseline_female


## -----------------------------------------------------------------------------
## Create a data frame that containg only the data from 2020 and
## the coung of the variable gender.
female_count_2020 <- authors_df %>% 
  filter(publication_years == 2020) %>%
  count(gender)

## create a new data frame to be used for the binomial calculation.
df_gender <- reshape_for_binomials(data_df = female_count_2020,
                                   gendercol = "gender", 
                                   level = 2020) 

df_gender

## ---- fig.width=6, fig.height=4-----------------------------------------------
## Calculate the binomial
## Create a new column with the baseline and calculate the binomial.

df_gender <- df_gender %>%
  calculate_binom_baseline(baseline_female = baseline_female)

df_gender

## Create a bar chart of showing the number of male, female and unknown gender
gender_total_df(df_gender, level) %>%
  gender_bar_chart("Year",
                 "Total number", "Number of male and female:")


## ---- fig.width=6-------------------------------------------------------------
percent_df(df_gender) %>%
  stacked_bar_chart(baseline_female, "Year", "Percentage of authors",
                      "Female baseline 2016-2019:") + coord_flip()


## ---- ,  warning=FALSE, message=FALSE, fig.width=9, fig.height=6--------------

balloon_plot(data_df = authors_df, first_name = first_name, gender = gender) 


## -----------------------------------------------------------------------------
## calculate binomials for us and uk

UK_US_df <- reshape_for_binomials(authors_df %>%
                                     filter(country_code %in% c("UK", "US"),
                                            publication_years == 2020) %>%
                                     count(gender, country_code),
                                  gender, country_code)


baseline_uk_us <- sapply(UK_US_df$country_code, function(x) {
  baseline(authors_df %>%
             filter(country_code %in% x, 
                    publication_years %in% seq(2016, 2019)), gender)
})

UK_US_binom <- calculate_binom_baseline(UK_US_df, baseline_uk_us)

UK_US_binom

## ---- fig.width=6, fig.height=4-----------------------------------------------
percent_uk_us <- percent_df(UK_US_binom)

bullet_chart <- bullet_chart(percent_uk_us, baseline_uk_us, 
                             x_title = "Countries", y_title = "% Authors",
                             baseline_label = "Female baseline for 2016-2019")
bullet_chart

## -----------------------------------------------------------------------------
## calculate binomials for us and uk

UK_df <- reshape_for_binomials(authors_df %>%
                                     filter(country_code == "UK") %>%
                                     count(gender, publication_years),
                               gender, publication_years)

UK_df

## create a baseline vector containing values for each year from 2016 to 2020.
baseline_fr <- sapply(seq(2016, 2020), function(x) {
  baseline(authors_df %>%
             filter(country_code == "FR", publication_years %in% x), gender)
})

UK_binom <- calculate_binom_baseline(UK_df, baseline_female = baseline_fr)

UK_binom

## ---- fig.width=7, fig.height=5-----------------------------------------------
## Calculate the total number of submission per country and per year
percent_uk <- percent_df(UK_binom)
## calculate the number of submission from UK
total_uk <- authors_df %>%
  filter(country_code == "UK") %>%
  count(publication_years) %>%
  mutate(publication_years = factor(publication_years, 
                                    levels = publication_years))
## conversion factor to create the second y-axis
c <- min(total_uk$n) / 100
bullet_line_chart(percent_uk, baseline_fr, "year", "Authors submission (%)",
                  "French Female baseline", total_uk, publication_years, c = c,
                  "Total number", "Total submission UK")

