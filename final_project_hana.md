final\_project\_hana
================
Hana Akbarnejad
11/28/2019

``` r
# data import

opioid_death_data = janitor::clean_names(read_csv('./opioid_related_deaths.csv'))
```

    ## Parsed with column specification:
    ## cols(
    ##   Year = col_double(),
    ##   County = col_character(),
    ##   `Opioid Poisoning Deaths` = col_double()
    ## )

``` r
opioid_er_data = janitor::clean_names(read_csv('./opioid_related_visits.csv'))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Patient County Name` = col_character(),
    ##   `Rural/Urban` = col_character(),
    ##   Payer = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
opioid_treatment_distance = janitor::clean_names(read_csv('./distance_to_treatment.csv'))
```

    ## Parsed with column specification:
    ## cols(
    ##   STATEFP = col_character(),
    ##   COUNTYFP = col_character(),
    ##   YEAR = col_double(),
    ##   INDICATOR = col_character(),
    ##   VALUE = col_double(),
    ##   STATE = col_character(),
    ##   STATEABBREVIATION = col_character(),
    ##   COUNTY = col_character()
    ## )

``` r
opioid_demographics = janitor::clean_names(read_csv("./opioid_demographics.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   Year = col_double(),
    ##   Region = col_character(),
    ##   `Race or Ethnicity` = col_character(),
    ##   Sex = col_character(),
    ##   `Age Group` = col_character(),
    ##   `Opioid Poisoning Deaths` = col_double()
    ## )

``` r
prod_county = arcos::summarized_county_annual(state = "NY", key = "WaPo") %>% 
  janitor::clean_names()

county_pop = arcos::county_population(state = "NY", key = "WaPo") %>% 
  janitor::clean_names()
```

``` r
# clean opioid death data #

opioid_death_data = opioid_death_data %>% 
  filter(year >= 2010) 

# clean opioid er data #

opioid_er_data = opioid_er_data %>% 
  select(year, patient_county_name, rural_urban, payer, er_opioid, inpatient_total_opioid, er_inpatient_total_opioid, outpatient_opioid, overall_opioid) %>%
  rename(county = patient_county_name)

# clean opioid treatment data #
# data is only for the year 2017#

opioid_treatment_distance %>% 
  filter(state == 'New York') %>%
  select(state, county, value) %>%
  rename(distance = value) 
```

    ## # A tibble: 62 x 3
    ##    state    county             distance
    ##    <chr>    <chr>                 <dbl>
    ##  1 New York Albany County          5.97
    ##  2 New York Allegany County       14.6 
    ##  3 New York Bronx County           0.32
    ##  4 New York Broome County          7.96
    ##  5 New York Cattaraugus County     9.86
    ##  6 New York Cayuga County         11.9 
    ##  7 New York Chautauqua County      8.15
    ##  8 New York Chemung County         6.53
    ##  9 New York Chenango County       11.6 
    ## 10 New York Clinton County        14.4 
    ## # ... with 52 more rows

``` r
# Combine Data Sets #

opioid_total_data = left_join(opioid_er_data, opioid_death_data, by = c('county', 'year')) %>% 
  arrange(county, year) %>% 
  mutate(county = recode(county, "Kings (Brooklyn)" = "Kings",
         "New York (Manhattan)" = "New York"))

opioid_total_data %>% arrange(year, county) %>% print()
```

    ## # A tibble: 1,950 x 10
    ##     year county rural_urban payer er_opioid inpatient_total~
    ##    <dbl> <chr>  <chr>       <chr>     <dbl>            <dbl>
    ##  1  2010 Albany Urban       Medi~        13               14
    ##  2  2010 Albany Urban       Other         0                1
    ##  3  2010 Albany Urban       Medi~        20               11
    ##  4  2010 Albany Urban       Comm~        20                9
    ##  5  2010 Albany Urban       Unkn~         8                0
    ##  6  2010 Alleg~ Rural       Medi~         0                3
    ##  7  2010 Alleg~ Rural       Other         1                0
    ##  8  2010 Alleg~ Rural       Comm~         1                5
    ##  9  2010 Alleg~ Rural       Medi~         0                4
    ## 10  2010 Alleg~ Rural       Unkn~         2                0
    ## # ... with 1,940 more rows, and 4 more variables:
    ## #   er_inpatient_total_opioid <dbl>, outpatient_opioid <dbl>,
    ## #   overall_opioid <dbl>, opioid_poisoning_deaths <dbl>

``` r
# Number of Perscription Opioids by County #
prod_county %>% 
  group_by(buyer_county,year) %>% 
  summarize(numpills = sum(count)) %>% 
  arrange(desc(numpills))
```

    ## # A tibble: 434 x 3
    ## # Groups:   buyer_county [62]
    ##    buyer_county  year numpills
    ##    <chr>        <int>    <int>
    ##  1 SUFFOLK       2011   134486
    ##  2 SUFFOLK       2010   132513
    ##  3 SUFFOLK       2008   126750
    ##  4 SUFFOLK       2009   125565
    ##  5 SUFFOLK       2012   121018
    ##  6 SUFFOLK       2007   120391
    ##  7 ERIE          2011   110324
    ##  8 SUFFOLK       2006   109774
    ##  9 ERIE          2012   109624
    ## 10 ERIE          2010   106673
    ## # ... with 424 more rows

``` r
# Pills bought by Pharmacies in Each county per Year #

pharma_df = left_join(prod_county, county_pop, by = c("buyer_county", "year")) %>% 
  select(county_name, year, count, population) %>% 
  rename(county = county_name,
         pills_bought = count) %>% 
  mutate(county = as.factor(county))
```

``` r
# filter data for NYC
opioid_demographics %>% 
  filter(region == "NYC") %>% 
  rename(race = race_or_ethnicity)
```

    ## # A tibble: 1,789 x 6
    ##     year region race               sex   age_group opioid_poisoning_deaths
    ##    <dbl> <chr>  <chr>              <chr> <chr>                       <dbl>
    ##  1  2003 NYC    Black Non Hispanic F     9-Jan                           0
    ##  2  2003 NYC    Black Non Hispanic F     19-Oct                          0
    ##  3  2003 NYC    Black Non Hispanic F     65-74                           0
    ##  4  2003 NYC    Black Non Hispanic F     75-84                           0
    ##  5  2003 NYC    Black Non Hispanic F     85+                             0
    ##  6  2003 NYC    Black Non Hispanic F     <1                              0
    ##  7  2003 NYC    Black Non Hispanic F     20-24                           1
    ##  8  2003 NYC    Black Non Hispanic F     25-34                           3
    ##  9  2003 NYC    Black Non Hispanic F     55-64                           3
    ## 10  2003 NYC    Black Non Hispanic F     35-44                          10
    ## # ... with 1,779 more rows

``` r
# spaghetti plot of 
opioid_er_data %>% 
  group_by(year, county) %>% 
  summarize(sum_er_visit = sum(er_opioid)) %>% 
  ggplot(aes(x= year, y = sum_er_visit, color = county)) +
  geom_line(alpha = .3) +
  ylim(0, 1000) +
   theme(legend.position = "none")
```

<img src="final_project_hana_files/figure-gfm/er_visits-1.png" width="90%" />
