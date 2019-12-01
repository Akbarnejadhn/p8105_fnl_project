animated\_graphs
================
Olivia Wagner
11/28/2019

``` r
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
prod_county = arcos::summarized_county_annual(state = "NY", key = "WaPo") %>% 
  janitor::clean_names()

county_pop = arcos::county_population(state = "NY", key = "WaPo") %>% 
  janitor::clean_names()
```

``` r
# clean opioid death data #

opioid_death_data = opioid_death_data %>% 
  mutate(county = recode(county, 'St Lawrence' = 'St. Lawrence')) %>%
  filter(year >= 2010) 

# clean opioid er data #

opioid_er_data = opioid_er_data %>% 
  select(year, patient_county_name, rural_urban, payer, er_opioid, inpatient_total_opioid, er_inpatient_total_opioid, outpatient_opioid, overall_opioid) %>%
  rename(county = patient_county_name) %>% 
  mutate(county = recode(county, "Kings (Brooklyn)" = "Kings",
         "New York (Manhattan)" = "New York", 'Richmond (Staten Island)' = 'Richmond')) 


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
  arrange(county, year) 
```

``` r
# Number of Perscription Opioids by County #
prod_county = prod_county %>% 
  group_by(buyer_county,year) %>% 
  summarize(numpills = sum(count)) %>% 
  rename(county = buyer_county) %>%
  ungroup() %>%
  arrange(county, year) %>% 
  mutate(county = ifelse(county == 'SAINT LAWRENCE', 'ST. LAWRENCE', county)) 
```

## Animation Graphs

``` r
# bar chart for number of pills perscribed/overdoses #
# rename common columns #
patients = opioid_total_data %>% group_by(year, county) %>% summarize(opioids = sum(overall_opioid)) %>% filter(county != 'Statewide Total') %>% mutate(county = toupper(county))

# join the two data sets #
perscriptions_data = left_join(patients, prod_county, by = c('county', 'year'))

# filter by applicable counties #
perscriptions_data = perscriptions_data %>% filter(county != 'UNKNOWN', county != 'OTHER THAN NEW YORK STATE') %>% filter(year %in% c('2010', '2011', '2012')) 

# bar graph data #
bargraph = perscriptions_data %>% group_by(year) %>%
  mutate(rank = rank(-opioids)) %>%
  group_by(county) %>% 
  filter(rank <= 25) %>%
  ungroup()

# gganimate, bar chart top 25 counties #
bar = ggplot(bargraph, aes(x = rank,
                          y = as.numeric(opioids),
                          group = county, 
                          fill = as.factor(county),
                          color = as.factor(county))) +
  geom_bar(stat = 'identity', alpha = 0.8) +
  coord_flip(clip = "off", expand = FALSE)+
  geom_text(aes(y=0, label = paste(county, ' ')), vjust = 0.2, hjust = 1, size = 2.5, color = 'black')+
  geom_text(aes(label = as.numeric(opioids)), size = 2.5, hjust = 0, vjust=0.5, position=position_dodge(width = 1), color = 'black')+
  scale_x_continuous(trans = 'reverse', position = 'top')+
  scale_y_continuous(position = 'left')+
  labs(x = "County", y = "Opioid Related Treatment Cases")+
  theme(legend.position = 'none', plot.margin=unit(c(2, 2, 2, 5),"cm"))+
  labs(title = 'Number of Opioid Related Treatments',  
       subtitle  =  'Top 25 Counties: {closest_state}') +
  transition_states(year, state_length = 3)+
  enter_fade()+
  exit_shrink()+
  ease_aes('linear')
 
  print(bar)
```

``` r
# animated graph that shows how many pills were perscribed in each county as time passes#
# NY state instituted restrictions on opioids perscribed #

print(prod_county)
```

    ## # A tibble: 434 x 3
    ##    county    year numpills
    ##    <chr>    <int>    <int>
    ##  1 ALBANY    2006    15696
    ##  2 ALBANY    2007    18424
    ##  3 ALBANY    2008    20264
    ##  4 ALBANY    2009    20641
    ##  5 ALBANY    2010    21543
    ##  6 ALBANY    2011    21103
    ##  7 ALBANY    2012    21094
    ##  8 ALLEGANY  2006     3747
    ##  9 ALLEGANY  2007     3986
    ## 10 ALLEGANY  2008     3930
    ## # ... with 424 more rows

``` r
#data = prod_county #

time_plot = ggplot(data = prod_county, aes(x = year, y = numpills, group = county)) +
  geom_line()+
  geom_segment(aes(xend = 2013 , yend = numpills), linetype = 'dotted' , color = 'grey') +
  geom_point(size = 2) +
  geom_text(aes(x = max(year)+1, label = county), hjust = 1) +
  transition_reveal(year)+
  coord_cartesian(clip = 'off') +
  labs(title = 'Number of Pills Perscribed by County', x = 'Year', y = 'Number of Pills Perscribed') +
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5)) +
  scale_x_continuous(limits = c(2006, 2014))

print(time_plot)
```

``` r
# data by insurance provider #


insurance_data = opioid_total_data %>% group_by(payer, year, county) %>% 
  filter(county != 'Unknown', county != 'Statewide Total', county != 'Other than New York State') %>%
  summarise(opioid_treatment = sum(overall_opioid), opioid_death = sum(opioid_poisoning_deaths)) %>%
  filter(payer %in% c('Commercial', 'Medicaid', 'Medicare'))


# graph of opioid deaths v treatment overtime by payer #

boxplot_treatment = ggplot(data = insurance_data, aes(x = payer, y = opioid_treatment, fill = as.factor(payer), alpha = 0.35))+
  geom_boxplot() +
  transition_states(year, transition_length = 2, state_length = 3) +
  enter_fade()+
  exit_shrink()+
  ease_aes('sine-in-out')+
  labs(title = 'Distribution of Opioid Treatment by Insurance Payer: {closest_state}', x = 'Insurance Payer', y = 'Number of Opioid Related Treatments')+
  theme(legend.position = 'none')

print(boxplot_treatment)
```
