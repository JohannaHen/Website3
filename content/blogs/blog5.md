---
categories:
- ""
- ""
date: "2017-10-31T21:28:43-05:00"
description: ""
draft: false
image: pic10.jpg
keywords: ""
slug: RStudio
title: RStudio
---

---
title: 'Session 4: Homework 2'
author: 'Study Group A3: David Blrtsyan, Yusen Chen, Nguissaly Gueye, Johanna Henriksson,
  Terence Tang'
date: "`r Sys.Date()`"
output:
  html_document:
    theme: flatly
    highlight: zenburn
    number_sections: yes
    toc: yes
    toc_float: yes
    code_folding: show
  pdf_document:
    toc: yes
---


```{r, setup, include=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```


```{r load-libraries, include=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(lubridate)
library(here)
library(skimr)
library(janitor)
library(httr)
library(readxl)
library(vroom)
```



# Trump's Approval Margins

As we saw in class, fivethirtyeight.com has detailed data on [all polls that track the president's approval ](https://projects.fivethirtyeight.com/trump-approval-ratings)

```{r, cache=TRUE}
# Importing approval polls data
approval_polllist <- read_csv('https://projects.fivethirtyeight.com/trump-approval-data/approval_polllist.csv') 

glimpse(approval_polllist)

approval_polllist_tidy <- approval_polllist %>%
   mutate(enddate_date = mdy(enddate),
          enddate_month = month(enddate_date, label=TRUE),
          enddate_year = year(enddate_date))
```

## Create a plot

We would like to calculate the average net approval rate (approve- disapprove) for each week since he got into office. We plot the net approval, along with its 95% confidence interval.

```{r trump_margins, echo=FALSE, warning=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "trump_approval_margin.png"), error = FALSE)

#clean data
approvalrate <- approval_polllist %>%
  mutate(net_approval = approve - disapprove,
         enddate = mdy(enddate),
         week = week(enddate),
         year = year(enddate),
         year_cha = paste(as.character(year))) %>%
  
  #calculate SD and CI
  group_by(year, week) %>%
  mutate(average_net_approval_week = mean(net_approval)) %>%
  mutate(sd_approval = sd(net_approval) / sqrt(count(week)-1),
         upper = average_net_approval_week + 1.96 * sd_approval,
         lower = average_net_approval_week - 1.96 * sd_approval)

glimpse(approvalrate)

#plot
ggplot(approvalrate, aes(x = week, y = average_net_approval_week)) +
  geom_line(aes(colour = year_cha)) +
  
  #upper and lower line
  geom_line(aes(y = upper, colour = year_cha)) +
  geom_line(aes(y = lower, colour = year_cha)) +
  
  #y=0 line
  geom_hline(yintercept = 0, colour = "orange") +
  geom_point(size = 1, aes(colour = year_cha)) +
  geom_ribbon(alpha = 0.2,
              aes(ymin = lower, ymax = upper, fill = year_cha)) +
  
  #facet by year
  facet_wrap(~year,
             scales = "fixed",
             ncol = 2) +
  
  #scale_fill_manual(values=c("red", "green", "blue", "purple")) +  # unnecessary since the random fill is exactly what we need
  scale_x_continuous(breaks = seq(0,52,13)) +
  scale_y_continuous(breaks = seq(-20,7.5,2.5)) +
  
  theme_bw() +
  theme(legend.position = "none") +
  labs (
    title = "Estimating Net Approval (approve-disapprove) for Donald Trump",
    x     = "Week of the year",
    y     = "Average Net Approval (%)",
    subtitle = "Weekly average of all polls"
  )
```

## Compare Confidence Intervals

Compare the confidence intervals for `week 15` (6-12 April 2020) and `week 34` (17-23 August 2020). Can you explain what's going on?

```{r trump_margins_CI, echo=FALSE, out.width="100%"}
approvalrate %>%
  filter(year == 2020, week == 15 | week == 34) %>% 
  summarise(mean(upper), mean(lower))
```
>In early-mid April (week 15), unlike its European counterparts, the US was only at the beginning of the growing wave of covid-19 cases clustered priomarily in NYC. However, by the end of August (week 34) there were far more cases and deaths recorded across the country together with the job losses. Moreover, the summer highlighted the persistent social issue of the America with the Black Lives Matter protests that Trump neglected. These reasons may explain a mucher greater discrepancy in the opinions of the respondents that in turn led to a larger standard error in net approvals and therefore CI expanding from ca. 1.33% to 3.08%.

# Gapminder revisited

We shall recall the `gapminder` data frame from the gapminder package. That data frame contains just six columns from the larger [data in Gapminder World](https://www.gapminder.org/data/). In this part, we will join a few dataframes with more data than the 'gapminder' package. Specifically, we will look at data on 

- Life expectancy at birth (life_expectancy_years.csv)
- GDP per capita in constant 2010 US$ (https://data.worldbank.org/indicator/NY.GDP.PCAP.KD)
- Female fertility: The number of babies per woman (https://data.worldbank.org/indicator/SP.DYN.TFRT.IN)
- Primary school enrollment as % of children attending primary school (https://data.worldbank.org/indicator/SE.PRM.NENR)
- Mortality rate, for under 5, per 1000 live births (https://data.worldbank.org/indicator/SH.DYN.MORT)
- HIV prevalence (adults_with_hiv_percent_age_15_49.csv): The estimated number of people living with HIV per 100 population of age group 15-49.

We use the `wbstats` package to download data from the World Bank. The relevant World Bank indicators are `SP.DYN.TFRT.IN`, `SE.PRM.NENR`, `NY.GDP.PCAP.KD`, and `SH.DYN.MORT`

```{r, get_data, cache=TRUE}
# loading gapminder HIV data
hiv <- read_csv(here::here("data","adults_with_hiv_percent_age_15_49.csv"))
life_expectancy <- read_csv(here::here("data","life_expectancy_years.csv"))

# get World bank data using wbstats
indicators <- c("SP.DYN.TFRT.IN","SE.PRM.NENR", "SH.DYN.MORT", "NY.GDP.PCAP.KD")


library(wbstats)

worldbank_data <- wb_data(country="countries_only", #countries only - no aggregates like Latin America, Europe, etc.
                          indicator = indicators, 
                          start_date = 1960, 
                          end_date = 2016)

# get a dataframe of information regarding countries, indicators, sources, regions, indicator topics, lending types, income levels,  from the World Bank API 
countries <-  wbstats::wb_cachelist$countries

```

We have to join the 3 dataframes (life_expectancy, worldbank_data, and HIV) into one. But before, we may tidy up the data first and then perform [join operations](http://r4ds.had.co.nz/relational-data.html).

```{r, tidy_data_base}
tidylife_expectancy <- life_expectancy %>%
  pivot_longer(!country, #pivot all columns but country  as we the data in question is longitudinal, with same variables measured at different points in time
               names_to = "years",
               values_to = "life_expectancy") %>% 
  filter(years>=1979) #& years<=2011) filter for years as data in HIV starts in 1979

tidyworldbank <- worldbank_data %>% 
  select(-c(1,2)) %>% 
  rename(years=date)
  
regions_countries <- countries %>% 
  select(3,9)

tidyhiv <- hiv %>% 
  pivot_longer(!country, #pivot all columns but country
               names_to = "years",
               values_to = "hiv_prevalence")

join_tables<-tidylife_expectancy %>% 
  inner_join(tidyhiv,key="country") %>% 
  transform(years = as.numeric(years)) %>% 
  inner_join(tidyworldbank) %>% 
  inner_join(regions_countries)
```

1. What is the relationship between HIV prevalence and life expectancy? Generate a scatterplot with a smoothing line to report your results.

```{r, cache=TRUE}
join_tables %>% 
  filter(!is.na(hiv_prevalence)) %>% 
  ggplot(aes(y=life_expectancy,
             x=hiv_prevalence))+
  geom_point(aplha=0)+
  geom_smooth(method=lm)+
  facet_wrap(~region, scales="free")+
  labs(title="The lesser the HIV spread is, the longer is the life expectancy",
       subtile = "Data from 1979 to 2011",
       y= "Life expectancy",
       x="HIV prevalence (% of adults aged 15-49 years)")+
  scale_y_continuous()+
  scale_x_continuous()+
  theme(legend.position="none")+
  theme_minimal()+
  NULL
```
>Life expectancy and HIV prevalance are clearly inversely related, with the most distinct impact observed in the region of Sub-Saharan Africa, where life expectancy imrpove by ca. 5 years with every 10% lesser spread of HIV among the adults. However, when examining the data from regions as whole, we should account for the variations between countries and across years.

2. What is the relationship between fertility rate and GDP per capita? Generate a scatterplot with a smoothing line to report your results.

```{r, cache=TRUE}
join_tables %>% 
  filter(!is.na(NY.GDP.PCAP.KD)) %>% 
  ggplot(aes(x=SP.DYN.TFRT.IN,
             y=NY.GDP.PCAP.KD))+
  geom_point(aplha=0.2)+
  geom_smooth(method=lm)+
  facet_wrap(~region, scales="free")+
  labs(title="The sub-replacement fertility rate (<2.1) notaby raises GDP pp",
       subtile = "Data from 1979 to 2011",
       x= "Fertility rate",
       y="GDP per Capita (constant 2010 US$")+
  scale_x_continuous()+
  scale_y_log10()+
  theme_minimal()+
  NULL
```
>The fertility rate and GDP per capita are also inversely related, as the smaller the number of children in a household that require expenses on education and upbringings, the better ones they can receive leading to exponentially higher productivity and outputs. However, we should bear in mind the extent to which reverse causality may distort the initial findings.

3. Which regions have the most observations with missing HIV data? Generate a bar chart (`geom_col()`), in descending order.

```{r, cache=TRUE}
missing_values_hiv <-join_tables %>% 
  group_by(region) %>% 
  summarise(hiv_missing=sum(is.na(hiv_prevalence))) 
  
ggplot(missing_values_hiv,aes(x=hiv_missing,y=reorder(region,hiv_missing)))+
  geom_col()+
  labs(title = "Harder to capture true HIV prevalence in Africa?",
       subtitle = "Data from 1979 to 2011",
       x= "Number of missing recordings",
       y="")
```

4. How has mortality rate for under 5 changed by region? In each region, find the top 5 countries that have seen the greatest improvement, as well as those 5 countries where mortality rates have had the least improvement or even deterioration.
```{r, cache=TRUE}
#Pivot wider to use the two reference years as variables
join_tables_changes <- join_tables %>%
  pivot_wider(names_from = years, values_from = SH.DYN.MORT) %>% 
  select("country","region","1979","2011")

colnames(join_tables_changes)[3] <- "year_1979"
colnames(join_tables_changes)[4] <- "year_2011"

join_tables_changes_1979 <- join_tables_changes %>% 
    filter(!is.na(year_1979))
join_tables_changes_1979$year_2011 = NULL

join_tables_changes_2011 <- join_tables_changes %>% 
    filter(!is.na(year_2011))
join_tables_changes_2011$year_1979 = NULL

join_tables_changes <- join_tables_changes_1979 %>%
inner_join(join_tables_changes_2011, key = "country")

#mutate variable changes
join_tables_changes <- join_tables_changes %>% 
  mutate(changes_in_perc = year_2011/year_1979 - 1) %>% 
  filter(!is.na(changes_in_perc)) %>% 
  select('country', 'region', 'changes_in_perc')

#Split dataset by region
changes_by_region <- split(join_tables_changes, join_tables_changes$region)
str(changes_by_region)

#Create top 5 and bottom 5 table for each region
east_asia_pacific_bottom5 <- changes_by_region$`East Asia & Pacific` %>%
  slice_max(changes_in_perc, n = 5) %>% 
    print()
east_asia_pacific_top5 <- changes_by_region$`East Asia & Pacific`%>%
  slice_min(changes_in_perc, n = 5) %>% 
    print()

europ_ca_bottom5 <- changes_by_region$`Europe & Central Asia`%>%
  slice_max(changes_in_perc, n = 5) %>% 
    print()
europ_ca_top5 <- changes_by_region$`Europe & Central Asia`%>%
  slice_min(changes_in_perc, n = 5) %>% 
    print()

latin_amer_carr_bottom5 <- changes_by_region$`Latin America & Caribbean`%>%
  slice_max(changes_in_perc, n = 5) %>% 
    print()
latin_amer_carr_top5 <- changes_by_region$`Latin America & Caribbean`%>%
  slice_min(changes_in_perc, n = 5) %>% 
    print()

middle_east_bottom5 <- changes_by_region$`Middle East & North Africa`%>%
  slice_max(changes_in_perc, n = 5) %>% 
    print()
middle_east_top5 <- changes_by_region$`Middle East & North Africa`%>%
  slice_min(changes_in_perc, n = 5) %>% 
    print()

north_amer_top <- changes_by_region$`North America`%>%
  slice_min(changes_in_perc, n = 5) %>% 
    print()

sount_asia_bottom5 <- changes_by_region$`South Asia`%>%
  slice_max(changes_in_perc, n = 5) %>% 
    print()
sount_asia_top5 <- changes_by_region$`South Asia`%>%
  slice_min(changes_in_perc, n = 5) %>% 
    print()

africa_bottom5 <- changes_by_region$`Sub-Saharan Africa`%>%
  slice_max(changes_in_perc, n = 5) %>% 
    print()
africa_top5 <- changes_by_region$`Sub-Saharan Africa`%>%
  slice_min(changes_in_perc, n = 5) %>% 
    print()

#Overview by region

changes_by_region_summary <- join_tables_changes %>% 
  group_by(region) %>% 
  summarise(min = max(changes_in_perc),mean = mean(changes_in_perc),median= median(changes_in_perc), max = min(changes_in_perc)) %>% 
  print()
#The more negative change is, the greater is  the improvement as the parameter of mortality is negative.

ggplot(join_tables_changes, aes(x = changes_in_perc)) + geom_density(aes(fill = region), alpha = 0.4) +
  facet_wrap(~region, scales="free")+
    scale_x_continuous(labels = scales :: percent)+
    theme(legend.position ='none')+
    labs(title="Under-5 mortality has improved dramatically across the globe... but not equally across regions",
       subtile = "Data from 1979 to 2011",
       x= "Change in under-5 mortality",
       y="Number of countries")+
  NULL
```
>

5. Is there a relationship between primary school enrollment and fertility rate?
```{r, cache=TRUE}
join_tables %>% 
  filter(!is.na(SE.PRM.NENR)) %>% 
  ggplot(aes(x=SE.PRM.NENR,
             y=SP.DYN.TFRT.IN))+
  geom_point(aplha=0.2)+
  geom_smooth(method=lm)+
  labs(title="Smallest families are sending more children to primary school",
       subtile = "Data from 1979 to 2011",
       x= "Primary school enrollment (net %)",
       y="Fertility rate")
```
 > As expected, primary dschool enrollement is negatively correlated with the fertility rate: the fewer children are born per woman the more likely, the more likely children to receive access to education earlier on. The reverse causality story is equally applicable as the merits of smaller household sizes are evalated in schools.

# Challenge 1: CDC COVID-19 Public Use Data

Let us revisit the [CDC Covid-19 Case Surveillance Data](https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf). There are well over 3 million entries of individual, de-identified patient data. 

```{r, cache=TRUE}
# URL link to CDC to download data
url <- "https://data.cdc.gov/api/views/vbim-akqf/rows.csv?accessType=DOWNLOAD"

covid_data <- vroom::vroom(url)%>%
  clean_names()

```

Given the data we have, I would like you to produce two graphs that show death % rate:

```{r covid_challenge, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "covid_death_rate_comorbidities.png"), error = FALSE)
knitr::include_graphics(here::here("images", "covid_death_rate_icu.png"), error = FALSE)

```
1. by age group, sex, and whether the patient had co-morbidities or not
``` {r}
filtered_covid <-covid_data %>% 
  filter(death_yn!= "Unknown",
         death_yn!= "Missing",
         age_group!= "Unknown",
         sex!="Missing",
         sex!="Unknown",
         sex!="Other",
         sex!= "NA",
         medcond_yn!= "Missing",
         medcond_yn!= "Unknown",
         race_and_ethnicity_combined!="Unknown") %>%
  mutate(death_rate_count = case_when(death_yn == "Yes" ~ 1, TRUE ~ 0)) %>%
  mutate(comorbidities = case_when(medcond_yn == "Yes" ~ "With Comorbidities", TRUE ~ "Without Comorbidities")) %>%
  group_by(age_group, sex, comorbidities) %>%
  summarise(death_rate = mean(death_rate_count) * 100)

  filtered_covid %>%
  ggplot() +
  geom_bar(aes(death_rate, age_group), stat = "identity", fill = "#4863A0", alpha =0.9) +
  geom_text(aes(death_rate, age_group, label = round(death_rate, 1)), hjust = -0.1, size = 3) +
  facet_grid(comorbidities ~ sex) +
    labs(x = "",
         y = "",
         title = "Covid death % by age group, sex and presence of co-morbidities",
         caption = "Source: CDC") +
  theme_bw() +
  theme(plot.title = element_text(size = 11))+
  scale_x_continuous(labels = scales :: percent)
```
2. by age group, sex, and whether the patient was admitted to Intensive Care Unit (ICU) or not.

``` {r}
knitr::include_graphics(here::here("images", "covid_death_rate_icu.png"), error = FALSE)

filtered_covid_ICU <-covid_data %>% 
  filter(death_yn!= "Unknown",
         death_yn!= "Missing",
         age_group!= "Unknown",
         sex!="Missing",
         sex!="Unknown",
         sex!="Other",
         sex!= "NA",
         icu_yn!= "Missing",
         icu_yn!= "Unknown",
         race_and_ethnicity_combined!="Unknown") %>%
  mutate(death_rate_count = case_when(death_yn == "Yes" ~ 1, TRUE ~ 0)) %>%
  mutate(intensive_care = case_when(icu_yn == "Yes" ~ "Admitted to ICU", TRUE ~ "No ICU")) %>%
  group_by(age_group, sex, intensive_care) %>%
  summarise(death_rate = mean(death_rate_count) * 100)

filtered_covid_ICU %>%
  ggplot() +
  geom_bar(aes(death_rate, age_group), stat = "identity", fill = "#fc9272", alpha = 1) +
  geom_text(aes(death_rate, age_group, label = round(death_rate, 1)), hjust = -0.1, size = 3) +
  facet_grid(intensive_care ~ sex) +
    labs(x = "",
         y = "",
         title = "Covid death % by age group, sex and whether patient was admitted to ICU",
         caption = "Source: CDC") +
  theme_bw() +
  theme(plot.title = element_text(size = 11))+
  scale_x_continuous(labels = scales :: percent)
```


# Challenge 2: Excess rentals in TfL bike sharing

Recall the TfL data on how many bikes were hired every single day. We can get the latest data by running the following

```{r, get_tfl_data, cache=TRUE}

url <- "https://data.london.gov.uk/download/number-bicycle-hires/ac29363e-e0cb-47cc-a97a-e216d900a6b0/tfl-daily-cycle-hires.xlsx"

# Download TFL data to temporary file
httr::GET(url, write_disk(bike.temp <- tempfile(fileext = ".xlsx")))

# Use read_excel to read it as dataframe
bike0 <- read_excel(bike.temp,
                   sheet = "Data",
                   range = cell_cols("A:B"))

# change dates to get year, month, and week
bike <- bike0 %>% 
  clean_names() %>% 
  rename (bikes_hired = number_of_bicycle_hires) %>% 
  mutate (year = year(day),
          month = lubridate::month(day,label=TRUE),
          week = isoweek(day))
```

We can easily create a facet grid that plots bikes hired by month and year.

```{r tfl_month_year_grid, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "tfl_distributions_monthly.png"), error = FALSE)
```

Look at May and Jun and compare 2020 with the previous years. What's happening?
>In 2020 distribution is far less normal: there is a lot of variance in the data with much fatter tails and lower peak (covid-19 impact).

However, the challenge I want you to work on is to reproduce the following two graphs.

```{r tfl_absolute_monthly_change, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "tfl_monthly.png"), error = FALSE)
```

The second one looks at percentage changes from the expected level of weekly rentals. The two grey shaded rectangles correspond to the second (weeks 14-26) and fourth (weeks 40-52) quarters.

```{r tfl_percent_change, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "tfl_weekly.png"), error = FALSE)
```

For both of these graphs, you have to calculate the expected number of rentals per week or month between 2015-2019 and then, see how each week/month of 2020 compares to the expected rentals. Think of the calculation `excess_rentals = actual_rentals - expected_rentals`. 

Should you use the mean or the median to calculate your expected rentals? Why?

```{r}
library(scales)
bike2<-bike %>% 
  group_by(year,month) %>% 
  summarise(monthly_num=mean(bikes_hired)) %>% 
  ungroup()

bike3<-bike2%>% 
  filter(year>2014&year<2020) %>% 
  group_by(month) %>% 
  summarise(monthly_mean=mean(monthly_num)) %>% 
  ungroup()

bike4<-left_join(bike2,bike3,"month")%>% 
  mutate(posi=ifelse(monthly_num>monthly_mean,monthly_num,monthly_mean),
         nega=ifelse(monthly_num>monthly_mean,monthly_mean,monthly_num))%>% 
  filter(year>2014) 

```
>Mean is more precise for the expected rental value between 2015-2019 due to a more or less consistent distribution.

```{r, warning=FALSE}
bike_w<-bike %>% 
  group_by(year,week) %>% 
  summarise(weekly_num=mean(bikes_hired))%>% 
  ungroup()

bike_w2<-bike_w%>% 
  filter(year>2014&year<2020) %>% 
  group_by(week) %>% 
  summarise(weekly_mean=mean(weekly_num)) %>% 
  ungroup()

bike_w3<-left_join(bike_w,bike_w2,"week")%>% 
  mutate(diff=(weekly_num-weekly_mean)/weekly_mean,
        judge=weekly_num>weekly_mean,
        posi=ifelse(judge,diff,0),
         nega=ifelse(judge,0,diff))%>% 
  filter(year>2014) 
```


```{r, warning=FALSE}
bike4 %>% 
  ggplot(aes(x=month))+
  geom_ribbon(aes(ymin=nega, ymax=monthly_num,fill='red',group=0))+
  geom_ribbon(aes(ymin=monthly_mean, ymax=nega,fill='bluw',group=0))+
  geom_line(aes(y=monthly_num),group=1)+
  geom_line(aes(y=monthly_mean),colour='#0101DF',size=1,group=1)+
  facet_wrap(~year)+
  theme_minimal()+
  labs(title='Monthly changes in TfL bike rentals',
       subtitle='Change from monthly average shown in blue and calculated between 2015-2019',
       y = "Bike Rentals",
       x=element_blank(),
       caption='Source: TfL, London Data Store')+
  theme(legend.position ='none')

```

```{r, warning=FALSE}
ggplot(bike_w3,aes(x=week))+
  geom_area(aes(y=posi,fill='green'),outline.type='lower')+
  geom_area(aes(y=nega,fill='blue'),outline.type='lower')+
  geom_rect(aes(xmin=13, xmax=26, ymin=-Inf, ymax=Inf),fill='#545454',alpha = .005)+
  geom_rect(aes(xmin=39, xmax=52, ymin=-Inf, ymax=Inf),fill='#545454',alpha = .005)+
  geom_line(aes(y=diff))+
  facet_wrap(~year)+
  theme_minimal()+
  geom_rug(sides="b",aes(colour=judge)) +
  scale_y_continuous(labels = scales :: percent)+
  labs(title='Weekly changes in TfL bike rentals',
       subtitle='%changes from weekly averages calculated between 2015-2019',
       y=element_blank(),
       caption='Source: TfL, London Data Store')+
  scale_y_continuous(labels = percent,breaks=seq(-0.6,0.6,0.3))+
  scale_x_continuous(breaks=seq(0, 52, 13))+
  theme(legend.position="none")

```
# Details
- Approximately how much time did you spend on this problem set: 3 days
- What, if anything, gave you the most trouble: Under-5 Mortality from Gapminder question + getting the precise COVID-19 CDC