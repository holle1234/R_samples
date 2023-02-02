# credits to Dr Charlie Lanfear
# https://www.youtube.com/watch?v=pSWaOOniVBk


install.packages("gapminder")
install.packages("nycflights13")

library(gapminder)
library(dplyr)
library(nycflights13)

# view data using head (tail)
gapminder %>% 
  head(5)

# check summary
gapminder %>%
  summary()

# view data just calling its name
gapminder

# find data of Sweden Finland and Norway
sfn <- gapminder %>% 
  filter(country %in% c("Sweden", "Finland", "Norway"))

# keep only country, year and population
sfn_pop <- sfn %>% 
  select(country, year, pop)

# keep only rows where is year after 1960
sfn_pop <- sfn_pop %>% 
  filter(year>=1960)


# (1) set variables to uppercase just for fun
names(sfn_pop) <- sfn_pop %>% 
  names() %>% 
    toupper()

# (2) if you want to do this traditionally
names(sfn_pop) <- toupper(names(sfn_pop))

# rename POP to POPULATION rename(to, from)
sfn_pop <- sfn_pop %>% 
  rename(POPULATION=POP)


# use mutate() to show population in millions in a new col
sfn_pop <- sfn_pop %>% 
  mutate(POPM = POPULATION / 1E6)

# use ifelse() to show when pop exceeds 5 million
sfn_pop <- sfn_pop %>% 
  mutate(PAST_5M=ifelse(
    test=(POPM>=5),
    yes="T",
    no="F"))

# use case when to apply more conditions
sfn_pop <- sfn_pop %>% 
  mutate(RELPOP=
    case_when(
      POPM < 5 ~ "small",
      POPM > 5 & POPM < 7 ~ "medium",
      POPM > 7 ~ "large" # could also use TRUE ~ ...
    ))

# creating summaries the hard but much clearer way
sfn_summary <- sfn_pop %>% 
  filter(YEAR==1982) %>% 
  summarise(n_obs=n(),
            total_pop=sum(POPM),
            mean_pop=mean(POPM),
            min_pop=min(POPM),
            max_pop=max(POPM))

# creating summaries the easy way
sfn_summary <- sfn_pop %>% 
  filter(YEAR==1982) %>% 
  summarise_at(
    c(pm="POPM", pr="POPULATION"),
    tibble::lst(sum, mean, min, max, sd))


# creating summaries with group by
sfn_summary <- sfn_pop %>% 
  group_by(YEAR) %>% 
    summarise(n_countries=n_distinct(COUNTRY),
              totatl_pop=sum(POPM),
              avg_pop=mean(POPM)) 


# window functions mutate with group_by 
sfn_summary <- sfn_pop %>% 
  filter(YEAR>=1992) %>% 
  group_by(COUNTRY) %>% 
  mutate(lag_pop=lag(POPM, order_by=YEAR),
         pop_change=POPM - lag_pop)

print(sfn_summary)




# quick view of flight data
flights

# quick view of flight data
planes

# select tail numbers of planes flying to SeaTac
flights %>% filter(dest == "SEA") %>% select(tailnum) %>% 
  # select tailnum, manufacturer from planes and join with previous line
  left_join(planes %>% select(tailnum, manufacturer), by="tailnum") %>% 
  # we now have tibble with tails and manufactures
  # count occurrences and sort
  count(manufacturer, sort=T) %>% 
  head(7)





      
