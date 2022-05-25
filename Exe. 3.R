library(ggraph)
library(igraph)

library(arrow)
library(tidyverse)
library(gender)
library(wru)
library(lubridate)

library(ggplot2)
library(gridExtra)
library(grid)

library("gender")


app <- read_parquet('/Users/danystefan/Documents/01 McGill University/01 MMA/01 Summer 2022/ORGB 672/Assignments/Exe. 3/672_project_data/app_data_sample.parquet')
edges <- read_csv('/Users/danystefan/Documents/01 McGill University/01 MMA/01 Summer 2022/ORGB 672/Assignments/Exe. 3/672_project_data/edges_sample.csv')

# first name
names <- app %>% distinct(examiner_name_first)
# names and gender
names_gender <- names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )
names_gender <- names_gender %>% select(examiner_name_first, gender)

# join
app <- app %>% left_join(names_gender, by='examiner_name_first')


# last names
sur <- app %>% select(surname = examiner_name_last) %>% distinct()

race <- predict_race(voter.file = sur, surname.only = T) %>% as_tibble()
race <- race %>%
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))

# cleanup
race <- race %>% select(surname, race)

#join
app <- app %>% left_join(race, by=c("examiner_name_last" = "surname"))


# get dates
dates <- app %>% select(examiner_id, filing_date, appl_status_date)
# calculate start and end date
dates <- dates %>% mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date)))

dates <- dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
  ) %>% 
  filter(year(latest_date)<2018)

# join
app <- app %>% left_join(dates, by="examiner_id")


# get 2 work groups
w1 = app[substr(app$examiner_art_unit, 1, 3) == 163,]
w2 = app[substr(app$examiner_art_unit, 1, 3) == 177,]


# distributions
a <- ggplot(data=w1, aes(x=gender)) +geom_bar(aes(y = (..count..)/sum(..count..)) )  +ylab("Proportion")+xlab("Gender")+ylim(0,1)+ggtitle(paste0("Gender distribution for wg 163"))

b <- ggplot(data=w2, aes(x=gender)) +geom_bar(aes(y = (..count..)/sum(..count..)) )  +ylab("Proportion")+xlab("Gender")+ylim(0,1)+ggtitle(paste0("Gender distribution for wg 177"))

grid.arrange(a,b,ncol=2, widths=c(1,1))
