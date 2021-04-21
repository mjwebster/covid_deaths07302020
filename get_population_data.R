# this gets single-year-of-age data from the Decennial census
# can either get total population by age 
# or race/ethnic groups by age

#at the end, it creates weightings for age adjusting as well



library(tidycensus)#for getting census data via API
library(tidyverse)
library(janitor)
library(stringr)
library(purrr)#this is installed as part of tidyverse
options(scipen=999)

#library(devtools)

#install_github("walkerke/tidycensus")


#retrieve census api key from my Renviron file
census_api_key(Sys.getenv("CENSUS_API_KEY"))


#load the variable list from census
variable_list <- load_variables("2010", "sf1",  cache = FALSE)


#pull from the variable list the specific ones I want (from single-year-by age, with race breakdowns)
save_variables <-  variable_list %>% filter(str_sub(name, 1,6)== 'PCT012') %>% pull(name)

#pull the data using get_decennial function from tidycensus
#us_age_singleyr <-  get_decennial(geography = "us", variables = save_variables, year = "2010")
mn_age_singleyr <-  get_decennial(geography = "state", state="MN", variables = save_variables, year = "2010")

#To re-use this script without having to pull from API again, save the data as csv files
#and then use code below to import the data
#save_variables <-  variable_list %>% filter(grepl("PCT012", name))
#write.csv(save_variables, './data/decennial_sex_by_age_variables.csv', row.names=FALSE)
#write.csv(mn_age_singleyr, './data/mn_age_singleyr.csv', row.names=FALSE)
#write.csv(us_age_singleyr, './data/us_age_singleyr.csv', row.names=FALSE)

####IMPORT SAVED FILES INSTEAD -----############################
#save_variables <-  read_csv('./data/decennial_sex_by_age_variables.csv')
#us_age_singleyr <-  read_csv('./data/us_age_singleyr.csv')



# choose geography --------------------------------------------------------


#choose whether you want to use the US data or the MN data and set that here
age_singleyr <- mn_age_singleyr



#this will create a new dataframe for the remainder of the script
age_singleyr2 <-  left_join(age_singleyr, variable_list, by=c("variable"="name")) %>%  #join with variables
  clean_names()%>%  #clean the headers
  separate("label", sep="!!", c("grp", "gender", "age"))%>%  #separate the label column
  filter(age!='NA', gender!='NA') %>%   #eliminate the total rows that we don't want to keep   
  mutate(age2 = case_when(age=='Under 1 year' ~ 0,
                          age %in% c('100 to 104 years', '105 to 109 years', '110 years and over') ~ 100,
                          age=='1 year'~ 1,
                          str_detect(age,"years") ~ as.numeric(str_replace(age, " years", ""))))






#New age groups for COVID tracker, as of Aug 20, 2020
#this is 2010 single year by age data; newer data is not available
# the filter for concept=sex by age is to filter out the race tables that are also in the original table

covid_age_groups <-   age_singleyr2 %>% filter(age2!='NA', concept=='SEX BY AGE') %>% 
  mutate(agegroup = case_when(age2<=4 ~ '0-4',
         age2>=5 & age2 <=9 ~ '5-9',
         age2>=10 & age2 <=14 ~ '10-14',
         age2>=15 & age2 <=19 ~ '15-19',
         age2>=20 & age2<=29 ~ '20-29',
         age2>=30 & age2<=39 ~ '30-39',
         age2>=40 & age2<=49 ~ '40-49',
         age2>=50 & age2<=59 ~ '50-59',
         age2>=60 & age2<=69 ~ '60-69',
         age2>=70 & age2<=79 ~ '70-79',
         age2>=80 & age2<=89 ~ '80-89',
         age2>=90 & age2<=99 ~ '90-99',
         age2==100 ~ '100+')) %>% 
  group_by(agegroup) %>% 
  summarise(totpeople = sum(value)) %>% 
  mutate(pct_pop = totpeople/sum(totpeople))  #this pct_pop column would be the weight used to apply age-adjusted weighting


#write.csv(covid_age_groups, 'covid_age_groups.csv', row.names=FALSE)


# create age by race/ethnicity table
race_age <-  age_singleyr2 %>%
  filter(str_sub(variable,1,7) %in% c("PCT012H", "PCT012I", "PCT012J","PCT012K","PCT012L","PCT012M","PCT012N","PCT012O" )) %>%
  mutate(race_group = str_sub(concept, 12,200),
         race_group = str_replace(race_group, '\\(', ''),
         race_group = str_replace(race_group, '\\)', ''),
         big_group = case_when(race_group=='WHITE ALONE, NOT HISPANIC OR LATINO' ~ 'WHITE',
                               TRUE ~ 'POC'),
         race_ethnicity = case_when(str_detect(race_group, "INDIAN")~ 'American Indian',
                                    str_detect(race_group, "ASIAN") ~'Asian',
                                    str_detect(race_group, 'BLACK')~ 'Black',
                                    race_group=='HISPANIC OR LATINO'~ 'Hispanic',
                                    str_detect(race_group, 'NATIVE HAWAIIAN')~ 'Pacific Islander',
                                    str_detect(race_group, 'SOME OTHER')~ 'Other',
                                    str_detect(race_group, 'TWO OR MORE')~ 'Multi',
                                    str_detect(race_group, 'WHITE ALONE')~'White'))


#summarize to collapse the genders
# add weighting for age adjusting

race_age <-  race_age %>% 
  group_by(race_ethnicity, big_group, age2) %>% 
  summarize(pop = sum(value)) 

#export as csv -- single year of age by race/ethnicity (both genders combined)
write.csv(race_age, './data/mn_race_single_yr_age.csv', row.names=FALSE)





# apply age groupings to race_age table

# adjust groupings as needed below
race_age <-  race_age %>% mutate(agegroup = case_when(age2<=4 ~ '0-4',
                                                                           age2>=5 & age2 <=9 ~ '5-9',
                                                                           age2>=10 & age2 <=14 ~ '10-14',
                                                                           age2>=15 & age2 <=19 ~ '15-19',
                                                                           age2>=20 & age2<=29 ~ '20-29',
                                                                           age2>=30 & age2<=39 ~ '30-39',
                                                                           age2>=40 & age2<=49 ~ '40-49',
                                                                           age2>=50 & age2<=59 ~ '50-59',
                                                                           age2>=60 & age2<=69 ~ '60-69',
                                                                           age2>=70 & age2<=79 ~ '70-79',
                                                                           age2>=80 & age2<=89 ~ '80-89',
                                                                           age2>=90 & age2<=99 ~ '90-99',
                                                                           age2==100 ~ '100+'))


# then summarize the table to get data by race and age group

#create weightings for age adjusting


#this generates a weight for each race/age group across whole population
# (i.e. what percentage the 20-29 white people are within the whole population)
race_by_agegroup <-  race_age %>% 
  group_by(race_ethnicity,  agegroup) %>% 
  summarise(population = sum(pop)) %>% 
  ungroup() %>%  #need to ungroup in order to calculate the correct weighting
mutate(weight = population/sum(population)) 



#weights within each race group (i.e. what percentage 20-29 year olds are within the white population)

white <-  race_by_agegroup %>% 
  filter(race_ethnicity=='White') %>% 
  group_by(race_ethnicity,agegroup) %>% 
  summarise(pop = sum(population)) %>% 
  mutate(weight = pop/sum(pop))

black <-  race_by_agegroup %>% 
  filter(race_ethnicity=='Black') %>% 
  group_by(race_ethnicity,agegroup) %>% 
  summarise(pop = sum(population)) %>% 
  mutate(weight = pop/sum(pop))

asian <-  race_by_agegroup %>% 
  filter(race_ethnicity=='Asian') %>% 
  group_by(race_ethnicity,agegroup) %>% 
  summarise(pop = sum(population)) %>% 
  mutate(weight = pop/sum(pop))

americanindian <-  race_by_agegroup %>% 
  filter(race_ethnicity=='American Indian') %>% 
  group_by(race_ethnicity,agegroup) %>% 
  summarise(pop = sum(population)) %>% 
  mutate(weight = pop/sum(pop))

hispanic <-  race_by_agegroup %>% 
  filter(race_ethnicity=='Hispanic') %>% 
  group_by(race_ethnicity,agegroup) %>% 
  summarise(pop = sum(population)) %>% 
  mutate(weight = pop/sum(pop))

pacislander  <-  race_by_agegroup %>% 
  filter(race_ethnicity=='Pacific Islander') %>% 
  group_by(race_ethnicity,agegroup) %>% 
  summarise(pop = sum(population)) %>% 
  mutate(weight = pop/sum(pop))

#combine them into one table
weights_by_race_group <-  rbind(white, black, asian, americanindian, hispanic, pacislander)


