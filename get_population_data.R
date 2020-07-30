
library(tidycensus)#for getting census data via API
library(tidyverse)
library(janitor)
library(stringr)
library(purrr)#this is installed as part of tidyverse
options(scipen=999)




census_api_key(Sys.getenv("CENSUS_API_KEY"))


#variable_list <- load_variables("2010", "sf1",  cache = FALSE)

#save_variables <-  variable_list %>% filter(grepl("PCT012", name))
#write.csv(save_variables, './data/decennial_sex_by_age_variables.csv', row.names=FALSE)

#my_vars <-  variable_list %>% filter(str_detect(name, "^PCT012")) %>% pull(name)
#age_singleyr <-  get_decennial(geography = "state", variables = my_vars, year = "2010", state="MN")

#write.csv(age_singleyr, './data/age_singleyr.csv', row.names=FALSE)


####IMPORT SAVED FILES INSTEAD -----############################
save_variables <-  read_csv('./data/decennial_sex_by_age_variables.csv')
age_singleyr <-  read_csv('./data/age_singleyr.csv')






age_singleyr2 <-  left_join(age_singleyr, save_variables, by=c("variable"="name")) %>% clean_names()

age_singleyr2 <-  age_singleyr2 %>% separate("label", sep="!!", c("grp", "gender", "age"))


age_singleyr2 <-  age_singleyr2 %>% mutate(age2 = as.numeric(str_replace(age, " years", "")))

age_singleyr2$age2[age_singleyr2$age=='Under 1 year'] <- 0
age_singleyr2$age2[age_singleyr2$age=='1 year'] <- 1
age_singleyr2$age2[age_singleyr2$age=='100 to 104 years'] <- 100
age_singleyr2$age2[age_singleyr2$age=='105 to 109 years'] <- 100
age_singleyr2$age2[age_singleyr2$age=='110 years and over'] <- 100



age_singleyr2 %>% filter(is.na(age2), gender!='NA')

age_singleyr2 <-  age_singleyr2 %>% mutate(agegroup = case_when(age2<6 ~ '0-5',
                                                                age2>=6 & age2 <=19 ~ '6-19',
                                                                age2>=20 & age2<=29 ~ '20-29',
                                                                age2>=30 & age2<=39 ~ '30-39',
                                                                age2>=40 & age2<=49 ~ '40-49',
                                                                age2>=50 & age2<=59 ~ '50-59',
                                                                age2>=60 & age2<=69 ~ '60-69',
                                                                age2>=70 & age2<=79 ~ '70-79',
                                                                age2>=80 & age2<=89 ~ '80-89',
                                                                age2>=90 & age2<=99 ~ '90-99',
                                                                age2==100 ~ '100+'),
                                           alt_age_group = case_when(age2>=85 ~ '85+',
                                                                     age2>=65 & age2<85 ~ '65-84',
                                                                     age2<65 & age>=30 ~ '30-64'))



agegroups_allraces <- age_singleyr2 %>% filter(concept=='SEX BY AGE', age!='NA') %>% 
  group_by(agegroup) %>% summarise(totpop = sum(value)) %>% mutate(pct = totpop/sum(totpop))

#write.csv(agegroups_allraces, 'agegroups_allraces_2010.csv', row.names=FALSE)


#age_singleyr2 %>% filter(is.na(agegroup), concept=='SEX BY AGE')

#age_singleyr2 %>% count(concept)

age_singleyr2 <-  age_singleyr2 %>% mutate(race_group = str_sub(age_singleyr2$concept, 12,200))

age_singleyr2 <-  age_singleyr2 %>% mutate(race_ethnicity_flag = case_when(race_group=='(HISPANIC OR LATINO)'~'Y',
                                                                           grepl("NOT HISPANIC OR LATINO", race_group)~'Y',
                                                                           TRUE ~'N'))




# first version of age by race/ethnicity

age_by_race_eth <-  age_singleyr2 %>% filter(race_ethnicity_flag=='Y', age!='NA') %>% group_by(agegroup, race_group) %>% summarise(totpop = sum(value)) %>% mutate(pct = totpop/sum(totpop))

age_by_race_eth <-  age_by_race_eth %>% mutate(big_group = case_when(race_group=='(WHITE ALONE, NOT HISPANIC OR LATINO)' ~ 'white',
                                                                     TRUE ~ 'poc'))


age_by_race_eth <-  age_by_race_eth %>% mutate(big_age_group =case_when(agegroup %in% c("50-59", "60-69", "70-79", "80-89", "90-99", "100+") ~ '50+',
                                                                        agegroup %in% c("0-5", "6-19")~'Under 20',
                                                                        agegroup %in% c("20-29", "30-39", "40-49")~'20-49'))

big_group_10yrage <-  age_by_race_eth %>% 
  group_by(agegroup, big_group) %>% 
  summarise(pop=sum(totpop))



## second version

alt_age_by_race_eth <-  age_singleyr2 %>% filter(race_ethnicity_flag=='Y', age!='NA') %>%
  group_by(alt_age_group, race_group) %>% 
  summarise(totpop = sum(value))  %>% mutate(pct = totpop/sum(totpop)) 

alt_age_by_race_eth <-  alt_age_by_race_eth %>% 
  mutate(big_group = case_when(race_group=='(WHITE ALONE, NOT HISPANIC OR LATINO)' ~ 'white',
                                                                     TRUE ~ 'poc'))

alt_age_by_race_export <-  alt_age_by_race_eth %>% group_by(big_group, alt_age_group) %>% summarise(totpop = sum(totpop))

alt_age_by_race_export <-  pivot_wider(alt_age_by_race_export, names_from=big_group, values_from=totpop)

alt_age_by_race_export <-  alt_age_by_race_export %>% mutate(pct = poc/(poc+white))

big_group_10yrage <-  pivot_wider(big_group_10yrage, names_from=big_group, values_from=pop)

big_group_10yrage <- big_group_10yrage %>% mutate(pct = poc/(poc+white))


#write.csv(big_group_10yrage, './data/big_group_1yrage.csv', row.names=FALSE)

age_singleyr2 %>% filter(concept=='SEX BY AGE', age2!='NA') %>% summarise(tot=sum(value))
