# 1) Import library
library(tidyverse)  
library(haven)

# 2) Imported data from spss (ncrp3years) as "prison counts"
prison_counts <- read_sav("C:/Users/shaun/OneDrive/Documents/Toledo/Classes/Data Science 1/R/ncrp_3years_year_end.sav") %>% 
  
  # 3) Filter to only year 2010
  filter(RPTYEAR == 2010) %>% 
  
  # 4) All variables to lowercase
  rename_all(tolower) %>% 
  
  # 5) Year = rptyear
  mutate(year = as.integer(rptyear)) %>% 
  
  # 6) Create dummy variable, keep (year, dummy, state)
  mutate(inmate_dummy = 1) %>% 
  select(year, inmate_dummy, state) %>%

  # 7) Aggregate by state
  group_by(state) %>%
  summarize(inmate_dummy = sum(inmate_dummy))%>% 
  
  # 8) Rename inmate_dummy
  rename(inmate_count = inmate_dummy)
  
# 9) Import panel data, create year variable
census_data <- read_csv("C:/Users/shaun/OneDrive/Documents/Toledo/Classes/Data Science 1/R/census_panel.csv") %>% 
  mutate(year = as.integer(wave)) %>% 
  
  # 10) Merge 2010 state-level inmate count with census pop
  filter(year == 2010) %>% 
  rename_all(tolower) %>%
  select(state,pop) %>%
  
  # aggregate to state-level
  group_by(state) %>%
  summarize(pop = sum(pop))
  
  # Merge
  merged_data <- state_counts %>%
  inner_join(census_data, by = c("state", "year")) %>%

  # 11) Create incarceration rate
  mutate(incarceration_rate = (inmate_count / total_pop) * 100000)
  
  # 12) Sort by incarceration rate
  arrange(desc(incraceration_rate))
  
  # 13) Print 
  
  

  
  