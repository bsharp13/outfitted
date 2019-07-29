
library(magrittr)
library(tidyverse)
library(gmailr)

#-------------------------------------------------------------------------------

# Read in data
#-------------------------------------------------------------------------------
shirt <- read_csv('Shirts.csv')
pants <- read_csv('Pants.csv')

outfit <- shirt %>%  
  gather(key = 'Pants', value = 'Match', -(ID:Liking)) %>% 
  filter(Match == 1) %>% 
  select(-Match) %>% 
  left_join(pants, by = c('Pants' = 'Description'), suffix = c('_S', '_P')) %>% 
  filter(!is.na(ID_P))

#-------------------------------------------------------------------------------

# Randomly generate
#-------------------------------------------------------------------------------

#FIXME: Pull in weather

outfit %>% 
  mutate(
    #DaysSince = today() - LastWorn_S,
    Priority = 1
  ) %>% 
  arrange(sample(1:n()), desc(Priority)) %>% 

  select(Description, Pants)

#-------------------------------------------------------------------------------

# Send email
#-------------------------------------------------------------------------------



