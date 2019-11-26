
library(magrittr)
library(tidyverse)
library(gmailr)
library(htmlTable)

#-------------------------------------------------------------------------------

# Read in data
#-------------------------------------------------------------------------------
shirt <- read_csv('Shirts.csv')
pants <- read_csv('Pants.csv')
temp  <- read_csv('Climate.csv')

# TEMP - Remove color options
shirt %<>% select(ID:Pattern, Weight:DarkShort) 
pants %<>% select(-Color)

# Add columns for other pairs of jeans
shirt %<>%  
  mutate(Jeans1 = Jeans, Jeans2 = Jeans, Jeans3 = Jeans) %>% 
  select(-Jeans)

outfit <- shirt %>% 
  gather(key = 'Pants', value = 'Match', -(ID:Liking)) %>% 
  filter(Match == 1) %>% 
  select(-Match) %>% 
  inner_join(pants, by = c('Pants' = 'Description'), suffix = c('_S', '_P'))

#-------------------------------------------------------------------------------

# Limit options 
#-------------------------------------------------------------------------------
high <- temp %>% filter(week(Date) == week(today())) %>% pull(High) %>% max()
low  <- temp %>% filter(week(Date) == week(today())) %>% pull(Low) %>% min()

if (low < 50) {
  outfit %<>% filter(Shorts == 0)
}

#-------------------------------------------------------------------------------

# Pull in history
#-------------------------------------------------------------------------------
hist   <- read_csv('History.csv')

# TEMP - Convert to date until actual history is being recorded
hist %<>% mutate(Date = as.Date(Date))

hist_s <- hist %>% group_by(Shirt) %>% summarize(LastWorn_S = max(Date))
hist_p <- hist %>% group_by(Pants) %>% summarize(LastWorn_P = max(Date))

outfit %<>% 
  left_join(hist_s, by = c('ID_S' = 'Shirt')) %>% 
  left_join(hist_p, by = c('ID_P' = 'Pants')) %>% 
  replace_na(list(
    LastWorn_S = as.Date('2000-01-01'), LastWorn_P = as.Date('2000-01-01')
  ))

#-------------------------------------------------------------------------------

# Define probability distribution and generate
#-------------------------------------------------------------------------------
result <- outfit %>% 
  mutate(Priority = 1) %>% 
  slice(sample(1:n())) %>% 
  arrange(desc(Priority)) %>% 
  select(Description, Pants) %>% 
  group_by(Description) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  head(7)

tabl <- htmlTable(
  result,
  header = c('Shirt', 'Pants')
)

#-------------------------------------------------------------------------------

# Send email
#-------------------------------------------------------------------------------
mime() %>% 
  to('braden.sharp.13@gmail.com') %>% 
  from('braden.sharp.13@gmail.com') %>% 
  html_body(tabl) %>% 
  send_message()
