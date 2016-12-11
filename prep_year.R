library(shiny)
library(feather)
library(dplyr)
library(magrittr)

rels <- read_feather('corpora/uk_relations.feather') %>% rename(sentenceid = sent_id)
full <- read_feather('corpora/all_uk.feather')
md <- select(full, sentenceid, manifestoid, year, party, econMeans, socialMeans, gold_scale) 
comb <- inner_join(rels, md, by='sentenceid')
by_year <- select(comb, relation, arg1, arg2, year)
year_totals <- group_by(by_year, year) %>% summarise(n = n()) 

rel_types <- c('adjective', 'conjunction', 'subject', 'object')

rel_year_totals <- group_by(by_year, arg1, arg2, relation, year) %>% summarise(Freq=n()) 
rel_year_totals <- filter(rel_year_totals, relation %in% rel_types) %>%
  filter(grepl("^[[:alpha:]]*$", relation)) %>%
  filter(grepl("^[[:alpha:]]*$", arg1)) %>%
  filter(grepl("^[[:alpha:]]*$", arg2)) %>%
  filter(grepl("^[[:digit:]]*$", Freq))

rel_totals <- group_by(by_year, arg1, arg2, relation) %>% summarise(Freq=sum(n())) %>% filter(relation %in% rel_types)
rel_totals <- filter(rel_totals, relation %in% rel_types) %>%
  filter(grepl("^[[:alpha:]]*$", relation)) %>%
  filter(grepl("^[[:alpha:]]*$", arg1)) %>%
  filter(grepl("^[[:alpha:]]*$", arg2)) %>%
  filter(grepl("^[[:digit:]]*$", Freq)) 

arg1_totals<- rel_year_totals %>% group_by(arg1, relation) %>% summarize(Freq = sum(Freq)) %>% ungroup
arg2_totals <- rel_year_totals %>% group_by(arg2, relation) %>% summarize(Freq = sum(Freq)) %>% ungroup

arg1_year_freq <- rel_year_totals %>% group_by(arg1, relation, year) %>% summarize(Freq = sum(Freq)) %>% ungroup
arg2_year_freq <- rel_year_totals %>% group_by(arg2, relation, year) %>% summarize(Freq = sum(Freq)) %>% ungroup

write_feather(year_totals, path='corpora/uk_year_totals.feather')
write_feather(rel_year_totals, path='corpora/uk_rel_year_totals.feather')

write_feather(arg1_totals, path='corpora/uk_arg1_totals.feather')
write_feather(arg2_totals, path='corpora/uk_arg2_totals.feather')

write_feather(arg1_year_freq, path='corpora/uk_arg1_year_freq.feather')
write_feather(arg2_year_freq, path='corpora/uk_arg2_year_freq.feather')

