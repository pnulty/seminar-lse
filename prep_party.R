library(shiny)
library(feather)
library(dplyr)
library(magrittr)


rels <- read_feather('corpora/uk_relations.feather') %>% rename(sentenceid = sent_id)
full <- read_feather('corpora/all_uk.feather')
md <- select(full, sentenceid, manifestoid, year, party, econMeans, socialMeans, gold_scale) 
comb <- inner_join(rels, md, by='sentenceid')
by_party <- select(comb, relation, arg1, arg2, party)
party_totals <- group_by(by_party, party) %>% summarise(n = n()) 

rel_types <- c('adjective', 'conjunction', 'subject', 'object')

rel_party_totals <- group_by(by_party, arg1, arg2, relation, party) %>% summarise(Freq=n()) 
rel_party_totals <- filter(rel_party_totals, relation %in% rel_types) %>%
  filter(grepl("^[[:alpha:]]*$", relation)) %>%
  filter(grepl("^[[:alpha:]]*$", arg1)) %>%
  filter(grepl("^[[:alpha:]]*$", arg2)) %>%
  filter(grepl("^[[:digit:]]*$", Freq))




arg1_party_freq <- rel_party_totals %>% group_by(arg1, relation, party) %>% summarize(Freq = sum(Freq)) %>% ungroup
arg2_party_freq <- rel_party_totals %>% group_by(arg2, relation, party) %>% summarize(Freq = sum(Freq)) %>% ungroup

write_feather(party_totals, path='corpora/uk_party_totals.feather')
write_feather(rel_party_totals, path='corpora/uk_rel_party_totals.feather')


write_feather(arg1_party_freq, path='corpora/uk_arg1_party_freq.feather')
write_feather(arg2_party_freq, path='corpora/uk_arg2_party_freq.feather')

