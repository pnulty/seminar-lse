library(shiny)
library(feather)
library(dplyr)
library(tidyr)
library(magrittr)

rels <- read_feather('corpora/uk_relations.feather') %>% rename(sentenceid = sent_id)
feats <- read_feather('corpora/uk_features.feather') %>% rename(sentenceid = sent_id)
full <- read_feather('corpora/all_uk.feather')
md <- select(full, sentenceid, manifestoid, year, party, econMeans, socialMeans, gold_scale) %>%
  filter(party == 'Conservatives' | party == 'Labour', gold_scale != 'Neither')
comb <- inner_join(feats, md, by='sentenceid')
topfeats <- group_by(comb, feature) %>% summarize(Freq=n()) %>% arrange(desc(Freq)) %>% filter(Freq > 2)
feats <- select(comb, feature, econMeans, sentenceid) %>% filter(feature %in% topfeats$feature)

tmp <- group_by(feats, sentenceid, feature) %>% summarize(fe=n())
t2 <- spread(tmp, feature, fe, fill=0)

tmpmd <- select(md, econMeans, sentenceid)
trdt <- inner_join(t2, tmpmd, by='sentenceid') %>% ungroup %>% select(-sentenceid)%>% filter(!is.nan(econMeans))
trdtbin <- mutate(trdt, bin = ifelse(econMeans > mean(econMeans), 1, 0))
target <- trdtbin$bin
trdtbin <- select(trdtbin, -bin, -econMeans)
library("glmnet")

thisMod <- cv.glmnet(as.matrix(trdtbin), y = target, family = "binomial",
                     type.measure = "class", standardize = TRUE)

acc <- as.character(round(min(thisMod$cvm), 3))
cmat <- as.matrix(coef(thisMod, s = "lambda.min"))
coes <- data.frame(estimate = cmat[, 1], words = row.names(cmat)) %>% arrange(estimate)
coes <- data.frame(estimate = cmat[, 1], words = row.names(cmat)) %>% arrange(desc(estimate))
wordWeights <- data.frame(word = coes$words, co = coes[1]) %>% filter(estimate!=0) %>% filter(word != "(Intercept)")


###########################
trdt <- inner_join(t2, tmpmd, by='sentenceid') %>% ungroup %>% select(-sentenceid)%>% filter(!is.nan(econMeans))

target <- trdt$econMeans
trdt <- select(trdt, -econMeans)

thisMod <- cv.glmnet(as.matrix(trdt), y = target, family = "gaussian", alpha=.05,
                     type.measure = "mae", standardize = TRUE)

acc <- as.character(round(min(thisMod$cvm), 3))
cmat <- as.matrix(coef(thisMod, s = "lambda.min"))
coes <- data.frame(estimate = cmat[, 1], words = row.names(cmat)) %>% arrange(estimate)
coes <- data.frame(estimate = cmat[, 1], words = row.names(cmat)) %>% arrange(desc(estimate))
wordWeights <- data.frame(word = coes$words, co = coes[1]) %>% filter(estimate!=0) %>% filter(word != "(Intercept)")



###########################

party_totals <- group_by(comb, feature, party) %>% summarise(Freq = n()) %>% spread(party, Freq) %>% na.omit
party_odds <- mutate(party_totals, odds = log(Labour/Conservatives)) %>% arrange(desc(odds))

scale_totals <- group_by(comb, feature, gold_scale) %>% summarise(Freq = n()) %>% spread(gold_scale, Freq) %>% na.omit
scale_odds <- mutate(scale_totals, odds = (Economic/Social)) %>% arrange(desc(odds))

scale_totals <- group_by(comb, feature, econMeans) %>% summarise(Freq = sum(log(econMeans+3)))

write_feather(wordWeights, 'wordWeightsGaussian.feather')

