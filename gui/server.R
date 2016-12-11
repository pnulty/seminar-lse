library(dplyr)
library(shiny)
library(networkD3)
library(visNetwork)
library(igraph)
library(feather)
setwd("~/Dropbox/methodologySeminar")
year_base <- read_feather('corpora/uk_year_totals.feather')
rel_year_totals <- read_feather('corpora/uk_rel_year_totals.feather') 
arg1_year_freq <- read_feather('corpora/uk_arg1_year_freq.feather') 
arg2_year_freq <- read_feather('corpora/uk_arg2_year_freq.feather')
rel_party_totals <- read_feather('corpora/uk_rel_party_totals.feather') 
arg1_party_freq <- read_feather('corpora/uk_arg1_party_freq.feather') 
arg2_party_freq <- read_feather('corpora/uk_arg2_party_freq.feather') 


options(scipen=10)
getExact <- function(df, a1, a2, either){
  print("called get")
  print(either)
  df <- filter(df, grepl(either,arg1) | grepl(either, arg2))
  df <- filter(df, grepl(a1, arg1) & grepl(a2, arg2))
  print(nrow(df))
  return(df)
}

getNetworkVis <- function(networkData, threshold){
  networkData <- filter(networkData, Freq > threshold)
  validate(
    need(nrow(networkData) < 500, paste( 'There are ', nrow(networkData), ' results, up to 500 may be visualised.') )
  )
  validate(
    need(nrow(networkData) > 0, 'There are no results.')
  ) 
  ig <- graph.data.frame(d= networkData, directed = TRUE)
  isolates <- which(degree(ig, mode = 'total') == 0)
  tmp <- delete.vertices(ig, isolates)
  tmp <- igraph_to_networkD3(tmp)
  links <- tmp$links
  nodes <- tmp$nodes
  nodes$group <- 1
  ids <- as.numeric(row.names(nodes))
  vnodes <- data.frame(id= ids-1, label = nodes$name)
  vedges <- data.frame(from = links$source, to = links$target)
  edgeColors <- as.character(networkData$relation)
  edgeColors <- gsub('conjunction', 'Black',edgeColors)
  edgeColors <- gsub('adjective', 'Gold',edgeColors)
  edgeColors <- gsub('subject', 'Green',edgeColors)
  edgeColors <- gsub('object', 'Red',edgeColors)
  vedges$value <- log(networkData$Freq)
  vedges$color <- edgeColors
  vn <- visNetwork(vnodes, vedges) %>% visEdges(smooth = list(enabled = FALSE)) 
  return(vn)
}

sum_years <- function(allResults, start, end){
  print("called sum_years")
  allResults <- filter(allResults, year >= start & year <= end) 
  # sum the counts by year
  by_year <- group_by(allResults, arg1, arg2, relation)
  by_year_sum <- summarise(by_year, Freq=sum(Freq)) 
  tmp_arg1_base <- filter(arg1_year_freq,  year >= start & year <= end) %>% group_by(arg1, relation) %>% summarise(Freq = sum(Freq))
  tmp_arg2_base <- filter(arg2_year_freq,  year >= start & year <= end) %>% group_by(arg2, relation) %>% summarise(Freq = sum(Freq))
  tmp <- left_join(by_year_sum, tmp_arg1_base, by = c('arg1', 'relation'))
  tmp <- left_join(tmp, tmp_arg2_base, by = c('arg2', 'relation'))
  hNorm = tmp$Freq
  dNorm = tmp$Freq.y
  # sum of logs to avoid integer overflow
  by_year_sum$normScore <- by_year_sum$Freq / (exp(log(hNorm)+  log(dNorm)) * 0.78)
#  base <- filter(year_base, year >= start & year <= end)
#  by_year_sum$normFreq <- by_year_sum$Freq/sum(base$n)
  return(by_year_sum)
}


sum_parties <- function(allResults, this_p){
  print("called sum_parties")
  print(names(allResults))
  allResults <- filter(allResults, party==this_p) 
  # sum the counts by year
  by_party <- group_by(allResults, arg1, arg2, relation)
  by_party_sum <- summarise(by_party, Freq=sum(Freq)) 
  tmp_arg1_base <- filter(arg1_party_freq,  party==this_p) %>% group_by(arg1, relation) %>% summarise(Freq = sum(Freq))
  tmp_arg2_base <- filter(arg2_party_freq,  party==this_p) %>% group_by(arg2, relation) %>% summarise(Freq = sum(Freq))
  tmp <- left_join(by_party_sum, tmp_arg1_base, by = c('arg1', 'relation'))
  tmp <- left_join(tmp, tmp_arg2_base, by = c('arg2', 'relation'))
  hNorm = tmp$Freq
  dNorm = tmp$Freq.y
  # sum of logs to avoid integer overflow
  by_party_sum$normScore <- by_party_sum$Freq / (exp(log(hNorm)+  log(dNorm)) * 0.78)
  #  base <- filter(year_base, year >= start & year <= end)
  #  by_year_sum$normFreq <- by_year_sum$Freq/sum(base$n)
  return(by_party_sum)
}

shinyServer(function(input, output, session) {
  output$fuzzyTable <- renderDataTable({sum_years(rel_year_totals,input$years[1],input$years[2])}, searchDelay = 800)
  output$fuzzyPartyTable <- renderDataTable({sum_parties(rel_party_totals,input$fuzzyPartySelect)}, searchDelay = 800)
  exactTerms <- eventReactive(input$showExact, {c(input$exactArg1, input$exactArg2, input$exactEither)  })
  exactPartyTerms <- eventReactive(input$showExactParty, {c(input$exactPartyArg1, input$exactPartyArg2, input$exactPartyEither)  })
  output$exactTable <- renderDataTable({
    exactArgs <- exactTerms()
    tmp <- getExact(rel_year_totals, exactArgs[1], exactArgs[2], exactArgs[3])
    sum_years(tmp, input$exactYears[1],input$exactYears[2] )
  })
  
  output$exactTableParty <- renderDataTable({
    exactPartyArgs <- exactPartyTerms()
    tmp <- getExact(rel_party_totals, exactPartyArgs[1], exactPartyArgs[2], exactPartyArgs[3])
    sum_parties(tmp, input$exactParty)
  })
  
  networkTerms <- eventReactive(input$showNet, {c(input$networkArg1Vis, input$networkArg2Vis, input$networkEitherVis)  })
  networkTermsParty <- eventReactive(input$showNetParty, {c(input$networkArg1VisParty, input$networkArg2VisParty, input$networkEitherVisParty)  })
  output$networkVis <- renderVisNetwork({
    args <- networkTerms()
    tmp <- getExact(rel_year_totals, args[1], args[2], args[3])
    tmp <- sum_years(tmp, input$networkYears[1],input$networkYears[2])
    getNetworkVis(tmp, input$scoreThreshold)
  })
  output$networkVisParty <- renderVisNetwork({
    args <- networkTermsParty()
    tmp <- getExact(rel_party_totals, args[1], args[2], args[3])
    tmp <- sum_parties(tmp, input$networkPartySelect)
    print('in network vis party')
    getNetworkVis(tmp, input$scoreThresholdParty)
  })
  
  
})
