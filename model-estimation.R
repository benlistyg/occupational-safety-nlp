library(vroom)
library(stm)
library(tm)
library(dplyr)
library(stringr)

data = vroom::vroom('January2015toFebruary2023.csv')

data = data %>% 
  janitor::clean_names() %>% 
  select(event_date, state, primary_naics, final_narrative) %>% 
  mutate(event_date = lubridate::mdy(event_date), 
         primary_naics = factor(primary_naics),
         state = factor(state)) %>% 
  filter(event_date >= lubridate::ymd('2019-01-01')) %>% 
  mutate(event_date = as.numeric(event_date))

data$final_narrative = gsub('[[:punct:] ]+',' ', data$final_narrative)
data$final_narrative = gsub(pattern = "employee", replacement = "", data$final_narrative)
data$final_narrative = gsub(pattern = "right", replacement = "", data$final_narrative)
data$final_narrative = gsub(pattern = "left", replacement = "", data$final_narrative)

processed = textProcessor(documents = data$final_narrative,
                          removestopwords = T, 
                          lowercase = T, 
                          stem = F,
                          removenumbers = T,
                          removepunctuation = T,
                          verbose = T,
                          metadata = select(data, -final_narrative))

out = prepDocuments(processed$documents, processed$vocab, processed$meta)

meta = out$meta
docs = out$documents
vocab = out$vocab

stm_models = lapply(X = 3:6,
                    FUN = function(k){
                      stm::stm(documents = docs, vocab = vocab, 
                               prevalence = ~ state + s(event_date), 
                               K = k,
                               data = meta,
                               max.em.its = 10000, 
                               init.type = 'Spectral')
                    })

for(i in 1:length(stm_models)){
  assign(x = paste(c("model","topic",i),collapse = "_"), value = stm_models[[i]])
}

stm_4_effect<-estimateEffect(formula=1:4~+s(event_date),
                             stmobj=stm_models[[2]],
                             metadata=meta) 

stm_5_effect<-estimateEffect(formula=1:5~+s(event_date),
                             stmobj=stm_models[[3]],
                             metadata=meta) 

stm_6_effect<-estimateEffect(formula=1:6~+s(event_date),
                             stmobj=stm_models[[4]],
                             metadata=meta) 
