##########################################################
######### SPELL CORRECTION - HUNSPELL PACKAGE ############
####################  JANURAY 2020  ######################
##########################################################
options(warn = -1)
library(hunspell)
library(tm)
library(textclean)
library(dplyr)
library(tidyverse)

correct_city <- read.csv("D:/Data E/Data Sc R/Latent Sentiment Analysis/SaleskenProblemSolving-master/SaleskenProblemSolving-master/Correct_cities.csv")
correct_city_name <- correct_city$name
misspelt_city <- read.csv("D:/Data E/Data Sc R/Latent Sentiment Analysis/SaleskenProblemSolving-master/SaleskenProblemSolving-master/Misspelt_cities.csv")
misspelt_city_name <- misspelt_city$misspelt_name

cat(paste("Number of Cities:",nrow(correct_city),"\n"))
cat(paste("Number of Misspelt Cities: ",nrow(correct_city),"\n"))

mycorpus <- Corpus(VectorSource(correct_city_name))
#mycorpus <- tm_map(mycorpus,tolower)
replacesymbol <- function(x){x <- gsub("[[:punct:]]"," ",x)
                             x <- gsub(" ' "," ",x)
                             return(x)}
removeNonASCII <- function(x){
                               Encoding(x)="latin"
                               replace_non_ascii(x)}

mycorpus <- tm_map(mycorpus,content_transformer(replacesymbol))
mycorpus <- tm_map(mycorpus,content_transformer(removeNonASCII))
addwrd <- c()
for(i in 1:nrow(correct_city))
{
  cityname<-mycorpus[[i]][1]
  print(cityname)
  addwrd[i] <- cityname
}

df_format_city <- as.data.frame(matrix(nrow=nrow(correct_city),ncol=3))
colnames(df_format_city) <- c("name","country","id")
df_format_city$name <- as.character(addwrd)
df_format_city$country <- correct_city$country
df_format_city$id <- correct_city$id
write.csv(df_format_city,file="D:/Data E/Data Sc R/Latent Sentiment Analysis/SaleskenProblemSolving-master/SaleskenProblemSolving-master/Update_cities.csv",sep = " , ")

mycorpus_misspelt_city <- Corpus(VectorSource(misspelt_city_name))
replacesymbol <- function(x){x <- gsub("[[:punct:]]"," ",x)
x <- gsub(" ' "," ",x)
return(x)}
removeNonASCII <- function(x){
  Encoding(x)="latin"
  replace_non_ascii(x)}

mycorpus_misspelt_city <- tm_map(mycorpus_misspelt_city,content_transformer(replacesymbol))
mycorpus_misspelt_city <- tm_map(mycorpus_misspelt_city,content_transformer(removeNonASCII))
#mycorpus_misspelt_city <- tm_map(mycorpus_misspelt_city,tolower)

addwrd_misspelt_city <- c()
for(i in 1:nrow(misspelt_city))
{
  cityname<-mycorpus_misspelt_city[[i]][1]
  print(cityname)
  addwrd_misspelt_city[i]  <- cityname
}

df_format_city_misspelt <- as.data.frame(matrix(nrow=nrow(correct_city),ncol=2))
colnames(df_format_city_misspelt) <- c("name","country")
df_format_city_misspelt$name <- as.character(addwrd_misspelt_city)
df_format_city_misspelt$country <- misspelt_city$country

mydict <- dictionary(lang = "en_US", affix = NULL, add_words =addwrd, cache = TRUE)

search_city <- as.data.frame(matrix(nrow=nrow(correct_city),ncol=3))
colnames(search_city) <- c("id","suggested_name","country")


city_name <- hunspell_suggest(as.character(df_format_city_misspelt$name),dict = mydict)
search_city$id <-seq(1,nrow(correct_city),1)
search_city$suggested_name <- city_name
search_city$country <- df_format_city_misspelt$country

result <- as.data.frame(matrix(nrow=nrow(correct_city),ncol=3))
colnames(result) <- c("name","country","id")
result_name <- c()
result_country <-c()
result_id <- c()
for(i in 1:nrow(correct_city))
{
  
  result_name[i] <- df_format_city %>% filter(name==unique(map(search_city$suggested_name[i],1))) %>% filter(country==search_city$country[i]) %>% select(c("name"))
  result_country[i] <- df_format_city %>% filter(name==unique(map(search_city$suggested_name[i],1))) %>% filter(country==search_city$country[i]) %>% select(c("country"))
  result_id[i] <- df_format_city %>% filter(name==unique(map(search_city$suggested_name[i],1))) %>% filter(country==search_city$country[i]) %>% select(c("id"))
  
}
cat(paste("Suggested City from Misspelt City","\n"))

result$name <- result_name
result$country <- result_country
result$id <- result_id

result


  
   
   
   

