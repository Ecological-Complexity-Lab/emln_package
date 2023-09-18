library(dplyr)
library(tidyr)
library(readr)
library(tidyverse)
library(lubridate)

setwd('~')
x <- list.dirs('./GitHub/emln_db/EMLN_Datasets/datasets/', full.names = F, recursive = F)
sub.folders.short <- list.dirs(path = './GitHub/emln_db/EMLN_Datasets/datasets/', recursive = F, full.names = F)
sub.folders.long <- list.dirs(path = './GitHub/emln_db/EMLN_Datasets/datasets/', recursive = F, full.names = T)


#order the sub folders according to the id, using the short sub folders so I can use the id, but I eventually need the long to read the csvs
sub.folders.long <- sub.folders.long[str_order(sub.folders.short, numeric = T)]
sub.folders.short <- sub.folders.short[str_order(sub.folders.short, numeric = T)]

sub.folders.short.temporal <- grep(sub.folders.short, pattern = 'temporal|Temporal', value = T)
#copy them to the new directory
for (i in 6:length(sub.folders.short.temporal)) {
  if (i==5) {next}
  list_of_files <- NULL
  current_folder <- paste0("./GitHub/emln_db/EMLN_Datasets/datasets/",sub.folders.short.temporal[i])
  list_of_files <- distinct(read_csv(paste0(current_folder,"/layers.csv")))
  list_of_files <- list_of_files %>%
    pivot_wider(id_cols = layer, names_from = 'attribute', values_from = 'value')
  list_of_files$date <- mdy(list_of_files$name)
  for (j in colnames(list_of_files)) {list_of_files[[j]] <- hablar::retype(list_of_files[[j]])}
  list_of_files <- list_of_files %>% arrange(date) %>%
    mutate(layer=1:nrow(list_of_files)) %>%
    unnest(cols = everything()) %>%
    pivot_longer(!layer, names_to = 'attribute', values_to = 'value', values_transform = list(value=as.character)) %>%
    arrange(layer,attribute) %>%
    distinct()
  write_csv(list_of_files,paste0(current_folder,"/layers.csv"))
}


#chachoff
list_of_files$year[list_of_files$year==209] <- 2009

list_of_files <- list_of_files[order(unlist(list_of_files$date), decreasing = T),]
list_of_files$layer <- 1:nrow(list_of_files)
list_of_files <- list_of_files  %>% pivot_longer(!layer, names_to = 'attribute', values_to = 'value', values_transform = list(value=as.character)) %>%
  arrange(layer,attribute) %>%
  distinct()

months <- factor(unique(list_of_files$period), levels = c("january-feburary 2004","april-may 2004","august-september 2004"))
