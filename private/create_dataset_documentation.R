library(tidyverse)
generate_data_description <- function(dataset){
  dataset_name <- deparse(substitute(dataset))
  description <- dataset$description
  f <- paste('R/',dataset_name,'.R',sep='')
  table <- description %>%
    filter(!attribute %in% c('source', 'data_url', 'description', 'data_entry')) %>% pivot_wider(values_from = 'value', names_from = 'attribute')
  table <- knitr::kable(cbind(tibble(network_id = as.numeric(gsub(dataset_name, pattern = '^emln|_.+$', replacement = '')),table)),align = 'c')
  write_lines(x = "#'",file =  f, append = F) # Append=F means that the file will be created. If there is an existing file it will be overwritten.
  write_lines(paste0("#' @title ",dataset_name), f, append = T)
  write_lines(paste0("#' @docType ",'data'), f, append = T)
  write_lines(paste0("#' @description ", description[description$attribute == 'description','value']), f, append = T)
  write_lines(paste0("#' "), f, append = T)
  write_lines(paste0("#' ",table), f, append = T)
  write_lines(paste0("#' "), f, append = T)
  write_lines(paste0("#' @format ",'NULL'), f, append = T)
  write_lines(paste0("#' @usage ",'NULL'), f, append = T)
  write_lines(paste0("#' @source ",unlist(description[description$attribute == 'source','value'])), f, append = T)
  write_lines(paste0("#' @source ",unlist(description[description$attribute == 'data_url','value'])), f, append = T)
  write_lines(paste0("\'",dataset_name,"\'"), f, append = T)


}

for (i in gsub(list.files(path = './data/',full.names = F),pattern = '.rda',replacement = '')) {
  generate_data_description(i)
}
