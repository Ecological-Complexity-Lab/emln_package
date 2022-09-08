library(tidyverse)
generate_data_description <- function(dataset){
  description <- dataset$description
  f <- paste('man/',dataset,'.Rmd',sep='')
  table <- description %>%
    filter(!attribute %in% c('source', 'data_url', 'description', 'data_entry')) %>% pivot_wider(values_from = 'value', names_from = 'attribute')
  write_lines("#'", f, append = F) # Append=F means that the file will be created. If there is an exsiting file it will be overwritten.
  write_lines(paste("#' @name ",deparse(substitute(dataset))), f, append = T)
  write_lines(paste("#' @docType ",'data', f, append = T))
  write_lines(paste("#' ",knitr::kable(table), f, append = T))
  write_lines(paste("#' @source ",description[description$attribute=='source', 'value']), f, append = T)

}


generate_data_description(emln2_Environment_Benadi_JAE_2013)


description <- emln2_Environment_Benadi_JAE_2013$description
f <- paste('man/',emln2_Environment_Benadi_JAE_2013,'.Rmd',sep='')
