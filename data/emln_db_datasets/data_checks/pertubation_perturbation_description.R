descriptions_to_change <- c(19,50,51,74)
desc19 <- read.csv('~/GitHub/emln_db/EMLN_Datasets/datasets/19_Perturbation_Stewart_Sprules_2011/description.csv')
desc50 <- read.csv('~/GitHub/emln_db/EMLN_Datasets/datasets/50_Spatial_Perturbation_Bartomeus_2008/description.csv')
desc51 <- read.csv('~/GitHub/emln_db/EMLN_Datasets/datasets/51_Spatial_Perturbation_Vazquez_2002/description.csv')
desc74 <- read.csv('~/GitHub/emln_db/EMLN_Datasets/datasets/74_Temporal_Perturbation_Kaiser-Bunbury_2009/description.csv')
descriptions <- list(desc19,desc50,desc51,desc74)
for (i in 1:4) {
  descriptions[[i]]$value <- gsub(descriptions[[i]]$value , pattern = 'Pertubation', replacement = 'Perturbation')
}

write_csv(descriptions[[1]],'~/GitHub/emln_db/EMLN_Datasets/datasets/19_Perturbation_Stewart_Sprules_2011/description.csv')
write_csv(descriptions[[2]],'~/GitHub/emln_db/EMLN_Datasets/datasets/50_Spatial_Perturbation_Bartomeus_2008/description.csv')
write_csv(descriptions[[3]],'~/GitHub/emln_db/EMLN_Datasets/datasets/51_Spatial_Perturbation_Vazquez_2002/description.csv')
write_csv(descriptions[[4]],'~/GitHub/emln_db/EMLN_Datasets/datasets/74_Temporal_Perturbation_Kaiser-Bunbury_2009/description.csv')
