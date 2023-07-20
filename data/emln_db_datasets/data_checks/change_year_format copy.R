#change references year format
source('~/GitHub/emln_db/noa_functions.R')
references <- read_csv('references.csv')
write_csv(references,'references_old.csv')
write_csv(turn_to_datetime(references),'references.csv')

