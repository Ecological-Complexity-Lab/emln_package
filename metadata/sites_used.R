sites_used <- tibble(web_of_life = 'http://www.web-of-life.es/map.php', mangal = 'https://mangal.io', interaction_web_database = 'http://www.ecologia.ib.usp.br/iwdb/resources.html')
site_names <- c('web of life','mangal', 'interactions web database')
site_url <- c('http://www.web-of-life.es/map.php','https://mangal.io','http://www.ecologia.ib.usp.br/iwdb/resources.html')
sites_used <- NULL
for (i in 1:3) {
  sites_used <- rbind(sites_used, tibble(site_number = i, site_name = site_names[i], site_url =site_url[i]))
}

write_csv(sites_used,'network_sites_used.csv')
