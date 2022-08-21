##node_from and node_to --> to node_id
interactions <- read_csv('interactions.csv')
nodes <- read_csv('nodes.csv')

node_from <- interactions[interactions$attribute == 'node_from',]$value
node_to <- interactions[interactions$attribute == 'node_to',]$value

for (i in 1:length(node_from)) {
interactions[interactions$value == node_from[i],]$value <-  as.character(filter(nodes,attribute == 'node_name', value == node_from[i])$node_id)
}
for (i in 1:length(node_to)) {
  interactions[interactions$value == node_to[i],]$value <-  as.character(filter(nodes,attribute == 'node_name', value == node_to[i])$node_id)
}

write_csv(interactions,'interactions.csv')