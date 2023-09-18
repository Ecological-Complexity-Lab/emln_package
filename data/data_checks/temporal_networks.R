layers <- read_csv('layers.csv')
num_layers <- unique(layers$layer)

if (length(num_layers) < 100) {
  check <- append(check,getwd())
}

