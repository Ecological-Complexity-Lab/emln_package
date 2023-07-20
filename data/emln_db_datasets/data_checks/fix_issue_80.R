library(emln)
library(readr)



# networks_with_duplicated_nodes ------------------------------------------

networks_with_duplicated_nodes <- c(2, 14, 17, 37, 49, 59, 67, 72)
net2 <- load_emln(2)


net14 <- load_emln(14)

#network 14 fixed

net17 <- load_emln(17)

#network 17 is fixed
net37 <- load_emln(37)

#network 37 is fixed

net49 <- load_emln(49)

#network 49 is fixed

net67 <- load_emln(67)

#network 67 is fixed

net72 <-load_emln(72)

#network 72 is fixed


# nodes_not_the_same_between_nodes_interactions ---------------------------
nodes_not_the_same_between_nodes_interactions <- c(6, 20,21,24, 34, 62)


net6 <- load_emln(6)
#network 6 OK


net20 <- load_emln(20)
#network 20 OK

net21 <- load_emln(21)
#network 21 is OK

net24 <- load_emln(24)
#there are nodes that are not in the interactions - should i delete them?

net62 <- load_emln(62)
#network 62 is OK


##Same layer appears more than once in network$layers
layer_more_than_once <- c(10, 32, 33, 46, 50, 51, 55, 57, 58, 59, 60, 67, 71)
layer_more_than_once+1

net10 <- load_emln(10)
#network is OK

net32 <- load_emln(32)
#network 32 is OK

net33 <- load_emln(33)
#network 33 is OK

net46 <- load_emln(46)
#network 46 looks OK

net50 <- load_emln(50)
#network 50 is OK

net51 <- load_emln(51)
#network 51 looks OK

net55 <- load_emln(55)
#network 55 looks OK

net57 <- load_emln(57)
#network 57 looks OK

net58 <- load_emln(58)
#network 58 looks OK

net59 <- load_emln(59)
#doesn't exist anymore

net60 <- load_emln(60)
#network 61 looks OK

net66 <- load_emln(66)
#network 66 is fixed (former 67)

net67 <- load_emln(67)
#net 67 was fixed

net71 <- load_emln(70)
#looks ok

#NA in nodes
net27 <- load_emln(26)
#looks ok


net45 <- load_emln(44)
load_emln(45)





net13 <- load_emln(13)

net18 <- load_emln(18)

net27 <- load_emln(27)

net39 <- load_emln(39)

net43 <- load_emln(43)

net45 <- load_emln(45)

net61 <- load_emln(61)
