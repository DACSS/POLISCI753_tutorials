  library(statnet)
  library(igraph)
  library(igraphdata)
  library(network)
  library(intergraph)

  #Lets read the data into the enviroment. This will import it as an adjacency matric
  source("~/Google Drive/Updated Course Syntax Files/Network Analysis Working Directory/Import Scripts/US Airports.R")
  airport.ig<-network_igraph
  airport.stat<-network_statnet  
  
  data(karate)
  karate.ig<-karate
  karate.stat<-asNetwork(karate)
  delete.edge.attribute(karate.stat, "na")
  delete.vertex.attribute(karate.stat, "na")
  
save(karate.stat, karate.ig, airport.stat, airport.ig, file="Week1.rdata")
  