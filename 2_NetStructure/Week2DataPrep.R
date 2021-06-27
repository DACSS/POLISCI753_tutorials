  library(statnet)
  library(igraph)
  library(igraphdata)
  library(network)

  #Lets read the data into the enviroment. This will import it as an adjacency matric
  # source("~/Google Drive/Updated Course Syntax Files/Network Analysis Working Directory/Import Scripts/Game of Thrones LikeDislike.R")
  # gotlike.ig<-network_igraph
  # gotlike.stat<-network_statnet  
  setwd("~/Google Drive/Updated Course Syntax Files/Network Analysis Working Directory/")
  
  source("./Import Scripts/Game of Thrones TV.R")
  gottv.ig<-network_igraph
  gottv.stat<-network_statnet
  
 
  source("./Import Scripts/Swiss Climate Influence.R")
  climate.ig<-network_igraph
  climate.stat<-network_statnet
  ##the vertex attribute names suck, can it be fixed?
  #also statnet as the na attributes that need to be removed
  #see week 1 syntax
  
  source("~/Google Drive/Updated Course Syntax Files/Network Analysis Working Directory/Import Scripts/Game of Thrones Interactions.R")
  
  gotbook.ig<-network_igraph
  ##check this, one of the two isn't working correctly
  gotbook.ig<-igraph::delete.edges(gotbook.ig,which(E(gotbook.ig)$weight<5))

  gotbook.stat<-network_statnet
  ##check this, one of the two isn't working correctly
  gotbook.stat<-network::delete.edges(gotbook.stat, which(gotbook.stat%e%"weight"<5))

  save(climate.stat, climate.ig, gotbook.ig, gotbook.stat,gottv.ig,
     gottv.stat, file="~/Google Drive/Updated Course Syntax Files/tutorials/2_NetStructure/Week2.rdata")
  