  library(igraph)
  library(network)

  #Lets read the data into the enviroment. This will import it as an adjacency matric
  # source("~/Google Drive/Updated Course Syntax Files/Network Analysis Working Directory/Import Scripts/Game of Thrones LikeDislike.R")
  # gotlike.ig<-network_igraph
  # gotlike.stat<-network_statnet  
  setwd("~/Google Drive/Updated Course Syntax Files/Network Analysis Working Directory/")
  
  source("./Import Scripts/COW Trade.R")
  
  ##subset to year = 2007
  trade2007 <-subgraph.edges(network_igraph,eids=which(E(network_igraph)$year==2007))
  mat2007<-as.matrix(as_adjacency_matrix(trade2007, attr="weight"))
  mat2007[mat2007==-9]<-NA
  mat2007<-mat2007[rowSums(is.na(mat2007))<192,colSums(is.na(mat2007))<192]
  #mat2007<-sweep(mat2007, 2, colSums(mat2007, na.rm=TRUE), FUN="/")

trade2007.stat<-network(mat2007, matrix.type="adjacency")
trade100.stat<-network(mat2007>100,matrix.type="adjacency")

#igraph gets it wrong due to missing edges    
trade2007.ig <- graph_from_adjacency_matrix(mat2007,mode="directed",weighted=TRUE,
                                            diag=FALSE)

  source("./Import Scripts/Swiss Climate Influence.R")
  climate.ig<-network_igraph
  climate.stat<-network_statnet
  ##create node attributes to be used in exercises
  climate.nodes<-data.frame(name=V(climate.ig)$name,
                            totdegree=sna::degree(climate.stat),
                            indegree=sna::degree(climate.stat, cmode="indegree"),
                            outdegree=sna::degree(climate.stat, cmode="outdegree"))
  
  trade2007.nodes<-data.frame(names=V(trade2007.ig)$name, 
                              totdegree=sna::degree(trade2007.stat),
                              indegree=sna::degree(trade2007.stat, cmode="indegree"),
                              outdegree=sna::degree(trade2007.stat, cmode="outdegree"))
  
  x<-data.frame(scores=c(55,23,48,3,112,14,25), type=c("A", "B", "A", "B","B","A", "B"))
  
   save(climate.stat, climate.ig, trade2007.ig, trade2007.stat,climate.nodes,
        trade2007.nodes, trade100.stat, x, file="~/Google Drive/Updated Course Syntax Files/tutorials/3_NetDegree/Week3.rdata")
  