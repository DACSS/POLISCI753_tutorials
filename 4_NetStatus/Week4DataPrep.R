  library(igraph)
  library(network)

  getrc<-function(g, attr="weight"){
    mat<-as.matrix(as_adjacency_matrix(g, attr=attr))
    diag(mat)<-0
    mat2<-mat%*%mat
    rc<-diag(mat2)/rowSums(mat2)
    rc
  }
  getdc<-function(g,attr="weight"){
    mat<-as.matrix(as_adjacency_matrix(g, attr=attr))
    diag(mat)<-0
    mat2<-mat%*%mat
    dc<-1-diag(mat2)/rowSums(mat2)
    dc
  }
  
  #Lets read the data into the enviroment. This will import it as an adjacency matric
source("~/Google Drive/Updated Course Syntax Files/Network Analysis Working Directory/Import Scripts/Game of Thrones LikeDislike.R")
gotlike.ig<-network_igraph
gotlike.stat<-network_statnet  
setwd("~/Google Drive/Updated Course Syntax Files/Network Analysis Working Directory/")

source("~/Google Drive/Updated Course Syntax Files/Network Analysis Working Directory/Import Scripts/Game of Thrones Interactions.R")
gotbook.ig<-network_igraph
gotbook.ig<-igraph::delete.edges(gotbook.ig,which(E(gotbook.ig)$weight<5))
gotbook.ig<-induced.subgraph(gotbook.ig,which(igraph::degree(gotbook.ig)>0))
gotbook.stat<-network_statnet
gotbook.stat<-network::delete.edges(gotbook.stat, which(gotbook.stat%e%"weight"<5))
gotbook.stat<-get.inducedSubgraph(gotbook.stat, which(sna::degree(gotbook.stat)>0))

  source('./Import Scripts/IMF CPIS.R')
  
  ##subset to year = 2014
  imf2014.ig <-subgraph.edges(network_igraph,eids=which(E(network_igraph)$year==2014))

  imf2014.stat<-intergraph::asNetwork(imf2014.ig)
  mat2014<-as.matrix(as_adjacency_matrix(imf2014.ig, attr="weight"))
  mat2014sq<-mat2014 %*% mat2014
  imf100.stat<-network(mat2014>100000000,matrix.type="adjacency")
  imf100.ig<-graph_from_adjacency_matrix(mat2014>100000000,mode="directed", weighted=TRUE)

   imf2014.nodes<-data.frame(name=imf2014.stat%v%"vertex.names",
                            totdegree=sna::degree(imf2014.stat),
                            indegree=sna::degree(imf2014.stat, cmode="indegree"),
                            outdegree=sna::degree(imf2014.stat, cmode="outdegree"),
                            eigen=evcent(imf2014.stat),
                            bonpow=bonpow(imf2014.stat),                          
                            rc=diag(mat2014sq)/rowSums(mat2014sq),
                            dc=1-diag(mat2014sq)/rowSums(mat2014sq))
   
   imf2014.nodes$rc<-ifelse(is.nan(imf2014.nodes$rc),0,imf2014.nodes$rc)
   imf2014.nodes$dc<-ifelse(is.nan(imf2014.nodes$dc),0,imf2014.nodes$dc)
   imf2014.nodes$eigen.rc<-imf2014.nodes$eigen*imf2014.nodes$rc
   imf2014.nodes$eigen.dc<-imf2014.nodes$eigen*imf2014.nodes$dc
  imf2014.nodes<-select(imf2014.nodes,-rc,-dc)
   
   imf100.nodes<-data.frame(name=imf100.stat%v%"vertex.names",
                             totdegree=sna::degree(imf100.stat),
                             indegree=sna::degree(imf100.stat, cmode="indegree"),
                             outdegree=sna::degree(imf100.stat, cmode="outdegree"),
                             eigen=evcent(imf100.stat),
                             bonpow=bonpow(imf100.stat),
                             rc=getrc(imf100.ig,attr=NULL),
                             dc=getdc(imf100.ig,attr=NULL))
   imf100.nodes$rc<-ifelse(is.nan(imf100.nodes$rc),0,imf100.nodes$rc)
   imf100.nodes$dc<-ifelse(is.nan(imf100.nodes$dc),0,imf100.nodes$dc)
   imf100.nodes$eigen.rc<-imf100.nodes$eigen*imf100.nodes$rc
   imf100.nodes$eigen.dc<-imf100.nodes$eigen*imf100.nodes$dc
   imf100.nodes<-select(imf100.nodes,-rc,-dc)
   
   gotlike.nodes<-data.frame(name=gotlike.stat%v%"vertex.names",
                            totdegree=sna::degree(gotlike.stat),
                            indegree=sna::degree(gotlike.stat, cmode="indegree"),
                            outdegree=sna::degree(gotlike.stat, cmode="outdegree"),
                            eigen=evcent(gotlike.stat),
                            bonpow=bonpow(gotlike.stat),
                            rc=getrc(gotlike.ig),
                            dc=getdc(gotlike.ig))
   gotlike.nodes$rc<-ifelse(is.nan(gotlike.nodes$rc),0,gotlike.nodes$rc)
   gotlike.nodes$dc<-ifelse(is.nan(gotlike.nodes$dc),0,gotlike.nodes$dc)
   gotlike.nodes$eigen.rc<-gotlike.nodes$eigen*gotlike.nodes$rc
   gotlike.nodes$eigen.dc<-gotlike.nodes$eigen*gotlike.nodes$dc
   gotlike.nodes<-select(gotlike.nodes,-rc,-dc)
   
  gotbook.nodes<-data.frame(name=gotbook.stat%v%"vertex.names",
                          degree=sna::degree(gotbook.stat,gmode="graph"),
                          eigen=evcent(gotbook.stat),
                          bonpow=power_centrality(gotbook.ig),
                          rc=getrc(gotbook.ig),
                          dc=getdc(gotbook.ig))
  gotbook.nodes$rc<-ifelse(is.nan(gotbook.nodes$rc),0,gotbook.nodes$rc)
  gotbook.nodes$dc<-ifelse(is.nan(gotbook.nodes$dc),0,gotbook.nodes$dc)
  gotbook.nodes$eigen.rc<-gotbook.nodes$eigen*gotbook.nodes$rc
  gotbook.nodes$eigen.dc<-gotbook.nodes$eigen*gotbook.nodes$dc
  gotbook.nodes<-select(gotbook.nodes,-rc,-dc)
  
  save(imf2014.stat, imf2014.ig, gotlike.ig, gotlike.stat, gotbook.ig, gotbook.stat,
       imf2014.nodes, gotbook.nodes, mat2014, imf100.nodes,
       gotlike.nodes, imf100.stat, file="~/Google Drive/Updated Course Syntax Files/tutorials/4_NetStatus/Week4.rdata")
  setwd("~/Google Drive/Updated Course Syntax Files/tutorials/4_NetStatus/")