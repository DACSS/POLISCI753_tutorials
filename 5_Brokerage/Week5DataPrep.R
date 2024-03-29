setwd("~/Google Drive/Updated Course Syntax Files/Network Analysis Working Directory/")

  library(igraph)
  library(network)
library(tidyverse)

  getrc<-function(g, attr="weight"){
    mat<-as.matrix(as_adjacency_matrix(g, attr=attr))
    diag(mat)<-0
    mat2<-mat%*%mat
    rc<-diag(mat2)/rowSums(mat2)
    rc
  }
  getdc<-function(g, attr="weight"){
    mat<-as.matrix(as_adjacency_matrix(g, attr=attr))
    diag(mat)<-0
    mat2<-mat%*%mat
    dc<-1-diag(mat2)/rowSums(mat2)
    dc
  }
  
  #Lets read the data into the enviroment. This will import it as an adjacency matric
  source("./Import Scripts/Swiss Climate Preference Distance.R") 
  climpref.ig<-network_igraph
  climpref.stat<-network_statnet
  delete.vertex.attribute(climpref.stat,"na")
  delete.vertex.attribute(climpref.stat,"nodes")
  
  source("./Import Scripts/Swiss Climate Influence.R")
  climinfl.ig<-network_igraph
  climinfl.stat<-network_statnet
  delete.vertex.attribute(climinfl.stat,"na")
  delete.vertex.attribute(climinfl.stat,"nodes")
  
  source("~/Google Drive/Updated Course Syntax Files/Network Analysis Working Directory/Import Scripts/Game of Thrones Interactions.R")
gotbook.ig<-network_igraph
gotbook.ig<-igraph::delete.edges(gotbook.ig,which(E(gotbook.ig)$weight<5))
gotbook.ig<-induced.subgraph(gotbook.ig,which(igraph::degree(gotbook.ig)>0))
gotbook.stat<-network_statnet
gotbook.stat<-network::delete.edges(gotbook.stat, which(gotbook.stat%e%"weight"<5))
gotbook.stat<-get.inducedSubgraph(gotbook.stat, which(sna::degree(gotbook.stat)>0))

  gotbook.nodes<-data.frame(name=gotbook.stat%v%"vertex.names",
                          degree=sna::degree(gotbook.stat,gmode="graph"),
                          eigen=evcent(gotbook.stat),
                          bonpow=power_centrality(gotbook.ig),
                          rc=getrc(gotbook.ig),
                          dc=getdc(gotbook.ig))
  gotbook.nodes$rc<-ifelse(is.nan(gotbook.nodes$rc),0,gotbook.nodes$rc)
  gotbook.nodes$dc<-ifelse(is.nan(gotbook.nodes$dc),0,gotbook.nodes$dc)
  gotbook.nodes<-gotbook.nodes%>%
    mutate(eigen.rc=eigen*rc,
           eigen.dc=eigen*dc,
           close=sna::closeness(gotbook.stat, gmode="graph", cmode="suminvundir"),
           close.wt=igraph::closeness(gotbook.ig),
           between= sna::betweenness(gotbook.stat, gmode="graph"))%>%
    select(-rc,-dc)
  
  climpref.nodes<-data.frame(name=climpref.stat%v%"vertex.names",
                             mem.cc=climinfl.stat%v%"Climate.council",
                             mem.klim=climinfl.stat%v%"Klimaallianz",
                             mem.SK=climinfl.stat%v%"Stiftung.Klimarappen",
                             dec.maker=climinfl.stat%v%"dm",
                             orgtype3=climinfl.stat%v%"type3",
                             orgtype5=climinfl.stat%v%"type5",
                            degree=sna::degree(climpref.stat,gmode="graph"),
                            eigen=evcent(climpref.stat),
                            bonpow=bonpow(climpref.stat,gmode="graph"),
                            rc=getrc(climpref.ig),
                            dc=getdc(climpref.ig))
  climpref.nodes$rc<-ifelse(is.nan(climpref.nodes$rc),0,climpref.nodes$rc)
  climpref.nodes$dc<-ifelse(is.nan(climpref.nodes$dc),0,climpref.nodes$dc)
  climpref.nodes<-climpref.nodes%>%
    mutate(eigen.rc=eigen*rc,
           eigen.dc=eigen*dc,
           close=sna::closeness(climpref.stat, gmode="graph", cmode="undirected"),
           close.wt=igraph::closeness(climpref.ig),
           between= sna::betweenness(climpref.stat, gmode="graph"))%>%
    select(-rc,-dc)
  
  climinfl.nodes<-data.frame(name=climinfl.stat%v%"vertex.names",
                             mem.cc=climinfl.stat%v%"Climate.council",
                             mem.klim=climinfl.stat%v%"Klimaallianz",
                             mem.SK=climinfl.stat%v%"Stiftung.Klimarappen",
                             dec.maker=climinfl.stat%v%"dm",
                             orgtype3=climinfl.stat%v%"type3",
                             orgtype5=climinfl.stat%v%"type5",
                             totdegree=sna::degree(climinfl.stat),
                             indegree=sna::degree(climinfl.stat,cmode="indegree"),
                             outdegree=sna::degree(climinfl.stat,cmode="outdegree"),
                             eigen=evcent(climinfl.stat),
                            bonpow=bonpow(climinfl.stat),
                            rc=getrc(climinfl.ig, attr=NULL),
                            dc=getdc(climinfl.ig, attr=NULL))
  climinfl.nodes$rc<-ifelse(is.nan(climinfl.nodes$rc),0,climinfl.nodes$rc)
  climinfl.nodes$dc<-ifelse(is.nan(climinfl.nodes$dc),0,climinfl.nodes$dc)
  climinfl.nodes<-climinfl.nodes%>%
    mutate(eigen.rc=eigen*rc,
           eigen.dc=eigen*dc,
           close=sna::closeness(climinfl.stat, cmode="suminvdir"),
           close.wt.in=igraph::closeness(climinfl.ig, mode="in"),
           close.wt.out=igraph::closeness(climinfl.ig, mode="out"),
           between= sna::betweenness(climinfl.stat))%>%
    select(-rc,-dc)
 
  temp<-as.data.frame(brokerage(climinfl.stat,climinfl.nodes$orgtype5)$z.nli)
  climinfl.nodes<-climinfl.nodes%>%
    mutate(broker.tot = temp$t,
           broker.coord = temp$w_I,
           broker.itin = temp$w_O,
           broker.rep = temp$b_IO,
           broker.gate = temp$b_OI,
           broker.lia = temp$b_O)
  
  source("./Import Scripts/COW Trade.R")
  
  ##subset to year = 2007
  trade2007 <-subgraph.edges(network_igraph,eids=which(E(network_igraph)$year==2007))
  mat2007<-as.matrix(as_adjacency_matrix(trade2007, attr="weight"))
  mat2007[mat2007==-9]<-NA
  mat2007<-mat2007[rowSums(is.na(mat2007))<192,colSums(is.na(mat2007))<192]
  #mat2007<-sweep(mat2007, 2, colSums(mat2007, na.rm=TRUE), FUN="/")
  
  trade2007.stat<-network(mat2007, matrix.type="adjacency")
  trade100.stat<-network(mat2007>100,matrix.type="adjacency")
  trade100.ig<-graph_from_adjacency_matrix(mat2007>100)
  
  trade100.nodes<-data.frame(name=trade100.stat%v%"vertex.names",
                             totdegree=sna::degree(trade100.stat),
                             indegree=sna::degree(trade100.stat,cmode="indegree"),
                             outdegree=sna::degree(trade100.stat,cmode="outdegree"),
                             eigen=evcent(trade100.stat),
                             close=sna::closeness(trade100.stat, cmode="suminvdir"),
                             between= sna::betweenness(trade100.stat),
                             bonpow=power_centrality(trade100.ig))
  trade100.nodes$ccode<-V(trade2007)$ccode[which(trade100.stat%v%"vertex.names"%in%V(trade2007)$name)]
  trade100.nodes$continent<-countrycode::countrycode(trade100.nodes$ccode, 
                                      "cown", "continent")
  trade100.nodes$continent[61]<-"Europe"
  trade100.nodes$continent[173]<-"Asia"
  
  temp<-data.frame(brokerage(trade100.stat,trade100.nodes$continent)$z.nli)
  trade100.nodes<-trade100.nodes%>%
    mutate(broker.tot = temp$t,
           broker.coord = temp$w_I,
           broker.itin = temp$w_O,
           broker.rep = temp$b_IO,
           broker.gate = temp$b_OI,
           broker.lia = temp$b_O)
  
  save(climinfl.stat, climinfl.ig, climpref.ig,climpref.stat,gotbook.ig,
       gotbook.stat, gotbook.nodes, climinfl.nodes,climpref.nodes, trade100.stat, trade100.nodes,
       file="~/Google Drive/Updated Course Syntax Files/tutorials/5_Brokerage/Week5.rdata")
  setwd("~/Google Drive/Updated Course Syntax Files/tutorials/5_Brokerage/")