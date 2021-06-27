setwd("~/Google Drive/Updated Course Syntax Files/Network Analysis Working Directory/")

  library(igraph)
  library(network)
  library(tidyverse)
library(statnet)

  load("~/Google Drive/Updated Course Syntax Files/wiring.Rdata")
 
  #Lets read the data into the enviroment. This will import it as an adjacency matric
  data(sampson)
  monks.stat<-samplike
  monks.ig<-intergraph::asIgraph(monks.stat)
  V(monks.ig)$name<-V(monks.ig)$vertex.names
  monks.se<-equiv.clust(monks.stat, equiv.fun="sedist", method="hamming",mode="digraph")
  
  #monks.se with different clustering settings:
  monks.avg.se<-equiv.clust(monks.stat, equiv.fun="sedist", method="hamming", cluster.method="average", mode="digraph")
  monks.sing.se<-equiv.clust(monks.stat, equiv.fun="sedist", method="hamming", cluster.method="single", mode="digraph")
  monks.wrd.se<-equiv.clust(monks.stat, equiv.fun="sedist", method="hamming", cluster.method="ward.D", mode="digraph")
  
  
  ##florentine marriages
  data(florentine)
  flomarr.stat<-flomarriage
  flomarr.stat<-add.edge(flomarr.stat,12,12)
  flomarr.ig<-intergraph::asIgraph(flomarr.stat)
  V(flomarr.ig)$name<-V(flomarr.ig)$vertex.names
  flomarr.ig<-delete_vertex_attr(flomarr.ig,"vertex.names")
  flomarr.ig<-delete_vertex_attr(flomarr.ig,"na")
  
  flomarr.mat<-as.matrix(get.adjacency(flomarr.ig))
  
  flomarr.se<-equiv.clust(flomarr.stat, equiv.fun="sedist", method="hamming",mode="graph")
  #blockmodel and select partitions
  blk_mod<-blockmodel(flomarr.stat,flomarr.se,k=5)
  #assign block membership to vertex attribute
  flomarr.stat%v%"role" <- blk_mod$block.membership[match(flomarr.stat%v%"vertex.names",blk_mod$plabels)]
  V(flomarr.ig)$role <- blk_mod$block.membership[match(V(flomarr.ig)$name,blk_mod$plabels)]
  
  #Flomarr.se versions with different cluster settings:
  flomarr.wrd.se<-equiv.clust(flomarr.stat, equiv.fun="sedist", cluster.method="ward.D", method="hamming",mode="graph")
  flomarr.sing.se<-equiv.clust(flomarr.stat, equiv.fun="sedist", cluster.method="single", method="hamming",mode="graph")
  flomarr.avg.se<-equiv.clust(flomarr.stat, equiv.fun="sedist", cluster.method="average", method="hamming",mode="graph")
  
  
  source("~/Google Drive/Updated Course Syntax Files/Network Analysis Working Directory/Import Scripts/Game of Thrones Marriage.R")

  
  gotmarr.mat<-as.matrix(as_adj(gotmarr.ig,attr = "weight"))
  gotpart.mat<-as.matrix(as_adj(gotpart.ig,attr = "weight"))
  gotmarr.se<-equiv.clust(gotmarr.stat, method="euclidean",mode="graph")
  gotpart.se<-equiv.clust(gotpart.stat, method="euclidean",mode="graph")
  
  #got part with different cluster settings:
  gotpart.avg.se<-equiv.clust(gotpart.stat, method="euclidean", cluster.method="average", mode="graph")
  gotpart.sing.se<-equiv.clust(gotpart.stat, method="euclidean", cluster.method="single", mode="graph")
  gotpart.wrd.se<-equiv.clust(gotpart.stat, method="euclidean", cluster.method="ward.D", mode="graph")
  
  #gotmarr with different cluster settings:
  gotmarr.avg.se<-equiv.clust(gotmarr.stat, method="euclidean", cluster.method="average", mode="graph")
  gotmarr.sing.se<-equiv.clust(gotmarr.stat, method="euclidean", cluster.method="single", mode="graph")
  gotmarr.wrd.se<-equiv.clust(gotmarr.stat, method="euclidean", cluster.method="ward.D", mode="graph")
  
  get.eigen<-function(net, attr=NULL){
    #set attr="weight" if weighted network
    eigen<-evcent(net)
    mat<-as.matrix.network(net, attr=attr)
    diag(mat)<-0
    mat2<-mat%*%mat
    rc<-diag(mat2)/rowSums(mat2)
    dc<-1-rc
    data.frame(name=net%v%"vertex.names",
               eigen=eigen,
               eigen.rc=eigen*rc,
               eigen.dc=eigen*dc)
  }
  get.brokerage<-function(net, attr="attr"){
    temp<-data.frame(brokerage(net, cl = net%v%"attr")$z.nli)
    temp$name=net%v%"vertex.names"
    mutate(temp, broker.tot = temp$t,
           broker.coord = temp$w_I,
           broker.itin = temp$w_O,
           broker.rep = temp$b_IO,
           broker.gate = temp$b_OI,
           broker.lia = temp$b_O)
  }
  plot.block<-function(x=blk_mod, main=NULL, cex.lab=1){
    plot.sociomatrix(x$blocked.data, labels=list(x$plabels,x$plabels),
                     main=main, drawlines = FALSE, cex.lab=cex.lab)
    for (j in 2:length(x$plabels)) if (x$block.membership[j] !=
                                       x$block.membership[j-1]) 
      abline(v = j - 0.5, h = j - 0.5, lty = 3)
  }
  
  gotmarr.nodes<-data.frame(name=gotmarr.stat%v%"vertex.names",
                            degree=sna::degree(gotmarr.stat,gmode="graph"),
                            degree.wt=strength(gotmarr.ig),
                            bonpow=bonpow(gotmarr.stat),
                            betweenness=betweenness(gotmarr.stat, gmode="graph"),
                            close=sna::closeness(gotmarr.stat, gmode="graph"),
                            constraint=constraint(gotmarr.ig))
  gotmarr.nodes<-full_join(gotmarr.nodes,get.eigen(gotmarr.stat, "weight"), by="name")
  
  gotpart.nodes<-data.frame(name=gotpart.stat%v%"vertex.names",
                            degree=sna::degree(gotpart.stat,gmode="graph"),
                            degree.wt=strength(gotpart.ig),
                            bonpow=bonpow(gotpart.stat),
                            betweenness=betweenness(gotpart.stat, gmode="graph"),
                            close=sna::closeness(gotpart.stat, gmode="graph"),
                            constraint=constraint(gotpart.ig))
  gotpart.nodes<-full_join(gotpart.nodes,get.eigen(gotpart.stat, "weight"), by="name")
  
  flomarr.nodes<-data.frame(name=flomarr.stat%v%"vertex.names",
                            degree=sna::degree(flomarr.stat,gmode="graph"),
                            #       degree.wt=strength(network.ig),
                            bonpow=bonpow(flomarr.stat),
                            betweenness=betweenness(flomarr.stat, gmode="graph"),
                            close=sna::closeness(flomarr.stat, gmode="graph"),
                            constraint=constraint(flomarr.ig))
  
  save(monks.stat, monks.ig, monks.se, gotmarr.stat, gotmarr.ig, 
       gotmarr.se, gotmarr.mat, gotpart.ig, gotpart.stat, 
       gotpart.mat, gotpart.se, flomarr.stat, flomarr.ig, 
       flomarr.mat, flomarr.se, monks.avg.se, monks.sing.se, monks.wrd.se, 
       flomarr.avg.se, flomarr.sing.se, flomarr.wrd.se, 
       gotpart.avg.se, gotpart.sing.se, gotpart.wrd.se, gotmarr.nodes,
       gotmarr.avg.se, gotmarr.sing.se, gotmarr.wrd.se, gotpart.nodes,
       flomarr.nodes, get.eigen, get.brokerage, plot.block,
       file="~/Google Drive/Updated Course Syntax Files/tutorials/6_NetRoles/Week6.rdata")
  setwd("~/Google Drive/Updated Course Syntax Files/tutorials/6_NetRoles/")