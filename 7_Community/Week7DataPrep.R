setwd("~/Google Drive/Updated Course Syntax Files/Network Analysis Working Directory/")

  library(igraph)
  library(network)
  library(tidyverse)
  library(statnet)
  library(intergraph)

source("./Import Scripts/US Airports.R")

#select only cities with lots of passangers
citypass<-group_by(network_edgelist,V1)%>%
  summarise(pass=sum(as.numeric(Passengers)))%>%
  filter(pass>12000)

temp<-network_edgelist%>%
  group_by(V1,V2)%>%
  summarise(dist=mean(as.numeric(Distance)))%>%
  ungroup()%>%
  filter(V1%in%citypass$V1 & V2%in%citypass$V1)

airports.geo.stat<-as.network.matrix(temp, matrix.type="edgelist",
                    ignore.eval=FALSE, names.eval="dist")
rules<-rbind(attrmap(),data.frame(type=c("vertex", "vertex", "edge", "edge", "edge"),
        fromcls="network",fromattr=c("vertex.names", "na", "dist", "na", "flights"),
        tocls="igraph", toattr=c("name",NA,"weight",NA, "weight")))

airports.geo.ig<-asIgraph(airports.geo.stat, amap=rules)
V(airports.geo.ig)$name<-airports.geo.stat%v%"vertex.names"

temp<-network_edgelist%>%
  group_by(V1, V2)%>%
  summarise(flights=sum(as.numeric(Departures)))%>%
  ungroup()%>%
  filter(V1%in%citypass$V1 & V2%in%citypass$V1)

airports.fl.stat<-as.network.matrix(temp, matrix.type="edgelist",
                                     ignore.eval=FALSE, names.eval="flights")
airports.fl.ig<-asIgraph(airports.fl.stat, amap=rules)
V(airports.fl.ig)$name<-airports.fl.stat%v%"vertex.names"

source("./Import Scripts/COW Alliances.R")
network_edgelist <- network_edgelist[is.na(network_edgelist$dyad_end_date),]
alliances.ig   <- subgraph.edges(network_igraph, eids = which(is.na(E(network_igraph)$dyad_end_date)))
E(alliances.ig)$weight <- count_multiple(alliances.ig)
alliances.ig<- igraph::simplify(alliances.ig, remove.multiple=TRUE,
                    edge.attr.comb=list(defense="sum", 
                   neutrality="sum",nonaggression="sum", entente= "sum", 
                   weight="mean", "ignore"))
alliances.stat  <- intergraph::asNetwork(alliances.ig)

##florentine marriages
data(florentine)
flomarr.stat<-flomarriage
flomarr.stat<-add.edge(flomarr.stat,12,12)
flomarr.ig<-intergraph::asIgraph(flomarr.stat)
V(flomarr.ig)$name<-V(flomarr.ig)$vertex.names
flomarr.ig<-delete_vertex_attr(flomarr.ig,"vertex.names")
flomarr.ig<-delete_vertex_attr(flomarr.ig,"na")

source("~/Google Drive/Updated Course Syntax Files/Network Analysis Working Directory/Import Scripts/Game of Thrones Marriage.R")
gotmarr.mat<-as.matrix(as_adj(gotmarr.ig,attr = "weight"))
gotpart.mat<-as.matrix(as_adj(gotpart.ig,attr = "weight"))

get.eigen<-function(net, attr=NULL){
  #set attr="weight" if weighted network
  eigen<-sna::evcent(net)
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

nodes.by.gp<-function(network.nodes, groupvar){
  network.nodes%>%
    select(-name)%>%
    group_by_(groupvar) %>%
    mutate(n=n())%>%
    summarise_all(mean, na.rm=TRUE)%>%
    as.matrix()%>%
    print(digits=2)
}

compare.algs<-function(alg.a,alg.b,compare.meth=c("vi", "nmi", "split.join", "rand", "adjusted.rand")){
  #create list of community objects and methods
  comm.compare<-expand.grid(alg.a=alg.a, alg.b=alg.b, meth=compare.meth, result=NA, stringsAsFactors = FALSE)
  #compare community partitions using a loop
  for(i in 1:nrow(comm.compare)){
    comm1<-get(comm.compare$alg.a[i])
    comm2<-get(comm.compare$alg.b[i])
    method<-comm.compare$meth[i]
    comm.compare$result[i]<-compare(comm1, comm2, method)
  }
  return(comm.compare)
}

giant.component <- function(graph) {
  cl <- clusters(graph)
  induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}


flomarr.nodes<-data.frame(name=flomarr.stat%v%"vertex.names",
                          degree=sna::degree(flomarr.stat,gmode="graph"),
                          bonpow=sna::bonpow(flomarr.stat),
                          betweenness=sna::betweenness(flomarr.stat, gmode="graph"),
                          close=sna::closeness(flomarr.stat, cmode="suminvundir"),
                          constraint=constraint(flomarr.ig))

#add eigenvector centrality using custom function
flomarr.nodes<-full_join(flomarr.nodes,get.eigen(flomarr.stat), by="name")

gotmarr.nodes<-data.frame(name=gotmarr.stat%v%"vertex.names",
                          degree=sna::degree(gotmarr.stat,gmode="graph"),
                          degree.wt=strength(gotmarr.ig),
                          bonpow=sna::bonpow(gotmarr.stat),
                          betweenness=sna::betweenness(gotmarr.stat, gmode="graph"),
                          close=sna::closeness(gotmarr.stat, gmode="graph"),
                          constraint=constraint(gotmarr.ig))

#add eigenvector centrality using custom function
gotmarr.nodes<-full_join(gotmarr.nodes,get.eigen(gotmarr.stat, "weight"), by="name")


  save(gotmarr.stat, gotmarr.ig, flomarr.stat, flomarr.ig, flomarr.nodes,
       get.eigen, get.brokerage, plot.block, gotmarr.nodes,
       airports.geo.stat, airports.fl.stat, airports.fl.ig, airports.geo.ig,
       nodes.by.gp,giant.component,compare.algs, alliances.ig, alliances.stat,
       file="~/Google Drive/Updated Course Syntax Files/tutorials/7_Community/Week7.rdata")
  setwd("~/Google Drive/Updated Course Syntax Files/tutorials/7_Community/")