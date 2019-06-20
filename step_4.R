##read data
home <- "~/Desktop/pythontest/STEP2019_4/STEP2019_4"
setwd(home)
library(data.table)
library(igraph)

link_nk <- fread("links_nicknames.txt", header = F, sep = "\t",data.table = F)
nk <- fread("nicknames.txt", header = F, sep = "\t",data.table = F)
#link_nk_g <- read.graph("links_nicknames.txt",directed=F)#link_nk[match(6,link_nk[,1]),]not connected
link_graph <- graph.data.frame(link_nk,directed=T)
plot(link_graph,layout=layout.auto)


##次数：各ノードに接続しているエッジの数(友達数)
order_size <- degree(link_graph)
order_size_in <- degree(link_graph, mode = "in")
order_size_out <- degree(link_graph, mode = "out")


##クラスター係数：(自分とつながっている点同士が繋がっているかどうかの指標/次数0でないもののみ)
cluster_factor <- transitivity(link_graph,type="local",isolates="zero")

##次数中心性：多くのエッジを持つものを評価
order_centularity <- degree(link_graph) / (vcount(link_graph) - 1)

##最短経路
#shortest_path_length <- shortest.paths(link_graph,mode="out")#有向の時はout
#shortest_path <- all_shortest_paths(link_graph,from = "0",to = "2",mode="out")#有向の時はout
#from_to_shortest_patterns <- get.all.shortest.paths(link_graph,"12","0",mode="out")
vname <- names(V(link_graph))
all_shortest_p<- lapply(seq_along(vname),function(st)all_shortest_paths(link_graph,from = st,to = vname[-st],mode="out"))

#all_shortest_p2 <- lapply(all_shortest_p,function(x)lapply(x$res,function(y)nk[as.numeric(names(y)),2]))

all_shortest_p2 <- vector("list",length(all_shortest_p))
names(all_shortest_p2) <- nk[as.numeric(vname)+1,2]
for(i in seq_along(all_shortest_p2)){
  select_path <- all_shortest_p[[i]]$res
  select_vname <- sapply(select_path,function(x)names(x))
  goalIdx <- sapply(select_vname,function(x)as.numeric(x[length(x)]))
  goalName <- nk[goalIdx+1,2]
  names(select_path) <- goalName
  for(j in seq_along(select_path)){
    pathIdx <- as.numeric(names(select_path[[j]]))
    pathName <- nk[pathIdx+1,2]
    all_shortest_p2[[i]][[j]] <- pathName
  }
  names(all_shortest_p2[[i]]) <- goalName
}
#from_to_shortest_patterns <- get.all.shortest.paths(link_graph,"12","0",mode="out")


