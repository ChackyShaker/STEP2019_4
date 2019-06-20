home <- "~/Desktop/pythontest/STEP2019_4/STEP2019_4"
setwd(home)
library(data.table)
library(igraph)

link_pages <- fread("links_pages.txt", header = F, sep = "\t",data.table = F)
pages <- fread("pages.txt", header = F, sep = "\t",data.table = F)

link_graph <- graph.data.frame(link_pages,directed=T) #V(link_graph) vertice

##Hubsite 同定
order_size_in <- degree(link_graph, mode = "in")
order_size_all <- degree(link_graph, mode = "all")
order_size_out <- degree(link_graph, mode = "out")
#order_size_in <- as.matrix(order_size_in)
hub_top_5_idx <- order(order_size_in, decreasing = T)[1:5]



hub_name_idx <- vector(length = length(hub_top_5_idx))
for (i in 1:length(hub_top_5_idx)){
  hub_select <- hub_top_5_idx[i]
  name <- names(order_size_in[hub_select])
  hub_name_idx[i] <- name
}
hub_name_idx <- as.numeric(hub_name_idx) #元々のデータに対応するもの

hub_site_info <- vector("list",length = length(hub_name_idx)) 
for (j in 1:length(hub_name_idx)){
  name_idx_select <- hub_name_idx[j]
  hub_site_name <- pages[match(name_idx_select,pages[,1]),]
  hub_site_info[[j]] <- hub_site_name
}

##Hubsiteについて検討する
i = 1
j = 1
neighbor_list <- vector("list",length(hub_top_5_idx))
for (i in 1:length(hub_top_5_idx)){
  choice_idx <- hub_top_5_idx[i]
  neighbor <- ego(link_graph, order = 1, nodes = choice_idx, mode = c("in"), mindist = 0)
  neighbor_site <- vector(length = length(as.numeric(neighbor[[1]])))
 for (j in 1:length(as.numeric(neighbor[[1]]))){
   choice_neighbor <- as.numeric(rownames(as.matrix(neighbor[[1]][j]))) +1
   neighbor_site_part  <- pages[choice_neighbor,]$V2
   neighbor_site[j] <- neighbor_site_part 
 }
  neighbor_list[[i]] <- neighbor_site
}


##inがないpageはたどり着けない＝孤立したページ
isolated_pages_idx <- which(order_size_in==0)
isolated_pages_idx <- as.numeric(names(isolated_pages_idx))
iso_page_names <- lapply(isolated_pages_idx,function(x)pages$V2[match(x,pages$V1)])
write.table(iso_page_names,file = "iso_page_names.txt" ,col.names = NA ,sep="\t",quote = F)

















################################################################################################################
#以下メモ書き 本当はtopicmodel使いたかったな


##Subsite 同定
sub_site_info <- vector("list",length = length(hub_site_info)) 
for (i in 1:length(hub_site_info)){
  hub_id <- hub_site_info[[i]]$V1
  sub_info <- link_pages[hub_id == link_pages$V2,]
  sub_site_info[[i]] <- sub_info
}


##Sub_sub_site 同定
i = 1
sub_sub_site_info <- vector("list",length = length(hub_site_info)) 
for (i in 1:length(sub_site_info)){
  sub_id <- sub_site_info[[i]]$V1
  sub_sub_info <- lapply(sub_id[1:10], function(x)link_pages[x == link_pages$V2,])
  sub_sub_info <- table(unlist(sub_sub_info))
  sub_sub_site_info[[i]] <- sub_sub_info
}

##topic_modelに適用できる形にデータ変形
#長さが違うので長さ揃える(一番長いところで揃えればよい) 数字が飛んでいるところは0とする
h <- 2
names(sub_sub_site_info[[h]])[length(names(sub_sub_site_info[[h]]))]#今回は1483244 h 1,2,3での

#count_data_row <- vector("list",length(sub_sub_site_info))
#あまりにデータ数が多いので100まででやる
for (i in 1:length(sub_sub_site_info)){
  #h <- 1
  #missing_value_completion <- numeric(length(names(sub_sub_site_info[[h]])))
  missing_value_completion <- numeric(100)
  choice_info <- sub_sub_site_info[[i]]
  input_num_idx <- as.numeric(names(choice_info)) +1
  for (k in 1:length(which(input_num_idx <= 100)){
    choice_num <- input_num_idx[k] 
    missing_value_completion[choice_num] <- choice_info[names(choice_info) == choice_num - 1]
  }
  count_data_row[[i]] <- missing_value_completion
}



hub_net_count <- do.call("rbind",sub_sub_site_info)



##
library(CountClust)
FitGoM_result <- FitGoM(t(new_x), K=5, tol=0.1)#row:sample/col:gene
