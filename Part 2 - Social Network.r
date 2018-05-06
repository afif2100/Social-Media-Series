library(networkD3)
library(igraph)
library(stringr) 
library(twitteR)
library(plyr)


#read data dari sebelummnya
d = twListToDF(readRDS('tweet-mentah10k.rds'))
alltweets<-d[1:500,]
alltweets$text


#split the data into two sets; one for retweet network and the other for mention network.
#create an edge-list for retweet network
sp = split(alltweets, alltweets$isRetweet)
rt = mutate(sp[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))
el = as.data.frame(cbind(sender = tolower(rt$sender), receiver = tolower(rt$screenName)))
el[1:10,] #show the first 5 edges in the edgelist


###
rt_graph <- graph_from_data_frame(d=el, directed=T)
glay = layout.fruchterman.reingold(rt_graph) 
plot(rt_graph)

###
wc <- cluster_walktrap(rt_graph)
members <- membership(wc)
d3_rt <- igraph_to_networkD3(rt_graph, group = members)

forceNetwork(Links = d3_rt$links, Nodes = d3_rt$nodes, fontSize = 26,
             Source = 'source', Target = 'target', linkColour = '#123',
             NodeID = 'name', Group = 'group',zoom=TRUE)
