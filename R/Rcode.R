# This idea is from below article (pass network of NBA team Golden State Warriors).
# http://opiateforthemass.es/articles/analyzing-golden-state-warriors-passing-network-using-graphframes-in-spark/

# The pass information of both team is from UEFA site.
# http://www.uefa.com/uefachampionsleague/season=2016/matches/round=2000637/match=2015787/postmatch/statistics/index.html#1/2016/2000637/2015787/pitch-view

# Betweenness in igraph.
# http://igraph.org/r/doc/betweenness.html

library(networkD3)
library(dplyr)
library(reshape2)
library(igraph)

# read the files
at_url<-'https://raw.githubusercontent.com/kmangyo/Football_Pass_Network/master/R/AT.csv'
bm_url<-'https://raw.githubusercontent.com/kmangyo/Football_Pass_Network/master/R/BM.csv'

# FC Bayern München
BM <- read.csv(bm_url, sep=',', row.names = NULL, stringsAsFactors = FALSE)
BM[2:ncol(BM)][is.na(BM[2:ncol(BM)])]<-0

BM_mtrx<-as.matrix(BM)
BM_df<-melt(BM_mtrx)
BM_df$from<-rep(BM_df[1:12,3],13)
names(BM_df)[2]<-'to'
BM_df<-BM_df[13:nrow(BM_df),]
BM_df<-BM_df[c(-1)]
BM_df$value<-as.numeric(as.character(BM_df$value))

BM_df$from<-gsub(" ", ".", BM_df$from)
BM_df$from<-gsub("\\?", ".", BM_df$from)

# Pass Network data manipulation for networkD3 packages
BM_df$source<-as.numeric(as.factor(as.character(BM_df$from)))-1
BM_df$target<-as.numeric(as.factor(as.character(BM_df$to)))-1
BM_df_link<-subset(BM_df, value>0)

BM_df_nodes<-BM_df %>% group_by(to) %>% summarise(size=sum(value))
BM_df_nodes$nodeid<-as.numeric(as.factor(as.character(BM_df_nodes$to)))-1
BM_df_nodes$position<-c('GK','MF','MF','FW','MF','MF','DF','DF','MF','FW','DF','MF')
BM_df_nodes$group<-as.numeric(as.factor(as.character(BM_df_nodes$position)))-1
BM_df_nodes<- BM_df_nodes %>% arrange(group)
BM_df_nodes<- BM_df_nodes %>% arrange(nodeid)

# Pass Network of BM
forceNetwork(Links = BM_df_link, Nodes = data.frame(BM_df_nodes),
             Source = "source", Target = "target",
             Value = "value", NodeID = "to", Nodesize = "size", colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
             Group = "position", opacity = 0.8, zoom = TRUE, fontSize = 16, linkDistance = 400,  fontFamily = "Arial", opacityNoHover = TRUE)

# Transform data for igraph package.
BM_df_link_igraph<-BM_df_link[c(3,1,2)]

BM_from<-list()
for(i in 1: nrow(BM_df_link_igraph)){
  BM_from[[i]]<-rep(BM_df_link_igraph[i,1],BM_df_link_igraph[i,3])
}

BM_to<-list()
for(i in 1: nrow(BM_df_link_igraph)){
  BM_to[[i]]<-rep(BM_df_link_igraph[i,2],BM_df_link_igraph[i,3])
}

BM_from<-melt(BM_from)
BM_to<-melt(BM_to)

BM_igraph<-data.frame(from=BM_from$value,to=BM_to$value)

BM_igraph <- graph.data.frame(BM_igraph, directed=TRUE)
degree(BM_igraph, mode=c('in'))
degree(BM_igraph, mode=c('out'))
betweenness(BM_igraph)

# Club Atlético de Madrid
ATM <- read.csv(at_url, sep=',', row.names = NULL, stringsAsFactors = FALSE)
ATM[2:ncol(ATM)][is.na(ATM[2:ncol(ATM)])]<-0

ATM_mtrx<-as.matrix(ATM)
ATM_df<-melt(ATM_mtrx)
ATM_df$from<-rep(ATM_df[1:14,3],15)
names(ATM_df)[2]<-'to'
ATM_df<-ATM_df[15:nrow(ATM_df),]
ATM_df<-ATM_df[c(-1)]
ATM_df$value<-as.numeric(as.character(ATM_df$value))

ATM_df$from<-gsub(" ", ".", ATM_df$from)
ATM_df$from<-gsub("\\?", ".", ATM_df$from)

# Pass Network data manipulation for networkD3 packages
ATM_df$source<-as.numeric(as.factor(as.character(ATM_df$from)))-1
ATM_df$target<-as.numeric(as.factor(as.character(ATM_df$to)))-1
ATM_df_link<-subset(ATM_df, value>0)

ATM_df_nodes<-ATM_df %>% group_by(to) %>% summarise(size=sum(value))
ATM_df_nodes$nodeid<-as.numeric(as.factor(as.character(ATM_df_nodes$to)))-1
ATM_df_nodes$position<- c('GK','DF','DF','MF','FW','FW','MF','MF','MF','DF','DF','DF','FW','MF')
ATM_df_nodes$group<-as.numeric(as.factor(as.character(ATM_df_nodes$position)))-1
ATM_df_nodes<- ATM_df_nodes %>% arrange(group)
ATM_df_nodes<- ATM_df_nodes %>% arrange(nodeid)

forceNetwork(Links = ATM_df_link, Nodes = data.frame(ATM_df_nodes),
             Source = "source", Target = "target",
             Value = "value", NodeID = "to", Nodesize = "size", colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
             Group = "position", opacity = 0.8, zoom = TRUE, fontSize = 16, linkDistance = 400,  fontFamily = "Arial", opacityNoHover = TRUE)

# Transform data for igraph package.
ATM_df_link_igraph<-ATM_df_link[c(3,1,2)]

ATM_from<-list()
for(i in 1: nrow(ATM_df_link_igraph)){
  ATM_from[[i]]<-rep(ATM_df_link_igraph[i,1],ATM_df_link_igraph[i,3])
}

ATM_to<-list()
for(i in 1: nrow(ATM_df_link_igraph)){
  ATM_to[[i]]<-rep(ATM_df_link_igraph[i,2],ATM_df_link_igraph[i,3])
}

ATM_from<-melt(ATM_from)
ATM_to<-melt(ATM_to)

ATM_igraph<-data.frame(from=ATM_from$value,to=ATM_to$value)

ATM_igraph <- graph.data.frame(ATM_igraph, directed=TRUE)
degree(ATM_igraph, mode=c('in'))
degree(ATM_igraph, mode=c('out'))
betweenness(ATM_igraph)

# ATM & BM Pass Network
df_link<-rbind(BM_df, ATM_df)
df_link$source<-as.numeric(as.factor(as.character(df_link$from)))-1
df_link$target<-as.numeric(as.factor(as.character(df_link$to)))-1
df_link<-subset(df_link, value>0)

df_nodes<-rbind(ATM_df, BM_df)

df_nodes<-df_nodes %>% group_by(to) %>% summarise(size=sum(value))
df_nodes$nodeid<-as.numeric(as.factor(as.character(df_nodes$to)))-1

df_nodes$team<-c(rep(c('atm'),14),rep(c('bm'),12))
df_nodes$group<-as.numeric(as.factor(as.character(df_nodes$team)))-1

df_nodes$nodeid<-as.numeric(as.factor(as.character(df_nodes$to)))-1
df_nodes<- df_nodes %>% arrange(group)
df_nodes<- df_nodes %>% arrange(nodeid)

forceNetwork(Links = df_link, Nodes = data.frame(df_nodes),
             Source = "source", Target = "target",
             Value = "value", NodeID = "to", Nodesize = "size", colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
             Group = "team", opacity = 0.8, zoom = TRUE, fontSize = 5, linkDistance = 90,  fontFamily = "Arial", opacityNoHover = TRUE)
