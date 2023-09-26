# ---------------
# Title: Exploring sociality with igraph
# Date: 25 sep 2023
# Author: mgranellruiz
# Goal: follow an igraph tutorial and compare with the csi values from the socialindices package.
# This code requires you to load the data from the file sociality-csi.R
# FIRST you can find how to create a dataframe to extarct all sociality vectors
# SECOND an exploration and plotting of the tutorial
# ---------------

# library ---------------------
library(lubridate)
library(dplyr)
library(ggplot2)
library(lme4)
library(ggstatsplot)
library(fitdistrplus)
library(gamlss)
library(ggside)
library(ggpubr)
library(stringr)
library(gridExtra)
library(ggtext)
library(tidyr)
library(patchwork)
source('/Users/mariagranell/Repositories/data/functions.R')
source('/Users/mariagranell/Repositories/elo-sociality/sociality/sociality-csi.R')
# path ------------------------
setwd('/Users/mariagranell/Repositories/elo-sociality/sociality')

############ FIRST ################

# make the calculations of sociality vectors a function -------------
calculate_igraph_vectors <- function (data_matrix){
vervet.edges <- graph_from_adjacency_matrix(as.matrix(data_matrix), mode="undirected", weighted=TRUE)

# Degree
vervet.degree <- degree(vervet.edges)
degree_df <- data.frame(Name = names(vervet.degree), Degree = as.numeric(vervet.degree))
# Weighted degree, or strength
vervet.strength <- strength(vervet.edges)
strenght_df <- data.frame(Name = names(vervet.strength), Strength = as.numeric(vervet.strength))
# Weighted closeness
vervet.closeness <- closeness(vervet.edges)
closeness_df <- data.frame(Name = names(vervet.closeness), Closeness = as.numeric(vervet.closeness))
# Weighted betweenness
vervet.betweenness <- betweenness(vervet.edges)
betweenness_df <- data.frame(Name = names(vervet.betweenness), Betweenness = as.numeric(vervet.betweenness))
# Eigenvector centrality
vervet.eigencent <- eigen_centrality(vervet.edges)
vervet.eigencent <- vervet.eigencent$vector
eigencent_df <- data.frame(Name = names(vervet.eigencent), Eigencent = as.numeric(vervet.eigencent))

  coefficients <- degree_df %>% left_join(strenght_df, by = "Name") %>%
    left_join(closeness_df, by = "Name") %>%
    left_join(betweenness_df, by = "Name") %>%
    left_join(eigencent_df, by = "Name")

  return(coefficients)
}

# put your data in a matrix ---------------
# 1st decide on a group (make data_matrix for "AK") and then calculate the igraph for that group
decide_gp <- "AK"
{
# cahnge the group name and the bind_rows
data_matrix1 <- ssc2 %>% filter(Group == decide_gp) %>%
  mutate(count = 1) %>%
  dplyr::select(Actor, Receiver) %>%
  group_by(Actor, Receiver) %>%
  summarize(n = n()) %>%
  reshape2::dcast(., Actor ~ Receiver, value.var = "n", fill = 0) %>%
  column_to_rownames(., var ="Actor")

# make the
cols <- colnames(data_matrix1)
rows <- rownames(data_matrix1)
# Names in list1 but not in list2
missing_in_list <- setdiff(cols, rows)

data_matrix1 <- ssc2 %>% filter(Group == decide_gp) %>%
  mutate(count = 1) %>%
  dplyr::select(Actor, Receiver) %>%
  group_by(Actor, Receiver) %>%
  summarize(n = n()) %>%
  bind_rows(data.frame(Actor = missing_in_list2, Receiver = missing_in_list2, n = 0)) %>% # make the matrix square
  #bind_rows(data.frame(Actor = missing_in_list2, Receiver = "Ratel", n = 0)) %>% # add "Ratel" if you´re in LT
  reshape2::dcast(., Actor ~ Receiver, value.var = "n", fill = 0) %>%
  column_to_rownames(., var ="Actor")

rm(cols,rows,data_matrix1,missing_in_list,decide_gp)
} # prepare the data in a square matrix

#AK <- calculate_igraph_vectors(data_matrix1) %>% mutate(Group = "AK")
#NH <- calculate_igraph_vectors(data_matrix1) %>% mutate(Group = "NH")
#BD <- calculate_igraph_vectors(data_matrix1) %>% mutate(Group = "BD")
#KB <- calculate_igraph_vectors(data_matrix1) %>% mutate(Group = "KB")
#LT <- calculate_igraph_vectors(data_matrix1) %>% mutate(Group = "LT") # make a change when doing data_matrix1
#CR <- calculate_igraph_vectors(data_matrix1) %>% mutate(Group = "CR")

igraph_combined <- AK %>% rbind(NH,BD,KB,LT,CR)
rm(AK,NH,BD,KB,LT,CR)
# combine with CSI
sociality_vectors <- left_join(igraph_combined, csiall, by = c("Name" = "focal", "Group" = "group"))

############ SECOND ################
# tutorial : http://www.randigriffin.com/2017/04/26/primate-social-networks-in-igraph.html
# load packages
library("igraph")
library("data.table")


# link with actual lh --------
lh <- read.csv('/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/tbl_maria/factchecked_LH.csv')
lh <- lh %>%
  dplyr::select(-OtherID) %>%                             # Exclude the "OtherID" column
  distinct() %>%                                          # remove etra entries
  filter(
  AnimalCode != "Rat" & Tenure_type != "MigrationGroup1", # remove the first migration of Rat
  AnimalCode != "Kom" & Tenure_type != "MigrationGroup2"  # remove the second migration of Kom
)
adults <- lh %>% filter(Age_class == "adult" | Age_class == "sub-adult")
adults <- unique(adults$AnimalCode)

ssc2 <- change_group_names(ssc,c("Group"))

ssc2 <- ssc2 %>%
  #filter(Actor %in% adults, Receiver %in% adults) %>%
  left_join(lh, by = c("Focal" = "AnimalCode", "Group" = "Group_mb"))

# you need to put your data in a matrix
data_matrix <- ssc2 %>%
  mutate(count = 1) %>%
  dplyr::select(Actor, Receiver) %>%
  group_by(Actor, Receiver) %>%
  summarize(n = n()) %>%
  #bind_rows(data.frame(Actor = c("Nko", "Gobe"), Receiver = "Nkos", n = 0)) %>%
  reshape2::dcast(., Actor ~ Receiver, value.var = "n", fill = 0) %>%
  column_to_rownames(., var ="Actor")

# start following the tutorial ------------
# Convert adjacency matrix into edge list with weight attribute
vervet.edges <- graph_from_adjacency_matrix(as.matrix(data_matrix), mode="undirected", weighted=TRUE)

########################################################################
# Visualization
########################################################################

# Weighted edges and color coded sex for vervet
vervet.sexes <- V(vervet.edges)$name
vervet.sexes <- gsub('^.{3}$', "skyblue", vervet.sexes) # if is 3 letters male
vervet.sexes <- gsub("^.{4}$", "hotpink", vervet.sexes) # if is 4 letters female
layout(1)
plot(vervet.edges, main="vervet- pretty version",
     vertex.label.cex=0.5,
     vertex.color=vervet.sexes,
     vertex.size=10,
     edge.width=edge_attr(vervet.edges)$weight*0.09,
     edge.color="black")

# Add attributes to new network, vervet.edges2
vervet.edges2 <- set.vertex.attribute(vervet.edges, "color", value=vervet.sexes)
vervet.edges2 <- set.vertex.attribute(vervet.edges2, "size", value=10)
vervet.edges2 <- set.edge.attribute(vervet.edges2, "width", value=edge_attr(vervet.edges)$weight*0.1)
vervet.edges2 <- set.edge.attribute(vervet.edges2, "color", value="black")

# Visualize different graph layouts for vervet
layout(matrix(1:9, 3, 3, byrow=TRUE))
par(mar=c(1,0,3,0))
plot(vervet.edges2, main="Grid", layout=layout_on_grid)
plot(vervet.edges2, main="Circle", layout=layout_in_circle)
plot(vervet.edges2, main="Sphere", layout=layout_on_sphere)
plot(vervet.edges2, main="Simulated annealing", layout=layout_with_dh)
plot(vervet.edges2, main="Force directed", layout=layout_with_fr)
plot(vervet.edges2, main="Spring model", layout=layout_with_kk)
plot(vervet.edges2, main="Multidimensional scaling", layout=layout_with_mds)
plot(vervet.edges2, main="Randomly", layout=layout_randomly)
plot(vervet.edges2, main="Nicely", layout=layout_nicely)

########################################################################
# Analysis- Individual level
########################################################################
{
# Degree
vervet.degree <- degree(vervet.edges)
degree_df <- data.frame(Name = names(vervet.degree), Degree = as.numeric(vervet.degree))
# Weighted degree, or strength
vervet.strength <- strength(vervet.edges)
strenght_df <- data.frame(Name = names(vervet.strength), Strength = as.numeric(vervet.strength))
# Weighted closeness
vervet.closeness <- closeness(vervet.edges)
closeness_df <- data.frame(Name = names(vervet.closeness), Closeness = as.numeric(vervet.closeness))
# Weighted betweenness
vervet.betweenness <- betweenness(vervet.edges)
betweenness_df <- data.frame(Name = names(vervet.betweenness), Betweenness = as.numeric(vervet.betweenness))
# Eigenvector centrality
vervet.eigencent <- eigen_centrality(vervet.edges)
vervet.eigencent <- vervet.eigencent$vector
eigencent_df <- data.frame(Name = names(vervet.eigencent), Eigencent = as.numeric(vervet.eigencent))

  coefficients <- degree_df %>% left_join(strenght_df, by = "Name") %>%
    left_join(closeness_df, by = "Name") %>%
    left_join(betweenness_df, by = "Name") %>%
    left_join(eigencent_df, by = "Name")

}


# Local weighted clustering coefficient
vervet.trans <- transitivity(vervet.edges, type="weighted", isolates="zero")

# Set grid layout attribute'
vervet.edges3 <- set.graph.attribute(vervet.edges2, "layout", layout.grid)
layout(matrix(1:5, 1, 5, byrow=TRUE))
par(mar=c(0,2,2,0))
plot(vervet.edges3, vertex.label=NA, main="Degree", vertex.size=(30*vervet.degree)/max(vervet.degree))
plot(vervet.edges3, vertex.label=NA, main="Strength", vertex.size=(30*vervet.strength)/max(vervet.strength))
plot(vervet.edges3, vertex.label=NA, main="Closeness", vertex.size=(30*vervet.closeness)/max(vervet.closeness))
plot(vervet.edges3, vertex.label=NA, main="Betweenness", vertex.size=(30*vervet.betweenness)/max(vervet.betweenness))
plot(vervet.edges3, vertex.label=NA, main="Eigenvector Centrality", vertex.size=(30*vervet.eigencent)/max(vervet.eigencent))

########################################################################
# Analysis- Network level
########################################################################

# Centralization scores for vervet
vervet.centz <- centr_degree(vervet.edges)$centralization
vervet.centz[2] <- centr_clo(vervet.edges)$centralization
vervet.centz[3] <- centr_betw(vervet.edges)$centralization
vervet.centz[4] <- centr_eigen(vervet.edges)$centralization

# Plot histograms of centrality scores
layout(matrix(1:5, 1, 5, byrow = TRUE))
par(mar=c(3,5,3,1))
hist(vervet.degree, main="Degree", col="gray", xlab="", ylab="")
hist(vervet.strength, main="Strength", col="gray", xlab="", ylab="")
hist(vervet.closeness, main="Closeness", col="gray", xlab="", ylab="")
hist(vervet.betweenness, main="Betweenness", col="gray", xlab="", ylab="")
hist(vervet.eigencent, main="Eigenvector centrality", col="gray", xlab="", ylab="")


# Clustering algorithm for baboons
vervet.clust <- cluster_spinglass(vervet.edges)

# Overlay modules on network plots
layout(1)
par(mar = c(0, 0, 0, 0))
plot(vervet.edges2, mark.groups=communities(vervet.clust), main="vervet")