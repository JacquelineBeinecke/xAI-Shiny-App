# create data tables
library(uuid)

# read original data
nodeFeatures = read.delim("C:/Users/vakt2/01_VANESSA/01_Master - WI Marburg/04_Masterarbeit/01_Entwicklungsprojekt/RShiny App/Neue Daten/Daten Bastian/KIDNEY_mRNA_FEATURES.txt", sep = " ")
nodelist = read.csv("C:/Users/vakt2/01_VANESSA/01_Master - WI Marburg/04_Masterarbeit/01_Entwicklungsprojekt/RShiny App/Neue Daten/Daten Bastian/human.name_2_string.csv", sep="\t")
edgeFeatures = read.delim("C:/Users/vakt2/01_VANESSA/01_Master - WI Marburg/04_Masterarbeit/01_Entwicklungsprojekt/RShiny App/Neue Daten/Daten Bastian/KIDNEY_PPI(1).txt", sep = " ")
edgelist = read.delim("C:/Users/vakt2/01_VANESSA/01_Master - WI Marburg/04_Masterarbeit/01_Entwicklungsprojekt/RShiny App/Neue Daten/Daten Bastian/SUBGRAPH_EDGE_RELEVANCE.txt", sep = " ")

# structure nodeFeatures
nodeFeatures = t(nodeFeatures)
nodeFeatures = cbind(label = rownames(nodeFeatures), nodeFeatures)
rownames(nodeFeatures) = 1:nrow(nodeFeatures)
nodeFeatures_df = as.data.frame(nodeFeatures)
nodeFeatures_df$rel_pos = c(rep(0))
nodeFeatures_df$rel_pos_neg = c(rep(0))
nodeFeatures_df = nodeFeatures_df[,c(1,308,309,2:307)]

# structure nodelist
nodelist = nodelist[,c(2,3)]
colnames(nodelist) = c("label", "id")

# make nodelist and nodeFeatures coherent and then merge nodeFeatures into nodelist
intersection1 = intersect(nodelist$label, nodeFeatures_df$label)
nodelist = nodelist[c(nodelist$label %in% intersection),]
nodeFeatures_df = nodeFeatures_df[c(nodeFeatures_df$label %in% intersection),]
nodelist = merge(nodelist, nodeFeatures_df, by = "label")

#structure edgeFeatures
colnames(edgeFeatures) = c("from", "to", "confidence")
edgeFeatures = edgeFeatures[!duplicated(cbind(pmin(edgeFeatures$from, edgeFeatures$to), pmax(edgeFeatures$from, edgeFeatures$to))),]

# structure edgelist & edgeFeatures and make sure that edgelist describes a homogeneous graph (only one edge between two nodes)
colnames(edgelist) = c("from", "to", "rel_pos")
edgelist = edgelist[!duplicated(cbind(pmin(edgelist$from, edgelist$to), pmax(edgelist$from, edgelist$to))),]
edgelist$id = UUIDgenerate(use.time = TRUE, n = nrow(edgelist))
edgelist$rel_pos_neg = c(rep(0))
edgelist$confidence = c(rep(0))
edgelist = edgelist[,c(1,2,4,3,5)]

# merge confidence values of edgeFeatures into edgelist
for (index in 1:nrow(edgeFeatures)) {
  
  if(edgeFeatures$from[index] %in% edgelist$from && edgeFeatures$to[index] %in% edgelist$to ||
     edgeFeatures$to[index] %in% edgelist$from && edgeFeatures$from[index] %in% edgelist$to) {
    
    from_value = edgeFeatures[index,1]
    to_value = edgeFeatures[index,2]
    confidence_value = edgeFeatures[index,3]
    
    for (i in 1:nrow(edgelist)) {
      if (edgelist$from[i] == from_value && edgelist$to[i] == to_value ||
          edgelist$to[i] == from_value && edgelist$from[i] == to_value) {
        
        edgelist$confidence[i] = confidence_value
      }
    }
  }
}

# extract nodes from nodelist that edges exist for
nodes_in_edgelist_from = unique(edgelist[,1, drop = FALSE])
colnames(nodes_in_edgelist_from) = "label"
nodes_in_edgelist_to = unique(edgelist[,2, drop = FALSE])
colnames(nodes_in_edgelist_to) = "label"
nodes_in_edgelist = unique(rbind(nodes_in_edgelist_from, nodes_in_edgelist_to))

nodelist_subset = nodelist
nodelist_reduced = data.frame()
for (x in 1:nrow(nodes_in_edgelist)){
  nodelist_subset = subset(nodelist, label == nodes_in_edgelist[x,1])
  nodelist_reduced = rbind(nodelist_reduced, nodelist_subset)
}

# transfer values of columns "from" and "to" into ids of nodes from nodelist
edgelist_new = edgelist
for (y in 1:nrow(edgelist_new)){
  edgelist_new$from[y] = nodelist[which(nodelist$label == edgelist_new$from[y]), 2]
  edgelist_new$to[y] = nodelist[which(nodelist$label == edgelist_new$to[y]), 2]
}

# output 2 files
write.csv(nodelist_reduced, file = "nodelist_2.csv", row.names = FALSE)
write.csv(edgelist_new, file = "edgelist_2.csv", row.names = FALSE)