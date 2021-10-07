# create data tables
library(uuid)

# read original data
nodeFeatures = read.delim("C:/Users/vakt2/01_VANESSA/01_Master - WI Marburg/04_Masterarbeit/01_Entwicklungsprojekt/RShiny App/nodeFeatures.cct")
edgelist = read.csv("C:/Users/vakt2/01_VANESSA/01_Master - WI Marburg/04_Masterarbeit/01_Entwicklungsprojekt/RShiny App/edges_small.csv", sep = ",")
nodelist = read.csv("C:/Users/vakt2/01_VANESSA/01_Master - WI Marburg/04_Masterarbeit/01_Entwicklungsprojekt/RShiny App/mapping.csv", sep="\t")

# structure edgelist and make sure that edgelist describes a homogenous graph (only one edge between two nodes)
edgelist = edgelist[!duplicated(cbind(pmin(edgelist$from, edgelist$to), pmax(edgelist$from, edgelist$to))),]
colnames(edgelist) = c("from", "to", "confidence")
edgelist$id = UUIDgenerate(use.time = TRUE, n = nrow(edgelist))
edgelist$rel_pos = round(runif(n = nrow(edgelist), min = 0, max = 100),2)
edgelist$rel_pos_neg = round(runif(n = nrow(edgelist), min = -100, max = 100),2)
edgelist = edgelist[,c(1,2,4,5,6,3)]

# structure nodelist
nodelist = nodelist[,c(2,3)]
colnames(nodelist) = c("label", "id")

# structure nodeFeatures
colnames(nodeFeatures)[1] = "label"
nodeFeatures$rel_pos = round(runif(n = nrow(nodeFeatures), min = 0, max = 100), 2)
nodeFeatures$rel_pos_neg = round(runif(n = nrow(nodeFeatures), min = -100, max = 100), 2)
nodeFeatures = nodeFeatures[,c(1,81,82,2:80)]

# reduce amount of nodes
nodelist = nodelist[c(1:100),]

# make data files coherent (nodelist, edgelist and nodeFeatures contain different nodes --> reduce to the same nodes)
intersection = intersect(nodelist$label, nodeFeatures$label)
nodelist = nodelist[c(nodelist$label %in% intersection),]
nodeFeatures = nodeFeatures[c(nodeFeatures$label %in% intersection),]
edgelist = edgelist[c(edgelist$from %in% nodelist$id),]
edgelist = edgelist[c(edgelist$to %in% nodelist$id),]

# output 2 files
nodelist = merge(nodelist, nodeFeatures, by = "label")
write.csv(nodelist, file = "nodelist.csv", row.names = FALSE)
write.csv(edgelist, file = "edgelist.csv", row.names = FALSE)