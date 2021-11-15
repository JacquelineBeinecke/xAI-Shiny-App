## this file gets loaded before the ui and server and contains helpful functions
## that make the server and ui code a bit shorter and more understandable

##################################
######## disable tabs ############
##################################

# JavaScript that dis/enables the ability to click the tab
app_jscode <-
  "shinyjs.disableTab = function(name) {
    var tab = $('.nav li a[data-value=' + name + ']');
    tab.bind('click.tab', function(e) {
      e.preventDefault();
      return false;
    });
    tab.addClass('disabled');
  }
  shinyjs.enableTab = function(name) {
    var tab = $('.nav li a[data-value=' + name + ']');
    tab.unbind('click.tab');
    tab.removeClass('disabled');
  }"

# CSS script that makes it look as if the tab cannot be clicked
app_css <-
  ".nav li a.disabled {
    background-color: #f8f8f8 !important;
    color: #777777 !important;
    cursor: not-allowed !important;
    border-color: #aaa !important;
  } "




#############################################
######## functions for table vis ############
#############################################

## function to update the table that is visualized on edges
## new_node_table: new table with nodes
update_shown_node_table <- function(new_node_table){
  table <- new_node_table
  table <- table[, -2]
  datatable(
    table,
    rownames = FALSE,
    extensions = "FixedColumns",
    options = list(scrollX = TRUE, fixedColumns = list(leftColumns = 2))
  )
}

## function to update the table that is visualized on edges
## new_edge_table: new table with edges
## nodelist_table: complete nodelist_table (needed to get label of nodes)
update_shown_edge_table <- function(new_edge_table, nodelist_table){
  table <- new_edge_table
  
  # replace ids with labels for showing in the table
  for(idx in 1:nrow(table)){
    table$from[idx] <- nodelist_table$label[which(nodelist_table$id==table$from[idx])]
    table$to[idx] <- nodelist_table$label[which(nodelist_table$id==table$to[idx])]
  }
  
  # sort by from label and remove all ID columns for vis
  table <- table[order(table$from), ]
  table <- table[, c(1, 2, 4:ncol(table))]
  
  datatable(
    table,
    rownames = FALSE,
    extensions = "FixedColumns",
    options = list(scrollX = TRUE, fixedColumns = list(leftColumns = 2))
  )
}





#############################################
######## functions for tooltips  ############
#############################################

## function to calculate node_tooltip
update_node_tooltip <- function(nodelist, edgelist){
  degree <- c() # initialze "degree" vector
  for(i in 1:length(nodelist$id)){
    degree[i] <- nrow(edgelist[which(edgelist$from == nodelist$id[i]),]) + nrow(edgelist[which(edgelist$to == nodelist$id[i]),])
  } 
  degree <- data.frame(degree)
  
  # tooltip for nodes: create html String containing tooltip information: label, rel_pos, rel_pos_neg, degree, then create additional column "title" in nodes
  nodes_tooltip <- cbind(nodelist[, c(1, 3, 4)], degree)
  
  html_string <- ""
  for (index in 1:ncol(nodes_tooltip)) {
    html_string <- paste0(html_string, "<p><b>", colnames(nodes_tooltip)[index], ": ", "</b>", as.character(nodes_tooltip[1:nrow(nodes_tooltip), index]), "</p>")
  }
  nodes_tooltip$title <- html_string
  nodes_tooltip$title
  return(nodes_tooltip$title)
}

## function to calculate edge_tooltip
update_edge_tooltip <- function(nodelist, edgelist){
  # tooltip for edges: translate node ids into label names, create html String containing tooltip information: "from-to", rel_pos, rel_pos_neg, then create additional column "title" in edges
  edges_tooltip <- edgelist[, c(1, 2, 4, 5)]
  string <- c()
  
  # replace ids with labels for showing in the table
  for(idx in 1:nrow(edges_tooltip)){
    edges_tooltip$from[idx] <- nodelist$label[which(nodelist$id==edges_tooltip$from[idx])]
    edges_tooltip$to[idx] <- nodelist$label[which(nodelist$id==edges_tooltip$to[idx])]
  }
  
  edges_tooltip$from_to <- paste0(as.character(edges_tooltip[1:nrow(edges_tooltip), 1]), " - ", as.character(edges_tooltip[1:nrow(edges_tooltip), 2]))
  edges_tooltip <- edges_tooltip[, c(5, 3, 4)]
  
  
  for(index in 1:ncol(edges_tooltip)){
    string <- paste0(string, "<p><b>", colnames(edges_tooltip)[index], ": ", "</b>", as.character(edges_tooltip[1:nrow(edges_tooltip), index]), "</p>")
  }
  
  return(string)
}


#############################################
########  functions for colors   ############
#############################################

get_rel_pos_colors_and_border <- function(nodelist){
  nodes <- nodelist
  # define amount of different groups to differentiate by color and set the same amount of colors
  amount <- 5
  pos_colors <- c("#FAFAFA", "#E0E0E0", "#9E9E9E", "#616161", "#212121") #light to dark (left to right)
  
  # calculate intervals
  intervals <- cut(nodes$rel_pos, breaks = round(seq(from = min(nodes$rel_pos), to = max(nodes$rel_pos), by = (max(nodes$rel_pos)-min(nodes$rel_pos))/5),5), include.lowest = TRUE)
  # map a color to each group
  names(pos_colors) <- levels(intervals)
  
  # classify all nodes into groups with different colors
  nodes$group <- intervals
  nodes$color.background <- pos_colors[nodes$group]
  nodes$color.highlight.background <- pos_colors[nodes$group]
  nodes$color.hover.background <- pos_colors[nodes$group]
  # all border colors remain unchanged
  nodes$color.border <- c(rep("#0a4ea3", nrow(nodes)))
  nodes$color.highlight.border <- c(rep("red", nrow(nodes)))
  nodes$color.hover.border <- c(rep("red", nrow(nodes)))
  
  all_rel_pos <- list("Nodes" = nodes, "Colors" = pos_colors, "Borders" = levels(intervals))
  return(all_rel_pos)
}


get_rel_pos_neg_colors_and_border <- function(nodelist){
  nodes <- nodelist
  # define amount of different groups to differentiate by color
  amount <- 5
  neg_colors <- c("#0D47A1", "#1976D2", "#2196F3", "#90CAF9", "#E3F2FD")
  pos_colors <- c("#FFEBEE", "#FFCDD2", "#E57373", "#D32F2F", "#B71C1C")
  
  # calculate intervals
  nodes_positive <- nodes[which(nodes$rel_pos_neg > 0), ]
  nodes_negative <- nodes[which(nodes$rel_pos_neg <= 0), ]
  
  # calculate intervals
  intervals_pos <- cut(nodes_positive$rel_pos_neg, breaks = round(seq(from = min(nodes_positive$rel_pos_neg), to = max(nodes_positive$rel_pos_neg), by = (max(nodes_positive$rel_pos_neg)-min(nodes_positive$rel_pos_neg))/5),5), include.lowest = TRUE)
  intervals_neg <- cut(nodes_negative$rel_pos_neg, breaks = round(seq(from = min(nodes_negative$rel_pos_neg), to = max(nodes_negative$rel_pos_neg), by = -(min(nodes_negative$rel_pos_neg)-max(nodes_negative$rel_pos_neg))/5),5), include.lowest = TRUE)
  
  # map a color to each group
  names(pos_colors) <- levels(intervals_pos)
  names(neg_colors) <- levels(intervals_neg)

  # classify all nodes into groups with different colors
  nodes_negative$group <- intervals_neg
  nodes_negative$color.background <- neg_colors[nodes_negative$group]
  nodes_negative$color.highlight.background <- neg_colors[nodes_negative$group]
  nodes_negative$color.hover.background <- neg_colors[nodes_negative$group]
  
  nodes_positive$group <- intervals_pos
  nodes_positive$color.background <- pos_colors[nodes_positive$group]
  nodes_positive$color.highlight.background <- pos_colors[nodes_positive$group]
  nodes_positive$color.hover.background <- pos_colors[nodes_positive$group]
  
  nodes <- rbind(nodes_negative, nodes_positive)
  
  # all border colors remain unchanged
  nodes$color.border <- c(rep("#0a4ea3", nrow(nodes)))
  nodes$color.highlight.border <- c(rep("red", nrow(nodes)))
  nodes$color.hover.border <- c(rep("red", nrow(nodes)))
  
  all_rel_pos_neg <- list("Nodes" = nodes, "Pos_Colors" = pos_colors, "Neg_Colors" = neg_colors, "Borders" = c(levels(intervals_neg), levels(intervals_pos)))
  return(all_rel_pos_neg)
}

get_degree_colors_and_border <- function(nodelist){
  nodes <- nodelist
  # define amount of different groups to differentiate by color and set the same amount of colors
  amount <- 5
  degree_colors <- c("#E1F5FE", "#B3E5FC", "#29B6F6", "#0288D1", "#01579B")

  # calculate intervals
  if(max(nodes$degree) < 5){
    intervals <- cut(nodes$degree, breaks = seq(from = 0, to = max(nodes$degree), by = 1), dig.lab = 0, include.lowest = TRUE)
  }else{
    intervals <- cut(nodes$degree, breaks = round(seq(from = 0, to = max(nodes$degree), by = max(nodes$degree)/5),0), dig.lab = 0, include.lowest = TRUE)
  }
  # map a color to each group
  names(degree_colors) <- levels(intervals)
  
  # classify all nodes into groups with different colors
  nodes$group <- intervals
  nodes$color.background <- degree_colors[nodes$group]
  nodes$color.highlight.background <- degree_colors[nodes$group]
  nodes$color.hover.background <- degree_colors[nodes$group]
  # all border colors remain unchanged
  nodes$color.border <- c(rep("#0a4ea3", nrow(nodes)))
  nodes$color.highlight.border <- c(rep("red", nrow(nodes)))
  nodes$color.hover.border <- c(rep("red", nrow(nodes)))
  all_degrees <- list("Nodes" = nodes, "Colors" = degree_colors, "Borders" = levels(intervals))
  return(all_degrees)
}

get_default_colors_and_border <- function(nodelist){
  nodes <- nodelist
  
  # initial colors
  nodes$group <- c(rep("A", nrow(nodes)))
  nodes$color.background <- c(rep("#f5f6f7", nrow(nodes)))
  nodes$color.border <- c(rep("#0a4ea3", nrow(nodes)))
  nodes$color.highlight.background <- c(rep("#f5f6f7", nrow(nodes)))
  nodes$color.hover.background <- c(rep("#f5f6f7", nrow(nodes)))
  nodes$color.highlight.border <- c(rep("red", nrow(nodes)))
  nodes$color.hover.border <- c(rep("red", nrow(nodes)))
  
  return(nodes)
}