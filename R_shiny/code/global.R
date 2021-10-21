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
  
  
  for (index in 1:ncol(edges_tooltip)) {
    string <- paste0(string, "<p><b>", colnames(edges_tooltip)[index], ": ", "</b>", as.character(edges_tooltip[1:nrow(edges_tooltip), index]), "</p>")
  }
  
  return(string)
}