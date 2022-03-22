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

###############################################
######## function for loading data ############
###############################################

load_graph_from_json <- function(json_graph){
  # turn graph into data frame
  graph_df <- data.frame(json_graph)
  
  ########################
  ### reading in nodes ###
  ########################
  
  nodelist <- as.data.frame(graph_df$data[[1]])
  colnames(nodelist) <- graph_df$columns[[1]]
  #nodelist <- read.csv("E:\\Uni\\Doktor-Goettingen\\Data\\kirc_random_ui_MAX\\kirc_random_nodes_ui_format_100.csv")
  # order data frame by node label from A-Z
  nodelist <- nodelist[order(nodelist$label), ]
  
  # initialize global variables for API / download
  nodelist_table <<- nodelist

  # initialize global variables for node addition
  # vector, containing all names of node features, including rel_pos and rel_pos_neg
  node_features_list <<- nodelist_table[, c(3:ncol(nodelist_table))]
  # all columns of nodelist but with only one row that is initialized with placeholder and zeros for relevances / attributes of a new node
  temporary_added_node_feature <<- nodelist_table[0, ]
  temporary_added_node_feature[nrow(temporary_added_node_feature) + 1, ] <<- c("label_value", "id_value", rep(0, length(colnames(nodelist_table)) - 2))
  temporary_added_node_feature[, 3:ncol(temporary_added_node_feature)] <<- as.numeric(temporary_added_node_feature[, 3:ncol(temporary_added_node_feature)])
  
  ########################
  ### reading in edges ###
  ########################
  
  edgelist <- as.data.frame(graph_df$data[[2]])
  colnames(edgelist) <- graph_df$columns[[2]]
  #edgelist <- read.csv("E:\\Uni\\Doktor-Goettingen\\Data\\kirc_random_ui_MAX\\kirc_random_egdes_ui_format_100.csv")
  # order data frame from A-Z
  edgelist <- edgelist[order(edgelist$from), ]
  
  # initialize global variables for API / download
  edgelist_table <<- edgelist
  
  # initialize global variables for edge addition
  # vector, containing all names of edge features, including rel_pos and rel_pos_neg
  if(ncol(edgelist_table)>3){
    edge_features_list <<- subset(edgelist_table, select = -c(1:3))
    
    # all columns of edgelist but with only one row that is initialized with placeholder and zeros for adding an edge
    temporary_added_edge_feature <<- edgelist_table[0, ]
    temporary_added_edge_feature[nrow(temporary_added_edge_feature) + 1, ] <<- c("from_value", "to_value", "id_value", rep(0, length(colnames(edgelist_table)) - 3))
    temporary_added_edge_feature[, 4:ncol(temporary_added_edge_feature)] <<- as.numeric(temporary_added_edge_feature[, 4:ncol(temporary_added_edge_feature)])
  }else{
    temporary_added_edge_feature <<- edgelist_table[0, ]
    temporary_added_edge_feature[nrow(temporary_added_edge_feature) + 1, ] <<- c("from_value", "to_value", "id_value")
  }
  
  #################################
  ### init modification history ###
  #################################
  
  # in case the user uploads new data after some modifications have already been made, global variables for modification actions need to be empty again
  modification_history <<- data.frame(action = c(0), element = c(0))
  all_deleted_nodes <<- data.frame()
  all_deleted_nodes_edges <<- list()
  all_deleted_edges <<- data.frame()
  all_added_edges <<- data.frame()
  all_added_nodes <<- data.frame()
}

getPatientNames <- function(dataset){
  # get list of patient names
  patient_names <- GET(paste(api_path,"/data/patient_name",sep=""), query = list(dataset_name = dataset))
  stop_for_status(patient_names)
  patient_names <- fromJSON(content(patient_names, type = "text"), flatten = TRUE)
  
  return(patient_names)
}

getNodeRelevances <- function(pat_id, graph_idx){
  # get node relevances from api
  r <- GET(paste(api_path, "/importances/nodes",sep=""), query = list(patient_id = pat_id, graph_id = graph_idx))
  stop_for_status(r)
  node_rel <- data.frame(t(fromJSON(content(r, type = "text"))))
  colnames(node_rel) <- c("node_ids", "rel_pos_node", "rel_pos_neg_node")
  node_rel[["rel_pos_node"]] <- as.numeric(node_rel[["rel_pos_node"]])
  node_rel[["rel_pos_neg_node"]] <- as.numeric(node_rel[["rel_pos_neg_node"]])

  return(node_rel)
}

getEdgeRelevances <- function(pat_id, graph_idx){
  # get node relevances from api
  r <- GET(paste(api_path, "/importances/edges",sep=""), query = list(patient_id = pat_id, graph_id = graph_idx))
  stop_for_status(r)
  edge_rel <- data.frame(t(fromJSON(content(r, type = "text"))))
  colnames(edge_rel) <- c("edge_ids", "rel_pos_edge")
  
  return(edge_rel)
}  

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
 
  if(ncol(table)>3){
    if(nrow(table)>0){
      # replace ids with labels for showing in the table
      for(idx in 1:nrow(table)){
        table$from[idx] <- nodelist_table$label[which(nodelist_table$id==table$from[idx])]
        table$to[idx] <- nodelist_table$label[which(nodelist_table$id==table$to[idx])]
      }
      
      # sort by  label and remove all ID columns for vis
      table <- table[order(table$from), ]
      table <- table[, c(1, 2, 4:ncol(table))]
    }
  }else{
    if(nrow(table)>0){
      # replace ids with labels for showing in the table
      for(idx in 1:nrow(table)){
        table$from[idx] <- nodelist_table$label[which(nodelist_table$id==table$from[idx])]
        table$to[idx] <- nodelist_table$label[which(nodelist_table$id==table$to[idx])]
      }
      
      # sort by  label and remove all ID columns for vis
      table <- table[order(table$from), ]
      table <- table[, c(1, 2)]
    }else{
      table <- table[, c(1, 2)]
    }
  }
  
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
  
  # add relevances to nodelist
  nodelist <- add_rel_to_nodelist(nodelist)
 
  # tooltip for nodes: create html String containing tooltip information: label, rel_pos, rel_pos_neg, degree, then create additional column "title" in nodes
  cols <- which(colnames(nodelist) %in% c("label", "rel_pos", "rel_pos_neg"))
  nodes_tooltip <- cbind(nodelist[, cols], degree)
  colnames(nodes_tooltip) <- c(colnames(nodelist)[cols], "degree")
  
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
  # add relevances to edgelist
  edgelist <- add_rel_to_edgelist(edgelist)

  # tooltip for edges: translate node ids into label names, create html String containing tooltip information: "from-to", rel_pos, rel_pos_neg, then create additional column "title" in edges
  edges_tooltip <- edgelist[, which(colnames(edgelist) %in% c("from","to","rel_pos","rel_pos_neg"))]
  string <- c()
  
  # replace ids with labels for showing in the table
  for(idx in 1:nrow(edges_tooltip)){
    edges_tooltip$from[idx] <- nodelist$label[which(nodelist$id==edges_tooltip$from[idx])]
    edges_tooltip$to[idx] <- nodelist$label[which(nodelist$id==edges_tooltip$to[idx])]
  }
  
  edges_tooltip$from_to <- paste0(as.character(edges_tooltip[1:nrow(edges_tooltip), 1]), " - ", as.character(edges_tooltip[1:nrow(edges_tooltip), 2]))
  
  from_to_col_nr <- which(colnames(edges_tooltip) == "from_to")
  rel_pos_nr <- which(colnames(edges_tooltip) == "rel_pos")
  rel_pos_neg_nr <- which(colnames(edges_tooltip) == "rel_pos_neg")
  
  edges_tooltip <- edges_tooltip[, c(from_to_col_nr, rel_pos_nr, rel_pos_neg_nr), drop = FALSE]
  
  for(index in 1:ncol(edges_tooltip)){
    string <- paste0(string, "<p><b>", colnames(edges_tooltip)[index], ": ", "</b>", as.character(edges_tooltip[1:nrow(edges_tooltip), index]), "</p>")
  }
  
  return(string)
}

#############################################
####### functions for relevances  ###########
#############################################

add_rel_to_nodelist <- function(nodes){
  # sort node_relevances so they match the nodelist
  n_rel <- node_rel[match(nodes[["id"]], node_rel[["node_ids"]]),]
  # now cbind relevances values to nodelist
  nodes <- cbind(nodes, data.frame(n_rel[, 2:ncol(n_rel)]))
  
  # change colnames for rel_pos_node and rel_pos_neg_node if given
  if("rel_pos_node" %in% colnames(nodes)){
    colnames(nodes)[which(colnames(nodes)=="rel_pos_node")] <- "rel_pos"
    nodes$rel_pos <- as.numeric(nodes$rel_pos)
  }
  if("rel_pos_neg_node" %in% colnames(nodes)){
    colnames(nodes)[which(colnames(nodes)=="rel_pos_neg_node")] <- "rel_pos_neg"
    nodes$rel_pos_neg <- as.numeric(nodes$rel_pos_neg)
  }
  
  return(nodes)
}

add_rel_to_edgelist <- function(edges){
  # sort node_relevances so they match the nodelist
  e_rel <- edge_rel[match(edges[["id"]], edge_rel[["edge_ids"]]),]

  # now cbind relevances values to nodelist
  if(ncol(e_rel)>2){
    edges <- cbind(edges, data.frame(e_rel[, 2:ncol(e_rel)]))
  }else{
    edges <- cbind(edges, rel_pos_edge = e_rel$rel_pos_edge)
  }
  
  # change colnames for rel_pos_node and rel_pos_neg_node if given
  if("rel_pos_edge" %in% colnames(edges)){
    colnames(edges)[which(colnames(edges)=="rel_pos_edge")] <- "rel_pos"
    edges$rel_pos <- as.numeric(edges$rel_pos)
  }
  if("rel_pos_neg_edge" %in% colnames(edges)){
    colnames(edges)[which(colnames(edges)=="rel_pos_neg_edge")] <- "rel_pos_neg"
    edges$rel_pos_neg <- as.numeric(edges$rel_pos_neg)
  }

  return(edges)
}

#############################################
########  functions for colors   ############
#############################################

get_rel_colors_for_edge <- function(edgelist){
  # add relevances to nodelist
  edges <- add_rel_to_edgelist(edgelist)
  
  # scale values to [0.1,1]
  values <- edges[["rel_pos"]]
  values <- ((values-min(values))/(max(values)-min(values)))*(1-0.1)+0.1
  
  # generate grayscale
  color <- vector()
  for (i in 1:length(values)) {
    color[i] <- rgb(1 - values[i],1 - values[i],1 - values[i])
  }
  
  return(color)
}

get_rel_pos_colors_and_border <- function(nodelist){
  # add relevances to nodelist
  nodes <- add_rel_to_nodelist(nodelist)
  
  # define amount of different groups to differentiate by color and set the same amount of colors
  amount <- 5
  #pos_colors <- c("#FAFAFA", "#E0E0E0", "#9E9E9E", "#616161", "#212121") #light to dark (left to right)
  pos_colors <- c("#E1F5FE", "#B3E5FC", "#29B6F6", "#0288D1", "#01579B")
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
  # add relevances to nodelist
  nodes <- add_rel_to_nodelist(nodelist)
  
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
    intervals <- cut(nodes$degree, breaks = seq(from = 0, to = max(nodes$degree), by = 1), include.lowest = TRUE)
  }else{
    intervals <- cut(nodes$degree, breaks = round(seq(from = 0, to = max(nodes$degree), by = max(nodes$degree)/5),0), include.lowest = TRUE)
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

##########################################
########  functions for api   ############
##########################################

post_modifications <- function(pat_id, graph_idx, modification_history,all_deleted_nodes,all_added_nodes,all_deleted_edges,all_added_edges, all_deleted_nodes_edges){
  # get amounts of rows in dataframes with saved ids
  counter_deleted_nodes <- 1
  counter_added_nodes <- 1
  counter_deleted_edges <- 1
  counter_added_edges <-1
  
  # first row is initialized with 0, so we need at least 2 rows
  if(nrow(modification_history) > 1){
    
    # iterate over all modifications
    for(i in 2:nrow(modification_history)){
      action <- modification_history[i,1]
      element <- modification_history[i,2]
      # get id of deleted node
      if(action == "deleted" & element == "node"){
        # edges going to this node also need to be removed, BEFORE the node gets deleted
        del_edges <- all_deleted_nodes_edges[[counter_deleted_nodes]]
        # if there are edges to delete, delete them. otherwise do nothing
        if(nrow(del_edges) > 0){
          for(k in 1:nrow(del_edges)){
            r <- DELETE(paste(api_path, "/graph_delete_edge",sep=""), query = list(patient_id = pat_id, graph_id = graph_idx, edge_index_left = del_edges[["from"]][k], edge_index_right = del_edges[["to"]][k]))
            stop_for_status(r)
          }
        }
        # after edges are deleted, delete node
        r <- DELETE(paste(api_path, "/graph_delete_node",sep=""), query = list(patient_id = pat_id, graph_id = graph_idx, deleted_node_id = all_deleted_nodes[["id"]][counter_deleted_nodes], deleted_node_label = all_deleted_nodes[["label"]][counter_deleted_nodes]))
        stop_for_status(r)
        counter_deleted_nodes = counter_deleted_nodes + 1
      }
      # get id of deleted edge
      if(action == "deleted" & element == "edge"){
        r <- DELETE(paste(api_path, "/graph_delete_edge",sep=""), query = list(patient_id = pat_id, graph_id = graph_idx, edge_index_left = all_deleted_edges[["from"]][counter_deleted_edges], edge_index_right = all_deleted_edges[["to"]][counter_deleted_edges]))
        stop_for_status(r)
        counter_deleted_edges = counter_deleted_edges + 1
      }
      # get id label and values of added node
      if(action == "added" & element == "node"){
        node <- all_added_nodes[counter_added_nodes,]
        body <- list(patient_id = pat_id, graph_id = graph_idx, label = node[["label"]], id = node[["id"]], features = as.numeric(node[!colnames(node) %in% c("id","label")]))
        
        r <- POST(paste(api_path, "/add_node_json",sep=""), body = body, encode = "json")
        # throw error if status returns something else than 200 (so if it didnt work)
        stop_for_status(r)
        counter_added_nodes = counter_added_nodes + 1
      }
      # get id, label and values of added edge
      if(action == "added" & element == "edge"){
        edge <- all_added_edges[counter_added_edges,]
        body <- list(patient_id = pat_id, graph_id = graph_idx, features = as.numeric(edge[!colnames(edge) %in% c("id","from","to")]), new_edge_index_left = edge[["from"]], new_edge_index_right = edge[["to"]])
        
        r <- POST(paste(api_path, "/add_edge_json",sep=""), body = body, encode = "json")
        # throw error if status returns something else than 200 (so if it didnt work)
        stop_for_status(r)
        counter_added_edges = counter_added_edges + 1
      }
      
  
    }
  }
}

get_max_graphs <- function(pat_id){
  r <- GET(paste(api_path, "/data/highest_graph_id",sep=""), query = list(patient_id = pat_id))
  stop_for_status(r)
  max_graph <- as.numeric(fromJSON(content(r, type = "text"), flatten = TRUE))
  
  return(max_graph)
}

delete_graphs <- function(pat_id, graph_idx, max_graph){
  for(i in (graph_idx+1):max_graph){
    # delete latest graph, the user will never be able to return to it
    r <- DELETE(paste(api_path, "/data/graph/",sep=""), query = list(patient_id = pat_id, graph_id = i))
    stop_for_status(r)
  }
}

##########################################
########  function for warning   #########
##########################################

ovwriting_warning <- function(graph_idx, max_graph){
  if(graph_idx != max_graph){
    if(max_graph-graph_idx>1){
      message <- HTML(paste0("<span style='color:red; font-size:14px'> <br/> Warning: If you perform modifications on this graph and save them by pressing predict/retrain the following graphs well be overwritten: ", graph_idx+1,"-", max_graph ,"</span>"))
    }else{
      message <- HTML(paste0("<span style='color:red; font-size:14px'> <br/> Warning: If you perform modifications on this graph and save them by pressing predict/retrain the following graph well be overwritten: ", graph_idx+1,"</span>"))
    }
  }else{
      message <- HTML(" ")
  }
  
  return(message)
}

##########################################
########   functions for init    #########
##########################################

initGlobalVars <- function(){
  # global variables that contain the modified graph data. These will be downloaded by the user (and would be returned to an API)
  nodelist_table <<- data.frame()
  edgelist_table <<- data.frame()
  
  # this contains all selected nodes and their corresponding edges
  small_nodelist_for_table <<- data.frame()
  small_edgelist <<- data.frame()
  
  # this contains more than the selected nodes, because we also need to visualize the neighboring nodes
  small_nodelist_for_graph <<- data.frame()
  
  # empty data tables that will be used to save user modification actions and thus allow the undo function
  modification_history <<- data.frame(action = c(0), element = c(0))
  all_deleted_nodes <<- data.frame()
  all_deleted_nodes_edges <<- list()
  all_deleted_edges <<- data.frame()
  all_added_edges <<- data.frame()
  all_added_nodes <<- data.frame()
  
  # global variable for temporary use during node addition
  node_features_list <<- data.frame()
  temporary_added_node_feature <<- data.frame()
  
  # global variable for temporary use during edge addition
  edge_features_list <<- data.frame()
  temporary_added_edge_feature <<- data.frame()
  
  # global variable that indexes the graphs (this always get +1 if predict or retrain is pressed)
  graph_idx <<- 0
}

#############################################################
########  functions for adding/deleting node/edge   #########
#############################################################

calculate_nodes_that_can_have_edges_added_to_them <- function(nodes, edges){
  
  nodes_that_can_be_connected <- c()
  for(id in nodes$id){
    amount_edges <- (length(which(edges$to == id)) + length(which(edges$from == id)))
    
    # if there are 3 nodes in the graph each node can have a total of 3-1 edges
    if(amount_edges < (nrow(nodes) - 1)){
      nodes_that_can_be_connected <- c(nodes_that_can_be_connected, nodes$label[which(nodes$id==id)])
    }
  }
  
  return(nodes_that_can_be_connected)
}

calculate_nodes_that_can_be_connected_to_selected_node <- function(first_node, nodes, edges){
  selected_node_for_addition <- nodes[which(nodes$label == first_node), 2]
  connected_nodes_add <- unique(c(
    edges$to[which(edges$from == selected_node_for_addition)],
    edges$from[which(edges$to == selected_node_for_addition)]
  ))
  connected_nodes_labels_add <- c()
  for (index in 1:length(connected_nodes_add)) {
    connected_nodes_labels_add <- c(connected_nodes_labels_add, nodes$label[nodes$id == connected_nodes_add[index]])
  }
  not_connected_nodes_labels <- setdiff(nodes$label, connected_nodes_labels_add)
  not_connected_nodes_labels <- not_connected_nodes_labels[-c(which(not_connected_nodes_labels == first_node))]
  not_connected_nodes_labels <- sort(not_connected_nodes_labels)
  
  return(not_connected_nodes_labels)
}

first_node_of_connections_that_can_be_removed <- function(nodes, edges){
  nodes_with_edges <- unique(c(edges$from, edges$to))
  node_labels2 <- c()
  for (index in 1:length(nodes_with_edges)) {
    next_node <- nodes$label[which(nodes$id == nodes_with_edges[index])]
    node_labels2 <- c(node_labels2, next_node)
  }
  # only let the user select the nodes that were selected (not all that are shown in the graph vis)
  node_labels <- nodes$label[nodes$label %in% node_labels2]
  node_labels <- sort(node_labels)
  
  return(node_labels)
}

second_node_of_connections_that_can_be_removed <- function(first_node, nodes, edges){
  # get node id from selected node label
  selected_node <- nodes[which(nodes$label == first_node), 2]
  # get connected nodes
  connected_nodes <- unique(c(
    edges$to[which(edges$from == selected_node)],
    edges$from[which(edges$to == selected_node)]
  ))
  connected_nodes_labels <- c()
  for(index in 1:length(connected_nodes)) {
    connected_nodes_labels <- c(connected_nodes_labels, nodes$label[nodes$id == connected_nodes[index]])
  }
  connected_nodes_labels <- sort(connected_nodes_labels)
  
  return(connected_nodes_labels)  
}

#########################################
########  function for sorting  #########
#########################################

sort_by_user_selection <- function(sort_by, nodes, degree){
  if(sort_by == "degree_highlow"){
    nodelist_for_table <- nodes[order(cbind(nodes,degree)[["degree"]], decreasing = TRUE),]
  }
  if(sort_by == "degree_lowhigh"){
    nodelist_for_table <- nodes[order(cbind(nodes,degree)[["degree"]], decreasing = FALSE),]
  }
  if(sort_by == "rel_pos_highlow"){
    # first change the order of node_ids so that it is the same as in the data.frame of node_relevances
    nodelist_table <- nodes[match(node_rel[["node_ids"]], nodes[["id"]]),]
    # now that the nodes are in the same order just bind the relevances to the nodes_dataframe and sort by the relevances
    nodelist_for_table <- nodelist_table[order(cbind(nodelist_table,node_rel)[["rel_pos_node"]], decreasing = TRUE),]
  }
  if(sort_by == "rel_pos_lowhigh"){
    # first change the order of node_ids so that it is the same as in the data.frame of node_relevances
    nodelist_table <- nodes[match(node_rel[["node_ids"]], nodes[["id"]]),]
    # now that the nodes are in the same order just bind the relevances to the nodes_dataframe and sort by the relevances
    nodelist_for_table <- nodelist_table[order(cbind(nodelist_table,node_rel)[["rel_pos_node"]], decreasing = FALSE),]
  }
  if(sort_by == "rel_pos_neg_highlow"){
    # first change the order of node_ids so that it is the same as in the data.frame of node_relevances
    nodelist_table <- nodes[match(node_rel[["node_ids"]], nodes[["id"]]),]
    # now that the nodes are in the same order just bind the relevances to the nodes_dataframe and sort by the relevances
    nodelist_for_table <- nodelist_table[order(cbind(nodelist_table,node_rel)[["rel_pos_neg_node"]], decreasing = TRUE),]
  }
  if(sort_by == "rel_pos_neg_lowhigh"){
    # first change the order of node_ids so that it is the same as in the data.frame of node_relevances
    nodelist_table <- nodes[match(node_rel[["node_ids"]], nodes[["id"]]),]
    # now that the nodes are in the same order just bind the relevances to the nodes_dataframe and sort by the relevances
    nodelist_for_table <- nodelist_table[order(cbind(nodelist_table,node_rel)[["rel_pos_neg_node"]], decreasing = FALSE),]
  }
  
  return(nodelist_for_table)
}

##################################################################
########  function for creating smaller node/edge lists  #########
##################################################################

calculate_small_tables <- function(nodes, edges, radio, slider){
  ### create sub node table ###
  degree <- c() # initialze "degree" vector
  for(i in 1:length(nodes$id)){
    degree[i] <- nrow(edges[which(edges$from == nodes$id[i]),]) + nrow(edges[which(edges$to == nodes$id[i]),])
  } 
  degree <- data.frame(degree)
  
  # sort nodelist by XAI values (method is selected by radiobutton)
  nodelist_for_table <- sort_by_user_selection(radio, nodes, degree)
  
  # only show the amount of nodes selected by the sliding bar
  nodelist_for_table <- nodelist_for_table[1:slider,] 
  
  # for plotting the nodes, we also need the nodes they are connected to
  nodelist_for_graph <- nodelist_for_table #we want to make the table for plotting bigger
  # iterate over our nodes
  for(ids in nodelist_for_table$id){
    # check if node is in edgelist$from
    if(length(which(edges$from == ids)) > 0){
      # check if the connected nodes are in our nodelist
      for(i in which(edges$from == ids)){
        # if node id is not yet in table, rbind it to table and save in nodelist_for_graph
        if(!(edges$to[i] %in% nodelist_for_graph$id)){
          nodelist_for_graph <- rbind(nodelist_for_graph, nodes[which(nodes$id==edges$to[i]), ])
        }
      }
    }
    # check if node is in edgelist$to
    if(length(which(edges$to == ids)) > 0){
      # check if the connected nodes are in our nodelist
      for(i in which(edges$to == ids)){
        # if node id is not yet in table, rbind it to table and save in nodelist_for_graph
        if(!(edges$from[i] %in% nodelist_for_graph$id)){
          nodelist_for_graph <- rbind(nodelist_for_graph, nodes[which(nodes$id==edges$from[i]), ])
        }
      }
    }           
  }
  # save in global variable
  small_nodelist_for_table <<- nodelist_for_table
  small_nodelist_for_graph <<- nodelist_for_graph
  
  ### create sub edge table ###    
  
  # vector to save the rows that contain a node we want
  rows <- c()
  
  # iterate over our nodes
  for(ids in nodelist_for_table$id){
    # check if there is an entry in edgelist$from
    if(length(which(edges$from == ids)) > 0){
      rows <- append(rows, which(edges$from == ids))
    }
    # check if there is an entry in edgelist$to
    if(length(which(edges$to == ids)) > 0){
      rows <- append(rows, which(edges$to == ids))
    }               
  }
  
  # some rows could have been counted double so only save unique values
  rows <- unique(rows) 
  
  # save in global variable
  small_edgelist <<- edges[rows,]
  
}
