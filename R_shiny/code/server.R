server <- function(input, output, session) {
  ##################################
  ####### global variables #########
  ##################################
  
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
  
  # global variable that monitors if sorting data py rel_pos/rel_pos_neg is enabled or not (is disabled when no relevance values are given)
  sorting <<- 0
  
  # global variable that indexes the graphs (this always get +1 if predict or retrain is pressed)
  graph_idx <<- 0
  
  ##################################
  ######### upload nodes ###########
  ##################################
  
  # expand capacity of shiny upload to 30 MB
  options(shiny.maxRequestSize = 30 * 1024^2)
  
  
  # information on required structure for the nodelist ----------------------------------
  info_table_nodes <- data.frame('labels' = c("Unique name for each node"), 'id' = c("id of the node"), 'Optional: rel_pos' = c("xAI method computing only positive relevance values"), 'Optional: rel_pos_neg' = c("xAI method computing positive and negative relevance values"), 'Optional...' = c("Additional columns with node attributes (numeric)"))
  output$required_structure_nodelist <- renderTable({
    info_table_nodes
  })
  
  # validity checks on input file, containing data on nodes ----------------------------------
  # @return input file named nodelist in case the validity checks are successful, otherwise in case of an error return NULL
  uploading_nodes <- eventReactive(input$upload_nodes, {
    
    # read user input
    nodelist <- read.csv(input$upload_nodes$datapath)
    
    # check 1: containing required columns
    if (!("label" %in% colnames(nodelist)) || !("id" %in% colnames(nodelist))) {
      output$error_upload_nodes <- renderUI({
        HTML("<span style='color:red; font-size:14px'> <br/> ERROR: Required column(s) missing. Make sure that your data contains columns named 'label', 'id'! </span>")
      })
      
      # disable third tab
      shinyjs::js$disableTab("Interact")
      
      return(NULL)
    } else {
      
      # check 2: bring columns in the correct order, drop = FALSE ensures that if only one column remains, the data frame is still a data frame
        remove_columns <- c("label", "id", colnames(nodelist)[which(colnames(nodelist) %in% c("rel_pos", "rel_pos_neg"))])
        nodelist_only_features <- nodelist[, !names(nodelist) %in% remove_columns, drop = FALSE]
        target_column_order <- c(remove_columns, colnames(nodelist_only_features))
        nodelist <- nodelist[, target_column_order]
      
      
      
      # check 3a: unique label
      if (length(unique(nodelist$label)) != nrow(nodelist)) {
        output$error_upload_nodes <- renderUI({
          HTML("<span style='color:red; font-size:14px'> <br/> ERROR: The column 'label' contains duplicates. Make sure that its data points are unique! </span>")
        })
        
        # disable third tab
        shinyjs::js$disableTab("Interact")
        
        return(NULL)
      } else {
        
        # check 3b: unique id
        if (length(unique(nodelist$id)) != nrow(nodelist)) {
          output$error_upload_nodes <- renderUI({
            HTML("<span style='color:red; font-size:14px'> <br/> ERROR: The column 'id' contains duplicates. Make sure that its data points are unique! </span>")
          })
          
          # disable third tab
          shinyjs::js$disableTab("Interact")
          
          return(NULL)
        } else {
          
          # check 4: all node attributes require numeric values
          if (!is.numeric(unlist(nodelist[, !names(nodelist) %in% remove_columns], use.names = FALSE))) {
            output$error_upload_nodes <- renderUI({
              HTML("<span style='color:red; font-size:14px'> <br/> ERROR: Wrong data format of the feature values. Make sure that column(s) of node attributes only contain numeric values! </span>")
            })
            
            # disable third tab
            shinyjs::js$disableTab("Interact")
            
            return(NULL)
          } else {
            
            # check 5: rel_pos should only contain values >= 0
            if("rel_pos" %in% colnames(nodelist)){
              for (index in 1:nrow(nodelist)) {
                if (nodelist$rel_pos[index] < 0) {
                  output$error_upload_nodes <- renderUI({
                    HTML("<span style='color:red; font-size:14px'> <br/> ERROR: In the column 'rel_pos' are negative values. Make sure that it only contains values greater than or equal to zero! </span>")
                  })
                  
                  # disable third tab
                  shinyjs::js$disableTab("Interact")
                  
                  return(NULL)
                }
              }
            }
            
            # check 6: the graph must be homogeneous, all nodes of the same type
            if (anyNA.data.frame(nodelist)) {
              output$error_upload_nodes <- renderUI({
                HTML("<span style='color:red; font-size:14px'> <br/> ERROR: The graph has nodes of different types. You must enter a homogeneous graph with all nodes of the same type! </span>")
              })
              
              # disable third tab
              shinyjs::js$disableTab("Interact")
              
              return(NULL)
            } else {
              
              # inform user that upload was successful
              output$error_upload_nodes <- renderUI({
                HTML(paste0("<p style = 'color:green; font-size:14px'>", "Upload successful.", br(), "The data contains ", "<b>", nrow(nodelist), " nodes", "</b>", " and ", "<b>", ncol(nodelist) - 4, " attribute(s)", "</b>", ".", "</p>"))
              })
              
              # order data frame by node label from A-Z
              nodelist <- nodelist[order(nodelist$label), ]
              
              # initialize global variables for API / download
              nodelist_table <<- nodelist
              
              # update selection of patients by name
              names <- colnames(nodelist_table)[!colnames(nodelist_table) %in% c("id", "label", "rel_pos", "rel_pos_neg")]
              updateSelectizeInput(session, "choose_patient_own_data", choices = names, server = TRUE)
              
              # initialize global variables for node addition
              # vector, containing all names of node features, including rel_pos and rel_pos_neg
              node_features_list <<- nodelist_table[, c(3:ncol(nodelist_table))]
              # all columns of nodelist but with only one row that is initialized with placeholder and zeros for relevances / attributes of a new node
              temporary_added_node_feature <<- nodelist_table[0, ]
              temporary_added_node_feature[nrow(temporary_added_node_feature) + 1, ] <<- c("label_value", "id_value", rep(0, length(colnames(nodelist_table)) - 2))
              temporary_added_node_feature[, 3:ncol(temporary_added_node_feature)] <<- as.numeric(temporary_added_node_feature[, 3:ncol(temporary_added_node_feature)])
              
              # update max Slider value to amount of nodes
              max = length(nodelist_table[[1]])
              updateSliderInput(session, "slider", max=max)
              
              # disable third tab
              shinyjs::js$disableTab("Interact")
              
              return(nodelist)
            }
          }
        }
      }
    }
  })
  
  # preview uploaded nodelist in a data table ----------------------------------
  output$preview_nodes <- renderDataTable({
    table <- uploading_nodes()
    datatable(
      table,
      rownames = FALSE,
      extensions = "FixedColumns",
      options = list(scrollX = TRUE),
    )
  })
  
  # inform user that data on edges needs to be re-entered if data on nodes has been changed ----------------------------------
  observeEvent(input$upload_nodes, {
    if (!is.null(uploading_edges())) {
      output$error_upload_edges <- renderUI({
        HTML("<span style='color:red; font-size:14px'> <br/> ERROR: The data on nodes has been changed. Re-upload data on edges! </span>")
      })
    }
  })
  
  ##################################
  ######### upload edges ###########
  ##################################
  
  # information on required structure for the edgelist ----------------------------------
  info_table_edges <- data.frame('from' = c("id of the node"), 'to' = c("id of the connected node"), 'id' = c("id of the edge"), 'Optional: rel_pos' = c("xAI method computing only positive relevance values"), 'Optional: rel_pos_neg' = c("xAI method computing positive and negative relevance values"), 'Optional...' = c("Additional columns with edge attributes (numeric)"))
  output$required_structure_edgelist <- renderTable({
    info_table_edges
  })
  
  
  # reactive expression provides the information of whether nodelist has been uploaded successfully ----------------------------------
  # @ return "false" as long as uploading_nodes function returns NULL and "true" when nodelist was uploaded
  output$nodelist_uploaded <- reactive({
    return(!is.null(uploading_nodes()))
  })
  outputOptions(output, "nodelist_uploaded", suspendWhenHidden = FALSE)
  
  
  # validity checks on input file, containing data on edges ----------------------------------
  # @return input file named edgelist in case the validity checks are successful, otherwise in case of an error return NULL
  uploading_edges <- eventReactive(input$upload_edges, {
    # read user input
    edgelist <- read.csv(input$upload_edges$datapath)
    
    # check 1: containing required columns
    if (!("from" %in% colnames(edgelist)) || !("to" %in% colnames(edgelist)) || !("id" %in% colnames(edgelist))) {
      output$error_upload_edges <- renderUI({
        HTML("<span style='color:red; font-size:14px'> <br/> ERROR: Required column(s) missing. Make sure that your data contains columns named 'from', 'to', 'id'. </span>")
      })
      
      # disable third tab
      shinyjs::js$disableTab("Interact")
      
      return(NULL)
    } else {
      
      # check 2: bring columns in the correct order, drop = FALSE ensures that if only one column remains, the data frame is still a data frame
      remove_columns <- c("from", "to", "id", colnames(edgelist)[which(colnames(edgelist) %in% c("rel_pos", "rel_pos_neg"))])
      edgelist_only_features <- edgelist[, !names(edgelist) %in% remove_columns, drop = FALSE]
      target_column_order <- c(remove_columns, colnames(edgelist_only_features))
      edgelist <- edgelist[, target_column_order]
    
      # check 3: unique id
      if (length(unique(edgelist$id)) != nrow(edgelist)) {
        output$error_upload_edges <- renderUI({
          HTML("<span style='color:red; font-size:14px'> <br/> ERROR: The column 'id' contains duplicates. Make sure that its data points are unique! </span>")
        })
        
        # disable third tab
        shinyjs::js$disableTab("Interact")
        
        return(NULL)
      } else {
        
        # check 4: all edge attributes require numeric values
        if (!is.numeric(unlist(edgelist[, (length(remove_columns)+1):ncol(edgelist)], use.names = FALSE))) {
          output$error_upload_edges <- renderUI({
            HTML("<span style='color:red; font-size:14px'> <br/> ERROR: Wrong data format of the feature values. Make sure that column(s) of edge attributes only contain numeric values! </span>")
          })
          # disable third tab
          shinyjs::js$disableTab("Interact")

          return(NULL)
        } else {
          
          # check 5: rel_pos should only contain values >= 0
          if("rel_pos" %in% colnames(edgelist)){
            for (index in 1:nrow(edgelist)) {
              if (edgelist$rel_pos[index] < 0) {
                output$error_upload_edges <- renderUI({
                  HTML("<span style='color:red; font-size:14px'> <br/> ERROR: In the column 'rel_pos' are negative values. Make sure that it only contains values greater than or equal to zero! </span>")
                })
                
                # disable third tab
                shinyjs::js$disableTab("Interact")
                
                return(NULL)
              }
            }
          }
          
          # check 6: no multigraph allowed, there should be only one edge between two nodes
          for (index in 1:nrow(edgelist)) {
            if (nrow(edgelist[!duplicated(cbind(pmin(edgelist$from, edgelist$to), pmax(edgelist$from, edgelist$to))), ]) != nrow(edgelist)) {
              output$error_upload_edges <- renderUI({
                HTML("<span style='color:red; font-size:14px'> <br/> ERROR: Data of a multigraph has been entered. Make sure that there is only one edge between two nodes! </span>")
              })
              
              # disable third tab
              shinyjs::js$disableTab("Interact")
              
              return(NULL)
            }
          }
          
          # check 7: the edgelist should only contain edges for which a node exists, in this case node ids of edgelist are a subset of node ids in nodelist
          unique_edgelist_from <- unique(edgelist$from)
          unique_edgelist_to <- unique(edgelist$to)
          unique_node_id_edgelist <- unique(c(unique_edgelist_from, unique_edgelist_to))
          
          if (!is.subset(unique_node_id_edgelist, nodelist_table$id)) {
            output$error_upload_edges <- renderUI({
              HTML("<span style='color:red; font-size:14px'> <br/> ERROR: Some edges refer to a node that is not mentioned in the list of nodes. Make sure that all node ids in 'from' and 'to' appear in the list of nodes (upload 1)! </span>")
            })
            
            # disable third tab
            shinyjs::js$disableTab("Interact")
            
            return(NULL)
          } else {
            
            # check 8: the graph must be homogeneous, all edges of the same type
            if (anyNA.data.frame(edgelist)) {
              output$error_upload_edges <- renderUI({
                HTML("<span style='color:red; font-size:14px'> <br/> ERROR: The graph has edges of different types. You must enter a homogeneous graph with all edges of the same type! </span>")
              })
              
              # disable third tab
              shinyjs::js$disableTab("Interact")
              
              return(NULL)
            } else {
              
              # inform user that edges upload was successful
              output$error_upload_edges <- renderUI({
                HTML(paste0("<p style = 'color:green; font-size:14px'>", "Upload successful.", br(), "The data contains ", "<b>", nrow(edgelist), " edges", "</b>", " and ", "<b>", ncol(edgelist) - length(remove_columns), " attribute(s)", "</b>", ".", "</p>"))
              })
              
              # order data frame from A-Z
              edgelist <- edgelist[order(edgelist$from), ]
              
              # initialize global variables for API / download
              edgelist_table <<- edgelist
              
              # initialize global variables for edge addition
              # vector, containing all names of edge features, including rel_pos and rel_pos_neg
              edge_features_list <<- subset(edgelist_table, select = -c(1:3))
              # all columns of edgelist but with only one row that is initialized with placeholder and zeros for adding an edge
              temporary_added_edge_feature <<- edgelist_table[0, ]
              temporary_added_edge_feature[nrow(temporary_added_edge_feature) + 1, ] <<- c("from_value", "to_value", "id_value", rep(0, length(colnames(edgelist_table)) - 3))
              temporary_added_edge_feature[, 4:ncol(temporary_added_edge_feature)] <<- as.numeric(temporary_added_edge_feature[, 4:ncol(temporary_added_edge_feature)])
              
              # in case the user uploads new data after some modifications have already been made, global variables for modification actions need to be empty again
              modification_history <<- data.frame(action = c(0), element = c(0))
              all_deleted_nodes <<- data.frame()
              all_deleted_nodes_edges <<- list()
              all_deleted_edges <<- data.frame()
              all_added_edges <<- data.frame()
              all_added_nodes <<- data.frame()
              
              # clear any printed error messages on the UI
              output$info_change <- renderUI({
                HTML(" ")
              })
              
              output$error_only_zeros <- renderUI({
                HTML(" ")
              })
              
              output$error_add_node <- renderUI({
                HTML(" ")
              })
              
              output$error_add_edge <- renderUI({
                HTML(" ")
              })
              
              # empty all text input fields of edge and node addition
              updateNumericInput(session, "edgefeature_value", value = 0)
              updateTextInput(session, "new_node_label", value = "", placeholder = "e.g. ABCC2")
              updateNumericInput(session, "nodefeature_value", value = 0)
              
              # reset the select Input of color nodes by
              updateSelectInput(session, "color_nodes", selected = "one color (default)")
              
              # enable third tab
              shinyjs::js$enableTab("Interact")

              return(edgelist)
            }
          }
        }
      }
    }
  })
  
  # preview uploaded edgelist in data table ----------------------------------
  output$preview_edges <- renderDataTable({
    table <- uploading_edges()
    datatable(
      table,
      rownames = FALSE,
      extensions = "FixedColumns",
      options = list(scrollX = TRUE),
    )
  })
  
  # reactive expression provides the information of whether edgelist has been uploaded successfully ----------------------------------
  # @ return "false" as long as uploading_edges function returns NULL and "true" when edgelist was uploaded
  output$edgelist_uploaded <- reactive({
    return(!is.null(uploading_edges()))
  })
  outputOptions(output, "edgelist_uploaded", suspendWhenHidden = FALSE)
  
  #######################################
  ######## upload whole dataset #########
  #######################################
  
  observeEvent(input$upload_dataset, {
      # get list of patient names
      patient_names <- GET(paste(api_path, "/data/patient_name",sep=""), query = list(dataset_name = input$choose_a_dataset))
      stop_for_status(patient_names)
      patient_names <- fromJSON(content(patient_names, type = "text"),flatten = TRUE)
      
      # update selection of patients by name
      updateSelectizeInput(session, "choose_patient", choices = patient_names, server = TRUE)
  })
  
   observeEvent(ignoreInit = TRUE, input$choose_patient, {
     # get patient id
     pat_id <- as.numeric(gsub("Patient ", "", input$choose_patient))
     
     # get graph of selected dataset and patient
     r <- GET(paste(api_path, "/data/dataset",sep=""), query = list(dataset_name = input$choose_a_dataset, patient_id = pat_id, graph_id = graph_idx))
     stop_for_status(r)
     graph <- fromJSON(content(r, type = "text"))
     
     load_graph_from_json(graph)
     
     # update max Slider value to amount of nodes
     max = length(nodelist_table[[1]])
     updateSliderInput(session, "slider", max=max)
     
     # set graph ID back to zero if new patient is selected
     graph_idx <<- 0
  
     # clear any printed error messages on the UI
     output$info_change <- renderUI({
       HTML(" ")
     })
  
     output$error_only_zeros <- renderUI({
       HTML(" ")
     })
  
     output$error_add_node <- renderUI({
       HTML(" ")
     })
  
     output$error_add_edge <- renderUI({
       HTML(" ")
     })
  
     # empty all text input fields of edge and node addition
     updateNumericInput(session, "edgefeature_value", value = 0)
     updateTextInput(session, "new_node_label", value = "", placeholder = "e.g. ABCC2")
     updateNumericInput(session, "nodefeature_value", value = 0)
  
     # reset the select Input of color nodes by
     updateSelectInput(session, "color_nodes", selected = "one color (default)")
  
     calculate_smaller_node_and_edge_list()
  
     # enable third tab
     shinyjs::js$enableTab("Interact")
   })
  
  ###################################################
  ######## dis/enable/update tabs/functions #########
  ###################################################
  
  # initially disable Interact tab by start of the shiny App ----------------------------------
  observe({
    if (is.null(input$upload_nodes)) {
      shinyjs::js$disableTab("Interact")
    }
  })
  
  # update radio buttons based on if rel_pos_neg or rel_pos is present
  observeEvent(ignoreInit = T,c(input$upload_edges, input$choose_patient),{
  if(!("rel_pos_neg" %in% colnames(nodelist_table) & !("rel_pos" %in% colnames(nodelist_table)))){
      shinyjs::disable("radio")
      
      sorting <<- 0
      
    }
    if(!("rel_pos" %in% colnames(nodelist_table)) & ("rel_pos_neg" %in% colnames(nodelist_table))){
      shinyjs::enable("radio")
       updateRadioButtons(session, "radio",
                          choices = list("rel_pos_neg (high to low)" = "rel_pos_neg_highlow",
                                         "rel_pos_neg (low to high)" = "rel_pos_neg_lowhigh"), 
                          selected = "rel_pos_neg_highlow")
      sorting <<- 1
    }
    if(("rel_pos" %in% colnames(nodelist_table)) & !("rel_pos_neg" %in% colnames(nodelist_table))){
      shinyjs::enable("radio")
      updateRadioButtons(session, "radio",
                         choices = list("rel_pos (high to low)" = "rel_pos_highlow",
                                        "rel_pos (low to high)" = "rel_pos_lowhigh"), 
                         selected = "rel_pos_highlow")
      sorting <<- 1
    }
    if(("rel_pos" %in% colnames(nodelist_table)) & ("rel_pos_neg" %in% colnames(nodelist_table))){
      shinyjs::enable("radio")
      updateRadioButtons(session, "radio",
                         choices = list("rel_pos (high to low)" = "rel_pos_highlow",
                                        "rel_pos (low to high)" = "rel_pos_lowhigh",
                                        "rel_pos_neg (high to low)" = "rel_pos_neg_highlow",
                                        "rel_pos_neg (low to high)" = "rel_pos_neg_lowhigh"), 
                         selected = "rel_pos_highlow")
      sorting <<- 1
    }
  })
  
  # disable selection of uploading own dataset
  shinyjs::disable("radio_input_type")
  
  ############################
  ######### Retrain ##########
  ############################
  
  observeEvent(input$retrain, {
    # get patient id
    pat_id <- as.numeric(gsub("Patient ", "", input$choose_patient))
    
    # post modification history to API if changes are made
    if(nrow(modification_history)>1){
      
      # create deepcopy of graph in the backend
      r <- POST(paste(api_path, "/deep_copy",sep=""), body = list(patient_id = pat_id, graph_id = graph_idx), encode = "json")
      # throw error if status returns something else than 200 (so if it didnt work)
      stop_for_status(r)
      
      # update graph_idx, so that changes now get made on the deep_copy and not the modification.
      graph_idx <<- graph_idx +1
      
      # send modifications to API
      post_modifications(pat_id, graph_idx, modification_history, all_deleted_nodes, all_added_nodes, all_deleted_edges, all_added_edges, all_deleted_nodes_edges)
    }
    
    
    # get retrained graph values
    r <- GET(paste(api_path, "/nn_retrain",sep=""), query = list(patient_id = pat_id, graph_id = graph_idx))
    stop_for_status(r)
    graph <- fromJSON(content(r, type = "text"))
    load_graph_from_json(graph)
    
    # reset the select Input of color nodes by
    updateSelectInput(session, "color_nodes", selected = "one color (default)")
    
    calculate_smaller_node_and_edge_list()
  })
  
  ############################
  ######### Predict ##########
  ############################
  
  observeEvent(input$predict, {
    # get patient id
    pat_id <- as.numeric(gsub("Patient ", "", input$choose_patient))
    
    # post modification history to API if changes are made
    if(nrow(modification_history)>1){
      
      # create deepcopy of graph in the backend
      r <- POST(paste(api_path, "/deep_copy",sep=""), body = list(patient_id = pat_id, graph_id = graph_idx), encode = "json")
      # throw error if status returns something else than 200 (so if it didnt work)
      stop_for_status(r)
      
      # update graph_idx, so that changes now get made on the deep_copy and not the modification.
      graph_idx <<- graph_idx +1
      
      # send modifications to API
      post_modifications(pat_id, graph_idx, modification_history, all_deleted_nodes, all_added_nodes, all_deleted_edges, all_added_edges, all_deleted_nodes_edges)   
    }
    
    # get retrained graph values
    r <- GET(paste(api_path, "/nn_predict",sep=""), query = list(patient_id = pat_id, graph_id = graph_idx))
    stop_for_status(r)
    graph <- fromJSON(content(r, type = "text"))
    load_graph_from_json(graph)
    
    # reset the select Input of color nodes by
    updateSelectInput(session, "color_nodes", selected = "one color (default)")
    
    calculate_smaller_node_and_edge_list()
  })
  

  ##################################
  ######### Network Graph ##########
  ##################################
  
  
  # observe upload of edges to update the graph if new data was uploaded ----------------------------------
  observeEvent(c( 
    # the events that trigger this
    input$upload_edges,
    input$slider,
    input$radio,
    input$choose_patient,
    input$retrain,
    input$predict
    ), {
    
    # create graph element
    output$graph <- renderVisNetwork({
      # read data on nodes and edges
      complete_nodelist <- nodelist_table
      complete_edgelist <- edgelist_table
      
      nodes <- small_nodelist_for_graph
      edges <- small_edgelist
      
      nodes$title <- update_node_tooltip(nodes, complete_edgelist)
      
      # plot the graph if edges are given (or left after edge deletions)
      if(nrow(edges) != 0){
        # update edge tooltip title (only if edges are given)
        edges$title <- update_edge_tooltip(complete_nodelist, edges)
        
        set.seed(3414) # set seed so the graph always looks the same for the same nodes and edges
        visNetwork(nodes, edges) %>%
          visInteraction(zoomView = TRUE, navigationButtons = TRUE, multiselect = TRUE, hover = TRUE) %>%
          # long click on nodes to select multiple nodes or by "Ctrl" + Click
          visIgraphLayout(layout = "layout_with_fr") %>%
          visNodes(
            size = 45,
            color = list(background = "#f5f6f7", border = "#0a4ea3", highlight = list(background = "#f5f6f7", border = "red"), hover = list(background = "#f5f6f7", border = "red"))
          ) %>% 
          visEdges(
            width = 5, hoverWidth = 3,
            color = list(color = "#0a4ea3", highlight = "red", hover = "red")
          ) %>%
          visOptions(
            highlightNearest = list(enabled = TRUE, degree = 1), #this shows subgraph of selected and and its degree 1 neighbours
            # drop down on top of the graph
            nodesIdSelection = TRUE
            #selectedBy = "rel_pos" #with this the drop down menu lets you choose nodes based on their rel_pos value
          )
        
      # if for some reason there are no edges
      } else {
        set.seed(3414) # set seed so the graph always looks the same for the same nodes and edges
        visNetwork(nodes) %>%
          visInteraction(zoomView = TRUE, navigationButtons = TRUE, multiselect = TRUE, hover = TRUE) %>%
          visNodes(
            size = 45,
            color = list(background = "#f5f6f7", border = "#0a4ea3", highlight = list(background = "#f5f6f7", border = "red"), hover = list(background = "#f5f6f7", border = "red"))
          ) %>%
          visOptions(
            # drop down on top of the graph
            nodesIdSelection = TRUE
            #selectedBy = "rel_pos" #with this the drop down menu lets you choose nodes based on their rel_pos value
          )
        
      }
    })
  })
  
  
  ##################################
  ### Initialize modify options ####
  ##################################
  
  # Initialize first dropdown of modification options ----------------------------------
  observeEvent(ignoreInit = T,{
    input$upload_edges
    input$choose_patient
    input$radio
    input$slider
    }, {
    #this makes sure that the smaller dataset gets calculated before the initialization of dropdowns
    calculate_smaller_node_and_edge_list()
     
    # get labels and lists
    node_labels <- small_nodelist_for_table$label
    edge_list <- small_edgelist
    node_list <- small_nodelist_for_graph
    
    # input for node deletion
    updateSelectizeInput(session, "choose_node_to_delete", choices = node_labels, server = TRUE)
    
    # first input for edge addition
    nodes_that_can_be_connected <- c()
    for(id in small_nodelist_for_table$id){
      amount_edges <- (length(which(small_edgelist$to == id)) + length(which(small_edgelist$from == id)))
      
      # if there are 3 nodes in the graph each node can have a total of 3-1 edges
      if(amount_edges < (nrow(small_nodelist_for_graph) - 1)){
        nodes_that_can_be_connected <- c(nodes_that_can_be_connected, small_nodelist_for_graph$label[which(small_nodelist_for_graph$id==id)])
      }
    }
    
    updateSelectizeInput(session, "choose_first_connected_node_add", choices = nodes_that_can_be_connected, server = TRUE)

    # first input for edge deletion - only include node labels which have an edge
    nodes_with_edges <- unique(c(edge_list$from, edge_list$to))
    node_labels2 <- c()
    for (index in 1:length(nodes_with_edges)) {
      next_node <- node_list$label[which(node_list$id == nodes_with_edges[index])]
      node_labels2 <- c(node_labels2, next_node)
    }
    # only let the user select the nodes that were selected (not all that are shown in the graph vis)
    node_labels <- node_labels[node_labels %in% node_labels2]
    node_labels <- sort(node_labels)
    
    updateSelectizeInput(session, "choose_first_connected_node_delete", choices = node_labels, server = TRUE)
    
    if(nrow(small_nodelist_for_table) != 0){
      shinyjs::enable("confirm_node_deletion")
    }
    
    if(nrow(small_edgelist) != 0){
      shinyjs::enable("confirm_edge_deletion")
    }
    # calculate the possible amount of edges between the nodes in small_nodelist_for_table
    # if 5 nodes are shown in the graph 4+3+2+1 edges are possible between these nodes
    # because there cannot be multiple edges between two nodes
    # and if there are 3 other nodes in the graph each of the 5 nodes can connect to
    # any of these 3 other nodes so 5*3 more edges are possible
    
    #amount of nodes in table
    nodes_in_table <- nrow(small_nodelist_for_table)
    #amount of nodes only in graph
    nodes_only_in_graph <- (nrow(small_nodelist_for_graph) - nodes_in_table)
    
    possible_amount_of_edges_between_nodes_in_table <- sum(1:(nodes_in_table-1))
    possible_amount_of_edges_between_nodes_in_table_and_nodes_in_graph <- (nodes_in_table*nodes_only_in_graph)
    
    total_possible_amount_of_edges <- (possible_amount_of_edges_between_nodes_in_table + possible_amount_of_edges_between_nodes_in_table_and_nodes_in_graph)
    
    if(nrow(small_edgelist) != total_possible_amount_of_edges){
      shinyjs::enable("confirm_edge_addition")
    }
  })
  
  # Initialize second dropdown of modification options ----------------------------------
  observeEvent(ignoreInit = T,{ # the events that trigger this
    input$upload_edges 
    input$choose_patient
    input$choose_first_connected_node_delete 
    input$choose_first_connected_node_add
    }, {
    # get labels and lists
    edgelist <- small_edgelist
    nodelist <- small_nodelist_for_graph
    node_labels <- nodelist$label
    
    # second input for edge deletion - only containing nodes that are connected to the first selected node
    selected_node <- input$choose_first_connected_node_delete
    selected_node <- nodelist[which(nodelist$label == selected_node), 2]
    connected_nodes <- unique(c(
      edgelist$to[which(edgelist$from == selected_node)],
      edgelist$from[which(edgelist$to == selected_node)]
    ))
    connected_nodes_labels <- c()
    for (index in 1:length(connected_nodes)) {
      connected_nodes_labels <- c(connected_nodes_labels, nodelist$label[nodelist$id == connected_nodes[index]])
    }
    connected_nodes_labels <- sort(connected_nodes_labels)
    
    updateSelectizeInput(session, "choose_second_connected_node_delete", choices = connected_nodes_labels, server = TRUE)
    
    # second input for edge addition - only containing nodes that are NOT connected to the first selected node
    selected_node_for_addition <- input$choose_first_connected_node_add
    selected_node_for_addition <- nodelist[which(nodelist$label == selected_node_for_addition), 2]
    connected_nodes_add <- unique(c(
      edgelist$to[which(edgelist$from == selected_node_for_addition)],
      edgelist$from[which(edgelist$to == selected_node_for_addition)]
    ))
    connected_nodes_labels_add <- c()
    for (index in 1:length(connected_nodes_add)) {
      connected_nodes_labels_add <- c(connected_nodes_labels_add, nodelist$label[nodelist$id == connected_nodes_add[index]])
    }
    not_connected_nodes_labels <- setdiff(nodelist$label, connected_nodes_labels_add)
    not_connected_nodes_labels <- not_connected_nodes_labels[-c(which(not_connected_nodes_labels == input$choose_first_connected_node_add))]
    not_connected_nodes_labels <- sort(not_connected_nodes_labels)
    
    updateSelectizeInput(session, "choose_second_connected_node_add", choices = not_connected_nodes_labels, server = TRUE)
    
    #disable button if there is no possible node to connect to
    if(length(not_connected_nodes_labels) == 0){
      shinyjs::disable("confirm_edge_addition")
    }else{
      shinyjs::enable("confirm_edge_addition")
    }
  })
  
  
  # initialize dropdown of node attributes for node addition ----------------------------------
  observeEvent({
    input$upload_edges
    input$choose_patient
    }, {
    node_features <- node_features_list
    feature_names <- colnames(node_features)
    updateSelectizeInput(session, "choose_node_feature", choices = feature_names, server = TRUE)
  })
  
  
  # initialize dropdown of edge attributes for edge addition ----------------------------------
  observeEvent({
    input$upload_edges
    input$choose_patient
  }, {
    edge_features <- edge_features_list
    feature_names <- colnames(edge_features)
    updateSelectizeInput(session, "choose_edge_feature", choices = feature_names, server = TRUE)
  })
  
  
  ##################################
  ######### Node deletion ##########
  ##################################
  
  # on button click "Delete Node" ----------------------------------
  observeEvent(input$confirm_node_deletion, {
    # disbale deletion button until deletion is done
    shinyjs::disable("confirm_node_deletion")
    
    # read the node that has been selected by the user for deletion
    nodelist <- nodelist_table
    id <- nodelist$id[which(nodelist$label == input$choose_node_to_delete)]
    
    # data frame with newly deleted node, its attributes and edges
    deleted_node <- nodelist[which(nodelist$id == id), ]
    
    # update global nodelist
    nodelist_table <<- nodelist_table[-c(which(nodelist_table$label == deleted_node$label)), ]
    small_nodelist_for_graph <<- small_nodelist_for_graph[-c(which(small_nodelist_for_graph$label == deleted_node$label)), ]
    small_nodelist_for_table <<- small_nodelist_for_table[-c(which(small_nodelist_for_table$label == deleted_node$label)), ]

    # update global edgelist and save deleted edges from node in a data frame
    deleted_nodes_edges <- data.frame()
    if (deleted_node$id %in% edgelist_table$from) {
      deleted_nodes_edges <- rbind(deleted_nodes_edges, edgelist_table[which(edgelist_table$from == deleted_node$id), ])
      edgelist_table <<- edgelist_table[-c(which(edgelist_table$from == deleted_node$id)), ]
      small_edgelist <<- small_edgelist[-c(which(small_edgelist$from == deleted_node$id)), ]
    }
    if (deleted_node$id %in% edgelist_table$to) {
      deleted_nodes_edges <- rbind(deleted_nodes_edges, edgelist_table[which(edgelist_table$to == deleted_node$id), ])
      edgelist_table <<- edgelist_table[-c(which(edgelist_table$to == deleted_node$id)), ]
      small_edgelist <<- small_edgelist[-c(which(small_edgelist$to == deleted_node$id)), ]
    }

    # update amount of nodes for Sliding bar
    max_nodes = length(nodelist_table[[1]])
    updateSliderInput(session, "slider", max=max_nodes)
    
    # update tooltip information of nodes, as their degree has changed
    update_nodes_graph <- small_nodelist_for_graph
    update_nodes_graph$title <- update_node_tooltip(update_nodes_graph, edgelist_table)
    
    # update graph
    visNetworkProxy("graph") %>%
      visUpdateNodes(nodes = update_nodes_graph) %>%
      visRemoveNodes(id = deleted_node$id)
    
    # update global variables for modification history
    modification_history[nrow(modification_history) + 1, ] <<- c("deleted", "node")
    all_deleted_nodes <<- rbind(all_deleted_nodes, deleted_node)
    if(nrow(deleted_nodes_edges) != 0){
      all_deleted_nodes_edges[[length(all_deleted_nodes_edges) + 1]] <<- deleted_nodes_edges
    }else{ #if the node didn't have any edges, we need to save a dataframe with the same columns as edgetable but with zero rows
      cols <- c("from", "to", "id", "rel_pos", "rel_pos_neg", "confidence")
      df <- data.frame(matrix(nrow = 0, ncol = length(cols)))
      colnames(df) <- cols
      all_deleted_nodes_edges[[length(all_deleted_nodes_edges) + 1]] <<- df
    }
    
    # update list of nodes for node deletion
    updateSelectizeInput(session, "choose_node_to_delete", choices = small_nodelist_for_table$label, server = TRUE)
    
    
    # update first input selection for edge addition
    nodes_that_can_be_connected <- c()
    for(id in small_nodelist_for_table$id){
      amount_edges <- (length(which(small_edgelist$to == id)) + length(which(small_edgelist$from == id)))
      # if there are 3 nodes in the graph each node can have a total of 3-1 edges
      if(amount_edges < (nrow(small_nodelist_for_graph) - 1)){
        nodes_that_can_be_connected <- c(nodes_that_can_be_connected, small_nodelist_for_graph$label[which(small_nodelist_for_graph$id==id)])
      }
    }
    
    updateSelectizeInput(session, "choose_first_connected_node_add", choices = nodes_that_can_be_connected, server = TRUE)
    
    # update list of nodes for edge deletion - only node labels which have an edge
    nodes_with_edges <- unique(c(small_edgelist$from, small_edgelist$to))
    node_labels <- c()
    for (index in 1:length(nodes_with_edges)) {
      next_node <- small_nodelist_for_table$label[which(small_nodelist_for_table$id == nodes_with_edges[index])]
      node_labels <- c(node_labels, next_node)
    }
    node_labels <- sort(node_labels)
    updateSelectizeInput(session, "choose_first_connected_node_delete", choices = node_labels, server = TRUE)
    
    # inform user on the change
    output$info_change <- renderUI({
      HTML(paste0("<p style = 'color:green;'>", "Node with the label ", "<b>", deleted_node$label, "</b>", " was", "<b>", " removed", "</b>", " from the graph.", "</p>"))
    })
    
    # update node and edge table
    output$feature_overview <- renderDataTable({
      update_shown_node_table(small_nodelist_for_table)
    })
    output$edge_feature_overview <- renderDataTable({
      update_shown_edge_table(small_edgelist, nodelist_table)
    })
    
    Sys.sleep(0.5)
    if(nrow(small_nodelist_for_table) != 0){
         shinyjs::enable("confirm_node_deletion")
    }
  })
  
  
  
  ##################################
  ######### Edge deletion ##########
  ##################################
  
  # on button click "Delete Edge" ----------------------------------
  observeEvent(input$confirm_edge_deletion, {
    # disable delete button until deletion is done
    shinyjs::disable("confirm_edge_deletion")
    nodelist <- nodelist_table
    edgelist <- edgelist_table
    
    # read which connected nodes have been selected by the user for deletion
    id_first_node <- nodelist$id[which(nodelist$label == input$choose_first_connected_node_delete)]
    id_second_node <- nodelist$id[which(nodelist$label == input$choose_second_connected_node_delete)]
    id_edge <- edgelist$id[which((edgelist$from == id_first_node & edgelist$to == id_second_node) |
                                   (edgelist$to == id_first_node & edgelist$from == id_second_node))]
    
    # newly deleted edge and its attributes
    deleted_edge <- edgelist_table[which(edgelist_table$id == id_edge), ]
    
    # update global variables for modification history
    modification_history[nrow(modification_history) + 1, ] <<- c("deleted", "edge")
    all_deleted_edges <<- rbind(all_deleted_edges, deleted_edge)
    
    # update global edgelist
    edgelist_table <<- edgelist_table[-c(which(edgelist_table$id == deleted_edge$id)), ]
    small_edgelist <<- small_edgelist[-c(which(small_edgelist$id == deleted_edge$id)), ]
    
    # update tooltip information of nodes, as their degree has changed
    update_nodes <- small_nodelist_for_graph
    update_nodes$title <- update_node_tooltip(update_nodes, edgelist_table)

    # update graph
    visNetworkProxy("graph") %>%
      visUpdateNodes(nodes = update_nodes) %>%
      visRemoveEdges(id = deleted_edge$id)
    
    # update first input selection for edge deletion - only node labels which have an edge
    nodes_with_edges <- unique(c(small_edgelist$from, small_edgelist$to))
    node_labels <- c()
    for (index in 1:length(nodes_with_edges)) {
      next_node <- small_nodelist_for_table$label[which(small_nodelist_for_table$id == nodes_with_edges[index])]
      node_labels <- c(node_labels, next_node)
    }
    node_labels <- sort(node_labels)
    
    updateSelectizeInput(session, "choose_first_connected_node_delete", choices = node_labels, server = TRUE)
    
    # update second input selection for edge deletion
    edgelist <- small_edgelist
    node_labels <- small_nodelist_for_table$label
    selected_node <- input$choose_first_connected_node_delete
    selected_node <- nodelist[which(node_labels == selected_node), 2]
    connected_nodes <- unique(c(
      edgelist$to[which(edgelist$from == selected_node)],
      edgelist$from[which(edgelist$to == selected_node)]
    ))
    connected_nodes_labels <- c()
    for (index in 1:length(connected_nodes)) {
      connected_nodes_labels <- c(connected_nodes_labels, node_labels[nodelist$id == connected_nodes[index]])
    }
    connected_nodes_labels <- sort(connected_nodes_labels)
    
    updateSelectizeInput(session, "choose_second_connected_node_delete", choices = connected_nodes_labels, server = TRUE)
    
    # update first input selection for edge addition
    nodes_that_can_be_connected <- c()
    for(id in small_nodelist_for_table$id){
      amount_edges <- (length(which(small_edgelist$to == id)) + length(which(small_edgelist$from == id)))
      # if there are 3 nodes in the graph each node can have a total of 3-1 edges
      if(amount_edges < (nrow(small_nodelist_for_graph) - 1)){
        nodes_that_can_be_connected <- c(nodes_that_can_be_connected, small_nodelist_for_graph$label[which(small_nodelist_for_graph$id==id)])
      }
    }
    
    updateSelectizeInput(session, "choose_first_connected_node_add", choices = nodes_that_can_be_connected, server = TRUE)
    
    
    # translate node ids into label names for the success message
    deleted_edge$from[1] <- nodelist_table[which(nodelist_table$id == deleted_edge[1, 1]), 1]
    deleted_edge$to[1] <- nodelist_table[which(nodelist_table$id == deleted_edge[1, 2]), 1]
    
    # inform user on the change
    output$info_change <- renderUI({
      HTML(paste0("<p style = 'color:green;'>", "Edge between ", "<b>", deleted_edge$from[1], "</b>", " and ", "<b>", deleted_edge$to[1], "</b>", " was", "<b>", " removed", "</b>", " from the graph.", "</p>"))
    })
    
    # update node and edge table
    output$feature_overview <- renderDataTable({
      update_shown_node_table(small_nodelist_for_table)
    })
    output$edge_feature_overview <- renderDataTable({
      update_shown_edge_table(small_edgelist, nodelist_table)
    })
    
    Sys.sleep(0.5)
    if(nrow(small_edgelist) != 0){
      shinyjs::enable("confirm_edge_deletion")
    }
  })
  
  
  ##################################
  ######### Edge addition ##########
  ##################################
  
  # on button click "Enter" ----------------------------------
  observeEvent(input$confirm_edgeFeature_value, {
    
    # read entered values for edge attributes
    selected_feature <- input$choose_edge_feature
    feature_value <- input$edgefeature_value
    
    # avoid NaN values
    if (is.na(feature_value)) {
      feature_value <- 0
    }
    
    # check 1: if the entered value for rel_pos is < 0, an error should appear
    if (selected_feature == "rel_pos" && feature_value < 0) {
      output$error_add_edge <- renderUI({
        HTML("<span style='color:red; font-size:14px'> <br/> ERROR: The entered relevance value for 'rel_pos' is negative. Make sure that you only enter values greater than or equal to zero for 'rel_pos'! </span>")
      })
    } else {
      
      # entered feature value of the user is saved in the global variable
      temporary_added_edge_feature[1, which(colnames(temporary_added_edge_feature) == selected_feature)] <<- feature_value
      
      # update global variable and thus the list of available attributes to enter a value
      edge_features_list <<- edge_features_list[-(which(colnames(edge_features_list) == selected_feature))]
      feature_names <- colnames(edge_features_list)
      updateSelectizeInput(session, "choose_edge_feature", choices = feature_names, server = TRUE)
      
      # remove any error message
      output$error_add_edge <- renderUI({
        HTML(" ")
      })
      
      # clear numeric input, after an edge value was entered
      updateNumericInput(session, "edgefeature_value", value = 0)
    }
  })
  
  
  # on button click "Add Edge" ----------------------------------
  observeEvent(input$confirm_edge_addition, {
    shinyjs::disable("confirm_edge_addition")
    nodelist <- nodelist_table
    edgelist <- edgelist_table
    
    # read selected nodes ids for edge addition and the entered feature values
    id_first_node <- nodelist$id[which(nodelist$label == input$choose_first_connected_node_add)]
    id_second_node <- nodelist$id[which(nodelist$label == input$choose_second_connected_node_add)]
    feature_value <- input$edgefeature_value
    selected_feature <- input$choose_edge_feature
    
    # added node receives a new unique id
    id_edge <- UUIDgenerate(use.time = TRUE, n = 1)
    
    # avoid NaN values
    if (is.na(feature_value)) {
      feature_value <- 0
    }
    
    # check 1: if the entered value for rel_pos is < 0, an error should appear
    if (selected_feature == "rel_pos" && feature_value < 0) {
      output$error_add_edge <- renderUI({
        HTML("<span style='color:red; font-size:14px'> <br/> ERROR: The entered relevance value for 'rel_pos' is negative. Make sure that you only enter values greater than or equal to zero for 'rel_pos'! </span>")
      })
    } else {
      
      # check 2: error message when a feature value is still entered and requires to press "enter" first
      if (feature_value != 0) {
        output$error_add_edge <- renderUI({
          HTML("<span style='color:red; font-size:14px'> <br/> ERROR: Entered attribute value has not been saved yet. Press ENTER to save the value or remove the value to initialize with 0! </span>")
        })
      } else {
        
        # connect user entries for edge attributes with the selected nodes and its id
        temporary_added_edge_feature$from[1] <<- id_first_node
        temporary_added_edge_feature$to[1] <<- id_second_node
        temporary_added_edge_feature$id[1] <<- id_edge
        
        # update global variable for modification history
        modification_history[nrow(modification_history) + 1, ] <<- c("added", "edge")
        all_added_edges <<- rbind(all_added_edges, temporary_added_edge_feature)
        
        # update global edgelist
        edgelist_table <<- rbind(edgelist_table, temporary_added_edge_feature)
        edgelist_table <<- edgelist_table[order(edgelist_table$from), ]
        small_edgelist <<- rbind(small_edgelist, temporary_added_edge_feature)
        
        # add tooltip information for the new edge (from-to, rel_pos, rel_pos_neg)
        added_edge <- temporary_added_edge_feature
        added_edge$title <- update_edge_tooltip(nodelist_table, added_edge)
        
        # update tooltip information of nodes, as their degree has changed
        update_nodes <- small_nodelist_for_graph[which(small_nodelist_for_graph$id == id_first_node | small_nodelist_for_graph$id == id_second_node), ]
        update_nodes$title <- update_node_tooltip(update_nodes, edgelist_table)

        # update graph
        visNetworkProxy("graph") %>%
          visUpdateNodes(nodes = update_nodes) %>%
          visUpdateEdges(edges = added_edge) %>%
          visSelectEdges(id = added_edge$id[1])
        
        # clear numeric input
        updateNumericInput(session, "edgefeature_value", value = 0)
        
        # reset temporary variables to initial state (ready for next edge addition)
        temporary_added_edge_feature <<- edgelist_table[0, ]
        temporary_added_edge_feature[nrow(temporary_added_edge_feature) + 1, ] <<- c("from_value", "to_value", "id_value", rep(0, length(colnames(edgelist_table)) - 3))
        temporary_added_edge_feature[, 4:ncol(temporary_added_edge_feature)] <<- as.numeric(temporary_added_edge_feature[, 4:ncol(temporary_added_edge_feature)])
        edge_features_list <<- subset(edgelist_table, select = -c(1:3))
        
        # selection list of edge features needs to contain all attributes again
        edge_features <- subset(edgelist_table, select = -c(1:3))
        feature_names <- colnames(edge_features)
        updateSelectizeInput(session, "choose_edge_feature", choices = feature_names, server = TRUE)
        
        # remove error message
        output$error_add_edge <- renderUI({
          HTML(" ")
        })
        
        # inform user on the change
        added_edge$from[1] <- nodelist_table[which(nodelist_table$id == added_edge[1, 1]), 1]
        added_edge$to[1] <- nodelist_table[which(nodelist_table$id == added_edge[1, 2]), 1]
        
        output$info_change <- renderUI({
          HTML(paste0("<p style = 'color:green;'>", "Edge between ", "<b>", added_edge$from[1], "</b>", " and ", "<b>", added_edge$to[1], "</b>", " was", "<b>", " added", "</b>", " to the graph.", "</p>"))
        })
        
        # update first input selection for edge addition
        nodes_that_can_be_connected <- c()
        for(id in small_nodelist_for_table$id){
          amount_edges <- (length(which(small_edgelist$to == id)) + length(which(small_edgelist$from == id)))
          # if there are 3 nodes in the graph each node can have a total of 3-1 edges
          if(amount_edges < (nrow(small_nodelist_for_graph) - 1)){
            nodes_that_can_be_connected <- c(nodes_that_can_be_connected, small_nodelist_for_graph$label[which(small_nodelist_for_graph$id==id)])
          }
        }
        
        updateSelectizeInput(session, "choose_first_connected_node_add", choices = nodes_that_can_be_connected, server = TRUE)
        
        # update second input selection for edge addition
        edgelist <- small_edgelist
        node_labels <- small_nodelist_for_table$label
        selected_node_for_addition <- input$choose_first_connected_node_add
        selected_node_for_addition <- nodelist[which(node_labels == selected_node_for_addition), 2]
        
        connected_nodes_add <- unique(c(
          edgelist$to[which(edgelist$from == selected_node_for_addition)],
          edgelist$from[which(edgelist$to == selected_node_for_addition)]
        ))
        connected_nodes_labels_add <- c()
        for (index in 1:length(connected_nodes_add)) {
          connected_nodes_labels_add <- c(connected_nodes_labels_add, node_labels[nodelist$id == connected_nodes_add[index]])
        }
        not_connected_nodes_labels <- setdiff(node_labels, connected_nodes_labels_add)
        not_connected_nodes_labels <- not_connected_nodes_labels[-c(which(not_connected_nodes_labels == input$choose_first_connected_node_add))]
        not_connected_nodes_labels <- sort(not_connected_nodes_labels)
        
        updateSelectizeInput(session, "choose_second_connected_node_add", choices = not_connected_nodes_labels, server = TRUE)
        
        
        # update first input selection for edge deletion - only node labels which have an edge
        nodes_with_edges <- unique(c(small_edgelist$from, small_edgelist$to))
        node_labels <- c()
        for (index in 1:length(nodes_with_edges)) {
          next_node <- small_nodelist_for_table$label[which(small_nodelist_for_table$id == nodes_with_edges[index])]
          node_labels <- c(node_labels, next_node)
        }
        node_labels <- sort(node_labels)
        updateSelectizeInput(session, "choose_first_connected_node_delete", choices = node_labels, server = TRUE)
        
        # update second input selection for edge deletion
        selected_node <- input$choose_first_connected_node_delete
        selected_node <- nodelist[which(nodelist$label == selected_node), 2]
        connected_nodes <- unique(c(
          edgelist$to[which(edgelist$from == selected_node)],
          edgelist$from[which(edgelist$to == selected_node)]
        ))
        connected_nodes_labels <- c()
        for (index in 1:length(connected_nodes)) {
          connected_nodes_labels <- c(connected_nodes_labels, nodelist$label[nodelist$id == connected_nodes[index]])
        }
        connected_nodes_labels <- sort(connected_nodes_labels)
        
        updateSelectizeInput(session, "choose_second_connected_node_delete", choices = connected_nodes_labels, server = TRUE)
      
        
        # update node and edge table
        output$feature_overview <- renderDataTable({
          update_shown_node_table(small_nodelist_for_table)
        })
        output$edge_feature_overview <- renderDataTable({
          update_shown_edge_table(small_edgelist, nodelist_table)
        })
        
        Sys.sleep(0.5)
        # calculate the possible amount of edges between the nodes in small_nodelist_for_table
        # if 5 nodes are shown in the graph 4+3+2+1 edges are possible between these nodes
        # because there cannot be multiple edges between two nodes
        # and if there are 3 other nodes in the graph each of the 5 nodes can connect to
        # any of these 3 other nodes so 5*3 more edges are possible
        
        #amount of nodes in table
        nodes_in_table <- nrow(small_nodelist_for_table)
        #amount of nodes only in graph
        nodes_only_in_graph <- (nrow(small_nodelist_for_graph) - nodes_in_table)
        
        possible_amount_of_edges_between_nodes_in_table <- sum(1:(nodes_in_table-1))
        possible_amount_of_edges_between_nodes_in_table_and_nodes_in_graph <- (nodes_in_table*nodes_only_in_graph)
        
        total_possible_amount_of_edges <- (possible_amount_of_edges_between_nodes_in_table + possible_amount_of_edges_between_nodes_in_table_and_nodes_in_graph)
        
        if(nrow(small_edgelist) != total_possible_amount_of_edges){
          shinyjs::enable("confirm_edge_addition")
        }
      }
    }
  })
  
  
  # on button click "Cancel" ----------------------------------
  observeEvent(input$cancel_edge_addition, {
    
    # clear numeric input
    updateNumericInput(session, "edgefeature_value", value = 0)
    
    # reset temporary variables to initial state (ready for next edge addition)
    temporary_added_edge_feature <<- edgelist_table[0, ]
    temporary_added_edge_feature[nrow(temporary_added_edge_feature) + 1, ] <<- c("from_value", "to_value", "id_value", rep(0, length(colnames(edgelist_table)) - 3))
    temporary_added_edge_feature[, 4:ncol(temporary_added_edge_feature)] <<- as.numeric(temporary_added_edge_feature[, 4:ncol(temporary_added_edge_feature)])
    edge_features_list <<- subset(edgelist_table, select = -c(1:3))
    
    # selection list of edge features needs to contain all attributes again
    edge_features <- subset(edgelist_table, select = -c(1:3))
    feature_names <- colnames(edge_features)
    updateSelectizeInput(session, "choose_edge_feature", choices = feature_names, server = TRUE)
    
    # remove any error message
    output$error_add_edge <- renderUI({
      HTML(" ")
    })
  })
  
  
  ##################################
  ######### Node addition ##########
  ##################################
  
  # on button click "Enter" ----------------------------------
  observeEvent(input$confirm_nodeFeature_value, {
    
    # read entered values for node attributes
    selected_feature <- input$choose_node_feature
    feature_value <- input$nodefeature_value
    label <- input$new_node_label
    
    # avoid NaN values
    if (is.na(feature_value)) {
      feature_value <- 0
    }
    
    # check 1: if the entered value for rel_pos is < 0, an error should appear
    if (selected_feature == "rel_pos" && feature_value < 0) {
      output$error_add_node <- renderUI({
        HTML("<span style='color:red; font-size:14px'> <br/> ERROR: The entered relevance value for 'rel_pos' is negative. Make sure that you only enter values greater than or equal to zero for 'rel_pos'! </span>")
      })
    } else {
      
      # check 2: user has to enter a node label
      if (label != "") {
        
        # check 3: node label must be unique
        test_whether_unique <- nodelist_table[which(nodelist_table$label == label), 1]
        
        if (length(test_whether_unique) == 0) {
          
          # entered feature value of the user is saved in the global variable
          temporary_added_node_feature[1, which(colnames(temporary_added_node_feature) == selected_feature)] <<- feature_value
          
          # update list of available attributes to enter a value
          node_features_list <<- node_features_list[-(which(colnames(node_features_list) == selected_feature))]
          feature_names <- colnames(node_features_list)
          updateSelectizeInput(session, "choose_node_feature", choices = feature_names, server = TRUE)
          
          # clear numeric input, after a node value was entered
          updateNumericInput(session, "nodefeature_value", value = 0)
          
          # remove error message if user enters everything correctly
          output$error_add_node <- renderUI({
            HTML(" ")
          })
        } else {
          output$error_add_node <- renderUI({
            HTML("<span style='color:red; font-size:14px'> <br/> ERROR: The entered label is already taken. Please enter a new label! </span>")
          })
        }
      } else {
        # info for user that label is missing
        output$error_add_node <- renderUI({
          HTML("<span style='color:red; font-size:14px'> <br/> ERROR: Node label is missing. Please enter a label for the new node first! </span>")
        })
      }
    }
  })
  
  
  # on button click "Add Node" ----------------------------------
  observeEvent(input$confirm_node_addition, {
    label <- input$new_node_label
    feature_value <- input$nodefeature_value
    selected_feature <- input$choose_node_feature
    
    # added node receives a new unique id
    id <- UUIDgenerate(use.time = TRUE, n = 1)
    
    # avoid NaN values
    if (is.na(feature_value)) {
      feature_value <- 0
    }
    
    # check 1: if the entered value for rel_pos is < 0, an error should appear
    if (selected_feature == "rel_pos" && feature_value < 0) {
      output$error_add_node <- renderUI({
        HTML("<span style='color:red; font-size:14px'> <br/> ERROR: The entered relevance value for 'rel_pos' is negative. Make sure that you only enter values greater than or equal to zero for 'rel_pos'! </span>")
      })
    } else {
      
      # check 2: user has to enter a node label
      if (label != "") {
        
        # check 3: node label must be unique
        test_whether_unique <- nodelist_table[which(nodelist_table$label == label), 1]
        
        if (length(test_whether_unique) == 0) {
          
          # check 4: throw error in case the user entered an attribute value but didn't save it
          if (feature_value == 0) {
            
            # connect user entries for attributes with entered label and add the UUID
            temporary_added_node_feature$id[1] <<- id
            temporary_added_node_feature$label[1] <<- label
            
            # update global variable for modification history
            modification_history[nrow(modification_history) + 1, ] <<- c("added", "node")
            all_added_nodes <<- rbind(all_added_nodes, temporary_added_node_feature)
            
            # prepare node for graph update - add tooltip information: label, rel_pos, rel_pos_neg, degree
            added_node <- temporary_added_node_feature
            added_node$title <- update_node_tooltip(added_node, edgelist_table)
            
            # update graph
            visNetworkProxy("graph") %>%
              visUpdateNodes(nodes = added_node) %>%
              visSelectNodes(id = added_node$id[1])
            
            # update global nodeslist table
            nodelist_table <<- rbind(nodelist_table, temporary_added_node_feature)
            nodelist_table <<- nodelist_table[order(nodelist_table$label), ]
            small_nodelist_for_table <<- rbind(small_nodelist_for_table, temporary_added_node_feature)
            small_nodelist_for_graph <<- rbind(small_nodelist_for_graph, temporary_added_node_feature)
            
            # update amount of nodes for Sliding bar
            max_nodes = length(nodelist_table[[1]])
            updateSliderInput(session, "slider", max=max_nodes)
            
            # update input selection for node deletion
            node_labels <- sort(small_nodelist_for_table$label)
            updateSelectizeInput(session, "choose_node_to_delete", choices = node_labels, server = TRUE)
            
            # update first input selection for edge addition
            nodes_that_can_be_connected <- c()
            for(id in small_nodelist_for_table$id){
              amount_edges <- (length(which(small_edgelist$to == id)) + length(which(small_edgelist$from == id)))
              # if there are 3 nodes in the graph each node can have a total of 3-1 edges
              if(amount_edges < (nrow(small_nodelist_for_graph) - 1)){
                nodes_that_can_be_connected <- c(nodes_that_can_be_connected, small_nodelist_for_graph$label[which(small_nodelist_for_graph$id==id)])
              }
            }
            
            updateSelectizeInput(session, "choose_first_connected_node_add", choices = nodes_that_can_be_connected, server = TRUE)
            
            
            # update first input selection for edge deletion
            # update list of nodes for edge deletion - only node labels which have an edge
            nodes_with_edges <- unique(c(small_edgelist$from, small_edgelist$to))
            node_labels <- c()
            for (index in 1:length(nodes_with_edges)) {
              next_node <- small_nodelist_for_table$label[which(small_nodelist_for_table$id == nodes_with_edges[index])]
              node_labels <- c(node_labels, next_node)
            }
            node_labels <- sort(node_labels)
            updateSelectizeInput(session, "choose_first_connected_node_delete", choices = node_labels, server = TRUE)
            
            # clear text input, after a new node was created
            updateTextInput(session, "new_node_label", value = "", placeholder = "e.g. ABCC2")
            updateNumericInput(session, "nodefeature_value", value = 0)
            
            # reset temporary variables to initial state (ready for next node addition)
            temporary_added_node_feature <<- nodelist_table[0, ]
            temporary_added_node_feature[nrow(temporary_added_node_feature) + 1, ] <<- c("label_value", "id_value", rep(0, length(colnames(nodelist_table)) - 2))
            temporary_added_node_feature[, 3:ncol(temporary_added_node_feature)] <<- as.numeric(temporary_added_node_feature[, 3:ncol(temporary_added_node_feature)])
            node_features_list <<- nodelist_table[, c(3:ncol(nodelist_table))]
            
            # selection list of node features needs to contain all attributes again
            node_features <- node_features_list
            feature_names <- colnames(node_features)
            updateSelectizeInput(session, "choose_node_feature", choices = feature_names, server = TRUE)
            
            # remove error message if user enters label correctly
            output$error_add_node <- renderUI({
              HTML(" ")
            })
            
            # inform user on the change
            output$info_change <- renderUI({
              HTML(paste0("<p style = 'color:green;'>", "Node with the label ", "<b>", added_node$label, "</b>", " was", "<b>", " added", "</b>", " to the graph.", "</p>"))
            })
            
            # update node and edge table
            output$feature_overview <- renderDataTable({
              update_shown_node_table(small_nodelist_for_table)
            })
            output$edge_feature_overview <- renderDataTable({
              update_shown_edge_table(small_edgelist, nodelist_table)
            })
            
          } else {
            # error message when a feature value is entered and requires to press "enter" first
            output$error_add_node <- renderUI({
              HTML("<span style='color:red; font-size:14px'> <br/> ERROR: Entered attribute value has not been saved yet. Press ENTER to save the value or remove the value to initialize with 0! </span>")
            })
          }
        } else {
          output$error_add_node <- renderUI({
            HTML("<span style='color:red; font-size:14px'> <br/> ERROR: The entered label is already taken. Please enter a new label! </span>")
          })
        }
      } else {
        # info for user that label is missing
        output$error_add_node <- renderUI({
          HTML("<span style='color:red; font-size:14px'> <br/> ERROR: Node label is missing. Please enter a label for the new node first! </span>")
        })
      }
    }
  })
  
  
  # on button click "Cancel" ----------------------------------
  observeEvent(input$cancel_node_addition, {
    
    # clear text / numeric input fields
    updateTextInput(session, "new_node_label", value = "", placeholder = "e.g. ABCC2")
    updateNumericInput(session, "nodefeature_value", value = 0)
    
    # reset temporary variables to initial state (ready for next node addition)
    temporary_added_node_feature <<- nodelist_table[0, ]
    temporary_added_node_feature[nrow(temporary_added_node_feature) + 1, ] <<- c("label_value", "id_value", rep(0, length(colnames(nodelist_table)) - 2))
    temporary_added_node_feature[, 3:ncol(temporary_added_node_feature)] <<- as.numeric(temporary_added_node_feature[, 3:ncol(temporary_added_node_feature)])
    node_features_list <<- nodelist_table[, c(3:ncol(nodelist_table))]
    
    # selection list of node features needs to contain all attributes again
    node_features <- node_features_list
    feature_names <- colnames(node_features)
    updateSelectizeInput(session, "choose_node_feature", choices = feature_names, server = TRUE)
    
    # remove any error information
    output$error_add_node <- renderUI({
      HTML(" ")
    })
  })
  
  ##################################
  ############## Undo ##############
  ##################################
  
  # disable undo-button when modification_history is empty, enable undo-button when there are actions to reverse ----------------------------------
  observeEvent(c(input$undo, input$upload_edges, input$confirm_edge_deletion, input$confirm_edge_addition, input$confirm_node_addition, input$confirm_node_deletion, input$predict, input$retrain), {
    if (modification_history[nrow(modification_history), 1] == 0 && modification_history[nrow(modification_history), 2] == 0) {
      shinyjs::disable("undo")
    } else {
      shinyjs::enable("undo")
    }
  })
  
  
  # on button click "undo" - revert the last user modification action ----------------------------------
  observeEvent(input$undo, {
    if (modification_history[nrow(modification_history), 1] == "deleted" && modification_history[nrow(modification_history), 2] == "node") {
      undo_node_deletion()
    } else if (modification_history[nrow(modification_history), 1] == "deleted" && modification_history[nrow(modification_history), 2] == "edge") {
      undo_edge_deletion()
    } else if (modification_history[nrow(modification_history), 1] == "added" && modification_history[nrow(modification_history), 2] == "edge") {
      undo_edge_addition()
    } else if (modification_history[nrow(modification_history), 1] == "added" && modification_history[nrow(modification_history), 2] == "node") {
      undo_node_addition()
    }
    
    # update amount of nodes for Sliding bar
    max_nodes = length(nodelist_table[[1]])
    updateSliderInput(session, "slider", max=max_nodes)
    
  })
  
  
  # function to reverse the last node deletion ----------------------------------
  undo_node_deletion <- function() {
    # extract node from list of all deleted nodes and also its edges
    add_node <- all_deleted_nodes[nrow(all_deleted_nodes), ]
    add_edges <- all_deleted_nodes_edges[[length(all_deleted_nodes_edges)]]
    
    # update global nodelist
    nodelist_table <<- rbind(nodelist_table, add_node)
    nodelist_table <<- nodelist_table[order(nodelist_table$label), ]
    small_nodelist_for_graph <<- rbind(small_nodelist_for_graph, add_node)
    small_nodelist_for_table <<- rbind(small_nodelist_for_table, add_node)
    
    # update global edgelist (ONLY IF THERE IS AN EDGE)
    if(nrow(add_edges) != 0){
      edgelist_table <<- rbind(edgelist_table, add_edges)
      edgelist_table <<- edgelist_table[order(edgelist_table$from), ]
      small_edgelist <<- rbind(small_edgelist, add_edges)
      
      # create tooltip information for edges
      add_edges$title <- update_edge_tooltip(nodelist_table, add_edges)
    }
    
    # update tooltip information of nodes, as their degree has changed
    update_nodes <- small_nodelist_for_graph
    update_nodes$title <- update_node_tooltip(update_nodes, edgelist_table)
    
    
    # update graph
    if(nrow(add_edges) != 0){
      visNetworkProxy("graph") %>%
        visUpdateNodes(nodes = update_nodes) %>%
        visUpdateEdges(edges = add_edges) %>%
        visSelectNodes(id = add_node$id[1])
    }else{
      visNetworkProxy("graph") %>%
        visUpdateNodes(nodes = update_nodes) %>%
        visSelectNodes(id = add_node$id[1])
    }
    # remove node from global variables of modification history
    modification_history <<- modification_history[-c(nrow(modification_history)), ]
    all_deleted_nodes <<- all_deleted_nodes[-c(nrow(all_deleted_nodes)), ]
    all_deleted_nodes_edges <<- all_deleted_nodes_edges[-length(all_deleted_nodes_edges)]
    
    # update list of nodes for node deletion
    updateSelectizeInput(session, "choose_node_to_delete", choices = small_nodelist_for_table$label, server = TRUE)
    
    # update list of nodes for edge addition
    # update first input selection for edge addition
    nodes_that_can_be_connected <- c()
    for(id in small_nodelist_for_table$id){
      amount_edges <- (length(which(small_edgelist$to == id)) + length(which(small_edgelist$from == id)))
      # if there are 3 nodes in the graph each node can have a total of 3-1 edges
      if(amount_edges < (nrow(small_nodelist_for_graph) - 1)){
        nodes_that_can_be_connected <- c(nodes_that_can_be_connected, small_nodelist_for_graph$label[which(small_nodelist_for_graph$id==id)])
      }
    }
    
    updateSelectizeInput(session, "choose_first_connected_node_add", choices = nodes_that_can_be_connected, server = TRUE)
    
    # update list of nodes for edge deletion - only node labels which have an edge
    nodes_with_edges <- unique(c(small_edgelist$from, small_edgelist$to))
    node_labels <- c()
    for (index in 1:length(nodes_with_edges)) {
      next_node <- small_nodelist_for_table$label[which(small_nodelist_for_table$id == nodes_with_edges[index])]
      node_labels <- c(node_labels, next_node)
    }
    node_labels <- sort(node_labels)
    updateSelectizeInput(session, "choose_first_connected_node_delete", choices = node_labels, server = TRUE)
    
    # inform user on the change
    output$info_change <- renderUI({
      HTML(paste0("<p style = 'color:green;'>", "Node with the label ", "<b>", add_node$label, "</b>", " and all its edges were", "<b>", " added", "</b>", " to the graph.", "</p>"))
    })
    
    # disable undo button, if modification_history is now empty
    if (modification_history[nrow(modification_history), 1] == 0 && modification_history[nrow(modification_history), 2] == 0) {
      shinyjs::disable("undo")
    }
    
    # update node and edge table
    output$feature_overview <- renderDataTable({
      update_shown_node_table(small_nodelist_for_table)
    })
    output$edge_feature_overview <- renderDataTable({
      update_shown_edge_table(small_edgelist, nodelist_table)
    })
    
    if(nrow(small_nodelist_for_table) != 0){
      shinyjs::enable("confirm_node_deletion")
    }
  }
  
  
  # function to reverse the last edge deletion ----------------------------------
  undo_edge_deletion <- function() {
    
    # extract edge from list of all deleted edge
    add_edge <- all_deleted_edges[nrow(all_deleted_edges), ]
    
    # update global edgelist
    edgelist_table <<- rbind(edgelist_table, add_edge)
    edgelist_table <<- edgelist_table[order(edgelist_table$from), ]
    small_edgelist <<- rbind(small_edgelist, add_edge)
    
    # add tooltip information for the edge (from-to, rel_pos, rel_pos_neg)
    add_edge$title <- update_edge_tooltip(nodelist_table, add_edge)
    
    # update tooltip information of nodes, as their degree has changed
    update_nodes <- small_nodelist_for_graph
    update_nodes$title <- update_node_tooltip(update_nodes, edgelist_table)
    
    # update graph
    visNetworkProxy("graph") %>%
      visUpdateNodes(nodes = update_nodes) %>%
      visUpdateEdges(edges = add_edge) %>%
      visSelectEdges(id = add_edge$id[1])
    
    # remove edge from global variables of modification history
    modification_history <<- modification_history[-c(nrow(modification_history)), ]
    all_deleted_edges <<- all_deleted_edges[-c(nrow(all_deleted_edges)), ]
    
    # update first input selection for edge deletion - only node labels which have an edge
    nodes_with_edges <- unique(c(small_edgelist$from, small_edgelist$to))
    node_labels <- c()
    for (index in 1:length(nodes_with_edges)) {
      next_node <- small_nodelist_for_table$label[which(small_nodelist_for_table$id == nodes_with_edges[index])]
      node_labels <- c(node_labels, next_node)
    }
    node_labels <- sort(node_labels)
    
    updateSelectizeInput(session, "choose_first_connected_node_delete", choices = node_labels, server = TRUE)
    
    # update second input selection for edge deletion
    selected_node <- input$choose_first_connected_node_delete
    selected_node <- nodelist_table[which(nodelist_table$label == selected_node), 2]
    connected_nodes <- unique(c(
      edgelist_table$to[which(edgelist_table$from == selected_node)],
      edgelist_table$from[which(edgelist_table$to == selected_node)]
    ))
    connected_nodes_labels <- c()
    for (index in 1:length(connected_nodes)) {
      connected_nodes_labels <- c(connected_nodes_labels, nodelist_table$label[nodelist_table$id == connected_nodes[index]])
    }
    connected_nodes_labels <- sort(connected_nodes_labels)
    
    updateSelectizeInput(session, "choose_second_connected_node_delete", choices = connected_nodes_labels, server = TRUE)
    
    # update first input selection for edge addition
    nodes_that_can_be_connected <- c()
    for(id in small_nodelist_for_table$id){
      amount_edges <- (length(which(small_edgelist$to == id)) + length(which(small_edgelist$from == id)))
      # if there are 3 nodes in the graph each node can have a total of 3-1 edges
      if(amount_edges < (nrow(small_nodelist_for_graph) - 1)){
        nodes_that_can_be_connected <- c(nodes_that_can_be_connected, small_nodelist_for_graph$label[which(small_nodelist_for_graph$id==id)])
      }
    }
    
    updateSelectizeInput(session, "choose_first_connected_node_add", choices = nodes_that_can_be_connected, server = TRUE)
    
    # inform user on the change
    add_edge$from[1] <- nodelist_table[which(nodelist_table$id == add_edge[1, 1]), 1]
    add_edge$to[1] <- nodelist_table[which(nodelist_table$id == add_edge[1, 2]), 1]
    
    output$info_change <- renderUI({
      HTML(paste0("<p style = 'color:green;'>", "Edge between ", "<b>", add_edge$from[1], "</b>", " and ", "<b>", add_edge$to[1], "</b>", " was", "<b>", " added", "</b>", " to the graph.", "</p>"))
    })
    
    # disable undo button, if modification_history is now empty
    if (modification_history[nrow(modification_history), 1] == 0 && modification_history[nrow(modification_history), 2] == 0) {
      shinyjs::disable("undo")
    }
    
    # update node and edge table
    output$feature_overview <- renderDataTable({
      update_shown_node_table(small_nodelist_for_table)
    })
    output$edge_feature_overview <- renderDataTable({
      update_shown_edge_table(small_edgelist, nodelist_table)
    })
    
    if(nrow(small_edgelist) != 0){
      shinyjs::enable("confirm_edge_deletion")
    }
  }
  
  # function to reverse the last edge addition ----------------------------------
  undo_edge_addition <- function() {
    
    # extract edge from list of all added edges
    delete_edge <- all_added_edges[nrow(all_added_edges), ]
    
    # update global edgelist
    edgelist_table <<- edgelist_table[-c(which(edgelist_table$id == delete_edge$id)), ]
    
    # check if the edge we want to now remove is in the small_edgelist
    # don't update anything if the edge is not in small_edgelist
    if(length(which(small_edgelist$id == delete_edge$id)) !=0 ){
      small_edgelist <<- small_edgelist[-c(which(small_edgelist$id == delete_edge$id)), ]
      
      # update tooltip information of nodes, as their degree has changed
      update_nodes <- small_nodelist_for_graph
      update_nodes$title <- update_node_tooltip(update_nodes, edgelist_table)
      
      # update graph
      visNetworkProxy("graph") %>%
        visUpdateNodes(nodes = update_nodes) %>%
        visRemoveEdges(id = delete_edge$id)
      
      # update edge table
      output$edge_feature_overview <- renderDataTable({
        update_shown_edge_table(small_edgelist, nodelist_table)
      })
    }
    
    # remove edge from global variables of modification history
    modification_history <<- modification_history[-c(nrow(modification_history)), ]
    all_added_edges <<- all_added_edges[-c(nrow(all_added_edges)), ]
    
    # update first input selection for edge addition
    nodes_that_can_be_connected <- c()
    for(id in small_nodelist_for_table$id){
      amount_edges <- (length(which(small_edgelist$to == id)) + length(which(small_edgelist$from == id)))
      # if there are 3 nodes in the graph each node can have a total of 3-1 edges
      if(amount_edges < (nrow(small_nodelist_for_graph) - 1)){
        nodes_that_can_be_connected <- c(nodes_that_can_be_connected, small_nodelist_for_graph$label[which(small_nodelist_for_graph$id==id)])
      }
    }
    
    updateSelectizeInput(session, "choose_first_connected_node_add", choices = nodes_that_can_be_connected, server = TRUE)
    
    # update second input selection for edge addition
    selected_node_for_addition <- input$choose_first_connected_node_add
    selected_node_for_addition <- nodelist_table[which(nodelist_table$label == selected_node_for_addition), 2]
    connected_nodes_add <- unique(c(
      edgelist_table$to[which(edgelist_table$from == selected_node_for_addition)],
      edgelist_table$from[which(edgelist_table$to == selected_node_for_addition)]
    ))
    connected_nodes_labels_add <- c()
    for (index in 1:length(connected_nodes_add)) {
      connected_nodes_labels_add <- c(connected_nodes_labels_add, nodelist_table$label[nodelist_table$id == connected_nodes_add[index]])
    }
    not_connected_nodes_labels <- setdiff(nodelist_table$label, connected_nodes_labels_add)
    not_connected_nodes_labels <- not_connected_nodes_labels[-c(which(not_connected_nodes_labels == input$choose_first_connected_node_add))]
    not_connected_nodes_labels <- sort(not_connected_nodes_labels)
    
    updateSelectizeInput(session, "choose_second_connected_node_add", choices = not_connected_nodes_labels, server = TRUE)
    
    #disable button if there is no possible node to connect to
    if(length(not_connected_nodes_labels) == 0){
      shinyjs::disable("confirm_edge_addition")
    }else{
      shinyjs::enable("confirm_edge_addition")
    }
    
    # update first input selection for edge deletion - only node labels which have an edge
    nodes_with_edges <- unique(c(small_edgelist$from, small_edgelist$to))
    node_labels <- c()
    for (index in 1:length(nodes_with_edges)) {
      next_node <- small_nodelist_for_table$label[which(small_nodelist_for_table$id == nodes_with_edges[index])]
      node_labels <- c(node_labels, next_node)
    }
    node_labels <- sort(node_labels)
    updateSelectizeInput(session, "choose_first_connected_node_delete", choices = node_labels, server = TRUE)
    
    # update second input selection for edge deletion
    selected_node <- input$choose_first_connected_node_delete
    selected_node <- nodelist_table[which(nodelist_table$label == selected_node), 2]
    connected_nodes <- unique(c(
      edgelist_table$to[which(edgelist_table$from == selected_node)],
      edgelist_table$from[which(edgelist_table$to == selected_node)]
    ))
    connected_nodes_labels <- c()
    for (index in 1:length(connected_nodes)) {
      connected_nodes_labels <- c(connected_nodes_labels, nodelist_table$label[nodelist_table$id == connected_nodes[index]])
    }
    connected_nodes_labels <- sort(connected_nodes_labels)
    
    updateSelectizeInput(session, "choose_second_connected_node_delete", choices = connected_nodes_labels, server = TRUE)
    
    # inform user on the change
    delete_edge$from[1] <- nodelist_table[which(nodelist_table$id == delete_edge[1, 1]), 1]
    delete_edge$to[1] <- nodelist_table[which(nodelist_table$id == delete_edge[1, 2]), 1]
    
    output$info_change <- renderUI({
      HTML(paste0("<p style = 'color:green;'>", "Edge between ", "<b>", delete_edge$from[1], "</b>", " and ", "<b>", delete_edge$to[1], "</b>", " was", "<b>", " removed", "</b>", " from the graph.", "</p>"))
    })
    
    # disable undo button, if modification_history is now empty
    if (modification_history[nrow(modification_history), 1] == 0 && modification_history[nrow(modification_history), 2] == 0) {
      shinyjs::disable("undo")
    }
    
    # calculate the possible amount of edges between the nodes in small_nodelist_for_table
    # if 5 nodes are shown in the graph 4+3+2+1 edges are possible between these nodes
    # because there cannot be multiple edges between two nodes
    # and if there are 3 other nodes in the graph each of the 5 nodes can connect to
    # any of these 3 other nodes so 5*3 more edges are possible
    
    #amount of nodes in table
    nodes_in_table <- nrow(small_nodelist_for_table)
    #amount of nodes only in graph
    nodes_only_in_graph <- (nrow(small_nodelist_for_graph) - nodes_in_table)
    
    possible_amount_of_edges_between_nodes_in_table <- sum(1:(nodes_in_table-1))
    possible_amount_of_edges_between_nodes_in_table_and_nodes_in_graph <- (nodes_in_table*nodes_only_in_graph)
    
    total_possible_amount_of_edges <- (possible_amount_of_edges_between_nodes_in_table + possible_amount_of_edges_between_nodes_in_table_and_nodes_in_graph)
    
    if(nrow(small_edgelist) != total_possible_amount_of_edges){
      shinyjs::enable("confirm_edge_addition")
    }
  }
  
  
  # function to reverse the last node addition ----------------------------------
  undo_node_addition <- function() {
    
    # extract node from list of all added nodes
    delete_node <- all_added_nodes[nrow(all_added_nodes), ]
    
    # update global nodelist
    nodelist_table <<- nodelist_table[-c(which(nodelist_table$label == delete_node$label)), ]
    
    # check if the node we want to now remove is in the small_nodelist_for_table or small_nodelist_for_graph
    # don't update anything if the node is not in one of them
    if(length(which(small_nodelist_for_graph$id == delete_node$id)) !=0){
      # update smaller table
      small_nodelist_for_graph <<- small_nodelist_for_graph[-c(which(small_nodelist_for_graph$label == delete_node$label)), ]
      
      # update graph
      visNetworkProxy("graph") %>%
        visRemoveNodes(id = delete_node$id)
      # if the node was shown in table remove it
      if(length(which(small_nodelist_for_table$id == delete_node$id)) !=0){
        # update smaller table
        small_nodelist_for_table <<- small_nodelist_for_table[-c(which(small_nodelist_for_table$label == delete_node$label)), ]
        
        # update node and edge table
        output$feature_overview <- renderDataTable({
          update_shown_node_table(small_nodelist_for_table)
        })
      }
    }

    # remove node from global variables of modification history
    modification_history <<- modification_history[-c(nrow(modification_history)), ]
    all_added_nodes <<- all_added_nodes[-c(nrow(all_added_nodes)), ]
    
    # update input selection for node deletion
    updateSelectizeInput(session, "choose_node_to_delete", choices = small_nodelist_for_table$label, server = TRUE)
    
    # update first input selection for edge addition
    nodes_that_can_be_connected <- c()
    for(id in small_nodelist_for_table$id){
      amount_edges <- (length(which(small_edgelist$to == id)) + length(which(small_edgelist$from == id)))
      # if there are 3 nodes in the graph each node can have a total of 3-1 edges
      if(amount_edges < (nrow(small_nodelist_for_graph) - 1)){
        nodes_that_can_be_connected <- c(nodes_that_can_be_connected, small_nodelist_for_graph$label[which(small_nodelist_for_graph$id==id)])
      }
    }
    
    updateSelectizeInput(session, "choose_first_connected_node_add", choices = nodes_that_can_be_connected, server = TRUE)
    
    # inform user on the change
    output$info_change <- renderUI({
      HTML(paste0("<p style = 'color:green;'>", "Node with the label ", "<b>", delete_node$label, "</b>", " was", "<b>", " removed", "</b>", " from the graph.", "</p>"))
    })
    
    # disable undo button, if modification_history is now empty
    if (modification_history[nrow(modification_history), 1] == 0 && modification_history[nrow(modification_history), 2] == 0) {
      shinyjs::disable("undo")
    }
  }
  
  
  ##################################
  ######### Color nodes ############
  ##################################
  
  # calculate the different legends and color the nodes everytime one of these events takes place
  calculate_legend_and_color_nodes <- eventReactive(c(input$color_nodes, input$confirm_edge_deletion, input$confirm_edge_addition, input$confirm_node_addition, input$confirm_node_deletion, input$undo),{
    ## "one color (default)"
    if(input$color_nodes == "one color (default)"){
      colors_and_borders <- get_default_colors_and_border(small_nodelist_for_graph)
      # update graph
      visNetworkProxy("graph") %>%
        visUpdateNodes(nodes = colors_and_borders)
      
      # empty range
      output$range <- renderUI({
        HTML("")
      })
    }
    ## "rel_pos"
    if(input$color_nodes == "rel_pos"){
      # inform user that rel_pos only has values 0
      if(all(nodelist_table$rel_pos==0)){
        output$error_only_zeros <- renderUI({
          HTML("<span style='color:red; font-size:14px'> <br/> ERROR: The variable 'rel_pos' only contains 0. Upload data on nodes with values for 'rel_pos' to use this function! </span>")
        })
        # calculate and plot legend
      }else{
        colors_and_borders <- get_rel_pos_colors_and_border(small_nodelist_for_graph)
        plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
        legend("top", legend = c(colors_and_borders[["Borders"]]), pch=21, pt.cex=2.5, cex=1.5, bty='n',
               col = "#0a4ea3", ncol = 3, pt.bg =colors_and_borders[["Colors"]])
        
        output$range <- renderUI({
          HTML(paste0("<p><b>", "Range: ", "</b>", "(", round(min(nodelist_table$rel_pos), 1), ")", " - ", "(", round(max(nodelist_table$rel_pos), 1), ")", "</p>"))
        })
        
        # update graph
        visNetworkProxy("graph") %>%
          visUpdateNodes(nodes = colors_and_borders[["Nodes"]])
      }
    }
    ## "rel_pos_neg"
    if(input$color_nodes == "rel_pos_neg"){
      # inform user that rel_pos_neg only has values 0
      if(all(nodelist_table$rel_pos_neg==0)){
        output$error_only_zeros <- renderUI({
          HTML("<span style='color:red; font-size:14px'> <br/> ERROR: The variable 'rel_pos_neg' only contains 0. Upload data on nodes with values for 'rel_pos_neg' to use this function! </span>")
        })
        # calculate and plot legend
      }else{
        colors_and_borders <- get_rel_pos_neg_colors_and_border(small_nodelist_for_graph)
        
        plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
        legend("top", legend = c(colors_and_borders[["Borders"]]), pch=21, pt.cex=2.5, cex=1.5, bty='n',
               col = "#0a4ea3", ncol = 4, pt.bg =c(colors_and_borders[["Neg_Colors"]], colors_and_borders[["Pos_Colors"]]) )
        
        output$range <- renderUI({
          HTML(paste0("<p><b>", "Range: ", "</b>", "(", round(min(nodelist_table$rel_pos_neg), 1), ")", " - ", "(", round(max(nodelist_table$rel_pos_neg), 1), ")", "</p>"))
        })
        
        # update graph
        visNetworkProxy("graph") %>%
          visUpdateNodes(nodes = colors_and_borders[["Nodes"]])
      }
    }
    ## "degree"
    if(input$color_nodes == "degree"){
      # calculate degrees for visualized nodes
      degree <- c() # initialze "degree" vector
      for(i in 1:length(small_nodelist_for_graph$id)){
        degree[i] <- nrow(edgelist_table[which(edgelist_table$from == small_nodelist_for_graph$id[i]),]) + nrow(edgelist_table[which(edgelist_table$to == small_nodelist_for_graph$id[i]),])
      } 
      degree <- data.frame(degree)
      temp_nodelist <- cbind(small_nodelist_for_graph, degree)
      # inform user that rel_pos only has values 0
      if(all(temp_nodelist$degree==0)){
        output$error_only_zeros <- renderUI({
          HTML("<span style='color:red; font-size:14px'> <br/> ERROR: The 'degree' of all nodes visualized is 0. Please select more nodes with edges to be visualized! </span>")
        })
        # calculate and plot legend
      }else{
        
        colors_and_borders <- get_degree_colors_and_border(temp_nodelist)
        plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
        legend("top", legend = c(colors_and_borders[["Borders"]]), pch=21, pt.cex=2.5, cex=1.5, bty='n',
               col = "#0a4ea3", ncol = 3, pt.bg =colors_and_borders[["Colors"]])
        
        # calculate degrees of complete dataset
        degree <- c() # initialze "degree" vector
        for(i in 1:length(nodelist_table$id)){
          degree[i] <- nrow(edgelist_table[which(edgelist_table$from == nodelist_table$id[i]),]) + nrow(edgelist_table[which(edgelist_table$to == nodelist_table$id[i]),])
        } 
        degree <- data.frame(degree)
        temp_nodelist <- cbind(nodelist_table, degree)
        
        output$range <- renderUI({
          HTML(paste0("<p><b>", "Range: ", "</b>", "(", round(min(temp_nodelist$degree), 1), ")", " - ", "(", round(max(temp_nodelist$degree), 1), ")", "</p>"))
        })
        
        # update graph
        visNetworkProxy("graph") %>%
          visUpdateNodes(nodes = colors_and_borders[["Nodes"]])
      }
    }
  })
  
  
  # render the legend and update vis with color
  output$legend <- renderPlot({
    calculate_legend_and_color_nodes()
  })
  
  # this makes sure that there is no empty whitespace placeholder where the legend 
  # should be in case that "one color (default)" is selected
  output$uilegend <- renderUI({
    if("rel_pos" %in% input$color_nodes | "rel_pos_neg" %in% input$color_nodes | "degree" %in% input$color_nodes){
      plotOutput(outputId = "legend", height= "250px", bg="transparent") #maybe remove background transparent (not testes yet)
    }
  })
  
  #######################################
  ######### Confusion matrix ############
  #######################################
  
  sens_spec <- eventReactive(c(input$upload_dataset, input$retrain, input$predict),{
    # get retrained graph values
    r <- GET(paste(api_path, "/data/performance_values",sep=""))
    stop_for_status(r)
    values <- as.numeric(fromJSON(content(r, type = "text"), flatten = TRUE))
    
    HTML(paste0("<font size='+1'>", "Sensitivity: ", values[5], "%, Specificity: ", values[6], "%", "</font>"))
  })
  
  output$sens_spec <- renderText({
   sens_spec()
  })
  
  
  calculate_conf_matrix <- eventReactive(c(input$upload_dataset, input$retrain, input$predict),{
    # get retrained graph values
    r <- GET(paste(api_path, "/data/performance_values",sep=""))
    stop_for_status(r)
    values <- as.numeric(fromJSON(content(r, type = "text"), flatten = TRUE))
    
    TClass <- factor(c("False", "False", "True", "True"))
    PClass <- factor(c("False", "True", "False", "True"))
    Color  <- factor(c(0,1,1,0))
    Y      <- c(values[1], values[2], values[3], values[4]) #(TN,FP,FN,TP)
    conf_matrix <- data.frame(TClass, PClass, Y, Color)
    
    ggplot(data =  conf_matrix, mapping = aes(x = TClass, y = PClass)) +
      geom_tile(aes(fill = Color, alpha = 0.2), colour = "white") +
      geom_text(aes(label = sprintf("%1.0f", Y)), size = 10) +
      scale_fill_manual(values=c("steelblue", "grey")) +
      theme_bw() + theme(legend.position = "none") + labs(x = "True Class", y = "Predicted Class", title = "Confusion matrix") +
      theme(axis.line=element_blank(),axis.ticks=element_blank(),axis.text.x=element_text(size = 20, margin=margin(-10,0,0,0)),
            axis.text.y=element_text(angle = 90, size = 20, hjust = 0.5, margin=margin(0,-10,0,0)), axis.title.x = element_text(size = 20, margin=margin(0,0,0,0)),
            axis.title.y = element_text(size = 20, margin=margin(0,0,0,0)), title = element_text(size=20),
            panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),panel.background = element_rect(fill = "transparent"), # bg of the panel
            plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
            )
   
  })
  # render the legend and update vis with color
  output$confmatrix <- renderPlot({
    calculate_conf_matrix()
  }, height=250, width=230, bg="transparent")
  
  ###################################################################
  ######### Reset Coloring when Slider or Radio is changed ##########
  ###################################################################
  
  observeEvent(ignoreInit = T,c( 
    # the events that trigger this
    input$upload_edges,
    input$choose_patient,
    input$slider,
    input$radio
  ), {
    
    if(("rel_pos" %in% colnames(nodelist_table)) & ("rel_pos_neg" %in% colnames(nodelist_table))){
      updateSelectInput(session, "color_nodes",
                        choices = list(
                          "one color (default)",
                          "rel_pos",
                          "rel_pos_neg",
                          "degree"),
                        selected = "one color (default)"
      )
    }
    if(!("rel_pos" %in% colnames(nodelist_table)) & ("rel_pos_neg" %in% colnames(nodelist_table))){
      updateSelectInput(session, "color_nodes",
                        choices = list(
                          "one color (default)",
                          "rel_pos_neg",
                          "degree"),
                        selected = "one color (default)"
      )
    }
    if(("rel_pos" %in% colnames(nodelist_table)) & !("rel_pos_neg" %in% colnames(nodelist_table))){
      updateSelectInput(session, "color_nodes",
                        choices = list(
                          "one color (default)",
                          "rel_pos",
                          "degree"),
                        selected = "one color (default)"
      )
    }
    if(!("rel_pos" %in% colnames(nodelist_table)) & !("rel_pos_neg" %in% colnames(nodelist_table))){
      updateSelectInput(session, "color_nodes",
                        choices = list(
                          "one color (default)",
                          "degree"),
                        selected = "one color (default)"
      )
    }
    
  })
  
  
  ############################################
  ######## Data Table Nodes and Edges ########
  ############################################
  
  # reactive expression that extracts and prepares the current data on nodes and edges ----------------------------------
  # @return list with data.frame of nodes for presenting in a table, 
  #         data.frame of nodes for graph vis, and data.frame of edges for table and graph vis
  calculate_smaller_node_and_edge_list <- eventReactive(c(input$upload_edges, input$choose_patient, input$slider, input$radio, input$choose_patient_own_data),{ 
    nodelist <- nodelist_table
    edgelist <- edgelist_table
    ### create sub node table ###
    
    # sort nodelist by XAI values (method is selected by radiobutton)
    if(sorting){
      if(input$radio == "rel_pos_highlow"){
        nodelist_for_table <- nodelist[order(nodelist[["rel_pos"]], decreasing = TRUE),]
      }
      if(input$radio == "rel_pos_lowhigh"){
        nodelist_for_table <- nodelist[order(nodelist[["rel_pos"]], decreasing = FALSE),]
      }
      if(input$radio == "rel_pos_neg_highlow"){
        nodelist_for_table <- nodelist[order(nodelist[["rel_pos_neg"]], decreasing = TRUE),]
      }
      if(input$radio == "rel_pos_neg_lowhigh"){
        nodelist_for_table <- nodelist[order(nodelist[["rel_pos_neg"]], decreasing = FALSE),]
      }
    }else{
      nodelist_for_table <- nodelist
    }
    
    # only show the amount of nodes selected by the sliding bar
    nodelist_for_table <- nodelist_for_table[1:input$slider,] 
    
    # for plotting the nodes, we also need the nodes they are connected to
    nodelist_for_graph <- nodelist_for_table #we want to make the table for plotting bigger
    # iterate over our nodes
    for(ids in nodelist_for_table$id){
      # check if node is in edgelist$from
      if(length(which(edgelist$from == ids)) > 0){
        # check if the connected nodes are in our nodelist
        for(i in which(edgelist$from == ids)){
          # if node id is not yet in table, rbind it to table and save in nodelist_for_graph
          if(!(edgelist$to[i] %in% nodelist_for_graph$id)){
            nodelist_for_graph <- rbind(nodelist_for_graph, nodelist[which(nodelist$id==edgelist$to[i]), ])
          }
        }
      }
      # check if node is in edgelist$to
      if(length(which(edgelist$to == ids)) > 0){
        # check if the connected nodes are in our nodelist
        for(i in which(edgelist$to == ids)){
          # if node id is not yet in table, rbind it to table and save in nodelist_for_graph
          if(!(edgelist$from[i] %in% nodelist_for_graph$id)){
            nodelist_for_graph <- rbind(nodelist_for_graph, nodelist[which(nodelist$id==edgelist$from[i]), ])
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
      # check if there is a entry in edgelist$from
      if(length(which(edgelist$from == ids)) > 0){
        rows <- append(rows, which(edgelist$from == ids))
      }
      # check if there is a entry in edgelist$to
      if(length(which(edgelist$to == ids)) > 0){
        rows <- append(rows, which(edgelist$to == ids))
      }               
    }
    # some rows could have been counted double so only save unique values
    rows <- unique(rows) 
    # select rows
    edgelist <- edgelist[rows,]
  
    # save in global variable
    small_edgelist <<- edgelist
    
    # update node and edge table
    output$feature_overview <- renderDataTable({
      update_shown_node_table(small_nodelist_for_table)
    })
    output$edge_feature_overview <- renderDataTable({
      update_shown_edge_table(small_edgelist, nodelist_table)
    })
    }
  )
  
  ##################################
  ####### Download Results #########
  ##################################
  
  # download the modified graph, being the current version of edgelist_table and nodelist_table ----------------------------------
  output$download <- downloadHandler(
    filename = function() {
      paste0("modified_network_data.zip")
    },
    content = function(file) {
      write.csv(nodelist_table, "modified_nodelist.csv", row.names = FALSE)
      write.csv(edgelist_table, "modified_edgelist.csv", row.names = FALSE)
      files <- c("modified_nodelist.csv", "modified_edgelist.csv")
      # create the zip file
      zip(file, files)
    },
    contentType = "application/zip"
  )
}

