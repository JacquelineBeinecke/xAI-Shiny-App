server <- function(input, output, session) {
  ##################################
  ####### global variables #########
  ##################################

  initGlobalVars()
  
  ##########################
  ####### Log File #########
  ##########################
  
  # initialize log output 
  logval <- reactiveValues(
    logOutput = "Log File \n"
  )
  
  # update shown log everytime logOutput changes
  output$log <- renderText({logval$logOutput})
  
  # function to add something to logOutput 
  updateLog <- function(text){
    logval$logOutput <- paste(logval$logOutput, text, sep = "\n")
  }
  
  
  
  #######################################
  ######## upload whole dataset #########
  #######################################
  
  # update selectize input for patients
  observeEvent(input$upload_dataset, {
      print("upload dataset")
      # this is for showing the loading spinner
      output$loading <- renderUI({HTML(" ")})
      # initially disable Interact tab by start of the shiny App 
      shinyjs::js$disableTab("Interact")
      
      # disable select dataset button until dataset is loaded
      shinyjs::disable("upload_dataset")
      
      # get token for url
      api_path <<- "http://127.0.0.1:5000"
      t <- GET(paste(api_path, "/", sep=""))
      stop_for_status(t)
      token <<- fromJSON(content(t, type = "text", encoding = "UTF-8"))
      api_path <<- paste(api_path, token, sep="/")
      
      # get list of patient names
      patient_names <<- getPatientNames(input$choose_a_dataset)
      
      # train initial GNN on selected dataset
      r <- POST(paste(api_path, "/gnn",sep=""), body = list(dataset_name = input$choose_a_dataset), encode = "json")
      # throw error if status returns something else than 200 (so if it didnt work)
      stop_for_status(r)
      
      # append patient info to patient
      for(i in 1:length(patient_names)){
        pat_id = i-1
        info <- getInitPatInfo(pat_id, 0, input$choose_a_dataset)
        pat_names[i] <<- paste(patient_names[i], " (In ", info[1], ", True label: ", info[2], ", Predicted label: ", info[3], ", GNNs prediction confidence: ", info[4], ")", sep="")
      }
      
      # init list that is indexed by pat_id+1 (because Ids start at 0 but R indexes start with 1...) 
      # and contains amount of graphs per patient
      current_graph_ids <<- rep(0, length(patient_names))
      
      # This is so that a new patient gets selected and the loading of a new graph gets triggered 
      updateSelectizeInput(session, "choose_patient", choices = pat_names, selected = pat_names[0])
      # This loads the first patient of the new dataset
      updateSelectizeInput(session, "choose_patient", choices = pat_names, selected = pat_names[1])
      
      # update log about selected dataset
      updateLog(paste("Dataset selected: ", input$choose_a_dataset, sep=""))
      
      # render sensitivity and specificity
      output$sens_spec <- renderText({
        sens_spec()
      })
      
      # render the legend and update vis with color
      output$confmatrix <- renderPlot({
        r <- calculate_conf_matrix()
        plot(r[[1]])
      }, height=250, width=230, bg="transparent")
      
      # update log about gnn conf matrix, gnn sens, spec
      updateLog(calculate_conf_matrix()[[2]])
      updateLog(calculate_conf_matrix()[[3]])
      
      
     
  })
  
  # load graph of selected patient
  observeEvent(ignoreInit = TRUE, input$choose_patient, {
    print("upload patient")
    # this is for showing the loading spinner
    output$loading <- renderUI({HTML(" ")})
    # write in log that changes were not saved 
    if(nrow(modification_history)>1){
      updateLog("Your modifications for this patient were not saved")
    }
     # get patient id
     pat_id <- as.numeric(strsplit(input$choose_patient, split = " ")[[1]][2])

     # get the amount of modified graphs saved for this patient
     max_graph_idx <<- get_max_graphs(pat_id)
     # if the patient was changed update the graph_id to a valid one
     if(max_graph_idx < current_graph_ids[pat_id+1]){
       current_graph_ids[pat_id+1] <<- max_graph_idx
     }

     # update log about selected patient
     updateLog(paste("Currently selected: Patient ", pat_id, ", Graph ", current_graph_ids[pat_id+1], sep=""))
     updateLog(paste("Amount of modified graphs for this patient: ", max_graph_idx, sep=""))
     
     # update backward and forward button
     if(max_graph_idx == 0){
       shinyjs::disable("backward")
       shinyjs::disable("forward")
     }else{
       if(current_graph_ids[pat_id+1]==0){
         shinyjs::disable("backward")
         shinyjs::enable("forward")
       }
       if(current_graph_ids[pat_id+1]==max_graph_idx){
         shinyjs::enable("backward")
         shinyjs::disable("forward")
       }
       if(current_graph_ids[pat_id+1] > 0 & current_graph_ids[pat_id+1] < max_graph_idx){
         shinyjs::enable("backward")
         shinyjs::enable("forward")
       }
     }
     
     # empty warning when new patient is selected
     output$warning_overwriting <- renderUI({ovwriting_warning(current_graph_ids[pat_id+1],max_graph_idx)})
     
     # show patient information
     if(current_graph_ids[pat_id+1]!=0){
       info <- getPatInfo(pat_id, current_graph_ids[pat_id+1], input$choose_a_dataset)
     }else{
       info <- getInitPatInfo(pat_id, current_graph_ids[pat_id+1], input$choose_a_dataset)
     }
     
     # update log about selected patient
     updateLog(paste("Patient Information: In ", info[1], ", True label = ", info[2], ", Predicted label = ", info[3], ", GNNs prediction confidence = ", info[4], sep=""))
     
     # Get node and edge relevance scores 
     node_rel <<- getNodeRelevances(pat_id, current_graph_ids[pat_id+1], input$choose_a_dataset)
     edge_rel <<- getEdgeRelevances(pat_id, current_graph_ids[pat_id+1], input$choose_a_dataset)
     
     # get graph of selected dataset and patient
     r <- GET(paste(api_path, "/data/dataset",sep=""), query = list(dataset_name = input$choose_a_dataset, patient_id = pat_id, graph_id = current_graph_ids[pat_id+1]))
     stop_for_status(r)
     graph <- fromJSON(content(r, type = "text", encoding = "UTF-8"))
     
     load_graph_from_json(graph)
     
     # update max Slider value to amount of nodes
     max = length(nodelist_table[[1]])
     updateSliderInput(session, "slider", max=max, step=1)
  
     # clear any printed error messages on the UI
     output$info_change <- renderUI({HTML(" ")})
     output$error_only_zeros <- renderUI({HTML(" ")})
     output$error_add_node <- renderUI({HTML(" ")})
     output$error_add_edge <- renderUI({HTML(" ")})
  
     # empty all text input fields of edge and node addition
     updateNumericInput(session, "edgefeature_value", value = 0)
     updateTextInput(session, "new_node_label", value = "", placeholder = "e.g. ABCC2")
     updateNumericInput(session, "nodefeature_value", value = 0)
  
     # reset the select Input of color nodes by
     updateSelectInput(session, "color_nodes", selected = "One color (default)")
  
     calculate_smaller_node_and_edge_list()
  
     # enable interact tab when dataset is selected
     shinyjs::js$enableTab("Interact")
     #enable select dataset button when dataset is loaded
     shinyjs::enable("upload_dataset")
   })
  
  
  #######################################################
  ######## update node/edgelist for graph/table #########
  #######################################################
  
  observeEvent(ignoreInit = T,c(
    input$radio,
    input$slider
  ), {
    print("update nodelist because slider or radio changed")
    calculate_smaller_node_and_edge_list()
  })
  
  ###################################################
  ######## dis/enable/update tabs/functions #########
  ###################################################
 
  # update radio buttons based on if rel_pos_neg or rel_pos is present
  observeEvent(ignoreInit = T,input$choose_patient,{
    print("update radio buttons")
    if(!("XAI_2" %in% colnames(node_rel) & !("GNNExplainer" %in% colnames(node_rel)))){
      shinyjs::disable("radio")
      updateRadioButtons(session, "radio",
                       choices = list("Name (A to Z)" = "name_az",
                                      "Name (Z to A)" = "name_za",
                                      "Degree (high to low)" = "degree_highlow",
                                      "Degree (low to high)" = "degree_lowhigh"), 
                       selected = "degree_highlow")
 
    }
    if(!("GNNExplainer" %in% colnames(node_rel)) & ("XAI_2" %in% colnames(node_rel))){
      shinyjs::enable("radio")
       updateRadioButtons(session, "radio",
                          choices = list("Name (A to Z)" = "name_az",
                                         "Name (Z to A)" = "name_za",
                                         "Degree (high to low)" = "degree_highlow",
                                         "Degree (low to high)" = "degree_lowhigh",
                                         "XAI_2 (high to low)" = "XAI_2_highlow",
                                         "XAI_2 (low to high)" = "XAI_2_lowhigh"), 
                          selected = "XAI_2_highlow")
    }
    if(("GNNExplainer" %in% colnames(node_rel)) & !("XAI_2" %in% colnames(node_rel))){
      shinyjs::enable("radio")
      updateRadioButtons(session, "radio",
                         choices = list("Name (A to Z)" = "name_az",
                                        "Name (Z to A)" = "name_za",
                                        "Degree (high to low)" = "degree_highlow",
                                        "Degree (low to high)" = "degree_lowhigh",
                                        "GNNExplainer (high to low)" = "GNNExplainer_highlow",
                                        "GNNExplainer (low to high)" = "GNNExplainer_lowhigh"), 
                         selected = "GNNExplainer_highlow")
    }
    if(("GNNExplainer" %in% colnames(node_rel)) & ("XAI_2" %in% colnames(node_rel))){
      shinyjs::enable("radio")
      updateRadioButtons(session, "radio",
                         choices = list("Name (A to Z)" = "name_az",
                                        "Name (Z to A)" = "name_za",
                                        "Degree (high to low)" = "degree_highlow",
                                        "Degree (low to high)" = "degree_lowhigh",
                                        "GNNExplainer (high to low)" = "GNNExplainer_highlow",
                                        "GNNExplainer (low to high)" = "GNNExplainer_lowhigh",
                                        "XAI_2 (high to low)" = "XAI_2_highlow",
                                        "XAI_2 (low to high)" = "XAI_2_lowhigh"), 
                         selected = "GNNExplainer_highlow")
    }
  })
  
  # initially disable Interact tab by start of the shiny App 
  shinyjs::js$disableTab("Interact")
  
  ############################
  ######### Retrain ##########
  ############################
  
  observeEvent(input$retrain, {
    # so the spinner start right after pressing
    output$spin <- renderText({})
    print("retrain")
    disable_all_action_buttons()
    
    # get patient id
    pat_id <- as.numeric(strsplit(input$choose_patient, split = " ")[[1]][2])
    max_graph <- get_max_graphs(pat_id)
    
    # post modification history to API if changes are made
    if(nrow(modification_history)>1){
      # if a graph that is not the latest graph is changed, deleted all graphs with higher id
      if(current_graph_ids[pat_id+1] != max_graph){
        delete_graphs(pat_id, current_graph_ids[pat_id+1], max_graph)
        if(max_graph-current_graph_ids[pat_id+1] > 1){
          updateLog(paste("The following modified graphs were deleted: ",current_graph_ids[pat_id+1]+1,"-", max_graph, sep=""))
        }else{
          updateLog(paste("The following modified graph was deleted: ",current_graph_ids[pat_id+1]+1, sep=""))
        }
      }
      
      # update log about selected patient
      updateLog(paste("Changes saved for patient ", pat_id, " in graph number ", current_graph_ids[pat_id+1]+1, sep=""))
      
      # create deepcopy of graph in the backend
      r <- POST(paste(api_path, "/deep_copy",sep=""), body = list(patient_id = pat_id, graph_id = current_graph_ids[pat_id+1]), encode = "json")
      # throw error if status returns something else than 200 (so if it didnt work)
      stop_for_status(r)
      
      # update current_graph_ids[pat_id+1], so that changes now get made on the deep_copy and not the modification.
      current_graph_ids[pat_id+1] <<- current_graph_ids[pat_id+1] +1
      
      # send modifications to API
      post_modifications(pat_id, current_graph_ids[pat_id+1], modification_history, all_deleted_nodes, all_added_nodes, all_deleted_edges, all_added_edges, all_deleted_nodes_edges)
      
      # update max graph
      max_graph <- get_max_graphs(pat_id)
    }
    
    # update log about Retraining
    updateLog("Retraining GNN")
  
    # get retrained graph values
    r <- POST(paste(api_path, "/nn_retrain",sep=""), body = list(dataset_name = input$choose_a_dataset), encode = "json")
    stop_for_status(r)
    
    # Get node and edge relevance scores 
    node_rel <<- getNodeRelevances(pat_id, current_graph_ids[pat_id+1], input$choose_a_dataset)
    edge_rel <<- getEdgeRelevances(pat_id, current_graph_ids[pat_id+1], input$choose_a_dataset)
    
    # reset the select Input of color nodes by
    updateSelectInput(session, "color_nodes", selected = "One color (default)")
    
    
    # remove warning message about changes being removed when switching patients
    output$warning_deletion <- renderUI({
      HTML(" ")
    })
    # remove warning message about graphs being overwritten
    output$warning_overwriting <- renderUI({
      HTML(" ")
    })
    
    # render sensitivity and specificity
    output$sens_spec <- renderText({
      sens_spec()
    })
    
    # render the legend and update vis with color
    output$confmatrix <- renderPlot({
      r <- calculate_conf_matrix()
      plot(r[[1]])
    }, height=250, width=230, bg="transparent")
    
    # update log about gnn conf matrix, gnn sens, spec
    updateLog(calculate_conf_matrix()[[2]])
    updateLog(calculate_conf_matrix()[[3]])
    
    # append patient info to patient number
    for(i in 1:length(patient_names)){
      p_id = i-1
      max_g <- get_max_graphs(p_id)
      info <- getPatInfo(p_id, max_g, input$choose_a_dataset)
      pat_names[i] <- paste(patient_names[i], " (In ", info[1], ", True label: ", info[2], ", Predicted label: ", info[3], ", GNNs prediction confidence: ", info[4], ")", sep="")
    }
    # Update patient names with new predictions
    updateSelectizeInput(session, "choose_patient", choices = pat_names, selected = pat_names[pat_id+1])
    
    # update patient information
    info <- getPatInfo(pat_id, current_graph_ids[pat_id+1], input$choose_a_dataset)
    
    # reset modification history
    modification_history <<- data.frame(action = c(0), element = c(0))
    all_deleted_nodes <<- data.frame()
    all_deleted_nodes_edges <<- list()
    all_deleted_edges <<- data.frame()
    all_added_edges <<- data.frame()
    all_added_nodes <<- data.frame()
    
    enable_all_action_buttons()
    # disable forward button
    shinyjs::disable("forward")
    shinyjs::disable("undo")
    
    # disable backward button if no new graph was saved (if no modifications have been made)
    if(max_graph == 0){
      shinyjs::disable("backward")
    }
  })
  
  ############################
  ######### Predict ##########
  ############################
  
  observeEvent(input$predict, {
    # so the spinner start right after pressing
    output$spin <- renderText({})
    print("predict")
    disable_all_action_buttons()
    
    # get patient id
    pat_id <- as.numeric(strsplit(input$choose_patient, split = " ")[[1]][2])
    max_graph <- get_max_graphs(pat_id)
    
    # post modification history to API if changes are made
    if(nrow(modification_history)>1){
      # if a graph that is not the latest graph is changed, deleted all graphs with higher id
      if(current_graph_ids[pat_id+1] != max_graph){
        delete_graphs(pat_id, current_graph_ids[pat_id+1], max_graph)
        if(max_graph-current_graph_ids[pat_id+1] > 1){
          updateLog(paste("The following modified graphs were deleted: ",current_graph_ids[pat_id+1]+1,"-", max_graph, sep=""))
        }else{
          updateLog(paste("The following modified graph was deleted: ",current_graph_ids[pat_id+1]+1, sep=""))
        }
      }
      
      # update log about selected patient
      updateLog(paste("Changes saved for patient ", pat_id, " in graph number ", current_graph_ids[pat_id+1]+1,sep=""))
      
      # create deepcopy of graph in the backend
      r <- POST(paste(api_path, "/deep_copy",sep=""), body = list(patient_id = pat_id, graph_id = current_graph_ids[pat_id+1]), encode = "json")
      # throw error if status returns something else than 200 (so if it didnt work)
      stop_for_status(r)
      
      # update current_graph_ids[pat_id+1], so that changes now get made on the deep_copy and not the modification.
      current_graph_ids[pat_id+1] <<- current_graph_ids[pat_id+1] + 1
 
      # send modifications to API
      post_modifications(pat_id, current_graph_ids[pat_id+1], modification_history, all_deleted_nodes, all_added_nodes, all_deleted_edges, all_added_edges, all_deleted_nodes_edges) 
      
      # enable restore button
      shinyjs::enable("backward")
      
      # update log about selected patient
      updateLog(paste("Currently selected: Patient ", pat_id, ", Graph ", current_graph_ids[pat_id+1], sep=""))
      
      # update max graph
      max_graph <- get_max_graphs(pat_id)
      
    }
    # update log about Predicting
    updateLog("Predicting on GNN")
    
    # get retrained graph values
    r <- POST(paste(api_path, "/nn_predict",sep=""), body = list(patient_id = pat_id, graph_id = current_graph_ids[pat_id+1], dataset_name = input$choose_a_dataset), encode = "json")
    stop_for_status(r)

    # Get node and edge relevance scores 
    node_rel <<- getNodeRelevances(pat_id, current_graph_ids[pat_id+1], input$choose_a_dataset)
    edge_rel <<- getEdgeRelevances(pat_id, current_graph_ids[pat_id+1], input$choose_a_dataset)
    
    # reset the select Input of color nodes by
    updateSelectInput(session, "color_nodes", selected = "One color (default)")

    # remove warning message about changes being removed when switching patients
    output$warning_deletion <- renderUI({
      HTML(" ")
    })
    # remove warning message about graphs being overwritten
    output$warning_overwriting <- renderUI({
      HTML(" ")
    })
    
    # update patient info to patient number
    info <- getPatInfo(pat_id, current_graph_ids[pat_id+1], input$choose_a_dataset)
    pat_names[pat_id+1] <- paste(patient_names[pat_id+1], " (In ", info[1], ", True label: ", info[2], ", Predicted label: ", info[3], ", GNNs prediction confidence: ", info[4], ")", sep="")
 
    # render sensitivity and specificity
    output$sens_spec <- renderText({
      sens_spec()
    })
    
    # render the legend and update vis with color
    # render the legend and update vis with color
    output$confmatrix <- renderPlot({
      r <- calculate_conf_matrix()
      plot(r[[1]])
    }, height=250, width=230, bg="transparent")
    
    # update log about gnn conf matrix, gnn sens, spec
    updateLog(calculate_conf_matrix()[[2]])
    updateLog(calculate_conf_matrix()[[3]])
    
    # Update patient names with new predictions
    updateSelectizeInput(session, "choose_patient", choices = pat_names, selected = pat_names[pat_id+1])
  
    # reset modification history
    modification_history <<- data.frame(action = c(0), element = c(0))
    all_deleted_nodes <<- data.frame()
    all_deleted_nodes_edges <<- list()
    all_deleted_edges <<- data.frame()
    all_added_edges <<- data.frame()
    all_added_nodes <<- data.frame()
    
    enable_all_action_buttons()
    # disable forward button
    shinyjs::disable("forward")
    shinyjs::disable("undo")

    # disable backward button if no new graph was saved (if no modifications have been made)
    if(max_graph == 0){
      shinyjs::disable("backward")
    }
  })
  

  ##################################
  ######### Network Graph ##########
  ##################################
  
  
  # observe upload of edges to update the graph if new data was uploaded ----------------------------------
  observeEvent(ignoreInit = T, c( 
    # the events that trigger this
    input$retrain,
    input$backward,
    input$forward,
    input$slider,
    input$radio,
    input$choose_patient
    ), {
    print("vis graph")
    # create graph element
    output$graph <- renderVisNetwork({
      # create tooltip
      nodes <- small_nodelist_for_graph
      nodes$title <- update_node_tooltip(nodes, edgelist_table)
      
      # plot the graph if edges are given (or left after edge deletions)
      if(nrow(small_edgelist) != 0){
        # update edge tooltip title (only if edges are given)
        small_edgelist$title <- update_edge_tooltip(nodelist_table, small_edgelist)
        small_edgelist$color <- get_rel_colors_for_edge(small_edgelist, input$radio_edge_rel)
        
        set.seed(3414) # set seed so the graph always looks the same for the same nodes and edges
        visNetwork(nodes, small_edgelist) %>%
          visInteraction(zoomView = TRUE, navigationButtons = TRUE, multiselect = TRUE, hover = TRUE) %>%
          # long click on nodes to select multiple nodes or by "Ctrl" + Click
          visIgraphLayout(layout = "layout_in_circle") %>% # vanessa had "layout_with_fr"
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
            selectedBy = list(variable = "label") #with this the drop down menu lets you choose nodes based on their rel_pos value
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
  
  
  
  
  ########################################
  ######### Jump between graphs ##########
  ########################################
  
  observeEvent(input$backward, {
    print("backward")
    # disable  all buttons until graph is loaded
    disable_all_action_buttons()
    
    if(nrow(modification_history)>1){
      updateLog("Your modifications for this graph were not saved")
    }
    
    # get patient id
    pat_id <- as.numeric(strsplit(input$choose_patient, split = " ")[[1]][2])
    
    # update graph id
    current_graph_ids[pat_id+1] <<- current_graph_ids[pat_id+1] - 1
    
    # restore patient graph
    r <- GET(paste(api_path, "/data/dataset",sep=""), query = list(dataset_name = input$choose_a_dataset, patient_id = pat_id, graph_id = current_graph_ids[pat_id+1]))
    stop_for_status(r)
    graph <- fromJSON(content(r, type = "text", encoding = "UTF-8"))
    
    load_graph_from_json(graph)
    
    # Get node and edge relevance scores 
    node_rel <<- getNodeRelevances(pat_id, current_graph_ids[pat_id+1], input$choose_a_dataset)
    edge_rel <<- getEdgeRelevances(pat_id, current_graph_ids[pat_id+1], input$choose_a_dataset)
    
    # update max Slider value to amount of nodes
    max = length(nodelist_table[[1]])
    updateSliderInput(session, "slider", max=max, step=1)
    
    calculate_smaller_node_and_edge_list()
    
    # clear any printed error messages on the UI
    output$info_change <- renderUI({HTML(" ")})
    output$error_only_zeros <- renderUI({HTML(" ")})
    output$error_add_node <- renderUI({HTML(" ")})
    output$error_add_edge <- renderUI({HTML(" ")})
    
    # empty all text input fields of edge and node addition
    updateNumericInput(session, "edgefeature_value", value = 0)
    updateTextInput(session, "new_node_label", value = "", placeholder = "e.g. ABCC2")
    updateNumericInput(session, "nodefeature_value", value = 0)
    
    # reset the select Input of color nodes by
    updateSelectInput(session, "color_nodes", selected = "One color (default)")
    
    # get the amount of modified graphs saved for this patient
    max_graph <- get_max_graphs(pat_id)
    
    # enable all buttons now that data is loaded
    enable_all_action_buttons()
    
    # disable backward button if there is no later graph
    if(current_graph_ids[pat_id+1] == 0){
      shinyjs::disable("backward")
    }
    # disable undo button 
    shinyjs::disable("undo")
    
    # show warning for user
    output$warning_overwriting <- renderUI({ovwriting_warning(current_graph_ids[pat_id+1],max_graph)})
    
    # update patient info to patient number
    info <- getPatInfo(pat_id, current_graph_ids[pat_id+1], input$choose_a_dataset)
    pat_names[pat_id+1] <- paste(patient_names[pat_id+1], " (In ", info[1], ", True label: ", info[2], ", Predicted label: ", info[3], ", GNNs prediction confidence: ", info[4], ")", sep="")
    
    # Update patient names with new predictions
    updateSelectizeInput(session, "choose_patient", choices = pat_names, selected = pat_names[pat_id+1])
    
    # update log about selected patient
    #updateLog(paste("Patient Information: In ", info[1], ", True label = ", info[2], ", Predicted label = ", info[3], ", GNNs prediction confidence = ", info[4], sep=""))
    
  })
  
  observeEvent(input$forward, {
    print("forward")
    # disable all buttons until graph is loaded
    disable_all_action_buttons()
    
    if(nrow(modification_history)>1){
      updateLog("Your modifications for this graph were not saved")
    }
    
    # get patient id
    pat_id <- as.numeric(strsplit(input$choose_patient, split = " ")[[1]][2])
    
    # update graph id
    current_graph_ids[pat_id+1] <<- current_graph_ids[pat_id+1] + 1
    
    # restore patient graph
    r <- GET(paste(api_path, "/data/dataset",sep=""), query = list(dataset_name = input$choose_a_dataset, patient_id = pat_id, graph_id = current_graph_ids[pat_id+1]))
    stop_for_status(r)
    graph <- fromJSON(content(r, type = "text", encoding = "UTF-8"))
    
    load_graph_from_json(graph)
    
    # Get node and edge relevance scores 
    node_rel <<- getNodeRelevances(pat_id, current_graph_ids[pat_id+1], input$choose_a_dataset)
    edge_rel <<- getEdgeRelevances(pat_id, current_graph_ids[pat_id+1], input$choose_a_dataset)
    
    # update max Slider value to amount of nodes
    max = length(nodelist_table[[1]])
    updateSliderInput(session, "slider", max=max, step=1)
    
    calculate_smaller_node_and_edge_list()
    
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
    updateSelectInput(session, "color_nodes", selected = "One color (default)")
    
    # get the amount of modified graphs saved for this patient
    max_graph <- get_max_graphs(pat_id)
    
    # enable all buttons now that data is loaded
    enable_all_action_buttons()
    
    if(current_graph_ids[pat_id+1] == max_graph){
      shinyjs::disable("forward")
    }
    if(current_graph_ids[pat_id+1] == 0){
      shinyjs::disable("backward")
    }
    
    # disable undo button 
    shinyjs::disable("undo")
    
    # show warning for user
    output$warning_overwriting <- renderUI({ovwriting_warning(current_graph_ids[pat_id+1],max_graph)})
    
    # update patient info to patient number
    info <- getPatInfo(pat_id, current_graph_ids[pat_id+1], input$choose_a_dataset)
    pat_names[pat_id+1] <- paste(patient_names[pat_id+1], " (In ", info[1], ", True label: ", info[2], ", Predicted label: ", info[3], ", GNNs prediction confidence: ", info[4], ")", sep="")
    
    # Update patient names with new predictions
    updateSelectizeInput(session, "choose_patient", choices = pat_names, selected = pat_names[pat_id+1])
  
  })
  
  ##################################
  ### Initialize modify options ####
  ##################################
  
  # Initialize first dropdown of modification options ----------------------------------
  observeEvent(ignoreInit = T,c(
    input$choose_patient,
    input$radio,
    input$slider,
    input$backward,
    input$forward
    ), {
      print("init first dropdown")
     
    # nodes that can be deleted  
    nodes_that_can_be_deleted = calculate_nodes_that_can_be_deleted(small_nodelist_for_table, small_edgelist)
    # input for node deletion
    updateSelectizeInput(session, "choose_node_to_delete", choices = nodes_that_can_be_deleted, server = TRUE)
    
    # disable delete button if only one node is in table
    if(length(calculate_nodes_that_can_be_deleted(small_nodelist_for_table, small_edgelist)) == 0){
      shinyjs::disable("confirm_node_deletion")
    }
    
    # first input for edge addition
    nodes_that_can_be_connected <- calculate_nodes_that_can_have_edges_added_to_them(small_nodelist_for_table, small_edgelist)
    updateSelectizeInput(session, "choose_first_connected_node_add", choices = nodes_that_can_be_connected, server = TRUE)

    # first input for edge deletion - only include node labels which have an edge
    node_labels <- first_node_of_connections_that_can_be_removed(small_nodelist_for_table, small_edgelist)
    updateSelectizeInput(session, "choose_first_connected_node_delete", choices = node_labels, server = TRUE)
    
    if(length(calculate_nodes_that_can_be_deleted(small_nodelist_for_table, small_edgelist)) > 0){
      shinyjs::enable("confirm_node_deletion")
    }
    
    if(nrow(small_edgelist) > 1){
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
    input$choose_patient
    input$choose_first_connected_node_delete 
    input$choose_first_connected_node_add
    }, {
      print("init second dropdown")
    # second input for edge deletion - only containing nodes that are connected to the first selected node
    connected_nodes_labels <- second_node_of_connections_that_can_be_removed(input$choose_first_connected_node_delete, small_nodelist_for_graph, small_edgelist)
    updateSelectizeInput(session, "choose_second_connected_node_delete", choices = connected_nodes_labels, server = TRUE)
    
    # second input for edge addition - only containing nodes that are NOT connected to the first selected node
    not_connected_nodes_labels <- calculate_nodes_that_can_be_connected_to_selected_node(input$choose_first_connected_node_add, small_nodelist_for_table, small_edgelist)
    updateSelectizeInput(session, "choose_second_connected_node_add", choices = not_connected_nodes_labels, server = TRUE)
    
    #disable button if there is no possible node to connect to
    if(length(not_connected_nodes_labels) == 0){
      shinyjs::disable("confirm_edge_addition")
      shinyjs::disable("confirm_edgeFeature_value")
      shinyjs::disable("cancel_edge_addition")
    }else{
      shinyjs::enable("confirm_edge_addition")
      shinyjs::enable("cancel_edge_addition")
      if(ncol(edge_features_list)==0){
        shinyjs::disable("confirm_edgeFeature_value")
      }else{
        shinyjs::enable("confirm_edgeFeature_value")
      }
      
    }
  })
  
  
  # initialize dropdown of node attributes for node addition ----------------------------------
  observeEvent(ignoreInit = T, input$choose_patient, {
    print("init node attributes")
    node_features_list <<- subset(nodelist_table, select = -c(1:2))
    feature_names <- colnames(node_features_list)
    updateSelectizeInput(session, "choose_node_feature", choices = feature_names, server = TRUE)
  })
  
  
  # initialize dropdown of edge attributes for edge addition ----------------------------------
  observeEvent(ignoreInit = T, input$choose_patient, {
    print("init edge attributes")
    edge_features <- edge_features_list
    feature_names <- colnames(edge_features)
    updateSelectizeInput(session, "choose_edge_feature", choices = feature_names, server = TRUE)
 
    #if there are no edge features disable enter button
    if(ncol(edge_features_list)==0){
      shinyjs::disable("confirm_edgeFeature_value")
    }
  })
  
  
  ##################################
  ######### Node deletion ##########
  ##################################
  
  # on button click "Delete Node" ----------------------------------
  observeEvent(input$confirm_node_deletion, {
    print("node deletion")
    # update log about selected node to delete
    updateLog(paste("Node deleted: ", input$choose_node_to_delete, sep=""))
    
    # disbale deletion button until deletion is done
    shinyjs::disable("confirm_node_deletion")
    
    # read the node that has been selected by the user for deletion
    id <- nodelist_table$id[which(nodelist_table$label == input$choose_node_to_delete)]
    
    # data frame with newly deleted node, its attributes and edges
    deleted_node <- nodelist_table[which(nodelist_table$id == id), ]
    
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
    
    nodes_that_can_be_deleted = calculate_nodes_that_can_be_deleted(small_nodelist_for_table, small_edgelist)
    # input for node deletion
    updateSelectizeInput(session, "choose_node_to_delete", choices = nodes_that_can_be_deleted, server = TRUE)
    
    # update first input selection for edge addition
    nodes_that_can_be_connected <- calculate_nodes_that_can_have_edges_added_to_them(small_nodelist_for_table, small_edgelist)
    updateSelectizeInput(session, "choose_first_connected_node_add", choices = nodes_that_can_be_connected, server = TRUE)
    
    # update list of nodes for edge deletion - only node labels which have an edge
    node_labels <- first_node_of_connections_that_can_be_removed(small_nodelist_for_table, small_edgelist)
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
    
    if(length(calculate_nodes_that_can_be_deleted(small_nodelist_for_table, small_edgelist)) > 0){
      shinyjs::enable("confirm_node_deletion")
    }
    
    if(nrow(small_edgelist) %in% c(0,1)){
      shinyjs::disable("confirm_edge_deletion")
    }
    
    if(length(calculate_nodes_that_can_be_deleted(small_nodelist_for_table, small_edgelist)) == 0){
      shinyjs::disable("confirm_node_deletion")
    }
    
    # show warning for user
    if(nrow(modification_history)>1){
      output$warning_deletion <- renderUI({
        HTML("<span style='color:red; font-size:14px'> <br/> Warning: If you choose a different patient now, all your modifications will be lost. Please save your modifications by either pressing predict or retrain. </span>")
      })
    }
    
    # update amount of nodes for Sliding bar
    max_nodes = length(nodelist_table[[1]])
    updateSliderInput(session, "slider", max=max_nodes, step=1)
    
  })
  
  
  
  ##################################
  ######### Edge deletion ##########
  ##################################
  
  # on button click "Delete Edge" ----------------------------------
  observeEvent(input$confirm_edge_deletion, {
    
    # disable delete button until deletion is done
    shinyjs::disable("confirm_edge_deletion")
    print("edge deletion")
    # update log about selected patient
    updateLog(paste("Edge deleted between nodes: ", input$choose_first_connected_node_delete, " and ", input$choose_second_connected_node_delete, sep=""))
    
    
    # read which connected nodes have been selected by the user for deletion
    id_first_node <- nodelist_table$id[which(nodelist_table$label == input$choose_first_connected_node_delete)]
    id_second_node <- nodelist_table$id[which(nodelist_table$label == input$choose_second_connected_node_delete)]
    id_edge <- edgelist_table$id[which((edgelist_table$from == id_first_node & edgelist_table$to == id_second_node) |
                                   (edgelist_table$to == id_first_node & edgelist_table$from == id_second_node))]
    
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
    node_labels <- first_node_of_connections_that_can_be_removed(small_nodelist_for_table, small_edgelist)
    updateSelectizeInput(session, "choose_first_connected_node_delete", choices = node_labels, server = TRUE)
    
    # update second input selection for edge deletion
    connected_nodes_labels <- second_node_of_connections_that_can_be_removed(input$choose_first_connected_node_delete, small_nodelist_for_graph, small_edgelist)
    updateSelectizeInput(session, "choose_second_connected_node_delete", choices = connected_nodes_labels, server = TRUE)
    
    # update first input selection for edge addition
    nodes_that_can_be_connected <- calculate_nodes_that_can_have_edges_added_to_them(small_nodelist_for_table, small_edgelist)
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
    if(nrow(small_edgelist) > 1){
      shinyjs::enable("confirm_edge_deletion")
    }
    
    
    # show warning for user
    if(nrow(modification_history)>1){
      output$warning_deletion <- renderUI({
        HTML("<span style='color:red; font-size:14px'> <br/> Warning: If you choose a different patient now, all your modifications will be lost. Please save your modifications by either pressing predict or retrain. </span>")
      })
    }
  })
  
  
  ##################################
  ######### Edge addition ##########
  ##################################
  
  # on button click "Enter" ----------------------------------
  observeEvent(input$confirm_edgeFeature_value, {
    print("enter edge feature value")
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
      
      # disable enter button if there are no more features to add values for
      if(length(feature_names)==0){
        shinyjs::disable("confirm_edgeFeature_value")
      }
    }
  })
  
  
  # on button click "Add Edge" ----------------------------------
  observeEvent(input$confirm_edge_addition, {
    print("add edge")
    shinyjs::disable("confirm_edge_addition")
    
    # read selected nodes ids for edge addition and the entered feature values
    id_first_node <- nodelist_table$id[which(nodelist_table$label == input$choose_first_connected_node_add)]
    id_second_node <- nodelist_table$id[which(nodelist_table$label == input$choose_second_connected_node_add)]
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
        
        shinyjs::enable("confirm_edge_addition")
        
        # enable enter of values again (it may have been turned off after enter all feature values)
        shinyjs::enable("confirm_edgeFeature_value")
      
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
        
        # update log about selected patient
        temp_log <- c()
        for(i in 1:ncol(temporary_added_edge_feature)){
          if(i!=ncol(temporary_added_edge_feature)){
            temp_log[i] <- paste(names(temporary_added_edge_feature)[i], ": ", nodelist_table$label[which(nodelist_table$id==temporary_added_edge_feature[[i]][1])], ", ", sep="")
          }else{ # this case is so that there is no comma at the end
            temp_log[i] <- paste(names(temporary_added_edge_feature)[i], ": ", temporary_added_edge_feature[[i]][1], sep="")
          }
        }
        updateLog(paste("Edge added: ", paste(temp_log, collapse=""), sep=""))
        
        # set relevance value to 0 as default (two xai methods two zeros, add more if there are more methods)
        edge_rel <<- rbind(edge_rel, c(id_edge,0,0))
        
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
        nodes_that_can_be_connected <- calculate_nodes_that_can_have_edges_added_to_them(small_nodelist_for_table, small_edgelist)
        updateSelectizeInput(session, "choose_first_connected_node_add", choices = nodes_that_can_be_connected, server = TRUE)
        
        # update second input selection for edge addition
        not_connected_nodes_labels <- calculate_nodes_that_can_be_connected_to_selected_node(input$choose_first_connected_node_add, small_nodelist_for_table, small_edgelist)
        updateSelectizeInput(session, "choose_second_connected_node_add", choices = not_connected_nodes_labels, server = TRUE)
        
        
        # update first input selection for edge deletion - only node labels which have an edge
        node_labels <- first_node_of_connections_that_can_be_removed(small_nodelist_for_table, small_edgelist)
        updateSelectizeInput(session, "choose_first_connected_node_delete", choices = node_labels, server = TRUE)
        
        # update second input selection for edge deletion
        connected_nodes_labels <- second_node_of_connections_that_can_be_removed(input$choose_first_connected_node_delete, small_nodelist_for_graph, small_edgelist)
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
        # enable enter of values again (it may have been turned off after enter all feature values)
        shinyjs::enable("confirm_edgeFeature_value")
        # show warning for user
        if(nrow(modification_history)>1){
          output$warning_deletion <- renderUI({
            HTML("<span style='color:red; font-size:14px'> <br/> Warning: If you choose a different patient now, all your modifications will be lost. Please save your modifications by either pressing predict or retrain. </span>")
          })
        }
        # enable edge deletion in case it was disabled before, because there were no more edges
        if(nrow(small_edgelist) > 1){
          shinyjs::enable("confirm_edge_deletion")
        }
      }
    }
  })
  
  
  # on button click "Cancel" ----------------------------------
  observeEvent(input$cancel_edge_addition, {
    print("cancel edge addition")
    # clear numeric input
    updateNumericInput(session, "edgefeature_value", value = 0)
    
    # reset temporary variables to initial state (ready for next edge addition)
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
    
    # selection list of edge features needs to contain all attributes again
    edge_features <- subset(edgelist_table, select = -c(1:3))
    feature_names <- colnames(edge_features)
    updateSelectizeInput(session, "choose_edge_feature", choices = feature_names, server = TRUE)
    
    # enable enter of values again (it may have been turned off after enter all feature values)
    shinyjs::enable("confirm_edgeFeature_value")
    
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
    print("enter node feature value")
    # read entered values for node attributes
    selected_feature <- input$choose_node_feature
    feature_value <- input$nodefeature_value
    label <- input$new_node_label
    
    # avoid NaN values
    if (is.na(feature_value)) {
      feature_value <- 0
    }
    
    # check 2: user has to enter a node label
      if (label != "") {
        
        # check 3: node label must be unique
        test_whether_unique <- nodelist_table[which(nodelist_table$label == label), 1]
        
        if (length(test_whether_unique) == 0) {
          
          # entered feature value of the user is saved in the global variable
          temporary_added_node_feature[1, which(colnames(temporary_added_node_feature) == selected_feature)] <<- feature_value
          
          # update list of available attributes to enter a value
          feature_names <- colnames(node_features_list)
          updateSelectizeInput(session, "choose_node_feature", choices = feature_names, server = TRUE)
          
          # clear numeric input, after a node value was entered
          updateNumericInput(session, "nodefeature_value", value = 0)
          
          # remove error message if user enters everything correctly
          output$error_add_node <- renderUI({
            HTML(" ")
          })
          
          # disable enter button if there are no more features to add values for
          if(length(feature_names)==0){
            shinyjs::disable("confirm_nodeFeature_value")
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
    
  })
  
  
  # on button click "Add Node" ----------------------------------
  observeEvent(input$confirm_node_addition, {
    print("add node")
    shinyjs::disable("confirm_node_addition")
    
    label <- input$new_node_label
    feature_value <- input$nodefeature_value
    selected_feature <- input$choose_node_feature
    
    # added node receives a new unique id
    id <- UUIDgenerate(use.time = TRUE, n = 1)
    
    # avoid NaN values
    if (is.na(feature_value)) {
      feature_value <- 0
    }
    
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
            
            # set relevance value to 0 as default (one xai method one zeros, add more if there are more methods)
            node_rel <<- rbind(node_rel, c(id, 0.0))

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
            
            # update log about selected patient
            temp_log <- c()
            for(i in 1:ncol(temporary_added_node_feature)){
              if(i!=ncol(temporary_added_node_feature)){
                temp_log[i] <- paste(names(temporary_added_node_feature)[i], ": ", temporary_added_node_feature[[i]][1], ", ", sep="")
              }else{ # this case is so that there is no comma at the end
                temp_log[i] <- paste(names(temporary_added_node_feature)[i], ": ", temporary_added_node_feature[[i]][1], sep="")
              }
            }
            updateLog(paste("Node added: ", paste(temp_log, collapse=""), sep=""))
           
            # update amount of nodes for Sliding bar
            max_nodes = length(nodelist_table[[1]])
            updateSliderInput(session, "slider", max=max_nodes, step=1)
            
            # update input for node deletion
            nodes_that_can_be_deleted = calculate_nodes_that_can_be_deleted(small_nodelist_for_table, small_edgelist)
            # input for node deletion
            updateSelectizeInput(session, "choose_node_to_delete", choices = nodes_that_can_be_deleted, server = TRUE)
            
            # update first input selection for edge addition
            nodes_that_can_be_connected <- calculate_nodes_that_can_have_edges_added_to_them(small_nodelist_for_table, small_edgelist)
            updateSelectizeInput(session, "choose_first_connected_node_add", choices = nodes_that_can_be_connected, server = TRUE)
            
            
            # update first input selection for edge deletion
            # update list of nodes for edge deletion - only node labels which have an edge
            node_labels <- first_node_of_connections_that_can_be_removed(small_nodelist_for_table, small_edgelist)
            updateSelectizeInput(session, "choose_first_connected_node_delete", choices = node_labels, server = TRUE)
            
            # clear text input, after a new node was created
            updateTextInput(session, "new_node_label", value = "", placeholder = "e.g. ABCC2")
            updateNumericInput(session, "nodefeature_value", value = 0)
            
            # reset temporary variables to initial state (ready for next node addition)
            temporary_added_node_feature <<- nodelist_table[0, ]
            temporary_added_node_feature[nrow(temporary_added_node_feature) + 1, ] <<- c("label_value", "id_value", rep(0, length(colnames(nodelist_table)) - 2))
            temporary_added_node_feature[, 3:ncol(temporary_added_node_feature)] <<- as.numeric(temporary_added_node_feature[, 3:ncol(temporary_added_node_feature)])
            
            # selection list of node features needs to contain all attributes again
            feature_names <- colnames(node_features_list)
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
            
            # enable enter of values again (it may have been turned off after enter all feature values)
            shinyjs::enable("confirm_nodeFeature_value")
            # show warning for user
            if(nrow(modification_history)>1){
              output$warning_deletion <- renderUI({
                HTML("<span style='color:red; font-size:14px'> <br/> Warning: If you choose a different patient now, all your modifications will be lost. Please save your modifications by either pressing predict or retrain. </span>")
              })
            }
            # enable node feature addition now that addition of last node is done
            shinyjs::enable("confirm_node_addition")
            
            if(length(calculate_nodes_that_can_be_deleted(small_nodelist_for_table, small_edgelist)) > 0){
              shinyjs::enable("confirm_node_deletion")
            }
            
          } else {
            # error message when a feature value is entered and requires to press "enter" first
            output$error_add_node <- renderUI({
              HTML("<span style='color:red; font-size:14px'> <br/> ERROR: Entered attribute value has not been saved yet. Press ENTER to save the value or remove the value to initialize with 0! </span>")
            })
            # enable node feature addition now that addition of last node is done
            shinyjs::enable("confirm_node_addition")
            
          }
        } else {
          output$error_add_node <- renderUI({
            HTML("<span style='color:red; font-size:14px'> <br/> ERROR: The entered label is already taken. Please enter a new label! </span>")
          })
          # enable node feature addition now that addition of last node is done
          shinyjs::enable("confirm_node_addition")
          
        }
      } else {
        # info for user that label is missing
        output$error_add_node <- renderUI({
          HTML("<span style='color:red; font-size:14px'> <br/> ERROR: Node label is missing. Please enter a label for the new node first! </span>")
        })
        # enable node feature addition now that addition of last node is done
        shinyjs::enable("confirm_node_addition")
        
      }
    
  })
  
  
  # on button click "Cancel" ----------------------------------
  observeEvent(input$cancel_node_addition, {
    print("cancel node addition")
    # clear text / numeric input fields
    updateTextInput(session, "new_node_label", value = "", placeholder = "e.g. ABCC2")
    updateNumericInput(session, "nodefeature_value", value = 0)
    
    # reset temporary variables to initial state (ready for next node addition)
    temporary_added_node_feature <<- nodelist_table[0, ]
    temporary_added_node_feature[nrow(temporary_added_node_feature) + 1, ] <<- c("label_value", "id_value", rep(0, length(colnames(nodelist_table)) - 2))
    temporary_added_node_feature[, 3:ncol(temporary_added_node_feature)] <<- as.numeric(temporary_added_node_feature[, 3:ncol(temporary_added_node_feature)])
    node_features_list <<- nodelist_table[, c(3:ncol(nodelist_table))]
    
    # selection list of node features needs to contain all attributes again
    feature_names <- colnames(node_features_list)
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
  observeEvent(ignoreInit = T, c(input$undo, input$confirm_edge_deletion, input$confirm_edge_addition, input$confirm_node_addition, input$confirm_node_deletion, input$predict, input$retrain, input$choose_patient), {
    print("update undo button")
    if (modification_history[nrow(modification_history), 1] == 0 && modification_history[nrow(modification_history), 2] == 0) {
      shinyjs::disable("undo")
      # remove warning message about changes being removed when switching patients
      output$warning_deletion <- renderUI({
        HTML(" ")
      })
    } else {
      shinyjs::enable("undo")
    }
  })
  
  
  # on button click "undo" - revert the last user modification action ----------------------------------
  observeEvent(input$undo, {
    print("undo")
    # update log about selected dataset
    updateLog(paste("The following action was undone: ", modification_history[nrow(modification_history), 2]," ", modification_history[nrow(modification_history), 1], sep=""))
    
    # check what modification was done last
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
    updateSliderInput(session, "slider", max=max_nodes, step=1)
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
    
    # update list of nodes for deletion
    nodes_that_can_be_deleted = calculate_nodes_that_can_be_deleted(small_nodelist_for_table, small_edgelist)
    # input for node deletion
    updateSelectizeInput(session, "choose_node_to_delete", choices = nodes_that_can_be_deleted, server = TRUE)
    
    # update first input selection for edge addition
    nodes_that_can_be_connected <- calculate_nodes_that_can_have_edges_added_to_them(small_nodelist_for_table, small_edgelist)
    updateSelectizeInput(session, "choose_first_connected_node_add", choices = nodes_that_can_be_connected, server = TRUE)
    
    # update list of nodes for edge deletion - only node labels which have an edge
    node_labels <- first_node_of_connections_that_can_be_removed(small_nodelist_for_table, small_edgelist)
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
    
    if(nrow(small_edgelist) > 1){
      shinyjs::enable("confirm_edge_deletion")
    }
    if(length(calculate_nodes_that_can_be_deleted(small_nodelist_for_table, small_edgelist)) > 0){
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
    node_labels <- first_node_of_connections_that_can_be_removed(small_nodelist_for_table, small_edgelist)
    updateSelectizeInput(session, "choose_first_connected_node_delete", choices = node_labels, server = TRUE)
    
    # update second input selection for edge deletion
    connected_nodes_labels <- second_node_of_connections_that_can_be_removed(input$choose_first_connected_node_delete, small_nodelist_for_graph, small_edgelist)
    updateSelectizeInput(session, "choose_second_connected_node_delete", choices = connected_nodes_labels, server = TRUE)
    
    # update first input selection for edge addition
    nodes_that_can_be_connected <- calculate_nodes_that_can_have_edges_added_to_them(small_nodelist_for_table, small_edgelist)
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
    
    if(nrow(small_edgelist) > 1){
      shinyjs::enable("confirm_edge_deletion")
    }
    if(length(calculate_nodes_that_can_be_deleted(small_nodelist_for_table, small_edgelist)) > 0){
      shinyjs::enable("confirm_node_deletion")
    }
  }
  
  # function to reverse the last edge addition ----------------------------------
  undo_edge_addition <- function() {
    
    # extract edge from list of all added edges
    delete_edge <- all_added_edges[nrow(all_added_edges), ]
    
    # update global edgelist
    edgelist_table <<- edgelist_table[-c(which(edgelist_table$id == delete_edge$id)), ]
    # update global edge relevance list
    edge_rel <<- edge_rel[-c(which(edge_rel$edge_ids == delete_edge$id)), ]
    
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
    nodes_that_can_be_connected <- calculate_nodes_that_can_have_edges_added_to_them(small_nodelist_for_table, small_edgelist)
    updateSelectizeInput(session, "choose_first_connected_node_add", choices = nodes_that_can_be_connected, server = TRUE)
    
    # update second input selection for edge addition
    not_connected_nodes_labels <- calculate_nodes_that_can_be_connected_to_selected_node(input$choose_first_connected_node_add, small_nodelist_for_table, small_edgelist)
    updateSelectizeInput(session, "choose_second_connected_node_add", choices = not_connected_nodes_labels, server = TRUE)
    
    #disable button if there is no possible node to connect to
    if(length(not_connected_nodes_labels) == 0){
      shinyjs::disable("confirm_edge_addition")
      shinyjs::disable("confirm_edgeFeature_value")
      shinyjs::disable("cancel_edge_addition")
    }else{
      shinyjs::enable("confirm_edge_addition")
      shinyjs::enable("cancel_edge_addition")
      if(ncol(edge_features_list)==0){
        shinyjs::disable("confirm_edgeFeature_value")
      }else{
        shinyjs::enable("confirm_edgeFeature_value")
      }
      
    }
    
    # update first input selection for edge deletion - only node labels which have an edge
    node_labels <- first_node_of_connections_that_can_be_removed(small_nodelist_for_table, small_edgelist)
    updateSelectizeInput(session, "choose_first_connected_node_delete", choices = node_labels, server = TRUE)
    
    # update second input selection for edge deletion
    connected_nodes_labels <- second_node_of_connections_that_can_be_removed(input$choose_first_connected_node_delete, small_nodelist_for_graph, small_edgelist)
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
    # disable edge deletion if there are no edges
    if(nrow(small_edgelist) %in% c(1,0)){
      shinyjs::disable("confirm_edge_deletion")
    }
    
    if(length(calculate_nodes_that_can_be_deleted(small_nodelist_for_table, small_edgelist)) == 0){
      shinyjs::disable("confirm_node_deletion")
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
    # update global node relevance list
    node_rel <<- node_rel[-c(which(node_rel$node_ids == delete_node$id)), ]
    
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
    nodes_that_can_be_deleted = calculate_nodes_that_can_be_deleted(small_nodelist_for_table, small_edgelist)
    # input for node deletion
    updateSelectizeInput(session, "choose_node_to_delete", choices = nodes_that_can_be_deleted, server = TRUE)
    
    # update first input selection for edge addition
    nodes_that_can_be_connected <- calculate_nodes_that_can_have_edges_added_to_them(small_nodelist_for_table, small_edgelist)
    updateSelectizeInput(session, "choose_first_connected_node_add", choices = nodes_that_can_be_connected, server = TRUE)
    
    # inform user on the change
    output$info_change <- renderUI({
      HTML(paste0("<p style = 'color:green;'>", "Node with the label ", "<b>", delete_node$label, "</b>", " was", "<b>", " removed", "</b>", " from the graph.", "</p>"))
    })
    
    # disable undo button, if modification_history is now empty
    if (modification_history[nrow(modification_history), 1] == 0 && modification_history[nrow(modification_history), 2] == 0) {
      shinyjs::disable("undo")
    }
    # enable node deletion button
    if(length(calculate_nodes_that_can_be_deleted(small_nodelist_for_table, small_edgelist)) > 0){
      shinyjs::enable("confirm_node_deletion")
    }
  }
  
  ########################################################
  ######## select xai method for edges and color #########
  ########################################################
  
  observeEvent(ignoreInit = T, c(input$radio_edge_rel), {
    # update edge tooltip title (only if edges are given)
    small_edgelist$title <- update_edge_tooltip(nodelist_table, small_edgelist)
    small_edgelist$color <- get_rel_colors_for_edge(small_edgelist, input$radio_edge_rel)

    # update graph
    visNetworkProxy("graph") %>%
      visUpdateEdges(edges = small_edgelist)
  })
  
  ##################################
  ######### Color nodes ############
  ##################################
  
  
  # calculate the different legends and color the nodes everytime one of these events takes place
  calculate_legend_and_color_nodes <- function(){
    ## "one color (default)"
    if(input$color_nodes == "One color (default)"){
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
    if(input$color_nodes == "GNNExplainer"){
      # inform user that rel_pos only has values 0
      if(all(node_rel$GNNExplainer==0)){
        output$error_only_zeros <- renderUI({
          HTML("<span style='color:red; font-size:14px'> <br/> ERROR: The variable 'GNNExplainer' only contains 0. Upload data on nodes with values for 'rel_pos' to use this function! </span>")
        })
        # calculate and plot legend
      }else{
        colors_and_borders <- get_rel_colors_for_node(small_nodelist_for_graph, "GNNExplainer")
        plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
        legend("top", legend = c(colors_and_borders[["Borders"]]), pch=21, pt.cex=2.5, cex=1.5, bty='n',
               col = "#0a4ea3", ncol = 3, pt.bg =colors_and_borders[["Colors"]])

        output$range <- renderUI({
          HTML(paste0("<p><b>", "Range: ", "</b>", "(", min(node_rel$GNNExplainer), ")", " - ", "(", max(node_rel$GNNExplainer), ")", "</p>"))
        })

        # update graph
        visNetworkProxy("graph") %>%
          visUpdateNodes(nodes = colors_and_borders[["Nodes"]])
      }
    }
    
    ## "rel_pos_neg"
    if(input$color_nodes == "XAI_2"){
      # inform user that rel_pos_neg only has values 0
      if(all(node_rel$XAI_2==0)){
        output$error_only_zeros <- renderUI({
          HTML("<span style='color:red; font-size:14px'> <br/> ERROR: The variable 'XAI_2' only contains 0. Upload data on nodes with values for 'rel_pos_neg' to use this function! </span>")
        })
        # calculate and plot legend
      }else{
        colors_and_borders <- get_rel_colors_for_node(small_nodelist_for_graph, "XAI_2")
        
        plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
        legend("top", legend = c(colors_and_borders[["Borders"]]), pch=21, pt.cex=2.5, cex=1.5, bty='n',
               col = "#0a4ea3", ncol = 4, pt.bg =c(colors_and_borders[["Neg_Colors"]], colors_and_borders[["Pos_Colors"]]) )
        
        output$range <- renderUI({
          HTML(paste0("<p><b>", "Range: ", "</b>", "(", round(min(node_rel$XAI_2), 1), ")", " - ", "(", round(max(node_rel$XAI_2), 1), ")", "</p>"))
        })
        
        # update graph
        visNetworkProxy("graph") %>%
          visUpdateNodes(nodes = colors_and_borders[["Nodes"]])
      }
    }
    ## "degree"
    if(input$color_nodes == "Degree"){
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
  }
  
  # only calculate new legend in these cases 
  observeEvent(ignoreInit = T, c(input$color_nodes, input$confirm_edge_deletion, input$confirm_edge_addition, input$confirm_node_addition, input$confirm_node_deletion, input$undo, input$slider, input$radio),{
    print("update node colors and legend")
    calculate_legend_and_color_nodes()
  })
  
  # render the legend and update vis with color
  output$legend <- renderPlot({
    calculate_legend_and_color_nodes()
  })
  
  # this makes sure that there is no empty whitespace placeholder where the legend 
  # should be in case that "one color (default)" is selected
  output$uilegend <- renderUI({
    if("GNNExplainer" %in% input$color_nodes | "XAI_2" %in% input$color_nodes | "Degree" %in% input$color_nodes){
      plotOutput(outputId = "legend", height= "250px") #maybe remove background transparent (not testes yet)
    }
  })

  
  ###################################################################
  ######### Reset Coloring when Slider or Radio is changed ##########
  ###################################################################
  
  observeEvent(ignoreInit = T,c( 
    # the events that trigger this
    input$choose_patient,
    input$slider,
    input$radio,
    input$radio_edge_rel
  ), {
    print("reset coloring")
    if(("GNNExplainer" %in% colnames(node_rel)) & ("XAI_2" %in% colnames(node_rel))){
      updateSelectInput(session, "color_nodes",
                        choices = list(
                          "One color (default)",
                          "GNNExplainer",
                          "XAI_2",
                          "Degree"),
                        selected = "One color (default)"
      )
    }
    if(!("GNNExplainer" %in% colnames(node_rel)) & ("XAI_2" %in% colnames(node_rel))){
      updateSelectInput(session, "color_nodes",
                        choices = list(
                          "One color (default)",
                          "XAI_2",
                          "Degree"),
                        selected = "One color (default)"
      )
    }
    if(("GNNExplainer" %in% colnames(node_rel)) & !("XAI_2" %in% colnames(node_rel))){
      updateSelectInput(session, "color_nodes",
                        choices = list(
                          "One color (default)",
                          "GNNExplainer",
                          "Degree"),
                        selected = "One color (default)"
      )
    }
    if(!("GNNExplainer" %in% colnames(node_rel)) & !("XAI_2" %in% colnames(node_rel))){
      updateSelectInput(session, "color_nodes",
                        choices = list(
                          "One color (default)",
                          "Degree"),
                        selected = "One color (default)"
      )
    }
    
  })
  
  
  ############################################
  ######## Data Table Nodes and Edges ########
  ############################################
  
  # reactive expression that extracts and prepares the current data on nodes and edges ----------------------------------
  # @return list with data.frame of nodes for presenting in a table, 
  #         data.frame of nodes for graph vis, and data.frame of edges for table and graph vis
  calculate_smaller_node_and_edge_list <- function(){ 
    print("inside eventReactive")
    # calculate small node and edge lists
    calculate_small_tables(nodelist_table, edgelist_table, input$radio, input$slider)
    
    # update node and edge table
    output$feature_overview <- renderDataTable({
      update_shown_node_table(small_nodelist_for_table)
    })
    output$edge_feature_overview <- renderDataTable({
      update_shown_edge_table(small_edgelist, nodelist_table)
    })
    
    output$hint_adding_edge <- renderUI({
      HTML(" ")
    })
    
    if(ncol(edgelist_table)<=3){
      shinyjs::hide("choose_edge_feature")
      shinyjs::hide("edgefeature_value")
      shinyjs::hide("confirm_edgeFeature_value")
      shinyjs::hide("hint_adding_edge")
    }else{
      output$hint_adding_edge <- renderUI({
        HTML("<span style='color:dimgray; font-size:14px; font-style:italic'> Hint: After adding the edge, its attribute values can't be changed! </span>")
      })
    }
    
  }
  
  ############################################
  ####### Hide graph actions buttons #########
  ############################################
  
  # the first time this button gets pressed its value is 1 and then the value always get added +1
  observeEvent(input$hide_vis_buttons,{
    # if the value of the action button is uneven hide the action buttons
    if(input$hide_vis_buttons[1] %% 2 != 0){
      updateActionButton(session, "hide_vis_buttons", label = "Show graph actions and information", icon = icon("minus"))
    # if the value of the action button is even show the action buttons
    }else{
      updateActionButton(session, "hide_vis_buttons", label = "Hide graph actions and information" , icon = icon("plus"))
    }
  })
  
  ##################################
  ####### Download Results #########
  ##################################
  
  output$info_download <- renderUI({HTML("<span style='color:gray; font-size:14px'> <br/> The download might take a few minutes. </span>")})
  
  
  
  #download the modified graph, being the current version of edgelist_table and nodelist_table ----------------------------------
  output$download <- downloadHandler(
    filename = function() {
      paste0("results.zip")
    },
    content = function(file) {
      # so the spinner start right after pressing
      output$spin <- renderText({})
      # this is for showing the loading spinner
      shinyjs::disable("download")
      shinyjs::disable("predict")
      shinyjs::disable("retrain")
      # init list of files
      files <- c()
      
      # cut the patients into "n" at a time for processing
      n = 45 #chunks of patients processed at a time
      
      amount_pat <- length(patient_names)
      complete_fits <- amount_pat %/% n
      rest_fit <- amount_pat %% n
      # create vectors of 'from' and 'to' patient values
      from = c()
      to = c()
      for(i in 1:(complete_fits+1)){
        if(i <= complete_fits){
          from = append(from, (i-1)*n)
          to = append(to, i*(n-1)+(i-1))
        }else{
          from = append(from, (i-1)*n)
          to = append(to, (i-1)*n+rest_fit-1)
        }
      }
      
      
      for(chunk in 1:length(from)){
        # get list of all chunk of graphs
        r <- GET(paste(api_path, "/save/results",sep=""), query = list(from_pat = from[chunk], to_pat = to[chunk], dataset_name = input$choose_a_dataset))
        stop_for_status(r)
        graph <- fromJSON(content(r, type = "text", encoding = "UTF-8"))
        
        # iterate over all graphs
        for(i in 1:length(graph)){
          # load nodes
          nodes <- as.data.frame(graph[[i]]$data[[1]])
          colnames(nodes) <- graph[[i]]$columns[[1]]
          
          # load edges
          edges <- as.data.frame(graph[[i]]$data[[2]])
          colnames(edges) <- graph[[i]]$columns[[2]]
          
          # remove ids and replace with labels
          if(nrow(edges)>0){
            # replace ids with labels for showing in the table
            for(idx in 1:nrow(edges)){
              edges$from[idx] <- nodes$label[which(nodes$id==edges$from[idx])]
              edges$to[idx] <- nodes$label[which(nodes$id==edges$to[idx])]
            }
            # sort by  label and remove all ID columns for vis
            edges <- edges[order(edges$from), ]
          }
          
          # remove node ids
          nodes$id <- NULL
          # remove edge ids
          edges$id <- NULL
          
          write.csv(nodes, paste(token,"_patient_",n*(chunk-1)+(i-1), "_node_relevances.csv",sep=""), row.names = FALSE)
          write.csv(edges, paste(token,"_patient_",n*(chunk-1)+(i-1), "_edge_relevances.csv",sep=""), row.names = FALSE)
          
          files <- append(files, c(paste(token,"_patient_",n*(chunk-1)+(i-1), "_node_relevances.csv",sep=""),paste(token,"_patient_",n*(chunk-1)+(i-1), "_edge_relevances.csv",sep="")))
        }
      }
      
      
      writeLines(logval$logOutput, paste(token,"_logFile.txt", sep=""))
      files <- append(files, paste(token,"_logFile.txt", sep=""))
      
      # create the zip file
      zip(file, files)
      
      shinyjs::enable("download")
      shinyjs::enable("predict")
      shinyjs::enable("retrain")
      
      # remove the single files after zip creation
      for(i in 1:length(graph)){
        unlink(paste(token,"_patient_",i-1, "_node_relevances.csv",sep=""))
        unlink(paste(token,"_patient_",i-1, "_edge_relevances.csv",sep=""))
      }
      unlink(paste(token,"_logFile.txt", sep=""))
      
    },
    contentType = "application/zip"
  )

}

  