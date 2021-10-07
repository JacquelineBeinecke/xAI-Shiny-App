library(shiny)
library(visNetwork)
library(igraph)
library(DT)
library(uuid)
library(shinyjs)
library(shinyBS)
library(zip)
library(rje)
library(png)


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


##################################
############# UI #################
##################################

ui <- fluidPage(
  
  # activate the function to dis/enable tabs
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = app_jscode, functions = c("disableTab", "enableTab")),
  shinyjs::inlineCSS(app_css),
  
  # header with title of the software
  titlePanel(h1("Interactive xAI Platform", style = {"color: white; background-color:#1dbbf4; padding: 20px;"})),
  
  # page structure with multiple tabs
  navbarPage("Menu", id = "tabs",
             
             # TAB 1
             tabPanel("Home",
                      
                      fluidRow(
                        # Information on purpose
                        column(8,
                               shiny::HTML("<br><center> <h3> What you'll find here. </h3> </center><br>"),
                               shiny::HTML("<p><center>
                      The Interactive xAI Platform can be used to better understand, validate and improve the decision making of Graph Neural Networks (GNN).
                      </p></center>"),
                               shiny::HTML("<p><center>
                      Therefore, this software allows you to visually explore and modify the graph data in order to test the resulting effects on the GNN prediction.
                      </p></center>"),
                               shiny::HTML("<p><center>
                      The platform has been developed for simple, undirected, homogeneous graphs which represent a biological network.
                      </p></center>")
                        ),
                        # Symbolic image of a network
                        column(4,
                               div(
                                 align = "center",
                                 tags$img(src = "network.png", width = "200px", height = "200px")
                               ),
                        )
                      ),
                      # Page break
                      tags$hr(),
                      
                      # Instructions for use
                      fluidRow(
                        shiny::HTML("<center> <h3> How to use this Platform. </h3> </center><br>")
                      ),
                      fluidRow(
                        # Step 1
                        column(3,
                               div(class = "panel panel-default", style = {"box-shadow: 2px 2px lightgrey"},
                                   div(class = "panel-body", width = "600px", align = "center",
                                       div(
                                         tags$img(src = "one.png", width = "50px", height = "50px")
                                       ),
                                       div(
                                         tags$h3("Upload Graph Data"),
                                         tags$p("Upload the data for nodes and edges, including their attributes and the computed relevance values from xAI methods.")
                                       )
                                   )
                               )
                        ),
                        # Step 2
                        column(3,
                               div(class = "panel panel-default", style = {"box-shadow: 2px 2px lightgrey"},
                                   div(class = "panel-body", width = "600px", align = "center",
                                       div(
                                         tags$img(src = "two.png", width = "50px", height = "50px")
                                       ),
                                       div(
                                         tags$h3("Explore the Graph"),
                                         tags$p("Explore the network to understand its structure and identify relevant components.")
                                       )
                                   )
                               )
                        ),
                        # Step 3
                        column(3,
                               div(class = "panel panel-default", style = {"box-shadow: 2px 2px lightgrey"},
                                   div(class = "panel-body", width = "600px", align = "center",
                                       div(
                                         tags$img(src = "three.png", width = "50px", height = "50px")
                                       ),
                                       div(
                                         tags$h3("Modify the Graph"),
                                         tags$p("Modify the graph based on the relevances or your domain knowledge by deleting / adding nodes or edges.")
                                       )
                                   )
                               )
                        ),
                        # Step 4
                        column(3,
                               div(class = "panel panel-default", style = {"box-shadow: 2px 2px lightgrey"},
                                   div(class = "panel-body", width = "600px", align = "center",
                                       div(
                                         tags$img(src = "four.png", width = "50px", height = "50px")
                                       ),
                                       div(
                                         tags$h3("Re-train or Predict"),
                                         tags$p("Download the modified graph data and use it for re-training or predictions on your GNN. Re-upload the modified graph together with newly computed relevances.")
                                       )
                                   )
                               )
                        )
                      )
             ),
             # TAB 2
             tabPanel("Upload Data",
                      
                      fluidRow(
                        column(12,
                               column(6,
                                      
                                      # upload data on nodes
                                      wellPanel(
                                        tags$h3("Step 1: Upload Data on Nodes"),
                                        fileInput("upload_nodes", " ", buttonLabel = "Upload..."),
                                        
                                        # placeholder for error messages
                                        htmlOutput("error_upload_nodes")
                                      ),
                                      
                                      # table representing the required data structure. it disappears after the upload was successful
                                      conditionalPanel(
                                        condition = "!(output.nodelist_uploaded)",
                                        tags$h3("The following Structure is mandatory:"),
                                        tags$p("Hint 1: Network visualizations with more than 200 nodes might get cluttered.", style = {"color: dimgray; font-style:italic; font-size:14px;"}),
                                        tags$p("Hint 2: If you don't have values for 'rel_pos' or 'rel_pos_neg', just enter a column with zeros.", style = {"color: dimgray; font-style:italic; font-size:14px;"}),
                                        tableOutput("required_structure_nodelist")
                                      ),
                                      # preview of the uploaded data on nodes
                                      dataTableOutput("preview_nodes"),
                               ),
                               column(6,
                                      
                                      # upload data on edges is available after successful upload of nodelist
                                      conditionalPanel(condition = "output.nodelist_uploaded",
                                                       wellPanel(
                                                         tags$h3("Step 2: Upload Data on Edges"),
                                                         fileInput("upload_edges", " ", buttonLabel = "Upload..."),
                                                         
                                                         # placeholder for error messages
                                                         htmlOutput("error_upload_edges")
                                                       )
                                      ),
                                      # table representing the required data structure. it disappears after the upload was successful
                                      conditionalPanel(condition = c("output.nodelist_uploaded && !(output.edgelist_uploaded)"),
                                                       tags$h3("The following Structure is mandatory:"),
                                                       tags$p("Hint 1: If you don't have values for 'rel_pos' or 'rel_pos_neg', just enter a column with zeros.", style = {"color: dimgray; font-style:italic; font-size:14px;"}),
                                                       tableOutput("required_structure_edgelist"),
                                      ),
                                      # preview of the uploaded data on edges
                                      conditionalPanel(condition = "output.nodelist_uploaded",
                                                       dataTableOutput("preview_edges")
                                      )
                               )
                        )
                      )
             ),
             # TAB 3
             
             # create tab on top to select amount of nodes to be shown
             tabPanel("Interact",
                      fluidRow(
                        column(4, 
                               radioButtons("radio", label = h3("Select relevance values to sort nodes by"),
                                            choices = list("rel_pos" = "rel_pos", "rel_pos_neg" = "rel_pos_neg"), 
                                            selected = "rel_pos"),
                        ),
                        column(8, 
                               #default nodes to display is 3 (value = 3)
                               #default max is 100 but this gets updated as soon as node and edge data are uploaded
                               #then the max value will always be the amount of nodes in the data
                               sliderInput("slider", label = h3("Select how many nodes to display"), 
                                           min = 1, max = 100, value = 3) 
                        )
                      ),
                      fluidRow(
                        column(12,
                               column(8,
                                      
                                      # graph object
                                      visNetworkOutput("graph"),
                                      
                                      # legend for rel_pos with sequential color palette "Gray"
                                      conditionalPanel(condition = "input.color_nodes == 'rel_pos'", align = "center",
                                                       tags$div(style = {"background-color:#FAFAFA; text-align: center; display:inline-block; width: 60px;"}, htmlOutput("lowest_rel_pos", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle;"})),
                                                       tags$div(style = {"background-color:#E0E0E0; text-align: center; display:inline-block; width: 60px;"}, htmlOutput("low_rel_pos", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle;"})),
                                                       tags$div(style = {"background-color:#9E9E9E; text-align: center; display:inline-block; width: 60px;"}, htmlOutput("middle_rel_pos", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle;"})),
                                                       tags$div(style = {"background-color:#616161; text-align: center; display:inline-block; width: 60px;"}, htmlOutput("high_rel_pos", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle; color:white;"})),
                                                       tags$div(style = {"background-color:#212121; text-align: center; display:inline-block; width: 60px;"}, htmlOutput("highest_rel_pos", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle; color:white;"}))
                                      ),
                                      # legend for rel_pos_neg with diverging color palette from "Blue" to "Red"
                                      conditionalPanel(condition = "input.color_nodes == 'rel_pos_neg'", align = "center",
                                                       # negative values
                                                       tags$div(style = {"background-color:#0D47A1; text-align: center; display:inline-block; width: 60px;"}, htmlOutput("neg_highest_relevance", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle; color:white;"})),
                                                       tags$div(style = {"background-color:#1976D2; text-align: center; display:inline-block; width: 60px;"}, htmlOutput("neg_high_relevance", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle; color:white;"})),
                                                       tags$div(style = {"background-color:#2196F3; text-align: center; display:inline-block; width: 60px;"}, htmlOutput("neg_middle_relevance", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle;"})),
                                                       tags$div(style = {"background-color:#90CAF9; text-align: center; display:inline-block; width: 60px;"}, htmlOutput("neg_low_relevance", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle;"})),
                                                       tags$div(style = {"background-color:#E3F2FD; text-align: center; display:inline-block; width: 60px;"}, htmlOutput("neg_lowest_relevance", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle;"})),
                                                       # zero
                                                       tags$div(style = {"background-color:white; text-align: center; display:inline-block; width: 10px;"}, htmlOutput("relevance_zero", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle;"})),
                                                       # positive values
                                                       tags$div(style = {"background-color:#FFEBEE; text-align: center; display:inline-block; width: 60px;"}, htmlOutput("pos_lowest_relevance", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle;"})),
                                                       tags$div(style = {"background-color:#FFCDD2; text-align: center; display:inline-block; width: 60px;"}, htmlOutput("pos_low_relevance", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle;"})),
                                                       tags$div(style = {"background-color:#E57373; text-align: center; display:inline-block; width: 60px;"}, htmlOutput("pos_middle_relevance", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle;"})),
                                                       tags$div(style = {"background-color:#D32F2F; text-align: center; display:inline-block; width: 60px;"}, htmlOutput("pos_high_relevance", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle; color:white;"})),
                                                       tags$div(style = {"background-color:#B71C1C; text-align: center; display:inline-block; width: 60px;"}, htmlOutput("pos_highest_relevance", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle; color:white;"}))
                                      ),
                                      # legend for degree with sequential color palette "Light Blue"
                                      conditionalPanel(condition = "input.color_nodes == 'degree'", align = "center",
                                                       tags$div(style = {"background-color:#E1F5FE; text-align: center; display:inline-block; width: 60px;"}, htmlOutput("lowest_degree", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle;"})),
                                                       tags$div(style = {"background-color:#B3E5FC; text-align: center; display:inline-block; width: 60px;"}, htmlOutput("low_degree", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle;"})),
                                                       tags$div(style = {"background-color:#29B6F6; text-align: center; display:inline-block; width: 60px;"}, htmlOutput("middle_degree", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle;"})),
                                                       tags$div(style = {"background-color:#0288D1; text-align: center; display:inline-block; width: 60px;"}, htmlOutput("high_degree", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle; color:white;"})),
                                                       tags$div(style = {"background-color:#01579B; text-align: center; display:inline-block; width: 60px;"}, htmlOutput("highest_degree", style = {"margin-top:5px; margin-bottom:0px; vertical-align: middle; color:white;"}))
                                      ),
                                      br(),
                                      
                                      # table with data on edges
                                      tags$h3("Data on Edges"),
                                      tags$p("Hint: One node label can occur multiple times in both columns 'from' and 'to'. Use search function to view all edges of a node.", style = {"color: dimgray; font-style:italic; font-size:14px;"}),
                                      dataTableOutput("edge_feature_overview"),
                                      br(),
                                      
                                      # table with data on nodes
                                      tags$h3("Data on Nodes"),
                                      dataTableOutput("feature_overview")
                               ),
                               
                               # side bar on the right
                               column(4,
                                      
                                      # download-, re-train-, predict-buttons
                                      fluidRow(
                                        column(12,
                                               align = "center",
                                               downloadButton("download", label = "Download", class = "btn-success"),
                                               tags$div(style = "display:inline-block", title = "This functionality hasn't launched yet",
                                                        actionButton("predict", "Predict", class = "btn-primary"),
                                                        actionButton("retrain", "Re-train", class = "btn-primary")
                                               ),
                                               verbatimTextOutput("message_no_predict"),
                                               verbatimTextOutput("message_no_retrain")
                                        )
                                      ),
                                      br(),
                                      
                                      # color nodes by attributes
                                      wellPanel(
                                        fluidRow(
                                          column(12,
                                                 column(12,
                                                        selectInput("color_nodes", h3("Color the Nodes by:"),
                                                                    choices = list(
                                                                      "one color (default)",
                                                                      "rel_pos",
                                                                      "rel_pos_neg",
                                                                      "degree"
                                                                    )
                                                        ),
                                                        # print the value range of the selected attribute in the current data set
                                                        htmlOutput("range")
                                                 ),
                                                 # placeholder for error messages
                                                 htmlOutput("error_only_zeros")
                                          )
                                        )
                                      ),
                                      # modification terminal
                                      wellPanel(
                                        fluidRow(
                                          column(12,
                                                 column(8,
                                                        # modification options
                                                        radioButtons("modify_options", h3("Modify Graph:"),
                                                                     choices = list(
                                                                       "Delete Node" = 1,
                                                                       "Add a new Node" = 2,
                                                                       "Delete Edge" = 3,
                                                                       "Add a new Edge" = 4
                                                                     ),
                                                                     selected = character(0)
                                                        ),
                                                 ),
                                                 # undo Button
                                                 column(4,
                                                        actionButton("undo", "Undo", icon("undo"), class = "btn-primary")
                                                 )
                                          ),
                                          # inform the user on the last change
                                          column(12,
                                                 htmlOutput("info_change"))
                                        )
                                      ),
                                      # delete node
                                      conditionalPanel(condition = "input.modify_options == 1",
                                                       wellPanel(
                                                         selectizeInput("choose_node_to_delete", h4("1. Select Node for Deletion:"), choices = c(), selected = character(0)),
                                                         actionButton("confirm_node_deletion", "Delete Node", class = "btn-primary")
                                                       )
                                      ),
                                      # add new node
                                      conditionalPanel(condition = "input.modify_options == 2",
                                                       wellPanel(
                                                         
                                                         # text input field for node label
                                                         textInput("new_node_label", h4("1. Enter Label for the new Node:"), placeholder = "e.g. ABCC2"),
                                                         
                                                         # dropdown to select node attribute
                                                         selectInput("choose_node_feature", h4("2. Enter Values for Node Attributes (optional, otherwise Value is 0):"), choices = c()),
                                                         fluidRow(
                                                           column(12,
                                                                  column(8,align = "left", style = {"padding: 0px;"},
                                                                         # enter node attribute values
                                                                         numericInput("nodefeature_value", NULL, value = 0)
                                                                  ),
                                                                  column(4,
                                                                         actionButton("confirm_nodeFeature_value", "Enter", class = "btn-primary")
                                                                  ),
                                                                  br(),
                                                                  br(),
                                                                  # placeholder for error messages
                                                                  htmlOutput("error_add_node"),
                                                           )
                                                         ),
                                                         br(),
                                                         br(),
                                                         tags$p("Hint: After adding the node, its attributes values can't be changed!", style = {"color: dimgray; font-style:italic; font-size:14px;"}),
                                                         actionButton("confirm_node_addition", "Add Node", class = "btn-primary"),
                                                         actionButton("cancel_node_addition", "Cancel"),
                                                       )
                                      ),
                                      # delete edge
                                      conditionalPanel(condition = "input.modify_options == 3",
                                                       wellPanel(
                                                         selectizeInput("choose_first_connected_node_delete", h4("1. Select Node for Edge Deletion:"),choices = c()),
                                                         selectizeInput("choose_second_connected_node_delete", h4("2. Select second Node to disconnect:"),choices = c(), selected = character(0)),
                                                         actionButton("confirm_edge_deletion", "Delete Edge", class = "btn-primary")
                                                       )
                                      ),
                                      # add new edge
                                      conditionalPanel(condition = "input.modify_options == 4",
                                                       wellPanel(
                                                         selectizeInput("choose_first_connected_node_add", h4("1. Select Node for Edge Addition:"),choices = c()),
                                                         selectizeInput("choose_second_connected_node_add", h4("2. Select second Node to connect to:"),choices = c(), selected = character(0)),
                                                         
                                                         # dropdown to select edge attribute
                                                         selectInput("choose_edge_feature", h4("3. Enter Values for Edge Attributes (optional, otherwise Value is 0):"), choices = c()),
                                                         fluidRow(
                                                           column(12,
                                                                  column(8,
                                                                         align = "left", style = {"padding: 0px;"},
                                                                         # enter edge attribute values
                                                                         numericInput("edgefeature_value", NULL, value = 0)
                                                                  ),
                                                                  column(4,
                                                                         actionButton("confirm_edgeFeature_value", "Enter", class = "btn-primary")
                                                                  ),
                                                                  br(),
                                                                  br(),
                                                                  htmlOutput("error_add_edge")
                                                           )
                                                         ),
                                                         br(),
                                                         br(),
                                                         tags$p("Hint: After adding the edge, its attribute values can't be changed!", style = {"color: dimgray; font-style:italic; font-size:14px;"}),
                                                         actionButton("confirm_edge_addition", "Add Edge", class = "btn-primary"),
                                                         actionButton("cancel_edge_addition", "Cancel"),
                                                       )
                                      )
                               )
                        )
                      )
             )
  )
)
