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
# for api #
library(jsonlite)
library(httr)
library(xml2)
library(ggplot2)


ui <- fluidPage(
  
  # api path
  api_path <<- "http://127.0.0.1:5000",
  
  # activate the function to dis/enable tabs
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = app_jscode, functions = c("disableTab", "enableTab")),
  shinyjs::inlineCSS(app_css),
  
  # header with title of the software
  titlePanel(h1("Interactive XAI Platform", style = {"color: white; background-color:#1dbbf4; padding: 20px;"})),
  
  # page structure with multiple tabs
  navbarPage("Menu", id = "tabs",
             
             # TAB 1
             tabPanel("Home",
                      
                      fluidRow(
                        # Information on purpose
                        column(8,
                               shiny::HTML("<br><center> <h3> What you'll find here. </h3> </center><br>"),
                               shiny::HTML("<p><center>
                      The Interactive XAI Platform can be used to better understand, validate and improve the decision making of Graph Neural Networks (GNN).
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
                                         tags$h3("Select Graph Data"),
                                         tags$p("Choose one of our pre-selected datasets to load into the UI.")
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
                                         tags$h3("Retrain or Predict"),
                                         tags$p("Use the modified graph data for retraining or predictions on your GNN.")
                                       )
                                   )
                               )
                        )
                      )
             ),
             # TAB 2
             tabPanel("Select Data",
                      fluidRow(column(12,
                                      # choose predefined datasets 
                                      wellPanel(
                                        selectizeInput("choose_a_dataset", h4("Select one of the following datasets:"),
                                                       choices = fromJSON(content(GET(paste(api_path,"/data/dataset_name",sep=""),type="basic"),"text", encoding = "UTF-8"),flatten = TRUE), selected = 1),
                                        actionButton("upload_dataset", "Select dataset", class = "btn-primary"),
                                        # placeholder for error messages
                                        htmlOutput("warning_switching_dataset")
                                        )
                                      )
                        )
                      
             ),
             # TAB 3
             
             # create tab on top to select amount of nodes to be shown
             tabPanel("Interact",
                      column(12,
                             fluidRow(
                               column(12,
                                      wellPanel(
                                        selectizeInput("choose_patient", h4("Select patient to see their graph:"), 
                                                       choices = c()
                                                       ),
                                        # placeholder for warning messages
                                        htmlOutput("warning_deletion"),
                                        
                                        # button to hide or show graph actions
                                        actionButton("hide_vis_buttons", label = "Hide graph actions and information" , icon = icon("plus"))
                                      )
                               )
                             )
                      
                      ),
                      fluidRow(
                        column(12,
                               # graph actions (sort and color)
                               conditionalPanel(condition = "input.hide_vis_buttons % 2 == 0",
                               column(7,
                                      column(6,
                                             radioButtons("radio", label = HTML("<h3>","Sort nodes by", "</h3>"),
                                                          choices = list("name (A to Z)" = "name_az",
                                                                         "name (Z to A)" = "name_za",
                                                                         "degree (high to low)" = "degree_highlow",
                                                                         "degree (low to high)" = "degree_lowhigh",
                                                                         "rel_pos (high to low)" = "rel_pos_highlow",
                                                                         "rel_pos (low to high)" = "rel_pos_lowhigh",
                                                                         "rel_pos_neg (high to low)" = "rel_pos_neg_highlow",
                                                                         "rel_pos_neg (low to high)" = "rel_pos_neg_lowhigh"), 
                                                          selected = "name_az", width = "500px"),
                                             
                                             
                                      ),
                                      column(6,
                                             radioButtons("radio_edge_rel", label = HTML("<h3>","Select XAI Method for edges", "</h3>"),
                                                          choices = list("Saliency" = "saliency",
                                                                         "Integrated Gradients" = "ig"), 
                                                          selected = "saliency", width = "500px"),
                                             # color nodes by attributes
                                             selectInput("color_nodes", h3("Color the Nodes by:"),
                                                                           choices = list(
                                                                             "one color (default)",
                                                                             "rel_pos",
                                                                             "rel_pos_neg",
                                                                             "degree")),
                                              # print the value range of the selected attribute in the current data set
                                              htmlOutput("range"),
                                              # placeholder for error messages
                                              htmlOutput("error_only_zeros")
                                      ),
                                      #default nodes to display is 3 (value = 3)
                                      #default max is 100 but this gets updated as soon as node and edge data are uploaded
                                      #then the max value will always be the amount of nodes in the data
                                      sliderInput("slider", label = HTML("<h3>","<p style='line-height:60%'>","Select how many nodes to display","</p></h3>", "<h5>","(their next neighbours will also be displayed in the graph)","</h5>"), 
                                                  min = 1, max = 100, value = 3, width = "500px", step=1) 
                                    ),
                               
                               column(5, align = "center",
                                      wellPanel(
                                        fluidRow(
                                          column(12,
                                                 htmlOutput("sens_spec"),
                                                 br(),
                                                 # output confusion matrix
                                                 tags$div(style = "height:250px",plotOutput("confmatrix")),
                                                 br(),
                                                 tags$div(style = "display:inline-block",
                                                          #downloadButton("download", label = "Download", class = "btn-success"),
                                                          actionButton("predict", "Predict", class = "btn-primary"),
                                                          actionButton("retrain", "Retrain", class = "btn-primary")
                                                 )
                                                 
                                          )
                                        )
                                      )
                               )
                               )
                        )
                      ),
                      fluidRow(
                        column(12,
                               fluidRow(
                                 # graph object
                                 visNetworkOutput("graph", height = "600px"),
                                 
                                 # output legend
                                 uiOutput(outputId = "uilegend")
                               )
                        ),
                        column(12,
                               column(8,
                                      # table with data on edges
                                      div(#style = "margin-top:-8em",
                                          tags$h3("Data on Edges"),
                                          tags$p("Hint: One node label can occur multiple times in both columns 'from' and 'to'. Use search function to view all edges of a node.", style = {"color: dimgray; font-style:italic; font-size:14px;"}),
                                          dataTableOutput("edge_feature_overview"),
                                          br()
                                      ),
                                        
                                        # table with data on nodes
                                        tags$h3("Data on Nodes"),
                                        dataTableOutput("feature_overview")
                                      ),
                                      
                               
                               
                               # side bar on the right
                               column(4,
                                      # modification terminal
                                      wellPanel(
                                        fluidRow(
                                          column(12,
                                                 tags$div(style = "display:inline-block; width:100px", title = "Undo modifications. Only enabled when modifications were performed.",
                                                          actionButton("undo", "Undo", icon("undo"), class = "btn-primary"),
                                                 ),
                                                 tags$div(style = "display:inline-block", title = "Move between graph states saved through predict/retrain.",
                                                          actionButton("backward", icon("backward"), class = "btn-primary"),
                                                          actionButton("forward", icon("forward"), class = "btn-primary")
                                                 ),
                                                 # placeholder for warning messages
                                                 htmlOutput("warning_overwriting"),
                                                 # modification options
                                                 radioButtons("modify_options", h3("Modify Graph:"),
                                                              choices = list(
                                                                "Delete Node" = 1,
                                                                "Add a new Node" = 2,
                                                                "Delete Edge" = 3,
                                                                "Add a new Edge" = 4
                                                                ), 
                                                              selected = character(0))
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
                                                         htmlOutput("hint_adding_edge"),
                                                         actionButton("confirm_edge_addition", "Add Edge", class = "btn-primary"),
                                                         actionButton("cancel_edge_addition", "Cancel"),
                                                       )
                                      ),
                                      tags$style(HTML("#log {height:600px}")),
                                      verbatimTextOutput("log", placeholder = FALSE)
                               )
                        )
                      )
             )
  )
)
