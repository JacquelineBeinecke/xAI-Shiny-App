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
library(shinycssloaders)

# for api #
library(jsonlite)
library(httr)
library(xml2)
library(ggplot2)



ui <- fluidPage(
  #api_path <<- "http://127.0.0.1:5000",
  titlePanel(
    windowTitle = "CLARUS",
    title = tags$head(tags$link(rel="icon", #<i class="fa-solid fa-magnifying-glass"></i>
                                href="data:image/x-icon;base64,AAABAAEAEBAQAAEAetc", 
                                type="image/x-icon")
    )),
    # activate the function to dis/enable tabs
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = app_jscode, functions = c("disableTab", "enableTab")),
  shinyjs::inlineCSS(app_css),
  
  # header with title of the software
  titlePanel(
           title=div(style = {"color: white; background-color:#1dbbf4; padding: 0px;"}, tags$img(width = "338px", height = "100.5px", src="clarus_white.png"))
           #h1("CLARUS", style = {"color: white; background-color:#1dbbf4; padding: 20px;font-family: Garet-Book;"},)
    ),
  
  # page structure with multiple tabs
  navbarPage("Menu", id = "tabs",
             
             # TAB 1
             tabPanel("Home",
                      
                      fluidRow(
                        # Information on purpose
                        column(8,
                               shiny::HTML("<br><center> <h3> What you'll find here. </h3> </center><br>"),
                               shiny::HTML("<p><center>
                      <b>CLARUS</b> (Intera<b>c</b>tive Exp<b>l</b>ainable Pl<b>a</b>tform for G<b>r</b>aph Ne<b>u</b>ral Network<b>s</b>) can be used to better understand, validate and improve the decision making of Graph Neural Networks (GNN).
                      </p></center>"),
                               shiny::HTML("<p><center>
                      Therefore, CLARUS allows you to visually explore and modify the graph data in order to test the resulting effects on the GNN prediction.
                      </p></center>"),
                               shiny::HTML("<p><center>
                      CLARUS has been developed for simple, undirected, homogeneous graphs which represent a biological network.
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
                                         tags$p("Use the modified graph data for retraining or predictions on the GNN.")
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
                                        selectizeInput("choose_a_dataset", h4("Select a dataset:"),
                                                       choices = list("Synthetic Dataset", "KIRC Dataset", "KIRC SubNet"), selected = 1),
                                        actionButton("upload_dataset", "Select dataset", class = "btn-primary"),
                                        div(style="display:inline-block; float:right; margin-top:0.5em",a(href="datasets.zip", "Download datasets as PKL", download=NA, target="_blank")),
                                        #downloadButton("download_dataset", label = "Download original dataset as PKL", class = "btn-success")
                                        ),
                                      div(style = "margin-top:-10em",
                                          conditionalPanel(condition = "input.upload_dataset > 0",
                                                           # placeholder for error messages
                                                           withSpinner(htmlOutput("loading"), type=4, size = 0.5)
                                          )
                                      )
                                      )
                        )
                      
             ),
             # TAB 3
             
             # create tab on top to select amount of nodes to be shown
             tabPanel("Interact",
                      column(1,
                             # button to hide or show graph actions
                             actionButton("hide_info_buttons", label="", icon = icon("plus"))
                      ),
                      column(11, 
                             conditionalPanel(condition = "input.hide_info_buttons % 2 == 0",
                                              wellPanel(
                                                fluidRow(
                                                  
                                                  column(6, 
                                                         selectizeInput("choose_patient", h3("Selected patient:"), 
                                                                        choices = c()
                                                         ),
                                                         # placeholder for warning messages
                                                         htmlOutput("warning_deletion"),
                                                         
                                                         #default nodes to display is 3 (value = 3)
                                                         #default max is 100 but this gets updated as soon as node and edge data are uploaded
                                                         #then the max value will always be the amount of nodes in the data
                                                         sliderInput("slider", label = HTML("<h3>Nodes displayed: </h3>", "<h6>","(their next neighbours will also be displayed in the graph)","</h6>"), 
                                                                     min = 1, max = 100, value = 3, width = "500px", step=1), 
                                                         
                                                         
                                                         
                                                  ),
                                                  column(5, align = 'center',
                                                         fluidRow(
                                                           column(12,
                                                                  htmlOutput("sens_spec"),
                                                                  br(),
                                                                  tags$div(style = "height:250px",plotOutput("confmatrix")),
                                                                  br(),
                                                                  
                                                                  tags$div(style = "display:inline-block;margin-right:-2.3em;margin-top:-10.3em",
                                                                           conditionalPanel(condition = "input.retrain > 0 | input.predict > 0",
                                                                                            withSpinner(htmlOutput("spin"), type=4, size = 0.7, hide.ui = FALSE),
                                                                                            
                                                                           ),
                                                                           
                                                                           tags$div(style = "margin-top:10.3em;margin-right:2.3em",
                                                                                    downloadButton("download", label = "Download Results", class = "btn-success"),
                                                                                    actionButton("predict", "Predict", class = "btn-primary"),
                                                                                    actionButton("retrain", "Retrain", class = "btn-primary"),
                                                                                    # if the user wants to download the results show the following
                                                                                    htmlOutput("info_download")
                                                                           ),
                                                                           
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
                               
                               
                               column(8,
                                      # graph object
                                      visNetworkOutput("graph", height = "600px"),
                                      # output legend
                                      uiOutput(outputId = "uilegend")
                                      
                                      
                                      ),
                                      
                               
                               
                               # side bar on the right
                               column(4,
                                      # modification terminal
                                      wellPanel(
                                        fluidRow(
                                          column(12,
                                                  selectInput("sort", label = HTML("<h4>","Sort nodes:", "</h4>"),
                                                              choices = list("Name (A to Z)" = "name_az",
                                                                             "Name (Z to A)" = "name_za",
                                                                             "Degree (high to low)" = "degree_highlow",
                                                                             "Degree (low to high)" = "degree_lowhigh",
                                                                             "GNNExplainer (high to low)" = "GNNExplainer_highlow",
                                                                             "GNNExplainer (low to high)" = "GNNExplainer_lowhigh"
                                                              )),
                                                  
                                                  selectInput("col_edge_rel", label = HTML("<h4>","Color edges:", "</h4>"),
                                                              choices = list("Saliency" = "saliency",
                                                                             "Integrated Gradients" = "ig")),
                                                  
                                                  # color nodes by attributes
                                                  selectInput("color_nodes", h4("Color nodes:"),
                                                              choices = list(
                                                                "One color (default)",
                                                                "GNNExplainer",
                                                                "Degree")),
                                                  # print the value range of the selected attribute in the current data set
                                                  htmlOutput("range"),
                                                  # placeholder for error messages
                                                  htmlOutput("error_only_zeros"),     
                                                )
                                        )),
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
                                                 radioButtons("modify_options", h4("Modify Graph:"),
                                                              choices = list(
                                                                "Delete Node" = 1,
                                                                "Add a new Node" = 2,
                                                                "Delete Edge" = 3,
                                                                "Add a new Edge" = 4
                                                                ), 
                                                              selected = character(0)),
                                          
                                          # inform the user on the last change
                                          
                                                 htmlOutput("info_change"),
                                        
                                        
                                               
                                      )
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
                                                         #actionButton("cancel_edge_addition", "Cancel"),
                                                       )
                                      ),
                                      
                                      ## coloring, sorting
                                      
                                      
                               )
                        ),
                        column(12,
                               column(6,
                                      # table with data on edges
                                      div(#style = "margin-top:-8em",
                                        tags$h3("Data on Edges"),
                                        tags$p("Hint: One node label can occur multiple times in both columns 'from' and 'to'. Use search function to view all edges of a node.", style = {"color: dimgray; font-style:italic; font-size:14px;"}),
                                        dataTableOutput("edge_feature_overview"),
                                        br()
                                      )
                                      ),
                               column(6,
                                      # table with data on nodes
                                      tags$h3("Data on Nodes"),
                                      dataTableOutput("feature_overview")
                                      )
                               ),
                        column(12,
                               tags$style(HTML("#log {height:600px}")),
                               verbatimTextOutput("log", placeholder = FALSE)
                        )
                      )
             )
  )
)
