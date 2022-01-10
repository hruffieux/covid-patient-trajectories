require(shiny)

load("FPCA_output.RData")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("FPCA trajectories & scores", fluid = TRUE,
             titlePanel('Estimated patient-level trajectories with severity & recovery scores'),
             p("Select a data type to visualize the corresponding scores."),
             p("Hover over a data point to see the estimated trajectories for a patient."),
             fluidRow(
               column(3, selectInput('score_type', 'Data type', score_names)),
               column(3, selectInput('color_type', 'Color', 
                                     c("Severity classes", "Recovery groups"))),
               multiple = TRUE
             ),
             mainPanel(fluidRow(
               splitLayout(cellWidths = c("4%", "48%", "48%"), 
                           plotOutput("legend"),
                           plotOutput("plot_scores",
                                      hover = hoverOpts(id ="plot_hover"), height = "500px"), 
                           plotOutput("plot_trajectories", height = "500px"))
             ),
             verbatimTextOutput("hover_info"), 
             width = 11)
    ),
    tabPanel("FPCA variability & correlation estimates", fluid = TRUE,
             titlePanel('Variance, auto-correlation and cross-correlation functions'),
             p("Select a data type to visualize the corresponding estimates."),
             fluidRow(
               column(3, selectInput('score_type_tab2', 'Data type', score_names)),
               multiple = TRUE
             ),
             mainPanel(fluidRow(
               splitLayout(cellWidths = c("50%", "50%"),
                           plotOutput("plot_correlation", height = "500px"),
                           plotOutput("plot_variance", height = "500px"))
             ),
             width = 11)
    ),
    tabPanel("Group-level longitudinal estimates", fluid = TRUE,
             titlePanel('Recovery group trajectories estimated by longitudinal mixed modeling.'),
             p("Select a data type to visualize the corresponding trajectory estimates."),
             fluidRow(
               tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 10px 0px 10px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: MediumSeaGreen;
               z-index: 105;
             }
          ")),
               conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                tags$div("Loading...",id="loadmessage"))
             ),
             fluidRow(
               column(3, selectInput('data_type_tab3', 'Data type', 
                                   c("Cell subsets" = "cell_subsets",
                                     "Cytokines" = "cytokines",
                                     "Polar metabolites" = "polar_metabolites",
                                     "Glycoproteins" = "glycoproteins",
                                     # "Lipoproteins" = "lipoproteins",
                                     "Lipoproteins: apolipoproteins" = "lipoproteins_apolipoprotein", 
                                     "Lipoproteins: cholesterol" = "lipoproteins_cholesterol", 
                                     "Lipoproteins: phospholipids" = "lipoproteins_phospholipids",
                                     "Lipoproteins: triglycerides" = "lipoproteins_triglycerides",
                                     "Ratios" = "log_ratios"
                                     ))),
               multiple = TRUE
             ),
             mainPanel(fluidRow(
                 splitLayout(cellWidths = c("25%", "25%", "25%", "25%"),
                             uiOutput("plot_group_level_trajectories"),
                             uiOutput("plot_group_level_trajectories2"),
                             uiOutput("plot_group_level_trajectories3"),
                             uiOutput("plot_group_level_trajectories4"))
              ),
             width = 11),
             fluidRow(imageOutput("legend_group_level_trajectories"))
    )
  )
)