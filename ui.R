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
    )
    ,
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
    )
  )
)