library(shiny)

#----------------------------------------------------UI------------------------------------------

shinyUI(fluidPage(pageWithSidebar(
  
  headerPanel("Bayesion Neural Network Fitting"), # Application title
  sidebarPanel(
    helpText("Data:"),
    selectInput("dataset", "Choose a dataset:", choices = c("Binary", "Measure")),
    numericInput("obs", "Number of observations to view:", 10),
    selectInput("model", "Choose a model:", choices = c("Classification", "Numerical")),
    hr(),
    helpText("Prior:"),
    h4('Alternative distribution'),
    selectInput('distribution', '', 
                list('normal',
                     'point mass',
                     't',
                     'log-normal',
                     'gamma',
                     'beta',
                     'uniform'), 
                'normal'),
    conditionalPanel(
      condition = 'input.distribution == "normal"',
      numericInput('normal_mean', 'Mean', value=0),
      numericInput('normal_sd', 'Standard deviation', value=1, min=0)
    ),
    conditionalPanel(
      condition = 'input.distribution == "point mass"',
      numericInput('point_location', 'Location', value=0)
    ),
    conditionalPanel(
      condition = 'input.distribution == "t"',
      numericInput('t_df', 'Degrees of freedom', value=1),
      numericInput('t_location', 'Location', value=0),
      numericInput('t_scale', 'Scale', value=1)
    ),
    conditionalPanel(
      condition = 'input.distribution == "log-normal"',
      numericInput('lognormal_location', 'Location', value=0),
      numericInput('lognormal_scale', 'Scale', value=1)
    ),
    conditionalPanel(
      condition = 'input.distribution == "gamma"',
      numericInput('gamma_shape', 'Shape', value=1),
      numericInput('gamma_rate', 'Rate', value=1)
    ),
    conditionalPanel(
      condition = 'input.distribution == "beta"',
      numericInput('beta_shape1', 'Shape1', value=1),
      numericInput('beta_shape2', 'Shape2', value=1)
    ),
    conditionalPanel(
      condition = 'input.distribution == "uniform"',
      numericInput('uniform_lb', 'Lower bound', value=0),
      numericInput('uniform_ub', 'Upper bound', value=1)
    ),
    
    hr(),
    helpText("Parameter:"),
    numericInput("nodes", "Number of nodes in the hidden layer:", 2), 
    hr(),
    helpText("Parameter:"),
    numericInput("cols", "Number of columns used for fit:", 4)
  ),
  
  mainPanel(
    
    tabsetPanel(
      tabPanel("The Data", tableOutput('contents'),br(),helpText("Data output:")),
      tabPanel("Neural Network", plotOutput("NN"),br(),helpText("NeuralNetwork output:"),tableOutput('NN_MSE')),
      tabPanel("Posterior", plotOutput("poste"),br(),helpText("Posterior result:"),tableOutput('B_MSE')),
      #tabPanel("MSE",tableOutput('NN_MSE')),
      # tabPanel("Prediction",plotOutput('B_Pred'))
      tabPanel("Prediction",plotOutput('B_Pred'),br(),helpText("Prediction result:"),tableOutput("predv"))
      # tabPanel("Prediction",plotOutput("prediction"), br(), helpText("Prediction result:"), tableOutput("prediction"))
    )
  )
))
)
