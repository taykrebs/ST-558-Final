library(shinydashboard)
library(plotly)
library(shiny)
library(shinyAce)
library(ggplot2)
library(tree)

data("diamonds")
variables <- c(" ", colnames(diamonds))
nms <- names(diamonds)


shinyUI(
  dashboardPage(skin = "red",
    dashboardHeader(title = "Diamonds Analysis", titleWidth = 300),
    dashboardSidebar(
      width = 300,
      sidebarMenu(
        menuItem('Introduction', tabName = 'intro', icon = icon('info')),
        menuItem('Data Exploration', tabName = 'explore'),
        menuItem('Unsupervised Learning', tabName = 'unsup'),
        menuItem('Supervised Learning', tabName = 'sup',
                 menuSubItem("Regression Tree Model", icon = icon("chart-line"), tabName = "treereg"),
                 menuSubItem("Random Forest", icon = icon("chart-line"), tabName = "rforest"),
                 menuSubItem("Custom Prediction",icon = icon("chart-line"), tabName = "predict")),
        menuItem('Data', tabName = 'data')
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = 'intro',
                HTML("<p>This application helps the user to understand the different aspects of diamonds and how they impact the price of the diamond.
                      The diamonds dataset that we will use in this application exercise consists of prices and quality information from about 54,000 <i>round </i>diamonds, 
                      and is included in the ggplot2 package. More information on the dataset can be found <a href='https://ggplot2.tidyverse.org/reference/diamonds.html'>here</a>.</p>
                      <br>"),
                h3('Background on Diamonds and Dataset'),
                HTML("<br>
                      Diamonds are the Precious stone consisting of a clear and colourless Crystalline form of pure carbon. 
                      They are the hardest Gemstones known to man. Diamonds are formed within the Earth. What makes
                      diamonds expensive is both the rarity of them and their desireability. Diamonds are considered 
                      rare because it takes a lot of pressure in the Earth and high temperatures to form them. The data set includes
                      many of the qualities used to price a diamond. Normally, the focus is on the four C's (cut, clarity, color, carat):
                      <br><br>
                      <img src='http://wilkersonjewelers.com/sitefiles/wp-content/uploads/2015/12/4c.jpg' alt='Four Cs' height='300' width='450'>
                      <br><br>"),
                h3('The Dataset includes the follow data points:'),
                HTML("</b><br>
                      <u>Carat : </u>Carat weight of the Diamond.
                      <br>
                      <u>Cut : </u>Describe cut quality of the diamond. Quality in increasing order Fair, Good, Very Good, Premium, Ideal.
                      <br>
                      <u>Color : </u>Color of the Diamond with D being the best and J the worst.
                      <br>
                      <u>Clarity : </u>Diamond Clarity refers to the absence of the Inclusions and Blemishes. In order from Best to Worst, FL = flawless, I3= level 3 inclusions) FL, IF, VVS1, VVS2, VS1, VS2, SI1, SI2, I1, I2, I3
                      <br>
                      <u>Depth : </u>The Height of a Diamond, measured from the Culet to the table, divided by its average Girdle Diameter.
                      <br>
                      <u>Table : </u>The Width of the Diamond's Table expressed as a Percentage of its Average Diameter.
                      <br>
                      <u>Price : </u>the Price of the Diamond.
                      <br>
                      <u>X : </u>Length of the Diamond in mm.
                      <br>
                      <u>Y : </u>Width of the Diamond in mm.
                      <br>
                      <u>Z : </u>Height of the Diamond in mm."),
                h2(strong("Table of Contents")),
                h4("The app has different tabs which walks through different techniques used in Data Science that we learned in ST 558 at NC State"),
                h4(strong("Data Exploration :")),
                h4("In this tab you will see basic summary of how the dataset looks like. You can do a statistical summary, and model correlation plots between variables in the dataset."),
                br(),
                h4(strong("Unsupervised Learning:")),
                h4("In this tab we'll try to find any relationship in the data. Our goal is not make predictions, but instead identify trends and/or correlation. I examined the PCA technique for unsupervised learning."),
                br(),
                h4(strong("Supervised Learning:")),
                h4("In this tab we'll build models and make predictions. The goal is to predict the price of a diamond based on different predictors. We'll be comparing RMSE and R-Squared to choose model. RMSE is calcated as"),
                withMathJax(
                  helpText(h4('$$\\left(\\sqrt{\\frac{1}{n}\\sum_1^n x^2}\\right)$$'))),
                br(),
                h4(strong("Data:")),
                h4("In this tab you can explore and download the underlying data we use in the app.")
        ),
        tabItem(tabName = 'data', 
                pageWithSidebar(
                  
                  headerPanel("Diamonds Data"),
                  
                  sidebarPanel(
                    h2("Data View"),
                    radioButtons("view", "",
                                 choices =
                                   list("Raw Data" = "raw",
                                        "Summary Statistics" = "summary")),
                    
                    submitButton("Update View"),
                    
                    conditionalPanel(
                      condition = "input.view == 'raw'",
                      numericInput("obs", "Number of observations to view:", 10,
                                   min = 1, max = nrow(diamonds)),
                      
                      checkboxInput("random", "Random sample"),
                      
                      selectInput("sort", "Sort Descending",
                                  choices = c(" ", colnames(diamonds[,c(1,5:10)])),
                                  multiple = FALSE),
                      
                      downloadButton("downloadData", "Download Data")      
                    )
                  ),
                  
                  mainPanel(
                    conditionalPanel(
                      condition = "input.view == 'summary'",
                      verbatimTextOutput("summary")
                    ),
                    conditionalPanel(
                      condition = "input.view == 'raw'",
                      tableOutput("table")
                    )
                  )
                )
        ),
        tabItem(tabName = 'unsup',
                fluidRow(
                  headerPanel("Unsupervised Learning"),
                  
                  box(
                    title = "PCA Table",
                    solidHeader = TRUE,  width = 12,
                    collapsible = TRUE, status = "primary",
                    div(style = 'overflow-y: scroll',
                        dataTableOutput("PCTable"))
                  ),
                  box(
                    title = "PCA Summary",
                    solidHeader = TRUE,  width = 12,
                    collapsible = TRUE, status = "primary",
                    verbatimTextOutput("pcSumm")
                  )
                ),
                fluidRow(
                  box(
                    title = "Select PCs to be plotted",
                    solidHeader = TRUE,  width = 4,
                    collapsible = TRUE, status = "primary",
                    selectizeInput('var1', 'Select PC on x-axis',
                                   choices = c(1,2,3,4,5,6),
                                   selected = 1),
                    selectizeInput('var2', 'Select PC on y-axis',
                                   choices = c(1,2,3,4,5,6),
                                   selected = 2)
                  ),
                  box(
                    title = "Biplot", width = 8,
                    solidHeader = TRUE,
                    collapsible = TRUE, status = "primary",
                    plotOutput("biPlot", height = 500, width = 500)
                  ),
                  box(
                    title = "Scree Plot", width = 6,
                    solidHeader = TRUE,
                    collapsible = TRUE, status = "primary",
                    plotOutput("screePlot")
                  ),
                  box(
                    title = "Cummulative Variance Plot", width = 6,
                    solidHeader = TRUE,
                    collapsible = TRUE, status = "primary",
                    plotOutput("cummVarPlot")
                  )
                  
                )
        ),
        tabItem(tabName = 'explore',
                pageWithSidebar(
                  
                  headerPanel("Diamonds Explorer"),
                  sidebarPanel(
                    sliderInput('sampleSize', 'Sample Size', min = 1, max = nrow(diamonds),
                                value = 5000, step = 1000, round = 0),
                    
                    checkboxInput('jitter', 'Jitter', value = FALSE),
                    checkboxInput('smooth', 'Smooth', value = TRUE),
                    
                    selectInput('x', 'X', choices = nms, selected = "carat"),
                    selectInput('y', 'Y', choices = nms, selected = "price"),
                    selectInput('color', 'Color', choices = nms, selected = "clarity"),
                    
                    selectInput('facet_row', 'Facet Row', c(None = '.', nms), selected = "none"),
                    selectInput('facet_col', 'Facet Column', c(None = '.', nms)),
                    helpText("You may download your plot using the plotly features which appear when hovering your cursor over the plot.")
                    
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Plot",plotlyOutput('trendPlot', height = "800px")),
                      tabPanel("Summary", verbatimTextOutput("summary2"))
                    )
                  )
                )
        ),
        tabItem(tabName = "treereg",
                fluidRow(
                  box(
                    title = "Model Inputs",
                    solidHeader = TRUE,  width = 4,
                    collapsible = TRUE, status = "primary",
                    sliderInput("treeNodes", "number of nodes",
                                min = 1, max = 10, value = 5),
                    checkboxGroupInput("checkGroup",
                                       label = h3("Dataset Variables"),
                                       choices = c('carat','cut', 'color', 'clarity', 'depth', 'table', 'x', 'y', 'z'),
                                       inline = F,
                                       selected = c('carat', 'cut', 'color', 'clarity')),
                    uiOutput("treemodel")
                  ),
                  box(
                    title = "Tree Summary",
                    solidHeader = TRUE,  width = 8,
                    collapsible = TRUE, status = "primary",
                    verbatimTextOutput("treesum")
                  ),
                  box(
                    title = "CV Fit",
                    solidHeader = TRUE,  width = 8,
                    collapsible = TRUE, status = "primary",
                    plotOutput("treefitprunedplot")
                  )
                )
        ),
        tabItem(tabName = "predict",
                h2("Diamond Price Prediction"),
                fluidRow(
                  box(
                    title = "Controls",width = 6,
                    sliderInput("sliderCarat", "Select Weight (Carat)", 0.2000, 5.0100, 
                                value = 0.7000),
                    selectInput("dropDownCut", "Choose Cut Quality:", 
                                choices = unique(diamonds$cut)),
                    selectInput("dropDownColor", "Choose Colour:", 
                                choices = unique(diamonds$color)),
                    selectInput("dropDownClarity", "Choose Clarity:", 
                                choices = unique(diamonds$clarity)),
                    submitButton("Submit")
                  ),
                  box(title = "Results",
                      h3("Predicted Price for Diamond(US $):"),
                      textOutput("predict")
                  )
                ),
                fluidRow(
                  uiOutput("instructions")
                )
        ),
        tabItem(tabName = "rforest",
                fluidRow(
                  box(
                    title = "Inputs", solidHeader = TRUE,  width = 4,
                    collapsible = TRUE, status = "primary",
                    checkboxGroupInput("rfcheckGroup",
                                       label = h3("Variables for Regression"),
                                       choices = c('carat','cut', 'color', 'clarity', 'depth', 'table', 'x', 'y', 'z'),
                                       inline = F,
                                       selected = c('carat','cut', 'color', 'clarity')),
                    uiOutput("rfmodelSelected")
                  ),
                  box(
                    title = "Training Model Summary",
                    solidHeader = TRUE,  width = 8,
                    collapsible = TRUE, status = "primary",
                    div(style = 'overflow-y: scroll',
                        verbatimTextOutput("rfSumm"))
                  ),
                  box(
                    title = "RMSE for Model",
                    solidHeader = TRUE,  width = 8,
                    collapsible = TRUE, status = "primary",
                    verbatimTextOutput("rfvalue")
                    )
                  )
             
      )
      
      
    )
    
  )
  
  )
)




