library(shinydashboard)
library(plotly)
library(shiny)
library(shinyAce)
library(ggplot2)
library(tidyverse)
library(randomForest)
library(tree)
library(DT)
library(caret)

data("diamonds")
variables <- c(" ", colnames(diamonds))
nms <- names(diamonds)
diamondData <-data("diamonds")

set.seed(123)
train <- sample(1:nrow(diamonds), size = nrow(diamonds)*0.8)
test <- dplyr::setdiff(1:nrow(diamonds), train)
diamondsTrain <- diamonds[train, ]
diamondsTest <- diamonds[test, ]


shinyServer(function(input, output){
  
  datasetInput <- reactive(diamonds)
  datasetInput2 <- reactive({
    diamonds[sample(nrow(diamonds), input$sampleSize),]
  })
  
##Data tab
  output$summary <- renderPrint({
    summary(datasetInput())
  })
  
  output$table <- renderTable({
    n <- nrow(diamonds)
    x <- datasetInput()
    if (input$random) {
      cat("foo")
      x <- x[sample(seq_len(n), input$obs, replace=FALSE), , drop=FALSE]
    } else {
      x <- head(x, input$obs)
    }
    if (input$sort != " ") {
      sorder <- order(-x[[input$sort]])
      x <- x[sorder, , drop=FALSE]
    }
    x
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("diamonds", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = TRUE)
    }
  )
  

  
  ##Unsupervised learning tab
    observe({
    PCAdata <- diamonds %>% select(c(1,5:6,8:10))
    pca<-prcomp(PCAdata, center=TRUE, scale=TRUE)
    
    princComps <- as.data.frame(pca$rotation)
    output$PCTable <- renderDataTable({
      datatable(round(princComps,3))
    })
    
    # Variability of each principal component
    variability <- pca$sdev^2
    # Variance explained by each principal component
    variabilityExpl <- variability / sum(variability)
    
    #ScreePlot
    output$screePlot <- renderPlot({
      # Plot variance explained for each principal component
      screeplot(pca, type = "lines")
    })
    
    output$cummVarPlot <- renderPlot({
      #Plot cumulative proportion of variance explained
      plot(cumsum(variabilityExpl), xlab = "Principal Component",
           ylab = "Cumulative Proportion of Variance Explained",
           ylim = c(0, 1), type = "b")
    })
    
    #BiPlot
    output$biPlot <- renderPlot({
      if(input$var1 == input$var2){
        print("X & Y variables should be different")
      }
      biplot(pca, choices = c(as.numeric(input$var1),
                              as.numeric(input$var2)), cex = 0.8,
             xlim = c(-0.08, 0.1), ylim = c(-0.07, 0.1))
    })
    
    #Print PCA Summary
    output$pcSumm <- renderPrint({
      summary(pca)
    })
    
  })
  
  
  #tree regression
  
  set.seed(123)
  observe({
    treefit <- tree(as.formula(paste("price ~ ", paste(input$checkGroup,collapse="+"))), data=diamondsTrain)
    treefitpruned <- prune.tree(treefit, best = input$treeNodes)
  
  
  output$treesum <- renderPrint({
    summary(treefitpruned)
  })
  
  plot(treefitpruned)
  text(treefitpruned, cex=.75)
  cvTree <- cv.tree(treefitpruned)
  
  output$treefitprunedplot <- renderPlot({
    plot(cvTree$size, cvTree$dev, type = 'b')
  })
  
  output$treemodel <- renderUI({
    text <- (paste("Model Selected : Price ~ ",paste(input$checkGroup,collapse="+")))
    h4(text)
  })
  

  })
  

  
    ##Exploration tab
    output$summary2 <- renderPrint({
      summary(datasetInput2())
    })
    
    output$trendPlot <- renderPlotly({
      
      p <- ggplot(datasetInput2(), aes_string(x = input$x, y = input$y, color = input$color)) + 
        geom_point()
      
      # if at least one facet column/row is specified, add it
      facets <- paste(input$facet_row, '~', input$facet_col)
      if (facets != '. ~ .') p <- p + facet_grid(facets)
      
      if (input$jitter)
        p <- p + geom_jitter()
      if (input$smooth)
        p <- p + geom_smooth()
      
      ggplotly(p) %>% 
        layout(autosize=TRUE)
    })
    
  #custom prediction
    model1 <- lm(price ~ carat+cut+color+clarity, data = diamonds)
    
    summpredict<-summary(model1)
    
    model1pred <- reactive({
      caratInput <- input$sliderCarat
      predict(model1, newdata = 
                data.frame(carat = caratInput,
                           cut = input$dropDownCut,color=input$dropDownColor,
                           clarity=input$dropDownClarity))
    })
    
    output$predict <- renderText({
      model1pred()
    })
    output$instructions <- renderUI(HTML("<ul><li><b>Price</b>: price in US dollars.</li>
                                 <li><b>Carat</b>: weight of the diamond (0.2-5.01)</li>
                                 <li><b>Cut</b>: quality of the cut (Fair, Good, Very Good, Premium, Ideal)</li>
                                 <li><b>Color</b>: diamond color, from J (worst) to D (best)</li>
                                 <li><b>Clarity</b>: a measurement of how clear the diamond is 
                                 (I1 (worst), SI1, SI2, VS1, VS2, VVS1, VVS2, IF (best))</li>
                                 </ul>"))
  
    ##random forest
    observe({
      set.seed(123)
      
      rfFit <- randomForest(as.formula(paste("price ~ ", paste(input$rfcheckGroup,collapse="+"))), data = diamondsTrain,
                            mtry = 4/3, ntree=300, importance = TRUE)
      
      #Predict
      rfPredict <- predict(rfFit,newdata = diamondsTest)
      
      #calc RSME
      output$rfvalue <- renderPrint({
        r2 <- summary(rfPredict)
        rfR <- RMSE(rfPredict, diamondsTest$price)
        paste("RMSE of model is", rfR)
      })
      
      #training model Summary
      output$rfSumm <- renderPrint({
        rfFit
      })
      
      #Update the rf model
      output$rfmodelSelected <- renderUI({
        text <- (paste("Model Selected : price ~ ",paste(input$rfcheckGroup,collapse="+")))
        h4(text)
      })
      
    })   
 
})