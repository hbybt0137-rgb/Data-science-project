#1cleaning data
df <- read.csv(file.choose())
df$Order_ID <- as.numeric(df$Order_ID)
str(df)
summary(df)
is.na(df)
sum(is.na(df))
colSums(is.na(df))
df<-na.omit(df)
colSums(df == "")
df[df==""]<-NA
sum(duplicated(df))
unique(df)
df=unique(df)
df$Order_ID=as.numeric(df$Order_ID)
df$Preparation_Time_min=as.numeric(df$Preparation_Time_min)
df$Delivery_Time_min=as.numeric(df$Delivery_Time_min)
df$Weather=as.factor(df$Weather)
df$Traffic_Level=as.factor(df$Traffic_Level)
df$Time_of_Day=as.factor(df$Time_of_Day)
df$Vehicle_Type=as.factor(df$Vehicle_Type)
df$Total_Time=df$Preparation_Time_min+df$Delivery_Time_min
df$speed=df$Distance_km/(df$Total_Time/60)
df$late_delivery_flag<-ifelse(df$speed>8.27,"on time","late")
df$Customer_Rating<-ifelse(df$speed>11.55,"5",
ifelse (df$speed>9.91,"4",
ifelse (df$speed>8.27,"3",
ifelse (df$speed>6,"2",
ifelse (df$speed<6,"1","0")))))
outlier1=boxplot(df$Distance_km)$out
outlier1
outlier2=boxplot(df$Preparation_Time_min)$out
outlier2
outlier3=boxplot(df$Courier_Experience_yrs)$out
outlier3
outlier4=boxplot(df$Delivery_Time_min)$out
outlier4
df=df[-which(df$Delivery_Time_min %in% outlier4),]
outlier4=boxplot(df$Delivery_Time_min)$out
df$late_delivery_flag<-as.factor(df$late_delivery_flag)
df$Customer_Rating<-as.factor(df$Customer_Rating)
##2Visualization
library(dplyr)
library(ggplot2)
data<- read.csv(file.choose(),encoding="UTF-8")
hist(
  data$Total_time,
  col="#89CFF0",
  border="black",
  main="tendency of Total_time",
  xlab="Total_time(min)",
  ylab="Number of orders"
)

boxplot(Delivery_Time_min ~ Vehicle_Type, 
        data = data,
        col = c("#89CFF0", "#FDFD96", "#FF6961"),
        main = "Delivery Time by Vehicle Type",
        xlab = "Vehicle Type",
        ylab = "Delivery_Time_min",
        las = 1)

plot(data$Total_time, data$Customer_Rating,
     xlab="Total_time",
     ylab="Customer Rating",
     main="Customer Rating vs Total_time",
     pch=19, col="#89CFF0")

hist(
  data$Preparation_Time_min,
  col="#FFA07A",
  border="black",
  main="the relation between prepration time and Number of orders",
  xlab="prepration time",
  ylab="Number of orders"
)

hist(
  data$Distance_km,
  col="#FFA07A",
  border="black",
  main="the relation between distance in(KM) and Number of orders",
  xlab="distance in (KM)",
  ylab="Number of orders"
)

boxplot(Delivery_Time_min ~ Weather, data=data,
        main="Delivery_Time_min", col=c("Sunny"="#FFD700", "Rainy"="#1f77b4", "Cloudy"="#7f7f7f","Snowy" = "#00CED1","Stormy" = "#FF4500"))

barplot(table(data$Weather),
        col=c("Sunny"="#FFD700", "Rainy"="#1f77b4", "Cloudy"="#7f7f7f","Snowy" = "#00CED1","Stormy" = "#FF4500"),
        main="Number of Orders by Weather",
        xlab="Weather", ylab="Number of orders")

barplot(table(data$Traffic_Level),
        col=c("Low"="#d62728", "Medium"="#FFA07A", "High"="#2ca02c"),
        main="Number of Orders by Traffic Level",
        xlab="Traffic Level", ylab="Number of orders")

plot(data$Distance_km, data$Delivery_Time_min,
     pch=19, col="#89CFF0",
     xlab="Distance (km)",
     ylab="Delivery_Time_min",
     main="Distance vs Delivery_Time_min")

order_hour_temp <- floor(data$Total_time / 60)
avg_hour <- aggregate(Delivery_Time_min ~ order_hour_temp,
                      data = data,
                      FUN = mean)
plot(x = avg_hour$order_hour_temp,
     y = avg_hour$Delivery_Time_min,
     type = "l",
     main = "Delivery Time Throughout the Day",
     xlab = "Hour of the Day",
     ylab = "Average Delivery Time (min)",
     col = "blue",
     lwd = 2)
points(x = avg_hour$order_hour_temp,
       y = avg_hour$Delivery_Time_min,
       pch = 19, col = "red")
###3K-Means Clustering:
library(tidyverse)
library(factoextra)
library(cluster)
library(ggpubr)
myfile <- file.choose()          
mydata <- read.csv(myfile)        
mydata
str(mydata)                      
mydata <- mydata[ , -c(1,2)]    
head(mydata)                      
colnames(mydata)                  
num_data <- mydata[, c("Preparation_Time_min", "Courier_Experience_yrs", 
                       "Delivery_Time_min", "speed", "Total_time", "Customer_Rating")]

sum(is.na(num_data))                
kmeans_cluestring <- kmeans(num_data, centers = 4, nstart = 20)  
num_data$Cluster <- kmeans_cluestring$cluster
cluster_summary <- aggregate(. ~ Cluster, data = num_data, FUN = mean)
cluster_summary

fviz_cluster(
  kmeans_cluestring,
  data = num_data,
  ellipse.type = "convex",   
  star.plot = TRUE            
) 
+ ggtitle("K-means Clustering Visualization")
### Decision Tree
library("rpart")
library("rpart.plot")
data<- read.csv(file.choose(),encoding="UTF-8")
set.seed(123)
index <- sample(1:nrow(data), 0.7 * nrow(data))
train <- data[index, ]
test  <- data[-index, ]
class_model <- rpart(late_delivery_flag ~ Distance_km + Weather + Traffic_Level +
                       Time_of_Day + Vehicle_Type + Preparation_Time_min +
                       Courier_Experience_yrs + Delivery_Time_min + speed +
                       Total_time + Customer_Rating,
                     data = train,
                     method = "class")
rpart.plot(class_model,
           main = "Decision Tree for Late Delivery Classification",
           type = 2,
           extra = 104)
class_pred <- predict(class_model, test, type = "class")
conf_matrix <- table(class_pred, test$late_delivery_flag)

conf_matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy
reg_model <- rpart(Delivery_Time_min ~ Distance_km + Weather + Traffic_Level +
                     Time_of_Day + Vehicle_Type + Preparation_Time_min +
                     Courier_Experience_yrs + speed +
                     Total_time + Customer_Rating,
                   data = train,
                   method = "anova")
rpart.plot(reg_model,
           main = "Decision Tree for Delivery Time Prediction",
           type = 2)
reg_pred <- predict(reg_model, test)

rmse <- sqrt(mean((reg_pred - test$Delivery_Time_min)^2))
rmse
#####5 GUI
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(cluster)
library(DT)
library(rpart)
library(rpart.plot)
library(tidyr)
library(factoextra) 


ui <- fluidPage(
  titlePanel(
    h1(" Food Delivery Project", style = "color: #333; font-weight: bold;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h3("Input Data"),
      
      fileInput("file_input", 
                "1. Upload Cleaned Dataset (.CSV)",
                multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      hr(),
      h4("2. Algorithm Parameters"),
      
      numericInput("k_clusters", 
                   "K-Means: Number of Clusters (2-6):",
                   value = 4, min = 2, max = 6, step = 1),
      
      
      actionButton("run_analysis", "Run Analysis Results", icon("play"), 
                   class = "btn-success", style = "margin-top: 20px;")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel(" Data Visualization",
                 fluidRow(
                   column(6, h4("First Visual"), plotlyOutput("plot_1")),
                   column(6, h4("Second Visual"), plotlyOutput("plot_2"))
                 ),
                 fluidRow(
                   column(6, h4("Third Visual"), plotlyOutput("plot_3")),
                   column(6, h4("Fourth Visual"), plotlyOutput("plot_4"))
                 ),
                 fluidRow(
                   column(6, h4("Fifth Visual"), plotlyOutput("plot_5")),
                   column(6, h4("Sixth Visual"), plotlyOutput("plot_6"))
                 ),
                 fluidRow(
                   column(6, h4("Seventh Visual "), plotlyOutput("plot_7")),
                   column(6, h4("Eightth Visual"), plotlyOutput("plot_8"))
                 ),
                 fluidRow(
                   column(6, h4("Ningth Visual"), plotlyOutput("plot_9")),
                   column(6, h4("Tength Visual"), plotlyOutput("plot_10"))
                 )
        ),
        
        tabPanel(" K-Means Clustering",
                 h3("Customer/Order Segments)"),
                 p("Visualization using Principal Components (PC1 & PC2) to represent the 6 features: Preparation_Time_min, Courier_Experience_yrs, Delivery_Time_min, speed, Total_time, Customer_Rating."),
                 
                 plotOutput("cluster_viz"),
                 
                 h4("Cluster Summary"),
                 verbatimTextOutput("cluster_description")
        ),
        
        tabPanel(" Decision Tree Model",
                 h3("Decision Tree Models"),
                 
                 h4("1. Decision Tree for Late Delivery Classification (Predicts: Late/On-Time)"),
                 p("Classification Model (rpart method='class')"),
                 plotOutput("class_tree_plot"),
                 
                 h4("2. Decision Tree for Delivery Time Prediction (Predicts: Delivery Time in min)"),
                 p("Regression Model (rpart method='anova')"),
                 plotOutput("reg_tree_plot"),
                 
                 h4("Model Performance & Interpretation"),
                 verbatimTextOutput("tree_metrics")
        )
      )
    )
  )
)

#_____________________Server__________________________
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    data = NULL,
    cleaned_data = NULL,
    kmeans_input_data = NULL, 
    clusters = NULL,
    class_tree_model = NULL,
    reg_tree_model = NULL,
    tree_accuracy = NULL,
    confusion_matrix = NULL
  )
  
  
  create_placeholder_df <- function(df) {
    
    
    if (!"Total_time" %in% names(df)) df$Total_time <- rnorm(nrow(df), mean = 40, sd = 15)
    if (!"Distance_km" %in% names(df)) df$Distance_km <- runif(nrow(df), min = 1, max = 25)
    if (!"Preparation_Time_min" %in% names(df)) df$Preparation_Time_min <- rnorm(nrow(df), mean = 20, sd = 5)
    if (!"Customer_Rating" %in% names(df)) df$Customer_Rating <- round(runif(nrow(df), min = 1, max = 5))
    if (!"Courier_Experience_yrs" %in% names(df)) df$Courier_Experience_yrs <- round(runif(nrow(df), min = 1, max = 10))
    if (!"Delivery_Time_min" %in% names(df)) df$Delivery_Time_min <- df$Total_time - df$Preparation_Time_min
    if (!"speed" %in% names(df)) {
      safe_time_min <- ifelse(df$Delivery_Time_min < 0.1, 0.1, df$Delivery_Time_min)
      df$speed <- df$Distance_km / (safe_time_min / 60)
    }
    
    
    if (!"Traffic_Level" %in% names(df)) df$Traffic_Level <- factor(sample(c("Low", "Medium", "High"), nrow(df), replace = TRUE))
    if (!"Vehicle_Type" %in% names(df)) df$Vehicle_Type <- factor(sample(c("Bike", "Scooter", "Car"), nrow(df), replace = TRUE, prob = c(0.4, 0.4, 0.2)))
    if (!"Weather" %in% names(df)) df$Weather <- factor(sample(c("Sunny", "Rainy", "Cloudy", "Snowy", "Stormy"), nrow(df), replace = TRUE))
    if (!"Time_of_Day" %in% names(df)) df$Time_of_Day <- factor(sample(c("Morning", "Afternoon", "Evening"), nrow(df), replace = TRUE))
    if (!"late_delivery_flag" %in% names(df)) df$late_delivery_flag <- factor(ifelse(df$Total_time > 45, "Late", "On-Time")) 
    
    df$Traffic_Level <- as.factor(df$Traffic_Level)
    df$Vehicle_Type <- as.factor(df$Vehicle_Type)
    df$Weather <- as.factor(df$Weather)
    df$Time_of_Day <- as.factor(df$Time_of_Day)
    df$late_delivery_flag <- as.factor(df$late_delivery_flag)
    
    df <- df %>% 
      mutate(speed = ifelse(is.infinite(speed) | is.nan(speed), median(df$speed, na.rm = TRUE), speed)) %>%
      filter(complete.cases(select(., Preparation_Time_min, Courier_Experience_yrs, Delivery_Time_min, speed, Total_time, Customer_Rating, late_delivery_flag)))
    
    return(df)
  }
  
  observeEvent(input$file_input, {
    req(input$file_input)
    
    tryCatch({
      df <- read.csv(input$file_input$datapath)
      
      temp_df <- create_placeholder_df(df) 
      
      if (nrow(temp_df) < 5) {
        stop("Dataset is too small or contains too many missing values after cleaning (must be > 5 rows).")
      }
      
      rv$data <- temp_df
      rv$cleaned_data <- temp_df
      
      rv$kmeans_input_data <- temp_df %>%
        select(Preparation_Time_min, Courier_Experience_yrs, Delivery_Time_min, speed, Total_time, Customer_Rating)
      
      showNotification(paste("Data loaded and prepared successfully! Rows for analysis:", nrow(temp_df)), 
                       type = "message", duration = 5) 
      
    }, error = function(e) {
      showNotification(paste("Error loading file or preparing data. Please check your CSV format. Error:", e$message), type = "error")
    })
  })
  
  observeEvent(input$run_analysis, {
    
    if (is.null(rv$cleaned_data) || is.null(rv$kmeans_input_data)) {
      showNotification("Please upload your CSV file first.", type = "error", duration = 5)
      return()
    }
    
    if (nrow(rv$cleaned_data) < 20) {
      showNotification("Data too small for analysis. Need at least 20 cleaned rows.", type = "error", duration = 5)
      return()
    }
    
    tryCatch({
      
      showNotification("Running K-Means Clustering on 6 features...", duration = 3, type = "message")
      
      scaled_features <- scale(rv$kmeans_input_data)
      
      set.seed(42)
      k_val <- as.numeric(input$k_clusters)
      
      
      kmeans_result <- kmeans(scaled_features, centers = k_val, nstart = 20)
      
      
      rv$cleaned_data$Cluster <- factor(kmeans_result$cluster) 
      rv$clusters <- kmeans_result
      
      
      
      showNotification("Training Decision Tree Models...", duration = 3, type = "message")
      
      
      tree_data <- rv$cleaned_data %>% 
        select(late_delivery_flag, Distance_km, Weather, Traffic_Level,
               Time_of_Day, Vehicle_Type, Preparation_Time_min,
               Courier_Experience_yrs, Delivery_Time_min, speed,
               Total_time, Customer_Rating) %>%
        na.omit() 
      
      set.seed(123) 
      sample_index <- sample(nrow(tree_data), floor(0.7 * nrow(tree_data)))
      train_data <- tree_data[sample_index, ]
      test_data <- tree_data[-sample_index, ]
      
      # Model 1: Classification Tree (as per 22.R)
      formula_vars_class <- c("Distance_km", "Weather", "Traffic_Level", "Time_of_Day", 
                              "Vehicle_Type", "Preparation_Time_min", "Courier_Experience_yrs", 
                              "Delivery_Time_min", "speed", "Total_time", "Customer_Rating")
      model_formula_class <- as.formula(paste("late_delivery_flag ~", paste(formula_vars_class, collapse = " + ")))
      
      class_model <- rpart(model_formula_class, data = train_data, method = "class")
      rv$class_tree_model <- class_model
      
      # Model 2: Regression Tree (as per 22.R)
      formula_vars_reg <- c("Distance_km", "Weather", "Traffic_Level", "Time_of_Day", 
                            "Vehicle_Type", "Preparation_Time_min", "Courier_Experience_yrs", 
                            "speed", "Total_time", "Customer_Rating")
      model_formula_reg <- as.formula(paste("Delivery_Time_min ~", paste(formula_vars_reg, collapse = " + ")))
      
      reg_model <- rpart(model_formula_reg, data = train_data, method = "anova")
      rv$reg_tree_model <- reg_model
      
      # Metrics for Classification Model
      predictions <- predict(class_model, test_data, type = "class")
      confusion_matrix <- table(Predicted = predictions, Actual = test_data$late_delivery_flag)
      accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      rv$tree_accuracy <- accuracy
      rv$confusion_matrix <- confusion_matrix
      
      
      showNotification("Analysis completed! Results updated in tabs.", type = "success", duration = 5)
      
    }, error = function(e) {
      
      showNotification(paste("Analysis Failed. Critical Error:", e$message, ". Please check your data."), type = "error", duration = NULL)
      return()
    })
  })
  
  #_____________________________Visualization_________________________________ 
  
  create_plot_1 <- function(df) { req(df); p <- ggplot(df, aes(x = Total_time)) + geom_histogram(bins = 30, fill = "#89CFF0", color = "black") + labs(title = "1. Total Delivery Time Distribution", x = "Total Time (min)") + theme_minimal(); return(ggplotly(p)) }
  create_plot_2 <- function(df) { req(df); p <- ggplot(df, aes(x = Vehicle_Type, y = Total_time, fill = Vehicle_Type)) + geom_boxplot() + labs(title = "2. Delivery Time by Vehicle Type") + theme_minimal() + theme(legend.position = "none"); return(ggplotly(p)) }
  create_plot_3 <- function(df) { req(df); p <- ggplot(df, aes(x = Total_time, y = Customer_Rating)) + geom_point(alpha = 0.6, color="#89CFF0") + geom_smooth(method = "lm", color="red") + labs(title = "3. Customer Rating vs. Total Time") + theme_minimal(); return(ggplotly(p)) }
  create_plot_4 <- function(df) { req(df); p <- ggplot(df, aes(x = Preparation_Time_min)) + geom_histogram(bins = 30, fill = "#FFA07A", color = "black") + labs(title = "4. Preparation Time Distribution", x = "Preparation Time (min)") + theme_minimal(); return(ggplotly(p)) }
  create_plot_5 <- function(df) { req(df); p <- ggplot(df, aes(x = Distance_km)) + geom_histogram(bins = 30, fill = "#FFA07A", color = "black") + labs(title = "5. Distance Distribution (km)", x = "Distance (km)") + theme_minimal(); return(ggplotly(p)) }
  create_plot_6 <- function(df) { req(df); p <- ggplot(df, aes(x = Weather, y = Total_time, fill = Weather)) + geom_boxplot() + labs(title = "6. Total Time by Weather Condition") + scale_fill_manual(values = c("Sunny"="#FFD700", "Rainy"="#1f77b4", "Cloudy"="#7f7f7f","Snowy" = "#00CED1","Stormy" = "#FF4500")) + theme_minimal() + theme(legend.position = "none"); return(ggplotly(p)) }
  create_plot_7 <- function(df) { req(df); p <- df %>% count(Weather) %>% ggplot(aes(x = Weather, y = n, fill = Weather)) + geom_bar(stat = "identity") + labs(title = "7. Orders by Weather Condition", y = "Number of Orders") + scale_fill_manual(values = c("Sunny"="#FFD700", "Rainy"="#1f77b4", "Cloudy"="#7f7f7f","Snowy" = "#00CED1","Stormy" = "#FF4500")) + theme_minimal() + theme(legend.position = "none"); return(ggplotly(p)) }
  create_plot_8 <- function(df) { req(df); p <- df %>% count(Traffic_Level) %>% ggplot(aes(x = Traffic_Level, y = n, fill = Traffic_Level)) + geom_bar(stat = "identity") + labs(title = "8. Orders by Traffic Level", y = "Number of Orders") + scale_fill_manual(values = c("Low"="#2ca02c", "Medium"="#FFA07A", "High"="#d62728")) + theme_minimal() + theme(legend.position = "none"); return(ggplotly(p)) }
  
  
  create_plot_9 <- function(df) { 
    req(df)
    p <- ggplot(df, aes(x = Distance_km, y = Delivery_Time_min)) +
      geom_point(alpha = 0.6, color="#89CFF0") +
      geom_smooth(method = "lm", color="red") +
      labs(title = "9. Distance vs. Delivery Time", x = "Distance (km)", y = "Delivery Time (min)") +
      theme_minimal()
    return(ggplotly(p)) 
  }
  
  create_plot_10 <- function(df) { 
    req(df)
    
    df$order_hour_temp <- floor(df$Total_time / 60)
    
    avg_hour <- df %>%
      group_by(order_hour_temp) %>%
      summarise(Avg_Delivery_Time = mean(Delivery_Time_min, na.rm = TRUE))
    
    p <- ggplot(avg_hour, aes(x = order_hour_temp, y = Avg_Delivery_Time)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 3) +
      labs(title = "10. Avg Delivery Time Throughout the Day", x = "Hour of the Day", y = "Average Delivery Time (min)") +
      theme_minimal()
    
    return(ggplotly(p)) 
  }
  
  
  output$plot_1 <- renderPlotly({ create_plot_1(rv$cleaned_data) })
  output$plot_2 <- renderPlotly({ create_plot_2(rv$cleaned_data) })
  output$plot_3 <- renderPlotly({ create_plot_3(rv$cleaned_data) })
  output$plot_4 <- renderPlotly({ create_plot_4(rv$cleaned_data) })
  output$plot_5 <- renderPlotly({ create_plot_5(rv$cleaned_data) })
  output$plot_6 <- renderPlotly({ create_plot_6(rv$cleaned_data) })
  output$plot_7 <- renderPlotly({ create_plot_7(rv$cleaned_data) })
  output$plot_8 <- renderPlotly({ create_plot_8(rv$cleaned_data) })
  output$plot_9 <- renderPlotly({ create_plot_9(rv$cleaned_data) })
  output$plot_10 <- renderPlotly({ create_plot_10(rv$cleaned_data) })
  
  #__________________________K-Means____________________________________ 
  output$cluster_viz <- renderPlot({
    req(rv$clusters, rv$kmeans_input_data)
    
    
    scaled_features <- scale(rv$kmeans_input_data)
    
    fviz_cluster(
      rv$clusters,
      data = scaled_features, 
      ellipse.type = "convex",
      star.plot = TRUE
    ) +
      ggtitle(paste("K-Means Clustering Visualization (K =", input$k_clusters, ") - PC1 & PC2")) +
      theme_minimal()
  })
  
  output$cluster_description <- renderPrint({
    req(rv$clusters, rv$cleaned_data)
    
    if (!"Cluster" %in% names(rv$cleaned_data)) {
      cat("Run K-Means analysis first to see cluster summary.")
      return()
    }
    
    cat("--- Cluster Summary (Averages) ---\n")
    
    rv$cleaned_data %>%
      group_by(Cluster) %>%
      summarise(
        N_Orders = n(),
        Avg_Prep_Time = mean(Preparation_Time_min, na.rm = TRUE),
        Avg_Courier_Experience = mean(Courier_Experience_yrs, na.rm = TRUE),
        Avg_Delivery_Time = mean(Delivery_Time_min, na.rm = TRUE),
        Avg_Speed = mean(speed, na.rm = TRUE),
        Avg_Total_Time = mean(Total_time, na.rm = TRUE),
        Avg_Customer_Rating = mean(Customer_Rating, na.rm = TRUE)
      )
  })
  
  #_________________________Decision Tree__________________________
  output$class_tree_plot <- renderPlot({
    req(rv$class_tree_model)
    rpart.plot(rv$class_tree_model, 
               main = "Decision Tree for Late Delivery Classification",
               type = 2, 
               extra = 104, 
               fallen.leaves = TRUE)
  })
  
  output$reg_tree_plot <- renderPlot({
    req(rv$reg_tree_model)
    rpart.plot(rv$reg_tree_model, 
               main = "Decision Tree for Delivery Time Prediction",
               type = 2)
  })
  
  output$tree_metrics <- renderPrint({
    req(rv$tree_accuracy, rv$confusion_matrix, rv$class_tree_model, rv$reg_tree_model)
    
    cat("--- Classification Model Performance (Late Delivery) ---\n")
    cat(paste("Accuracy on Test Data:", round(rv$tree_accuracy, 4), "\n\n"))
    cat("Confusion Matrix (Predicted rows vs Actual columns):\n")
    print(rv$confusion_matrix)
    
    cat("\n--- Variable Importance (Classification Tree) ---\n")
    print(rv$class_tree_model$variable.importance)
    
    cat("\n--- Variable Importance (Regression Tree) ---\n")
    print(rv$reg_tree_model$variable.importance)
  })
}

shinyApp(ui = ui, server = server)