# Introduction #
# 
# Goal: predict health care costs and charges of hospital patients using the 2015 de-
# identified NY inpatient discharge data provided by the State of New York
# 
# More information about the data and data features can be found at
# https://www.opendatanetwork.com/dataset/health.data.ny.gov/82xm-y6g8
# or by searching Hospital Inpatient Discharges (SPARCS De-Identified): 2015

# Data Preparation #
#install.packages("data.table")
library(data.table)

data <- fread("data/HospitalDischarges.csv")

pred_analytics <- function(data,vis=FALSE,final_model=TRUE){
  # High level understanding of our data
  head(data)
  tail(data)
  
  # many long feature names including spaces
  
  # Data Structure
  str(data)
  
  # Length of Stay set as type chr, must be changed to integer
  # Total Charges and Total Costs set as type chr, must be changed to integer
  # NA and blank data
  
  # Cleaning our data
  
  # remove plus sign from Length of stay, change to numeric
  data$`Length of Stay`[data$`Length of Stay`=='120+'] <- 120
  data$`Length of Stay` <- as.numeric(data$`Length of Stay`)
  
  # checking for NA and duplicate data
  any(is.na(data))
  any(duplicated(data))
  
  # remove NA and duplicate data
  data <- na.omit(data)
  data <- unique(data)
  
  # remove discharge year since all 2015
  data <- data[,-'Discharge Year']
  
  # remove abortion edit indicator since all N
  data <- data[,-'Abortion Edit Indicator']
  
  # remove birth weight since majority zero and several numbers in the thousands
  data <- data[,-'Birth Weight']
  
  # lets drop all columns that have a correlating ID column since they are redundant
  data <- data[,-'Facility Name']
  data <- data[,-'CCS Diagnosis Description']
  data <- data[,-'CCS Procedure Description']
  data <- data[,-'APR DRG Description']
  data <- data[,-'APR MDC Description']
  data <- data[,-'APR Severity of Illness Description']
  
  # remove dollar sign and commas from Total Charges and Total Costs
  data$`Total Charges` <- gsub(",", "", data$`Total Charges`)
  data$`Total Costs` <- gsub(",", "", data$`Total Costs`)
  
  data$`Total Charges` <- gsub("\\$", "", data$`Total Charges`)
  data$`Total Costs` <- gsub("\\$", "", data$`Total Costs`)
  
  # change Total Charges and Total Costs to numeric
  data$`Total Charges` <- as.numeric(data$`Total Charges`)
  data$`Total Costs` <- as.numeric(data$`Total Costs`)
  
  # change race 'other race' to just other
  data <- data[`Race` == "Other Race", `Race` := "Other"]
  
  # health service area, hospital county, age group, race, ethnicity, type of admission, gender
  # patient disposition,apr risk of mortality, apr medical surgical description,
  # payment typology 1, payment typology 2, payment typology 3, emergency department indicator,
  
  # zip code, facility id, operating certificate number, attending provider license number,
  # operating provider license number, CCS procedure code, CCS diagnosis code, 
  # APR Severity of Illness Code, APR DRG code, and APR MDC code 
  # should be treated as a factor since they are nominal numbers
  
  # convert the above features to factors
  col_names <- c('Health Service Area','Hospital County','Age Group','Race','Gender',
                 'Ethnicity','Type of Admission','Patient Disposition',
                 'APR Risk of Mortality','APR Medical Surgical Description',
                 'Payment Typology 1','Payment Typology 2','Payment Typology 3','Emergency Department Indicator',
                 'Zip Code - 3 digits','Facility Id','Operating Certificate Number',
                 'Operating Provider License Number','APR Severity of Illness Code',
                 'Attending Provider License Number','CCS Procedure Code','CCS Diagnosis Code',
                 'APR DRG Code','APR MDC Code')
  
  data[, (col_names) := lapply(.SD, as.factor), .SDcols = col_names]
  
  # Exploratory Data Analysis #
  
  # Initial Inspection
  dim(data)
  summary(data)
  
  # Uni-variate Analysis
  numerical_columns <- names(Filter(is.numeric, data))
  non_numeric_columns <- names(Filter(Negate(is.numeric), data))
  
  # categorical variables
  # for (col in non_numeric_columns) {
  #   print(table(data[[col]]))
  # }
  # 
  # for (col in non_numeric_columns) {
  #   barplot(table(data[[col]]), main = paste("Barplot of", col), xlab = col)
  # }
  # 
  # # numeric variables
  # 
  # for (col in numerical_columns) {
  #   hist(data[[col]], main = paste("Histogram of", col), xlab = col)
  # }
  # 
  # for (col in numerical_columns) {
  #   boxplot(data[[col]], main = paste("Boxplot of", col), xlab = col)
  # }
  # # Bi-variate Analysis
  # # numerical vs. numerical
  # for (col1 in numerical_columns) {
  #   for (col2 in numerical_columns) {
  #     if (col1 != col2) {
  #       plot(data[[col1]], data[[col2]], main = paste("Scatter plot of", col1, "vs.", col2),
  #            xlab = col1, ylab = col2)
  #     }
  #   }
  # }
  # 
  # # numerical vs. categorical
  # for (num_col in numerical_columns) {
  #   for (cat_col in non_numeric_columns) {
  #     boxplot(data[[num_col]] ~ data[[cat_col]], main = paste("Boxplot of", num_col, "by", cat_col),
  #             xlab = cat_col, ylab = num_col)
  #   }
  # }
  # 
  # # categorical vs. categorical
  # for (cat_col1 in non_numeric_columns) {
  #   for (cat_col2 in non_numeric_columns) {
  #     if (cat_col1 != cat_col2) {
  #       table_data <- table(data[[cat_col1]], data[[cat_col2]])
  #       print(table_data)
  #     }
  #   }
  # }
  
  
  # Correlation Analysis
  cor(data[, numerical_columns, with = FALSE])
  
  # ggplot Visualizations
  #install.packages("ggplot2")
  library(ggplot2)
  #dev.off()
  
  # plot between Total Costs and Total Charges since there was the highest correlation between the two
  ggplot(data = data, aes(x = `Total Charges`, y = `Total Costs`)) +
    geom_point(aes(color = `Total Charges`), alpha = 0.7) +  # Points with color gradient based on Total Charges
    geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "#4292c6") +  # Add a linear regression line with confidence interval
    scale_color_gradient(low = "yellow", high = "red") +  # Color gradient from low to high values
    labs(title = "Relationship between Total Charges and Total Costs",
         subtitle = "Scatter plot with linear regression line",
         x = "Total Charges",
         y = "Total Costs",
         color = "Total Charges") +
    theme_minimal() +  # Minimal theme for a cleaner look
    theme(plot.title = element_text(hjust = 0.5),  # Center the plot title
          plot.subtitle = element_text(hjust = 0.5))  # Center the plot subtitle
  
  # we have a lot of noise in our previous plots
  # this noise is caused by outliers and numerous levels, making graphs hard to interpret
  # before we go back and clean the data up further, lets revisit our less noisy graphs
  
  # check the number of levels using structure
  str(data)
  # Age group, gender, race, ethnicity, length of stay, type of admission, 
  # patient disposition, APR severity of illness code, 
  # APR severity of illness described, APR medical surgical description
  
  # the above features are less noisy, lets create violin plots using these features
  #install.packages("gridExtra")
  library(gridExtra)
  
  focus_cols <- c('Age Group','Race','Ethnicity','Type of Admission',
                  'APR Severity of Illness Code')
  y_cols <- c('Total Charges','Total Costs')
  
  create_violin <- function(x_col,y_col) {
    if (y_col == 'Total Charges'){
      stats <- boxplot.stats(data$`Total Charges`)$stats
    }
    if (y_col == 'Total Costs'){
      stats <- boxplot.stats(data$`Total Costs`)$stats
    }
    
    p <- ggplot(data, aes_string(x=paste("`", x_col, "`", sep=""), y=paste("`", y_col, "`", sep=""), fill=paste("`", x_col, "`", sep=""))) +
      geom_violin(outlier.shape = NA) +
      theme_bw() +
      labs(title=paste("Boxplot of", y_col, "by", x_col),
           x=x_col, y=y_col) +
      scale_fill_viridis_d() + 
      scale_y_continuous(limits = c(stats[1],stats[5])) + 
      coord_cartesian(ylim = c(stats[1],stats[5]))
    
    return(p)
  }
  
  violins <- list()
  if (vis==TRUE){
    for (x_col in focus_cols) {
      for (y_col in y_cols) {
        violins[[paste(x_col, y_col)]] <- create_violin(x_col, y_col)
      }
    }
    
    for (plot in violins){
      print(plot)
    }
  }
  
  # histograms
  create_histogram <- function(column_name) {
    stats <- boxplot.stats(data$column_name)$stats
    p <- ggplot(data, aes_string(x = paste("`", column_name, "`", sep=""))) +
      geom_histogram(bins = 30, fill = "blue", color = "black") + # Histogram settings
      theme_minimal() +
      ggtitle(paste("Histogram of", column_name)) +
      xlab(column_name) + 
      ylab("Frequency") +
      theme(plot.title = element_text(hjust = 0.5)) # Centering the title
    
    return(p)
  }
  
  if (vis==TRUE){
    histograms <- lapply(numerical_columns, create_histogram)
    grid.arrange(grobs = histograms, ncol = 1)
  }
  
  # it is very clear we have an abundance of outliers
  # additionally, my hypothesis is we have several features that are not contributing very much
  # to our data set i.e. discharge year = 2015 for every patient
  # our data also appears to be heavily skewed right, most averages for total cost and total 
  # charges fall towards the lower end with large tails
  
  # Predictive Analytics #
  
  # we are using Total Charges as our dependent variable, lets get rid of Total Costs
  data <- data[,-'Total Costs']
  
  # factor grouping to remove NA coefficients in later analysis
  # group by most significant counties and other if else
  data[, `Hospital County` := ifelse(`Hospital County`== "Montgomery", "Montgomery",
                                     #ifelse(`Hospital County` == "Delaware", "Delaware",
                                     ifelse(`Hospital County` == "Dutchess", "Dutchess",
                                            ifelse(`Hospital County` == "Otsego", "Otsego",
                                                   ifelse(`Hospital County` == "Rensselaer","Rensselar",
                                                          ifelse(`Hospital County` == "Ulster","Ulster","Other")))))]
  
  # group diagnosis code by range
  data$`CCS Diagnosis Code` <- as.numeric(data$`CCS Diagnosis Code`)
  data[, `CCS Diagnosis Code` := ifelse(`CCS Diagnosis Code` <= 20, "1-20",
                                        ifelse(`CCS Diagnosis Code` <= 40, "21-40",
                                               ifelse(`CCS Diagnosis Code` <= 60, "41-60",
                                                      ifelse(`CCS Diagnosis Code` <= 80, "61-80","81-99"))))]
  data$`CCS Diagnosis Code` <- as.factor(data$`CCS Diagnosis Code`)
  
  # group procedure code by range
  data$`CCS Procedure Code` <- as.numeric(data$`CCS Procedure Code`)
  data[, `CCS Procedure Code` := ifelse(`CCS Procedure Code` <= 50, "1-50",
                                        ifelse(`CCS Procedure Code` <= 100, "51-100",
                                               ifelse(`CCS Procedure Code` <= 150, "101-150",
                                                      ifelse(`CCS Procedure Code` <= 200, "151-200","201-250"))))]
  data$`CCS Procedure Code` <- as.factor(data$`CCS Procedure Code`)
  
  # group DRG code by range
  data$`APR DRG Code` <- as.numeric(data$`APR DRG Code`)
  data[, `APR DRG Code` := ifelse(`APR DRG Code` <= 200, "1-200",
                                  ifelse(`APR DRG Code` <= 400, "201-400",
                                         ifelse(`APR DRG Code` <= 600, "401-600",
                                                ifelse(`APR DRG Code` <= 800, "601-800","801-1000"))))]
  data$`APR DRG Code` <- as.factor(data$`APR DRG Code`)
  
  # group severity of illness code 3 and 4 together
  data[, `APR Severity of Illness Code` := ifelse(`APR Severity of Illness Code` == "1","1",
                                                  ifelse(`APR Severity of Illness Code` == "2","2","Other"))]
  
  # group risk of mortality minor, moderate together
  data[, `APR Risk of Mortality` := ifelse(`APR Risk of Mortality` == "Extreme","Extreme",
                                           ifelse(`APR Risk of Mortality` == "Major","Major","Other"))]
  
  # group rare levels into other category so we do not have new factor levels 
  # not present in the training data when we run on test
  processColumn <- function(data_col, threshold, colName) {
    freq <- table(data_col)
    rare_levels <- names(freq[freq < threshold])
    
    if (length(rare_levels) == 0) {
      return(list(data_col = data_col, modified = FALSE))
    }
    
    data_col <- as.character(data_col)
    data_col[data_col %in% rare_levels] <- "Other"
    data_col <- factor(data_col)
    return(list(data_col = data_col, modified = TRUE))
  }
  
  # Applying the function to each column and gathering modified columns
  modified_columns <- c()
  threshold <- nrow(data) * 0.001
  
  for (colName in names(data)) {
    if (is.factor(data[[colName]])) {
      result <- processColumn(data[[colName]], threshold, colName)
      data[[colName]] <- result$data_col
      if (result$modified) {
        modified_columns <- c(modified_columns, colName)
      }
    }
  }
  
  # splitting our data
  set.seed(64) # set random seed (also my football number)
  trainIdx <- sample(dim(data)[1],ceiling(dim(data)[1]*0.65))
  train <- data[trainIdx,]
  test <- data[-trainIdx,]
  
  # ensure that other has been used in model by adding it randomly to a training row
  # this is not the most scientific approach, but with our large dataset size, 
  # we are not risking impacting the quality of our model
  # we need to ensure that other is included in the train levels or when the model
  # is ran on the test data, it might find other as a new level and it would be unable to continue
  # additionally, I chose to add other to training columns that have had rare occurances swapped to other
  # so we are not bringing NA coefficients into the model
  
  if (length(modified_columns) > 0) {
    random_row_index <- sample(nrow(train), 1)
    for (colName in modified_columns) {
      train[random_row_index, colName] <- "Other"
    }
  }
  
  X_train <- train[,-'Total Charges']
  y_train <- train[,'Total Charges']
  
  X_test <- test[,-'Total Charges']
  y_test <- test[,'Total Charges']
  
  # creating our model
  options(max.print=1000000)
  
  if (final_model==FALSE){
    model1 <- lm(`Total Charges` ~ ., data=train)
    model1_summary <- summary(model1)
    print(model1_summary)
    
    
    # we have a lot factors from several of our features that are making our results hard to interpret
    # most likely this large number of factors is leading to overfitting, lets drop these columns in our model
    
    model2 <- lm(`Total Charges` ~. -`Attending Provider License Number`-`Operating Provider License Number`-`Other Provider License Number`, data=train)
    model2_summary <- summary(model2)
    print(model2_summary)
  }
  
  # we will also drop APR Medical Surgical Description, APR MDC Code, Facility ID, Operating Certificate Number due to NA coefficients
  model3 <- lm(`Total Charges` ~. -`Attending Provider License Number`-`Operating Provider License Number`-`Other Provider License Number`-`APR Medical Surgical Description`-`APR MDC Code`-`Facility Id`-`Operating Certificate Number`,data=train)
  model3_summary <- summary(model3)
  print(model3_summary)
  
  
  test_analysis <- function(model,name){
    # using our test data
    print(name)
    pred <- predict(model,test)
    
    # computing R^2
    SSE = sum((test$`Total Charges` - pred)^2)
    SST = sum((test$`Total Charges` - mean(train$`Total Charges`))^2)
    R2 <- 1 - SSE/SST
    print("R^2:")
    print(R2)
    
    # residual plot
    resid <- test$`Total Charges`-pred
    len <- length(resid)
    plot(1:len, 
         resid,
         pch = 4,
         cex = 0.5,
         col = "red",
         main = paste("Residual Plot",name),
         xlab = "Index of Testing Data",
         ylab = "Residual") # x-coord: index of training samples, y-coord: residual
  }
  
  model3_analysis <- test_analysis(model3,"Model")
}
# Now, lets test the robustness of our model by using different dataset sizes 
dataIdx <- sample(dim(data)[1],ceiling(dim(data)[1]*0.1))
data1 <- data[dataIdx,]
pred_analytics(data1)
# R^2: 0.667

dataIdx <- sample(dim(data)[1],ceiling(dim(data)[1]*0.075))
data2 <- data[dataIdx,]
pred_analytics(data2)
# R^2: 0.698

dataIdx <- sample(dim(data)[1],ceiling(dim(data)[1]*0.05))
data3 <- data[dataIdx,]
pred_analytics(data3)
# R^2: 0.662

dataIdx <- sample(dim(data)[1],ceiling(dim(data)[1]*0.01))
data4 <- data[dataIdx,]
pred_analytics(data4)
# R^2:

dataIdx <- sample(dim(data)[1],ceiling(dim(data)[1]*0.001))
data5 <- data[dataIdx,]
pred_analytics(data5)
# R^2: