#-------------------------------------------------------------------------------
# libraries
library(data.table)
library(caTools)
library(rpart)
library(rpart.plot)
library(ggplot2)
#-------------------------------------------------------------------------------
# loading in our data
big_data <- fread("HospitalDischarges.csv")

# we sample our dataset to reduce computational cost, computer keeps crashing
# cleaning data will also further reduce data size, so don't want to set initial
# sample size too low as well
data <- big_data[sample(nrow(big_data), 100000), ]
str(data)

#-------------------------------------------------------------------------------
# prepare data in same format as for linear regression (PART 1)
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
threshold <- nrow(data) * 0.01

for (colName in names(data)) {
  if (is.factor(data[[colName]])) {
    result <- processColumn(data[[colName]], threshold, colName)
    data[[colName]] <- result$data_col
    if (result$modified) {
      modified_columns <- c(modified_columns, colName)
    }
  }
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# splitting the dataset
set.seed(64)
spl = sample.split(data$`Total Charges`, SplitRatio = 0.7)
Train = subset(data, spl==TRUE)
Test = subset(data, spl==FALSE)

print(Train)
print(Test)

sink("Trees.txt",append=TRUE,split=TRUE)

#-------------------------------------------------------------------------------
# now, we will manually tune the minibucket hyperparameter
# first lets start with minibucket = 50 and using criterion = Gini (default)
HospitalTree50 = rpart(`Total Charges` ~ . -`Attending Provider License Number`-`Operating Provider License Number`-`Other Provider License Number`, data = Train, minbucket=50)
summary(HospitalTree50)

prp(HospitalTree50)
rpart.plot(HospitalTree50, box.palette = 'Blues')

# minibucket = 20, Gini
HospitalTree20 = rpart(`Total Charges` ~ . -`Attending Provider License Number`-`Operating Provider License Number`-`Other Provider License Number`, data = Train, method="anova", minbucket=20)
summary(HospitalTree20)

prp(HospitalTree20)
rpart.plot(HospitalTree20, box.palette = 'Blues')

# minibucket = 10, Gini
HospitalTree10 = rpart(`Total Charges` ~ . -`Attending Provider License Number`-`Operating Provider License Number`-`Other Provider License Number`, data = Train, method="anova", minbucket=10)
summary(HospitalTree10)

# one of the best models, so we will save the tree
jpeg("HospitalTree10.jpeg") # preparing to save the next tree as a JPEG
prp(HospitalTree10)
rpart.plot(HospitalTree10, box.palette = 'Blues')
dev.off() # saving as JPEG

# minibucket = 5, Gini
HospitalTree5 = rpart(`Total Charges` ~ . -`Attending Provider License Number`-`Operating Provider License Number`-`Other Provider License Number`, data = Train, method="anova", minbucket=5)
summary(HospitalTree5)

prp(HospitalTree5)
rpart.plot(HospitalTree5, box.palette = 'Blues')

#-------------------------------------------------------------------------------
# now, we will use criterion = information gain
# minibucket = 50, information gain
HospitalTree50b = rpart(`Total Charges` ~ . -`Attending Provider License Number`-`Operating Provider License Number`-`Other Provider License Number`, data = Train, minbucket=50, parms=list(split='information'))
summary(HospitalTree50b)

prp(HospitalTree50b)
rpart.plot(HospitalTree50b, box.palette = 'Blues')

# minibucket = 20, information gain
HospitalTree20b = rpart(`Total Charges` ~ . -`Attending Provider License Number`-`Operating Provider License Number`-`Other Provider License Number`, data = Train, method="anova", minbucket=20, parms=list(split='information'))
summary(HospitalTree20b)

prp(HospitalTree20b)
rpart.plot(HospitalTree20b, box.palette = 'Blues')

# minibucket = 10, information gain
HospitalTree10b = rpart(`Total Charges` ~ . -`Attending Provider License Number`-`Operating Provider License Number`-`Other Provider License Number`, data = Train, method="anova", minbucket=10, parms=list(split='information'))
summary(HospitalTree10b)

# one of the best models, so we will save the tree
jpeg("HospitalTree10b.jpeg") # preparing to save the next tree as a JPEG
prp(HospitalTree10b)
rpart.plot(HospitalTree10b, box.palette = 'Blues')
dev.off() # saving as JPEG

# minibucket = 5, information gain
HospitalTree5b = rpart(`Total Charges` ~ . -`Attending Provider License Number`-`Operating Provider License Number`-`Other Provider License Number`, data = Train, method="anova", minbucket=5, parms=list(split='information'))
summary(HospitalTree5b)

prp(HospitalTree5b)
rpart.plot(HospitalTree5b, box.palette = 'Blues')

#-------------------------------------------------------------------------------
# we will now make predictions on the training set to determine which of our minibucket and criterion hyper-parameters perform the best
# minibucket = 50, Gini
pred <- predict(HospitalTree50,Test)
SSE = sum((Test$`Total Charges` - pred)^2)
SST = sum((Test$`Total Charges` - mean(Train$`Total Charges`))^2)
R2_HospitalTree50 <- 1 - SSE/SST
print("HospitalTree50 R^2:")
print(R2_HospitalTree50)

# minibucket = 20, Gini
pred <- predict(HospitalTree20,Test)
SSE = sum((Test$`Total Charges` - pred)^2)
SST = sum((Test$`Total Charges` - mean(Train$`Total Charges`))^2)
R2_HospitalTree20 <- 1 - SSE/SST
print("HospitalTree20 R^2:")
print(R2_HospitalTree20)

# minibucket = 10, Gini
pred <- predict(HospitalTree10,Test)
SSE = sum((Test$`Total Charges` - pred)^2)
SST = sum((Test$`Total Charges` - mean(Train$`Total Charges`))^2)
R2_HospitalTree10 <- 1 - SSE/SST
print("HospitalTree10 R^2:")
print(R2_HospitalTree10)

# minibucket = 5, Gini
pred <- predict(HospitalTree5,Test)
SSE = sum((Test$`Total Charges` - pred)^2)
SST = sum((Test$`Total Charges` - mean(Train$`Total Charges`))^2)
R2_HospitalTree5 <- 1 - SSE/SST
print("HospitalTree5 R^2:")
print(R2_HospitalTree5)

#-------------------------------------------------------------------------------
# minibucket = 50, information gain
pred <- predict(HospitalTree50b,Test)
SSE = sum((Test$`Total Charges` - pred)^2)
SST = sum((Test$`Total Charges` - mean(Train$`Total Charges`))^2)
R2_HospitalTree50b <- 1 - SSE/SST
print("HospitalTree50b R^2:")
print(R2_HospitalTree50b)

# minibucket = 20, information gain
pred <- predict(HospitalTree20b,Test)
SSE = sum((Test$`Total Charges` - pred)^2)
SST = sum((Test$`Total Charges` - mean(Train$`Total Charges`))^2)
R2_HospitalTree20b <- 1 - SSE/SST
print("HospitalTree20b R^2:")
print(R2_HospitalTree20b)

# minibucket = 10, information gain
pred <- predict(HospitalTree10b,Test)
SSE = sum((Test$`Total Charges` - pred)^2)
SST = sum((Test$`Total Charges` - mean(Train$`Total Charges`))^2)
R2_HospitalTree10b <- 1 - SSE/SST
print("HospitalTree10b R^2:")
print(R2_HospitalTree10b)

# minibucket = 5, information gain
pred <- predict(HospitalTree5b,Test)
SSE = sum((Test$`Total Charges` - pred)^2)
SST = sum((Test$`Total Charges` - mean(Train$`Total Charges`))^2)
R2_HospitalTree5b <- 1 - SSE/SST
print("HospitalTree5b R^2:")
print(R2_HospitalTree5b)

#-------------------------------------------------------------------------------
# HospitalTree10 and HospitalTree10b are the best performing trees
# both have an R^2 value of 0.7248

# Closing the output to a file
sink()
file.show("Trees.txt")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Do clustering on the data set to group patients together. You can pick the method
# you like: hierarchical or k-means.
# a. OREM 7357: use of the techniques seen in class (such as wss/elbow’s method) to pick the optimal number of clusters.
# b. (everyone) Describe the clusters in words.

# let's first determine which columns to convert to numeric for our clustering
str(data)
clustering_data = data

# length of stay, total charges, emergency department indicator, severity of illness code, age group, zip
# all can be converted (if not already) to numeric without having to convert to a numeric format
clustering_data$`Length of Stay` <- as.numeric(clustering_data$`Length of Stay`)
clustering_data$`Total Charges` <- as.numeric(clustering_data$`Total Charges`)
clustering_data$`Emergency Department Indicator` <- as.numeric(clustering_data$`Emergency Department Indicator`)
clustering_data$`APR Severity of Illness Code` <- as.numeric(clustering_data$`APR Severity of Illness Code`)
clustering_data$`Age Group` <- as.numeric(clustering_data$`Age Group`)
clustering_data$`Zip Code - 3 digits` <- as.numeric(clustering_data$`Zip Code - 3 digits`)

# remove any NAs introduced by coersion
clustering_data <- na.omit(clustering_data)

# get rid of columns aside from our clustering columns
clustering_data[, c("Health Service Area", "Hospital County", "Operating Certificate Number", "Facility Id", "Gender", "Race", "Ethnicity",
         "Type of Admission", "Patient Disposition", "CCS Diagnosis Code", "CCS Procedure Code",
         "APR DRG Code", "APR MDC Code", "APR Risk of Mortality", "APR Medical Surgical Description",
         "Payment Typology 1", "Payment Typology 2", "Payment Typology 3", "Attending Provider License Number",
         "Operating Provider License Number", "Other Provider License Number") := NULL]

# check new dataset
str(clustering_data)

# normalize the data
preproc = preProcess(clustering_data)
dataClusterNorm = predict(preproc, clustering_data)

# check normalized dataset
str(dataClusterNorm)

#-------------------------------------------------------------------------------
# calculating elbow of WSS plot to determine the optimal number of clusters
wss_calculation <- function(data, max_clusters = 10) {
  wss <- numeric(max_clusters)
  for (k in 1:max_clusters) {
    set.seed(64) #sSet seed for reproducibility
    model <- kmeans(data, centers = k, nstart = 20)
    wss[k] <- model$tot.withinss
  }
  return(wss)
}

# maximum possible number of clusters = 10
max_clusters <- 10

# calculate WSS for 1 to max_clusters
wss_values <- wss_calculation(dataClusterNorm, max_clusters)

# plot the Elbow Curve
plot(1:max_clusters, wss_values, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K", ylab = "Total within-clusters sum of squares",
     main = "Elbow Method for Determining Optimal k")

# optimal number of clusters = 4

#-------------------------------------------------------------------------------
# k-means clustering
set.seed(64)
KmeansClustering = kmeans(dataClusterNorm, centers = 4)
table(KmeansClustering$cluster)

#-------------------------------------------------------------------------------
# hierarchical clustering
distances = dist(dataClusterNorm, method="euclidean")
HierClustering = hclust(distances, method="ward.D")
plot(HierClustering)

clusterGroups = cutree(HierClustering, k = 4)
table(clusterGroups)
tapply(clustering_data$`Length of Stay`, clusterGroups, mean)
tapply(clustering_data$`Total Charges`, clusterGroups, mean)
tapply(clustering_data$`Emergency Department Indicator`, clusterGroups, mean)
tapply(clustering_data$`APR Severity of Illness Code`, clusterGroups, mean)
tapply(clustering_data$`Age Group`, clusterGroups, mean)
tapply(clustering_data$`Zip Code - 3 digits`, clusterGroups, mean)

#-------------------------------------------------------------------------------
# describing our hierarchical clusters
# Cluster 1
# Size: 348 observations
# Length of Stay: On average, patients in this cluster have a relatively shorter length of stay in the hospital, averaging 2.8 days.
# Total Charges: This cluster has an average total charge of approximately $26,539, which suggests moderate hospitalization costs.
# Emergency Department Indicator: The mean value is 1.49, indicating a mix of visits and non-visits to the emergency department.
# Severity of Illness: The average severity of illness code is 1.0, indicating that patients in this cluster generally have a lower severity of illness.
# Age Group: The mean age group is 3.72, suggesting that patients in this cluster tend to be in the older age groups.
# Geographic Location (Zip Code): The average is around 9, indicating some geographic concentration, but the significance of this would depend on the specific zip code distribution.


# Cluster 2
# Size: 431 observations
# Length of Stay: Patients have a longer length of stay compared to Cluster 1, with an average of about 6 days.
# Total Charges: The average total charges are around $29,097, which are higher than those in Cluster 1, reflecting longer hospital stays or more intensive care.
# Emergency Department Indicator: The average of 1.03 indicates that most patients in this cluster likely did not visit the emergency department.
# Severity of Illness: With an average code of 1.95, patients in this cluster have a moderate to high severity of illness.
# Age Group: The average age group is lower than in Cluster 1, at 3.08, indicating a younger demographic.
# Geographic Location (Zip Code): The average zip code value is slightly lower than in Cluster 1, at around 8.39.


# Cluster 3
# Size: 404 observations
# Length of Stay: This cluster has an average length of stay of 4.37 days, which is between the values of Clusters 1 and 2.
# Total Charges: Average total charges are the lowest among all clusters, at about $22,319, which could reflect less intensive or shorter treatments.
# Emergency Department Indicator: The average value is exactly 2.0, indicating that all patients in this cluster visited the emergency department.
# Severity of Illness: The severity of illness is high with an average code of 1.99, similar to Cluster 2.
# Age Group: The mean age group is 3.89, indicating that this cluster also tends to include older patients.
# Geographic Location (Zip Code): Similar to Cluster 1, with an average around 8.55.


# Cluster 4
# Size: 391 observations
# Length of Stay: Similar to Cluster 1, with a short average length of stay of 2.66 days.
# Total Charges: This cluster has the lowest average total charges, at $7,914, suggesting less intensive or shorter hospital treatments.
# Emergency Department Indicator: The mean value is 1.0, suggesting that most patients did not visit the emergency department.
# Severity of Illness: The average severity of illness code is 1.0, indicating lower severity conditions similar to Cluster 1.
# Age Group: The average age group is significantly lower at 1.75, suggesting that this cluster predominantly consists of younger patients.
# Geographic Location (Zip Code): The average zip code value is the highest at around 9.86, possibly indicating a different geographic area compared to the other clusters.
#-------------------------------------------------------------------------------