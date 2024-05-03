# Clustering and Regression Tree Analysis of Hospital Discharge Data
 
## About this Project

The goal of this project was to build a both clustering and regression tree models to predict the Total Charges billed to hospital patients upon discharge.
 
## Dataset & Dependencies

We used the Hospital Inpatient Discharges (SPARCS De-Identified): 2015 dataset for this project which is made of discharge data for New York state hospitals during 2015.

We use the following Python/JupyterNotebook libraries in our work: 
* library(data.table)
* library(caTools)
* library(rpart)
* library(rpart.plot)
* library(ggplot2)
 
## Technical Approach

1. Load the dataset
2. Perform the same data cleaning and grouping of factors as part 1
3. Split the data
4. Create our regression tree models and manually tune the minibucket hyperparameter
5. Evaluate the different models and select the best performing model
6. Perform k-means and hierarchical clustering with a subset of selected features
7. Find the elbow of the WSS curve to find the optimal number of clusters
8. Utilize this number of clusters within the clustering models
9. Evaluate the created clusters
 
## Highlights

* K means clustering
* Hierarchical clustering
* Regression trees
 
## Conclusion

The tuned regression tree model scored an R^2 metric of 0.7248439 and the best performing clustering model used hierarchical clustering and 4 clusters.
