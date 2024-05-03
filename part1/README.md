# Linear Regression Analysis of Hospital Discharge Data
 
## About this Project

The goal of this project was to build a linear regression model to predict the Total Charges billed to hospital patients upon discharge.
 
## Dataset & Dependencies

We used the Hospital Inpatient Discharges (SPARCS De-Identified): 2015 dataset for this project which is made of discharge data for New York state hospitals during 2015.

We use the following Python/JupyterNotebook libraries in our work: 
* data.table for representing our tabular data.
* ggplot2 for data visualization.
* gridextra for grid graphics.
 
## Technical Approach

1. Load the dataset
2. Clean the data
3. Perform exploratory data analysis
4. Clean our data once again
5. Group some of our factors together to ensure significant coefficients in our model
6. Build our linear regresion models using different sample sizes
7. Test our models and select the best performing model
 
## Highlights

* Linear Regression
* Data Cleaning
* EDA
 
## Conclusion

The best performing linear regression model was using a sample size of 10% of the original dataset. This scored an R^2 value of 0.7064.
