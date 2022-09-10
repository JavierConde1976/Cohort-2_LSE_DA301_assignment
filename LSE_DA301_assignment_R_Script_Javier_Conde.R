## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# LSE_DA301_Assignment_RScript_Javier_Conde

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4: EDA using R-----------------------------------------------------------

# 1. Load and explore the data

# Install and import Tidyverse.

install.packages('tidyverse')
library(tidyverse)

# Import the data set.
turtle_sales <- read.csv(file.choose(), header=T)

# Print the data frame.
turtle_sales

View(turtle_sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns (Ranking, Year, Genre, Publisher). 

turtle_sales2 <- subset(turtle_sales, select=-c(Ranking, Year, Genre, Publisher))

# View the data frame and structure.

turtle_sales2
str(turtle_sales2)

# View the descriptive statistics.

summary(turtle_sales2)


################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.

qplot(Global_Sales, EU_Sales, data=turtle_sales2,
      main='Scatterplot global sales vs EU sales')

qplot(Global_Sales, NA_Sales, data=turtle_sales2,
      main='Scatterplot global sales vs NA sales')

qplot(EU_Sales, NA_Sales, data=turtle_sales2,
      main='Scatterplot EU sales vs NA sales')

## 2b) Histograms
# Create histograms.

qplot(Global_Sales, bins=25, data=turtle_sales2, main='Histogram global sales')
qplot(EU_Sales, bins=25, data=turtle_sales2, main='Histogram EU sales')
qplot(NA_Sales, bins=25, data=turtle_sales2, main='Histogram NA sales')

## 2c) Boxplots
# Create boxplots.

qplot(Global_Sales, data=turtle_sales2, colour=I('orange'), 
      main='Boxplot global sales', geom='boxplot')

qplot(EU_Sales, data=turtle_sales2, colour=I('orange'), 
      main='Boxplot EU sales', geom='boxplot')

qplot(NA_Sales, data=turtle_sales2, colour=I('orange'), 
      main='Boxplot NA sales', geom='boxplot')


###############################################################################

# 3. Determine the impact on sales per product_id.

## 3a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.

turtle_sales_product <- turtle_sales %>% group_by(Product) %>%
  summarise(across(.cols = c('NA_Sales', 'EU_Sales', 'Global_Sales'), ~sum(.)))

# View the data frame.

as_tibble(turtle_sales_product)

# Explore the data frame.

summary(turtle_sales_product)

View(turtle_sales_product)

## 3b) Determine which plot is the best to compare game sales.
# Create scatterplots.

qplot(Global_Sales, EU_Sales, data=turtle_sales_product,
      main='Scatterplot global sales vs EU sales grouped by product')

qplot(Global_Sales, NA_Sales, data=turtle_sales_product,
      main='Scatterplot global sales vs NA sales grouped by product')

qplot(EU_Sales, NA_Sales, data=turtle_sales_product,
      main='Scatterplot EU sales vs NA sales grouped by product')


# Create histograms.

qplot(Global_Sales, bins=25, data=turtle_sales_product, 
      main='Histogram global sales grouped by product')
qplot(EU_Sales, bins=25, data=turtle_sales_product, 
      main='Histogram global sales grouped by product')
qplot(NA_Sales, bins=25, data=turtle_sales_product, 
      main='Histogram global sales grouped by product')

# Create boxplots.

qplot(Global_Sales, data=turtle_sales_product, colour=I('orange'), 
      main='Boxplot global sales grouped by product', geom='boxplot')

qplot(EU_Sales, data=turtle_sales_product, colour=I('orange'), 
      main='Boxplot EU sales grouped by product', geom='boxplot')

qplot(NA_Sales, data=turtle_sales_product, colour=I('orange'), 
      main='Boxplot NA sales grouped by product', geom='boxplot')


###############################################################################

# 4. Observations and insights

# relationship between partial sales and global sales
# skewness

# Install the moments package and load the library.
install.packages('moments') 
library(moments)

skewness(turtle_sales2$Global_Sales)
skewness(turtle_sales2$EU_Sales)
skewness(turtle_sales2$NA_Sales)

# all skewed to the right or positively skewed

# some outliers to watch (on recommendations)



###############################################################################
###############################################################################


# Week 5: Cleaning and maniulating data using R -------------------------------

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 3. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 4. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.

turtle_sales_product

# View head, structore, descriptive statistics.

head(turtle_sales_product)
str(turtle_sales_product)
summary(turtle_sales_product)


# Check output: Determine the min, max, and mean values.
.
min(turtle_sales_product$Global_Sales)
min(turtle_sales_product$EU_Sales)
min(turtle_sales_product$NA_Sales)

max(turtle_sales_product$Global_Sales)
max(turtle_sales_product$EU_Sales)
max(turtle_sales_product$NA_Sales)

mean(turtle_sales_product$Global_Sales)
mean(turtle_sales_product$EU_Sales)
mean(turtle_sales_product$NA_Sales)


# View the descriptive statistics.

summary(turtle_sales_product)


###############################################################################

# 2. Determine the normality of the data set.

## 2a) Create Q-Q Plots
# Create Q-Q Plots.

qqnorm(turtle_sales_product$Global_Sales)
# Add a reference line:
qqline(turtle_sales_product$Global_Sales, col='blue')

qqnorm(turtle_sales_product$EU_Sales)
# Add a reference line:
qqline(turtle_sales_product$EU_Sales, col='blue')

qqnorm(turtle_sales_product$NA_Sales)
# Add a reference line:
qqline(turtle_sales_product$NA_Sales, col='blue')


## 2b) Perform Shapiro-Wilk test
# Install and import Moments.

install.packages('moments') 
library(moments)

# Perform Shapiro-Wilk test.

shapiro.test((turtle_sales_product$Global_Sales))
shapiro.test((turtle_sales_product$EU_Sales))
shapiro.test((turtle_sales_product$NA_Sales))

# Our p-values are way below 0.05 (Global 2.2e-16, EU 2.987e-16, NA 2.2e-16),
# so we can conclude that the sample data is not normally distributed.

## 2c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

skewness(turtle_sales_product$Global_Sales)
skewness(turtle_sales_product$EU_Sales)
skewness(turtle_sales_product$NA_Sales)

kurtosis(turtle_sales_product$Global_Sales)
kurtosis(turtle_sales_product2$EU_Sales)
kurtosis(turtle_sales_product2$NA_Sales)


## 2d) Determine correlation
# Determine correlation.


###############################################################################

# 3. Plot the data
# Create plots to gain insights into data.


###############################################################################

# 4. Observations and insights
# Your observations and insights here...


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.


# Determine a summary of the data frame.


###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.



## 2b) Create a plot (simple linear regression)
# Basic visualisation.


###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.


# Multiple linear regression model.


###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.



###############################################################################

# 5. Observations and insights
# Your observations and insights here...


###############################################################################
###############################################################################




