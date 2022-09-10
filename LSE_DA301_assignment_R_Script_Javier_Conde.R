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

# 3. Observations and insights

# Relationship between partial sales and global sales.
# Skewness:

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


# Week 5: Cleaning and manipulating data using R -------------------------------

# 1. Load and explore the data

# View data frame created in Week 4.

turtle_sales2

# View head qnd structure

head(turtle_sales2)
str(turtle_sales2)


# Check output: Determine the min, max, and mean values.
.
min(turtle_sales2$Global_Sales)
min(turtle_sales2$EU_Sales)
min(turtle_sales2$NA_Sales)

max(turtle_sales2$Global_Sales)
max(turtle_sales2$EU_Sales)
max(turtle_sales2$NA_Sales)

mean(turtle_sales2$Global_Sales)
mean(turtle_sales2$EU_Sales)
mean(turtle_sales2$NA_Sales)


# View the descriptive statistics.

summary(turtle_sales2)


###############################################################################

# 2. Determine the impact on sales per product_id.

# 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.

turtle_sales_product <- turtle_sales %>% group_by(Product) %>%
  summarise(across(.cols = c('NA_Sales', 'EU_Sales', 'Global_Sales'), ~sum(.)))

# View the data frame.

as_tibble(turtle_sales_product)

# Summary and View of the new data frame.

summary(turtle_sales_product)

View(turtle_sales_product)

## 2b) Create scatterplots, histograms and boxplots to gain insights into the sales data.
# Scatterplots.

qplot(Global_Sales, EU_Sales, data=turtle_sales_product,
      main='Scatterplot global sales vs EU sales grouped by product')

qplot(Global_Sales, NA_Sales, data=turtle_sales_product,
      main='Scatterplot global sales vs NA sales grouped by product')

qplot(EU_Sales, NA_Sales, data=turtle_sales_product,
      main='Scatterplot EU sales vs NA sales grouped by product')


# Histograms.

qplot(Global_Sales, bins=25, data=turtle_sales_product, 
      main='Histogram global sales grouped by product')
qplot(EU_Sales, bins=25, data=turtle_sales_product, 
      main='Histogram global sales grouped by product')
qplot(NA_Sales, bins=25, data=turtle_sales_product, 
      main='Histogram global sales grouped by product')

# Boxplots.

qplot(Global_Sales, data=turtle_sales_product, colour=I('orange'), 
      main='Boxplot global sales grouped by product', geom='boxplot')

qplot(EU_Sales, data=turtle_sales_product, colour=I('orange'), 
      main='Boxplot EU sales grouped by product', geom='boxplot')

qplot(NA_Sales, data=turtle_sales_product, colour=I('orange'), 
      main='Boxplot NA sales grouped by product', geom='boxplot')


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
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


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.

install.packages('moments') 
library(moments)

# Perform Shapiro-Wilk test.

shapiro.test((turtle_sales_product$Global_Sales))
shapiro.test((turtle_sales_product$EU_Sales))
shapiro.test((turtle_sales_product$NA_Sales))

# Our p-values are way below 0.05 (Global 2.2e-16, EU 2.987e-16, NA 2.2e-16),
# so we can conclude that the sample data is not normally distributed.

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

skewness(turtle_sales_product$Global_Sales)
skewness(turtle_sales_product$EU_Sales)
skewness(turtle_sales_product$NA_Sales)

kurtosis(turtle_sales_product$Global_Sales)
kurtosis(turtle_sales_product2$EU_Sales)
kurtosis(turtle_sales_product2$NA_Sales)


## 3d) Determine correlation between the sales data columns

round(cor(turtle_sales_product), digits=2)

# Strong correlation between Global_sales and NA_sales (0.92), Global_sales and
# EU_sales (0.85). Less good correlation between EU_sales and NA_sales.


###############################################################################

# 3. Plot the data
# Create plots to gain insights into data.

# Scatterplot global sales vs NA sales

ggplot(data=turtle_sales_product,mapping=aes(x=Global_Sales, y=NA_Sales)) +
  geom_point(color='black',
             alpha=0.75,
             size=2.5) +
  geom_smooth(method='lm', color='orange') +
  scale_x_continuous("Global sales") +
  scale_y_continuous("North America sales") +
  labs(title="Turtle Games global sales vs North America sales (Million GBP)")


# Scatterplot global sales vs Europe sales

ggplot(data=turtle_sales_product,mapping=aes(x=Global_Sales, y=EU_Sales)) +
  geom_point(color='black',
             alpha=0.75,
             size=2.5) +
  geom_smooth(method='lm', color='orange') +
  scale_x_continuous("Global sales") +
  scale_y_continuous("Europe sales") +
  labs(title="Turtle Games global sales vs North Europe sales(Million GBP)")

# Scatterplot global sales vs Europe sales

ggplot(data=turtle_sales_product,mapping=aes(x=EU_Sales, y=NA_Sales)) +
  geom_point(color='black',
             alpha=0.75,
             size=2.5) +
  geom_smooth(method='lm', color='orange') +
  scale_x_continuous("Global sales") +
  scale_y_continuous("Europe sales") +
  labs(title="Turtle Games Europe sales vs North America sales(Million GBP)")



###############################################################################

# 4. Observations and insights

# Scatterplots very useful to understand sales variables relationships
# Strong correlation between Global_sales and NA_sales (0.92), Global_sales and
# EU_sales (0.85). Less good correlation between EU_sales and NA_sales.
.


###############################################################################
###############################################################################

# Week 6: Making recommendations to the business using R------------------------

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




