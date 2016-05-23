#### 37301 Data Driven Marketing ######
#### Homework 5 #######
#### Author: Ben Eisenpress #########

# Load libraries
library(psych)
library(lattice)
library(plyr)
library(mfx)
library(RColorBrewer)

# Set working directory
setwd("/Users/ben/Dropbox/Chicago Booth/37103 Data Driven Marketing/Homework 6/37103-HW6")
# Load Data
load("Tuscan-Lifestyles.RData")

########################################################
################## Question 1 ##########################

summary(tuscan_DF)

histogram(tuscan_DF$numords)
histogram(tuscan_DF$totdol)
histogram(tuscan_DF$last)
histogram(tuscan_DF$dollars)

########################################################
################## Question 2 ##########################

# Source the createBins function for creating quartiles
source("createBins.R")

# Create recency, frequency and monetary indices from the data
tuscan_DF$r_index <- createBins(tuscan_DF$last, 5)
tuscan_DF$f_index <- createBins(tuscan_DF$numords, 5)
tuscan_DF$m_index <- createBins(tuscan_DF$totdol, 5)

# Re-order the frequency and monetary value indices
tuscan_DF$f_index = 6 - tuscan_DF$f_index
tuscan_DF$m_index = 6 - tuscan_DF$m_index


########################################################
################## Question 3 ##########################

# Create tables of mean response by RFM index
table_r = aggregate(buyer ~ r_index, FUN = mean, data = tuscan_DF)
table_f = aggregate(buyer ~ f_index, FUN = mean, data = tuscan_DF)
table_m = aggregate(buyer ~ m_index, FUN = mean, data = tuscan_DF)

# Calculate the maximum average response rate for any quintile of recency, fequency, and moneyary value
# Then multiply it by 1.05
# This value wil be used for bar charts
max_rate <- max(c(table_r$buyer,table_f$buyer,table_m$buyer))*1.05

# Create a bar chart of the response rates by RFM index
barchart(buyer ~ r_index, data = table_r, horizontal = FALSE, col = "plum1", ylim = c(0, max_rate), xlab = "Recency", ylab = "Response Rate")
barchart(buyer ~ f_index, data = table_f, horizontal = FALSE, col = "plum1", ylim = c(0, max_rate), xlab = "Frequency", ylab = "Response Rate")
barchart(buyer ~ m_index, data = table_m, horizontal = FALSE, col = "plum1", ylim = c(0, max_rate), xlab = "Monetary Value", ylab = "Response Rate")

# Create a table of response by recency and frequency
table_rf = aggregate(buyer ~ r_index + f_index, FUN = mean, data = tuscan_DF)

max_rate2 <- max(c(table_rf$buyer))*1.05

barchart(buyer ~ r_index, groups = f_index, data = table_rf, horizontal = FALSE, col = brewer.pal(5, "Set2"), 
         ylim = c(0, max_rate2), xlab = "Groups: Recency, Bars in Each Group: Frequency", ylab = "Response Rate")

########################################################
################## Question 4 ##########################

# Combine all 3 indices into one number
tuscan_DF$rfm_index = 100*tuscan_DF$r_index + 10*tuscan_DF$f_index + tuscan_DF$m_index

# Create a table of average response rates by index
table_rfm = aggregate(buyer ~ rfm_index, FUN = mean, data = tuscan_DF)

# Cacluate the maximum response rate for any individual index bucket.  This will be used for the bar chart.
max_rate3 <- max(c(table_rfm$buyer))*1.05

# Create a bar chart of response rates by index bucket
barchart(buyer ~ rfm_index, data = table_rfm, horizontal = FALSE, col = "plum1", scales = list(x = list(draw = FALSE)),
         ylim = c(0, max_rate3), xlab = "Recency", ylab = "Response Rate")

########################################################
################## Question 5 ##########################

# Create a table that contains the number of observations in each bucket
table_n_obs = aggregate(buyer ~ rfm_index, FUN = length, data = tuscan_DF)
colnames(table_n_obs)[2] = "n_obs"

# Create a table that contains the number of buyers in each bucket
table_n_buyers = aggregate(buyer ~ rfm_index, FUN = sum, data = tuscan_DF)
colnames(table_n_buyers)[2] = "n_buyers"

# Create a table that contains the response rate in each bucket
table_response = aggregate(buyer ~ rfm_index, FUN = mean, data = tuscan_DF)
colnames(table_response)[2] = "response"

# Create a table that contains the total dollars spent in each bucket
table_dollars = aggregate(dollars ~ rfm_index, FUN = sum, data = tuscan_DF)
colnames(table_dollars)[2] = "dollars"

# Merge summary data into one dataframe
rfm_DF = merge(table_n_obs, table_n_buyers, by = "rfm_index")
rfm_DF = merge(rfm_DF, table_response, by = "rfm_index")
rfm_DF = merge(rfm_DF, table_dollars, by = "rfm_index")

########################################################
################## Question 6 ##########################

# Calculate the mean response rate across all customers
mean_response <- mean(tuscan_DF$buyer)

# Calculate lift for each RFM bucket
rfm_DF$lift <- 100 * rfm_DF$response/mean_response

# Add a rank variable based on response rate
rfm_DF$rank_index <- rank(rfm_DF$response)

# Sort the data by response rate
rfm_DF <- rfm_DF[order(rfm_DF$response),]

# Plot the lift factor for each segment
plot(rfm_DF$rank_index, rfm_DF$lift, type = "o", pch = 21, lwd = 0.4, bg = "skyblue1", xlab = "RFM Segment Rank", ylab = "Lift")
# Add extra tick marks to the chart
axis(side = 2, at = seq(from = 0, to = 700, by = 100))
# Draw a horizonal line at 100
abline(h = 100)
# Add Gridlines
grid()


########################################################
################## Question 7 ##########################


# Sort the data by response rate, decending
rfm_DF <- rfm_DF[order(-rfm_DF$response),]

# Create a variable for the cumulative number of observations and buyers
rfm_DF$cum_obs <- cumsum(rfm_DF$n_obs)
rfm_DF$cum_buy <- cumsum(rfm_DF$n_buyers)

# Calculate cumulative average response rate
rfm_DF$cum_response <- rfm_DF$cum_buy / rfm_DF$cum_obs

# Calculate the cumulative lift
rfm_DF$cum_lift <- 100 * rfm_DF$cum_response / mean_response

# Transform the count of cumulative observations into a percentage
rfm_DF$cum_mailed <- 100 * rfm_DF$cum_obs/sum(rfm_DF$n_obs)

# Plot the cumulative lift
plot(rfm_DF$cum_mailed, rfm_DF$cum_lift, type = "o", pch = 21, lwd = 0.4, bg = "skyblue1", 
     xlab = "Cumulative Percent of Customers Targeted", ylab = "Cumulatie Lift")
# Add Gridlines
grid()

########################################################
################## Question 8 ##########################

# Calculate cumulative percent of buyers captured by targeting n percent of customers
rfm_DF$cum_buy_pct <- 100* rfm_DF$cum_buy / sum(rfm_DF$n_buyers)

# Plot the cumulative buyers reached
plot(rfm_DF$cum_mailed, rfm_DF$cum_buy_pct, type = "o", pch = 21, lwd = 0.4, bg = "skyblue1", 
     xlab = "Cumulative Percent of Customers Targeted", ylab = "Cumulatie Percent of Buyers Captured")
# Add a 45 degree line
abline(a = 0, b = 1)
# Add Gridlines
grid()

########################################################
################## Question 9 ##########################

# Each catalog costs $1
# COGS and variable costs are 50%. Therefore, profit is 50% of each order, less catalog cost
rfm_DF$profit <- .5 * rfm_DF$dollars - 1 * rfm_DF$n_obs

total_profit <- sum(rfm_DF$profit)

rfm_profit <- sum(subset(rfm_DF$profit, rfm_DF$profit > 0))

profit_improvment <- rfm_profit / total_profit - 1 
