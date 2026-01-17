####################################################################################
# Project: Momentum Strategy Analysis
#
# Description: This script constructs momentum based portfolios using historical 
# stock price data. Stocks are ranked based on past returns and winner loser  
# portfolios are evaluated using cumulative returns and factor regressions.
#
#
#
#Author: Chetan Tak
####################################################################################


## Load required libraries for data manipulation, time series handling, regression analysis and plotting

library(dplyr)
library(zoo)
library(tidyverse)
library(fixest)
library(ggplot2)

## Load monthly stock price data and Fama French factor data
# The stock dataset contains end of month adjusted prices for 100 stocks covering the # period January 2000 to November 2024

<- read.csv("data/exam2024-25-even.csv")
famafrench <- read.csv("data/fama-french-factors.csv")

##Convert the month variable from text to Date format - This ensures chronological 
# ordering and enables time series operations and alignment across datasets

p100$month<- as.Date(as.yearmon(p100$month, "%b %y"))

## Verify dataset dimensions - This checks the number of stocks (columns) and the    # number of monthly observations (rows)

ncol(p100)- 1
nrow(p100)

## Compute monthly percentage returns
# Returns are calculated over multiple formation periods to generate momentum signals

## Summary statistics of momentum returns at a given point in time

compute_returns <- function(data, lag_period) {
  apply(data, 2, function(x) {
    x <- as.numeric(x)  # Ensure numeric
    return(((x / dplyr::lag(x, lag_period)) - 1)*100)
  })
}


stock_columns <- p100[,-1]

## 1,3,6,9 and 12 months returns 

one_month_returns <- compute_returns(stock_columns, 1)
three_month_returns <- compute_returns(stock_columns, 3)
six_month_returns <- compute_returns(stock_columns, 6)
nine_month_returns <- compute_returns(stock_columns, 9)
twelve_month_returns <- compute_returns(stock_columns, 12)

## 3,6,9 and 12 mean of nov 2024

mean(three_month_returns[299,], na.rm = TRUE)
mean(six_month_returns[299,], na.rm = TRUE)
mean(nine_month_returns[299,], na.rm = TRUE)
mean(twelve_month_returns[299,], na.rm = TRUE)

## 3,6,9 and 12 standard devaition of nov 2024

sd_row_299 <- sd(three_month_returns[299, ], na.rm = TRUE)
sd_row_299 
sd_row_299 <- sd(six_month_returns[299, ], na.rm = TRUE)
sd_row_299
sd_row_299 <- sd(nine_month_returns[299, ], na.rm = TRUE)
sd_row_299
sd_row_299 <- sd(twelve_month_returns[299, ], na.rm = TRUE)
sd_row_299


## Rank stocks each month based on past returns higher ranked stocks indicate        # stronger momentum
# Stock rankings for past 3,6,9 and 12 months 

ranked_returns_3m <- t(apply(three_month_returns, 1, rank, na.last = "keep", ties.method = "average"))
ranked_returns_6m <- t(apply(six_month_returns, 1, rank, na.last = "keep", ties.method = "average"))
ranked_returns_9m <- t(apply(nine_month_returns, 1, rank, na.last = "keep", ties.method = "average"))
ranked_returns_12m <- t(apply(twelve_month_returns, 1, rank, na.last = "keep", ties.method = "average"))

# Add column names 

colnames(one_month_returns) <- paste0("Return_1m_", colnames(stock_columns))
colnames(three_month_returns) <- paste0("Return_3m_", colnames(stock_columns))
colnames(six_month_returns) <- paste0("Return_6m_", colnames(stock_columns))
colnames(nine_month_returns) <- paste0("Return_9m_", colnames(stock_columns))
colnames(twelve_month_returns) <- paste0("Return_12m_", colnames(stock_columns))

# Add column names for ranked returns

colnames(ranked_returns_3m) <- paste0("Rank_3m_", colnames(stock_columns))
colnames(ranked_returns_6m) <- paste0("Rank_6m_", colnames(stock_columns))
colnames(ranked_returns_9m) <- paste0("Rank_9m_", colnames(stock_columns))
colnames(ranked_returns_12m) <- paste0("Rank_12m_", colnames(stock_columns))

## Combine original price data with computed returns and rankings
# This creates a single consolidated dataset 


p100 <- cbind(
  p100,
  one_month_returns,
  three_month_returns,
  six_month_returns,
  nine_month_returns,
  twelve_month_returns,
  ranked_returns_3m,
  ranked_returns_6m,
  ranked_returns_9m,
  ranked_returns_12m
)

## Assign stocks into decile portfolios based on rankings
# The top decile represents winner stocks
# The bottom decile represents loser stock
# Middle deciles are ignored to focus on extreme portfolios

assign_explicit_deciles <- function(ranked_returns) {
  apply(ranked_returns, 1, function(row) {
    sapply(row, function(rank) {
      if (is.na(rank)) {
        return(NA)
      } else if (rank <= 10) {
        return(1)  # Bottom decile
      } else if (rank >= 91) {
        return(10) # Top decile
      } else {
        return(NA) # Middle ranks are ignored
      }
    })
  })
}


# Apply decile assignment for each formation period
# This generates winner and loser classifications over time

explicit_deciles_3m <- assign_explicit_deciles(ranked_returns_3m)
explicit_deciles_6m <- assign_explicit_deciles(ranked_returns_6m)
explicit_deciles_9m <- assign_explicit_deciles(ranked_returns_9m)
explicit_deciles_12m <- assign_explicit_deciles(ranked_returns_12m)

# Function to calculate returns for a specific decile (top or bottom)

compute_decile_returns_explicit <- function(returns, deciles, decile_number) {
  sapply(1:nrow(returns), function(i) {
    mean(returns[i, deciles[, i] == decile_number], na.rm = TRUE)
  })
}

# Calculate monthly returns for winner and loser decile portfolios
# Returns are based on one month holding period performance

top_returns_3m <- compute_decile_returns_explicit(one_month_returns, explicit_deciles_3m, 10)
bottom_returns_3m <- compute_decile_returns_explicit(one_month_returns, explicit_deciles_3m, 1)

top_returns_6m <- compute_decile_returns_explicit(one_month_returns, explicit_deciles_6m, 10)
bottom_returns_6m <- compute_decile_returns_explicit(one_month_returns, explicit_deciles_6m, 1)

top_returns_9m <- compute_decile_returns_explicit(one_month_returns, explicit_deciles_9m, 10)
bottom_returns_9m <- compute_decile_returns_explicit(one_month_returns, explicit_deciles_9m, 1)

top_returns_12m <- compute_decile_returns_explicit(one_month_returns, explicit_deciles_12m, 10)
bottom_returns_12m <- compute_decile_returns_explicit(one_month_returns, explicit_deciles_12m, 1)


# Create summary table reporting average winner and loser returns
# across different momentum formation periods

decile_summary_explicit <- data.frame(
  J = c(3, 6, 9, 12),
  Top_Decile_Avg_Return = c(mean(top_returns_3m, na.rm = TRUE),
                            mean(top_returns_6m, na.rm = TRUE),
                            mean(top_returns_9m, na.rm = TRUE),
                            mean(top_returns_12m, na.rm = TRUE)),
  Bottom_Decile_Avg_Return = c(mean(bottom_returns_3m, na.rm = TRUE),
                               mean(bottom_returns_6m, na.rm = TRUE),
                               mean(bottom_returns_9m, na.rm = TRUE),
                               mean(bottom_returns_12m, na.rm = TRUE))
)

## Append winner and loser portfolio returns to the main dataset p100
# This enables further performance analysis and visualization


p100 <- p100 %>%
  mutate(
    Top_Returns_3m = top_returns_3m,
    Bottom_Returns_3m = bottom_returns_3m,
    Top_Returns_6m = top_returns_6m,
    Bottom_Returns_6m = bottom_returns_6m,
    Top_Returns_9m = top_returns_9m,
    Bottom_Returns_9m = bottom_returns_9m,
    Top_Returns_12m = top_returns_12m,
    Bottom_Returns_12m = bottom_returns_12m
  )


## Compute cumulative returns for winner and loser portfolios


top_returns_3m[is.na(top_returns_3m) | is.nan(top_returns_3m)] <- 0
bottom_returns_3m[is.na(bottom_returns_3m) | is.nan(bottom_returns_3m)] <- 0

top_returns_6m[is.na(top_returns_6m) | is.nan(top_returns_6m)] <- 0
bottom_returns_6m[is.na(bottom_returns_6m) | is.nan(bottom_returns_6m)] <- 0

top_returns_9m[is.na(top_returns_9m) | is.nan(top_returns_9m)] <- 0
bottom_returns_9m[is.na(bottom_returns_9m) | is.nan(bottom_returns_9m)] <- 0

top_returns_12m[is.na(top_returns_12m) | is.nan(top_returns_12m)] <- 0
bottom_returns_12m[is.na(bottom_returns_12m) | is.nan(bottom_returns_12m)] <- 0



## Calculate cumulative sum for top and bottom deciles

cumulative_sum_top_3m <- compute_cumulative_sum(top_returns_3m)
cumulative_sum_bottom_3m <- compute_cumulative_sum(bottom_returns_3m)

cumulative_sum_top_6m <- compute_cumulative_sum(top_returns_6m)
cumulative_sum_bottom_6m <- compute_cumulative_sum(bottom_returns_6m)

cumulative_sum_top_9m <- compute_cumulative_sum(top_returns_9m)
cumulative_sum_bottom_9m <- compute_cumulative_sum(bottom_returns_9m)

cumulative_sum_top_12m <- compute_cumulative_sum(top_returns_12m)
cumulative_sum_bottom_12m <- compute_cumulative_sum(bottom_returns_12m)

# Add cumulative sum as new columns to the p100 dataset

p100 <- p100 %>%
  mutate(
    CumulativeSum_Top_Returns_3m = cumulative_sum_top_3m,
    CumulativeSum_Bottom_Returns_3m = cumulative_sum_bottom_3m,
    CumulativeSum_Top_Returns_6m = cumulative_sum_top_6m,
    CumulativeSum_Bottom_Returns_6m = cumulative_sum_bottom_6m,
    CumulativeSum_Top_Returns_9m = cumulative_sum_top_9m,
    CumulativeSum_Bottom_Returns_9m = cumulative_sum_bottom_9m,
    CumulativeSum_Top_Returns_12m = cumulative_sum_top_12m,
    CumulativeSum_Bottom_Returns_12m = cumulative_sum_bottom_12m
  )

# Function to compute cumulative returns in percentage

compute_cumulative_sum <- function(returns) {
  (cumsum(returns) / 100) * 100  # Directly sum percentage values and keep it as percentage
}

## Handle missing and undefined return values
# NA and NaN values are replaced with zero before cumulation


top_returns_3m[is.na(top_returns_3m) | is.nan(top_returns_3m)] <- 0
bottom_returns_3m[is.na(bottom_returns_3m) | is.nan(bottom_returns_3m)] <- 0

top_returns_6m[is.na(top_returns_6m) | is.nan(top_returns_6m)] <- 0
bottom_returns_6m[is.na(bottom_returns_6m) | is.nan(bottom_returns_6m)] <- 0

top_returns_9m[is.na(top_returns_9m) | is.nan(top_returns_9m)] <- 0
bottom_returns_9m[is.na(bottom_returns_9m) | is.nan(bottom_returns_9m)] <- 0

top_returns_12m[is.na(top_returns_12m) | is.nan(top_returns_12m)] <- 0
bottom_returns_12m[is.na(bottom_returns_12m) | is.nan(bottom_returns_12m)] <- 0

## Calculate cumulative sum for top and bottom deciles in percentage
# Compute cumulative returns for winner and loser portfolios
# This allows analysis of momentum performance over time

cumulative_sum_top_3m <- compute_cumulative_sum(top_returns_3m)
cumulative_sum_bottom_3m <- compute_cumulative_sum(bottom_returns_3m)

cumulative_sum_top_6m <- compute_cumulative_sum(top_returns_6m)
cumulative_sum_bottom_6m <- compute_cumulative_sum(bottom_returns_6m)

cumulative_sum_top_9m <- compute_cumulative_sum(top_returns_9m)
cumulative_sum_bottom_9m <- compute_cumulative_sum(bottom_returns_9m)

cumulative_sum_top_12m <- compute_cumulative_sum(top_returns_12m)
cumulative_sum_bottom_12m <- compute_cumulative_sum(bottom_returns_12m)

# Add cumulative sum as new columns to the p100 dataset


p100 <- p100 %>%
  mutate(
    CumulativeSum_Top_Returns_3m = cumulative_sum_top_3m,
    CumulativeSum_Bottom_Returns_3m = cumulative_sum_bottom_3m,
    CumulativeSum_Top_Returns_6m = cumulative_sum_top_6m,
    CumulativeSum_Bottom_Returns_6m = cumulative_sum_bottom_6m,
    CumulativeSum_Top_Returns_9m = cumulative_sum_top_9m,
    CumulativeSum_Bottom_Returns_9m = cumulative_sum_bottom_9m,
    CumulativeSum_Top_Returns_12m = cumulative_sum_top_12m,
    CumulativeSum_Bottom_Returns_12m = cumulative_sum_bottom_12m
  )


## Construct long short momentum portfolios
# Long position in winners and short position in losers

p100 <- p100 %>%
  mutate(
    Long_Short_3m = top_returns_3m - bottom_returns_3m,
    Long_Short_6m = top_returns_6m - bottom_returns_6m,
    Long_Short_9m = top_returns_9m - bottom_returns_9m,
    Long_Short_12m = top_returns_12m - bottom_returns_12m
  )


## Create Long-Short columns by subtracting Bottom from Top cumulative returns


p100 <- p100 %>%
  mutate(
    Cum_Long_Short_3m = CumulativeSum_Top_Returns_3m - CumulativeSum_Bottom_Returns_3m,
    Cum_Long_Short_6m = CumulativeSum_Top_Returns_6m - CumulativeSum_Bottom_Returns_6m,
    Cum_Long_Short_9m = CumulativeSum_Top_Returns_9m - CumulativeSum_Bottom_Returns_9m,
    Cum_Long_Short_12m = CumulativeSum_Top_Returns_12m - CumulativeSum_Bottom_Returns_12m
  )

## Visualize cumulative returns of winner portfolios
# This illustrates the persistence of momentum strategies


library(ggplot2)

## Prepare data for Winner graph

winner_data <- data.frame(
  Month = rep(1:nrow(p100), 4),
  Cumulative_Sum = c(
    p100$CumulativeSum_Top_Returns_3m,
    p100$CumulativeSum_Top_Returns_6m,
    p100$CumulativeSum_Top_Returns_9m,
    p100$CumulativeSum_Top_Returns_12m
  ),
  Period = rep(c("3M", "6M", "9M", "12M"), each = nrow(p100))
)

### Winner graph
ggplot(winner_data, aes(x = Month, y = Cumulative_Sum, color = Period)) +
  geom_line(size = 1) +
  labs(
    title = "Cumulative Winner",
    x = "Month",
    y = "Cumulative Sum",
    color = "Period"
  ) +
  theme_minimal()



## Visualize cumulative returns of loser portfolios
# This highlights underperformance of past losers

loser_data <- data.frame(
  Month = rep(1:nrow(p100), 4),
  Cumulative_Sum = c(
    p100$CumulativeSum_Bottom_Returns_3m,
    p100$CumulativeSum_Bottom_Returns_6m,
    p100$CumulativeSum_Bottom_Returns_9m,
    p100$CumulativeSum_Bottom_Returns_12m
  ),
  Period = rep(c("3M", "6M", "9M", "12M"), each = nrow(p100))
)

# Loser graph
ggplot(loser_data, aes(x = Month, y = Cumulative_Sum, color = Period)) +
  geom_line(size = 1) +
  labs(
    title = "Cumulative Loser",
    x = "Month",
    y = "Cumulative Sum",
    color = "Period"
  ) +
  theme_minimal()


## Visualize cumulative returns of long short momentum portfolios
# This represents the net performance of the strategy

long_short_data <- data.frame(
  Month = rep(1:nrow(p100), 4),
  Cumulative_Long_Short = c(
    p100$Cum_Long_Short_3m,
    p100$Cum_Long_Short_6m,
    p100$Cum_Long_Short_9m,
    p100$Cum_Long_Short_12m
  ),
  Period = rep(c("3M", "6M", "9M", "12M"), each = nrow(p100))
)

# Cumulative Long Short graph

ggplot(long_short_data, aes(x = Month, y = Cumulative_Long_Short, color = Period)) +
  geom_line(size = 1) +
  labs(
    title = "Cumulative Long Short",
    x = "Month",
    y = "Cumulative Return (Long - Short)",
    color = "Period"
  ) +
  theme_minimal()


## Evaluate momentum portfolio performance using the
# Fama French three factor model
# This tests whether momentum returns are explained by common risk factors

famafrench <- famafrench %>%
  mutate(
    Long_Short_3m = p100$Long_Short_3m,
    Long_Short_6m = p100$Long_Short_6m,
    Long_Short_9m = p100$Long_Short_9m,
    Long_Short_12m = p100$Long_Short_12m
  )

## Perform regressions

reg_3m <- lm(Long_Short_3m ~ Mkt-RF + SMB + HML, data = famafrench)
reg_6m <- lm(Long_Short_6m ~ Mkt-RF + SMB + HML, data = famafrench)
reg_9m <- lm(Long_Short_9m ~ Mkt-RF + SMB + HML, data = famafrench)
reg_12m <- lm(Long_Short_12m ~ Mkt-RF + SMB + HML, data = famafrench)

## Summarize the regression results
summary(reg_3m)
summary(reg_6m)
summary(reg_9m)
summary(reg_12m)



