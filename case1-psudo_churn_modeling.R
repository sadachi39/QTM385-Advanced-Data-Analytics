# !diagnostics off
############################################################
## Pseudo Churn CLV Analysis
## Author: Shin Adachi

setwd("/Users/shinsukeadachi/Desktop/QTM385-3/case1")

library(tidyverse)
library(scales)
library(gmodels)
library(lubridate)

############################################################
# Load data.
############################################################

# reading a gzipped file?

# working with a big table - read a few rows in.
temp <- read_csv("./data/brand_acq_tnsactions.csv.gz", n_max=10)
temp

# see if the delims work
temp <- read_delim("./data/brand_acq_tnsactions.csv.gz", delim="\t", n_max=10) 
temp

# read the full thing
df <- read_delim("./data/brand_acq_tnsactions.csv.gz", delim="\t", ) 
df

# is the file too big?
# one way to reduce size is to sample randomly from the dataframe.
# what are the issues with sampling randomly in this case?

# trick for reading a full file if it's too big
# note that this only works on unzipped files...
# skip <- 1000
# max <- 100000
# filepath <- "./data/brand_acq_tnsactions.csv.gz"
# con <- file(filepath, "r")
# i <- 0
# j <- 0
# out <- c()
# while (TRUE) {
#   if (j > max) {
#     break
#   }
#   line <- readLines(con, n = 1)
#   if (i < skip) {
#     next
#   } else {
#     out <- c(out, line)
#     i <- 0
#   }
#   i <- i + 1
#   j <- j + 1
# }
# outdf <- read_delim(out, delim="\t")

# sample by customer
custs <- df %>%
  distinct(custKey)
keep <- custs %>%
  sample_frac(0.10)
df.small <- df %>%
  filter(custKey %in% unlist(keep))
df.small %>%
  write_csv("./data/brand_acq_tnsactions_sample.csv")

dfbak <- df
df <- df.small
rm (con, filepath, i, j, line, max, out, skip, df.small, temp, keep, custs)

############################################################
# Inspect data.
############################################################
# start examining the data
df

# Data size
df %>% 
  dim() %>%
  comma()

# Customers
df %>%
  distinct(custKey)

# Dates
df %>%
  distinct(dtKey)

# time period
df %>%
  summarize(
    mindate=min(dtKey),
    maxdate=max(dtKey)
  )

############################################################
# Create some additional columns.
############################################################
df <- df %>%
  mutate(
    date=ymd(dtKey),
    year=year(date),
    month=month(date),
    week=week(date),
    ymo=ymd(sprintf("%04d-%02d-01", year, month))
  )

############################################################
# Create resampled dataset.
############################################################

resamp <- df %>%
  group_by(
    custKey, week
  ) %>%
  summarize(
    gross = sum(gross),
    num_sku = n()
  ) %>%
  arrange(custKey, week)

# Create an indicator for when each customer entered dataset,
# and the order of the purchase
resamp <- resamp %>%
  group_by(custKey) %>%
  arrange(week) %>%
  mutate(
    purchase_seq = row_number(),
    customer_entry = min(week),
    lifetime_seq = (week - customer_entry) + 1
  )

# Create codes for who has churned
# rule-based method - doesn't show up for 4 weeks
lifetime <- resamp %>%
  distinct(custKey, week) %>%
  group_by(custKey) %>%
  arrange(week) %>%
  summarize(
    # Summary lifetime info.
    firstdate=min(week),
    lastdate=max(week),
    lifetime=as.integer(lastdate-firstdate)+1,
    currentperiod=as.integer(today-lastdate),
    npurchases=n()
  ) %>%
  mutate(
    churned = as.integer(currentperiod > 4)
  )

CrossTable(lifetime$churned)


# Data-based method
today <- max(df$week)
lifetime <- resamp %>%
  distinct(custKey, week, gross) %>%
  group_by(custKey) %>%
  arrange(week) %>%
  mutate(
    lweek=lag(week),
    diff=week-lweek
  ) %>%
  summarize(
    # Summary lifetime info.
    firstdate=min(week),
    lastdate=max(week),
    lifetime=as.integer(lastdate-firstdate)+1,
    currentperiod=as.integer(today-lastdate),
    npurchases=n(),
    
    # Interpurchase info.
    muperiod=mean(diff, na.rm=T),
    muperiod=as.integer(ifelse(is.nan(muperiod), 0, muperiod)),
    sdperiod=sd(diff, na.rm=T),
    sdperiod=ifelse(is.na(sdperiod), 116, sdperiod),
    
    # Monetary info.
    mupurchase = mean(gross, na.rm=T),
    histvalue = sum(gross, na.rm=T)
  )

# Interpurchase heterogeneity
expperiod <- lifetime %>%
  filter(npurchases>1) %>%
  group_by(npurchases) %>%
  summarize(
    mu=mean(muperiod),
    sd=sd(muperiod),
    lwr=mu-1.96*sd,
    lwr=ifelse(lwr<0, 0, lwr),
    upr=mu+1.96*sd
  ) %>%
  arrange(npurchases)

# Code the churns using actual data
lifetime <- lifetime %>%
  left_join(expperiod) %>%
  mutate(
    churned = as.integer(currentperiod > (muperiod+1.96*sdperiod))
  )

CrossTable(lifetime$churned)

############################################################
# Examine some sample characteristics
############################################################

# Plot to see interpurchase heterogeneity
expperiod %>%
  ggplot(aes(x=npurchases, y=mu/4, ymin=lwr/4, ymax=upr/4)) +
  geom_errorbar(width=0) +
  geom_line() +
  scale_y_continuous(breaks=pretty_breaks()) +
  geom_abline(intercept = 1.8, slope=0, lty=2, col="blue") +
  labs(
    x="Number of Months with Purchase",
    y="Mean Interpurchase Period (Months)"
  )

# See number of customers by interpurchase period
lifetime %>%
  filter(npurchases>1) %>%
  ggplot(aes(x=muperiod/4)) +
  geom_histogram() +
  scale_y_continuous(labels = comma, breaks=pretty_breaks(10)) +
  labs(
    x="Interpurchase Period (Months)",
    y="Number of Customers"
  )

# Lifetime distributions
lifetime %>%
  filter(npurchases>1) %>%
  ggplot(aes(x=lifetime/4)) +
  geom_density(fill="black") +
  scale_y_continuous(labels = percent) +
  labs(
    x="Observed Lifetime (Months)",
    y="Percent of Customers"
  )


############################################################
# Link the churns back to the week-level dataset
############################################################

resamp <- resamp %>%
  left_join(
    lifetime %>%
      select(custKey, week=lastdate, churned)
  ) %>%
  mutate(
    churned = ifelse(is.na(churned), 0, churned)
  )

############################################################
# Run churn prediction
############################################################

# We still need to make sure we have RFM feats
# we have F = purchase_seq
# we have M = gross
# Need R
resamp <- resamp %>%
  group_by(custKey) %>%
  arrange(week) %>%
  mutate(
    
    # recency
    timelag = week - lag(week),
    timelag = ifelse(is.na(timelag), 0, timelag)
    
  )

# Set up our train test split
train.rat <- 0.70
N <- nrow(resamp)
train <- sample(1:N, size = ceiling(train.rat*N))
test <- (1:N)[-train]

df.train <- resamp[train,]
df.test <- resamp[test,]

# Our very basic model
glm.churn <- glm(churned ~ timelag + lifetime_seq + gross, 
                 data = df.train, family = binomial(link="logit"))
summary(glm.churn)

# Make some churn predictions on our test set
df.test <- df.test %>%
  ungroup() %>%
  mutate(
    yhat = predict(glm.churn, df.test, type = "response")
  )
hist(df.test$yhat, breaks = 30)

# Threshold
df.test <- df.test %>%
  mutate(
    churn_pred = as.integer(yhat > 0.023)
  )

# Check our confusion matrix
with(df.test, CrossTable(churned, churn_pred))
cm <- with(df.test, table(churned, churn_pred))
acc <- (cm[1,1] + cm[2,2]) / sum(cm)
pre <- (cm[2,2]) / (cm[2,1] + cm[2,2])
rec <- (cm[1,1]) / (cm[1,1] + cm[1,2])
f1 <- 2 * (pre * rec) / (pre + rec)

# How should we interpret these?
acc
f1

############################################################
# Estimate customer lifetime values
############################################################

# Make some churn predictions
resamp <- resamp %>%
  ungroup() %>%
  mutate(
    yhat = predict(glm.churn, resamp, type = "response")
  )
hist(resamp$yhat)

# Threshold
# Probability of alive = projection factor
resamp <- resamp %>%
  mutate(
    churn_pred = as.integer(yhat > 0.023),
    alive_pr = 1 - yhat,
    alive_pr_rescale = rescale(alive_pr, c(0, 1))
  )
hist(resamp$alive_pr)

# We only need the final probs
final_probs <- resamp %>%
  group_by(custKey) %>%
  arrange(week) %>%
  filter(row_number() == n())
hist(final_probs$alive_pr)
hist(final_probs$alive_pr_rescale)

# We map and then aggregate
lifetime <- lifetime %>%
  left_join(
    final_probs %>%
      select(-gross)
    )
  
# Project over estimated remaining lifetime per customer
clv <- lifetime %>%
  select(custKey, mupurchase, histvalue, lifetime, alive_pr_rescale) %>%
  ungroup() %>%
  mutate(
    est_lifetime = ceiling(mean(lifetime, na.rm=T)),
    remain_life = ifelse(
      lifetime <= est_lifetime, 
      est_lifetime - lifetime,
      lifetime + 12
    )
  )

# Now, do the geometric summations!
clv

pr_discount <- 0.05

# noteice this is not a vectorized formula....
calc_future_clv <- function(money, remaining_life, pr_churn, pr_discount) {
  
  # No money accumulated yet...
  base_clv <- 0
  
  # for each future time period, calculate the marginal addition to CLV
  for (t in 1:remaining_life) {
    
    discount_factor <- (( 1 + pr_discount )^t * (1 + pr_churn)^t)
    
    period_value <- money / discount_factor
    
    base_clv <- base_clv + period_value
    
  }
  
  base_clv
  
}

# test with hypothetical customer
calc_future_clv(100, 12, 1-.888, pr_discount)

# iterate thru the customer base
clv_estimates <- data.frame()

for (i in 1:nrow(clv)) {
  
  cust_i <- clv[i,]
  
  
  m <- cust_i$mupurchase
  rl <- cust_i$remain_life
  prc <- 1 - cust_i$alive_pr_rescale
  
  clv_hat_i <- calc_future_clv(m, rl, prc, pr_discount)
  
  clv_estimates <- rbind(
    clv_estimates,
    data.frame(
      i = i,
      clv_hat = clv_hat_i
    )
  )
  
}

clv_estimates <- clv_estimates %>%
  as_tibble()

clv_estimates

summary(clv_estimates)

clv <- clv %>%
  mutate(
    clv_hat = unlist(clv_estimates$clv_hat)
  )

lifetime
