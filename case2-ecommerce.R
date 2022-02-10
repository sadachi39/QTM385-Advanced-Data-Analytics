# CASE STUDY 2: PFIZER
# Author: Shin Adachi

library(tidyverse)
library(scales)
library(gmodels)
library(lubridate)

df <- read_delim("/Users/shinsukeadachi/Desktop/QTM385-3/case2/vaccines_tnsactions.csv", delim="\t")

# Look for customers that have lots of transactions.
df %>%
  group_by(customer_id, brand) %>%
  summarize(n=n()) %>%
  arrange(-n)

# Take a test case out for one customer.
test_case <- df %>%
  filter(customer_id == "148b20897005e9c384a9f4322d61b0c2", brand=="CEFASIBAN") 

# Add the resampling column to the data frame.
test_case <- test_case %>%
  mutate(
    month = as.Date(sprintf("%04d-%02d-01", year(date), month(date)))
  ) %>%
  group_by(
    customer_id, month, customer_type, customer_state, brand
  ) %>%
  summarize(
    num_trans = n(),
    doses = sum(doses),
    gross = sum(gross)
  )

# PLot the resampled purchases over time.
test_case %>%
  ggplot(aes(x=month, y=doses)) +
  geom_line() +
  geom_point() +
  geom_smooth(span=0.3)


# Create regression model to predict how much 
#  the right amount to purchase
test_case <- test_case %>%
  left_join(
    test_case %>%
      ungroup() %>%
      distinct(month) %>%
      arrange(month) %>%
      rowid_to_column() %>%
      rename(t = rowid)
  )

lm1 <- lm(doses ~ t, data=test_case)
summary(lm1)

# Do we need to look at the total 3-month stock?
test_case <- test_case %>%
  ungroup() %>%
  arrange(
    month
  ) %>%
  mutate(
    doseslag1 = lag(doses),
    doseslag2 = lag(doses, 2),
    doses3mo = doses + doseslag1 + doseslag2
  )

test_case %>%
  ggplot(aes(x=month, y=doses3mo)) +
  geom_line() +
  geom_point() +
  geom_smooth(span=0.5)

test_case


# Make predictions over time for one customer.
oos <- expand.grid(
  t=51:55
)
yhat <- predict(lm1, oos)
yhat

# Quick plot to show additional predicted purchases.
c(test_case$doses, yhat) %>%
  plot(type="b")

#######################################
# RESAMPLE DATA FOR MULTIPLE CUSTOMERS
#######################################
df.rs <- df %>%
  mutate(
    month = as.Date(sprintf("%04d-%02d-01", year(date), month(date)))
  ) %>%
  group_by(
    customer_id, month, customer_type, customer_state, brand
  ) %>%
  summarize(
    num_trans = n(),
    doses = sum(doses),
    gross = sum(gross)
  )

# Create the time-based resampler.
df.rs <- df.rs %>%
  left_join(
    df.rs %>%
      ungroup() %>%
      distinct(month) %>%
      arrange(month) %>%
      rowid_to_column() %>%
      rename(t = rowid)
  )

# Make predictions across all brands, for the "average" customer.
lm2 <- lm(doses ~ brand * t, data=df.rs)
summary(lm2)

oos <- expand.grid(
  t=51:55,
  brand=unique(df.rs$brand)
)

yhat <- predict(lm1, oos)



##################################################
# RESAMPLE TO CROSS-SECTIONAL UNIT OF ANALYSIS
##################################################

# aggregate customers into cross-sectional u of a
df.cs <- df.rs %>%
  group_by(customer_id, brand, customer_type, customer_state) %>%
  arrange(t) %>%
  summarize(
    num_periods = n(),
    num_trans = sum(num_trans),
    mu_doses = mean(doses, na.rm=T),
    total_doses = sum(doses),
    last_t = max(t),
    first_t = min(t),
    last_doses = last(doses)
  )

# Create a model that predicts most recent purchase amount.
lm3 <- lm(log(1+last_doses) ~ num_periods + num_trans + mu_doses + last_t + first_t + brand + customer_type,
          data=df.cs)
summary(lm3)


