## Case3 Digital Performance Management
## Author: Shin Adachi

library(tidyverse)

df <- read_csv("/Users/shinsukeadachi/Desktop/QTM385-3/case3/milestone-3/ecomm_rev_2020_omnipotent.csv")

lm1 <- lm(CART_TOTAL_Y1 ~ ., df %>% select(-CUSTID, -CART_TOTAL_Y0, -CART_TOTAL, -ASSIGNED_DISPLAY_AD, -ATTRIB_DISPLAY_AD))
summary(lm1)

lm2 <- lm(CART_TOTAL_Y0 ~ ., df %>% select(-CUSTID, -CART_TOTAL_Y1, -CART_TOTAL, -ASSIGNED_DISPLAY_AD, -ATTRIB_DISPLAY_AD))
summary(lm2)

df <- df %>%
  mutate(
    CART_EFFECT = CART_TOTAL_Y1 - CART_TOTAL_Y0
  )

lm3 <- lm(CART_TOTAL ~ PREVIOUS_CHECKOUTS + PAGE_VIEWS + ESTIMATED_INCOME_DECILE + PRODUCT_VIEWS, data=df)
summary(lm3)

df$yhat <- predict(lm3, df)

df <- df %>%
  mutate(
    prob_ntile = ntile(yhat, 5)
  )

df %>%
  select(
    CUSTID,
    CART_TOTAL,
    yhat,
    prob_ntile, 
    ATTRIB_DISPLAY_AD
  )

df %>%
  group_by(
    prob_ntile
  ) %>%
  summarize(
    mu=mean(CART_TOTAL)
  )

df <- df %>%
  mutate(
    NEW_TARGET = as.integer(prob_ntile == 5)
  )

lm5 <- lm(CART_TOTAL ~ NEW_TARGET - 1, data = df)
summary(lm5)

df %>%
  group_by(
    prob_ntile
  ) %>%
  summarize(
    mu=sum(ATTRIB_DISPLAY_AD),
    n=n()
  ) %>%
  ungroup() %>%
  mutate(
    pct=mu/sum(mu),
    orig_spend = pct * 10000
  )


new_roi <- (575*62.563 - 3329)/3329
new_roi * 100
