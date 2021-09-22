# link to blog post https://ljwoodley.github.io/posts/2021-09-21/post.html

library(tidyverse)
library(lubridate)

subscription_periods <- read_csv("https://raw.githubusercontent.com/dbt-labs/mrr-playbook/master/data/subscription_periods.csv")


# subscription_periods <- subscription_periods %>%
#   filter(customer_id == 1)

customer_months <- subscription_periods %>%
  group_by(customer_id) %>%
  expand(
    current_subscription_date = seq(
      min(start_date),
      max(end_date),
      by = "month"
    )
  )

transformed_subscription_periods <- subscription_periods %>%
  rename(
    subscription_start_date = start_date,
    subscription_end_date = end_date,
    current_monthly_amount = monthly_amount
  ) %>%
  group_by(subscription_id) %>%
  expand(
    subscription_id,
    customer_id,
    subscription_start_date,
    subscription_end_date,
    current_monthly_amount,
    current_subscription_date = seq(
      subscription_start_date,
      subscription_end_date,
      by = "month"
    )
  ) %>%
  group_by(customer_id) %>%
  mutate(
    previous_monthly_amount = lag(current_monthly_amount),
    next_subscription_date = lead(current_subscription_date),
    previous_subscription_date = lag(current_subscription_date),
    change_category = case_when(
      is.na(previous_subscription_date) ~ "new",
      interval(current_subscription_date, next_subscription_date) %/% months(1) > 1 |
        is.na(next_subscription_date) ~  "churn",
      interval(previous_subscription_date, current_subscription_date) %/% months(1) > 1 ~ "reactivation",
      current_subscription_date == subscription_start_date &
        current_monthly_amount < previous_monthly_amount ~ "downgrade",
      current_subscription_date == subscription_start_date &
        current_monthly_amount > previous_monthly_amount ~ "upgrade"
    ),
    current_monthly_amount = if_else(change_category == 'churn', 0, current_monthly_amount)
  ) %>%
  # when there are duplicated dates one of the change categories must be NA.
  # arranging allows us to drop the NA change category
  arrange(customer_id, current_subscription_date, change_category) %>%
  distinct(current_subscription_date, .keep_all = TRUE) %>%
  ungroup() %>%
  select(
    subscription_id,
    customer_id,
    previous_subscription_date,
    current_subscription_date,
    next_subscription_date,
    previous_monthly_amount,
    current_monthly_amount,
    change_category
  )

monthly_recurring_revenue <- customer_months %>%
  left_join(transformed_subscription_periods, by = c("customer_id", "current_subscription_date")) %>%
  group_by(customer_id) %>%
  fill(current_monthly_amount, .direction = "down") %>%
  mutate(mrr_change = coalesce(current_monthly_amount - lag(current_monthly_amount), current_monthly_amount)) %>%
  ungroup() %>%
  select(date_month = current_subscription_date,
         customer_id,
         mrr = current_monthly_amount,
         mrr_change,
         change_category)



