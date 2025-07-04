library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(hms)


### chinese dataset
df <- read.csv("raw_data/transactionlist2024_clean.csv", encoding="UTF-8", stringsAsFactors = FALSE)
View(df)


# df$`??????` <- as.Date(df$??????)
# 
# summary(df)
# 
# ggplot(df, aes(x = reorder(????????????, -??????, FUN = sum), y = ??????)) +
#   geom_bar(stat = "summary", fun = "sum", fill = "blue", alpha = 0.7) +
#   labs(title = "Revenue by Ticket Channel", x = "Ticket Channel", y = "Total Revenue") +
#   theme_minimal()
# 
# 
# # aggregate demands for food
# # transactions with columns Date, Product, Quantity
# daily_sales <- df %>%
#   group_by(????????????, ??????) %>%
#   summarise(Quantity = n(), .groups = "drop")
# daily_sales



### translated english dataset

df2 <- read.csv("translated_transaction_order_data.csv", stringsAsFactors = FALSE)
View(df2)

df2$`Opening.time` <- as_hms(df2$Opening.time)
df2$`Checkout.Time` <- as_hms(df2$Checkout.Time)
df2$`Order.Time` <- as_hms(df2$Order.Time)


summary(df2)


## most popular ticketing channels
ggplot(df2, aes(x = reorder(channel, -Price, FUN = sum), y = Price)) +
  geom_bar(stat = "summary", fun = "sum", fill = "blue", alpha = 0.7) +
  labs(title = "Revenue by Ticket Channel", x = "Ticket Channel", y = "Total Revenue") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))  # Wrap text instead of rotating  

## most popular meal for the restaurant 
df2 %>%
  group_by(Meal.Type, Branch) %>%
  summarise(Total.Orders = n()) %>%
  ggplot(aes(x = reorder(Meal.Type, -Total.Orders), y = Total.Orders, fill = Branch)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Orders by Meal Type and Branch", x = "Meal Type", y = "Number of Orders") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() 

# Revenue by branch
df2 %>%
  group_by(Branch) %>%
  summarise(Total_Revenue = sum(`Total.Price`, na.rm = TRUE)) %>%
  arrange(desc(Total_Revenue))

#### Revenue by Branch by Month
### to reload with new dataset

df2 %>%
  mutate(Month = floor_date(as.Date(`Purchase.Date`), "month")) %>%
  group_by(Branch, Month) %>%
  summarise(Monthly_Revenue = sum(`Total.Price`, na.rm = TRUE)) %>%
  ggplot(aes(x = Month, y = Monthly_Revenue, color = Branch)) +
  geom_line() +
  labs(title = "Monthly Revenue by Branch",
       x = "Month", y = "Revenue") +
  theme_minimal()

df2_broadway <- 
  df2 %>% filter(Branch == 'Broadway')
df2_caravel <- 
  df2 %>% filter(Branch == 'Caravel')

# by day
df2_broadway%>%
  mutate(`Purchase.Date` = as.Date(`Purchase.Date`)) %>%
  group_by(Branch, `Purchase.Date`) %>%
  summarise(Daily_Revenue = sum(`Total.Price`, na.rm = TRUE)) %>%
  ggplot(aes(x = `Purchase.Date`, y = Daily_Revenue, color = Branch)) +
  geom_line() +
  labs(title = "Daily Revenue in Broadway",
       x = "Date", y = "Revenue") +
  theme_minimal()

df2_caravel%>%
  mutate(`Purchase.Date` = as.Date(`Purchase.Date`)) %>%
  group_by(Branch, `Purchase.Date`) %>%
  summarise(Daily_Revenue = sum(`Total.Price`, na.rm = TRUE)) %>%
  ggplot(aes(x = `Purchase.Date`, y = Daily_Revenue, color = Branch)) +
  geom_line() +
  labs(title = "Daily Revenue in Caravel",
       x = "Date", y = "Revenue") +
  theme_minimal()

df2 %>%
  mutate(`Purchase.Date` = as.Date(`Purchase.Date`)) %>%
  group_by(Branch, `Purchase.Date`) %>%
  summarise(Daily_Revenue = sum(`Total.Price`, na.rm = TRUE)) %>%
  ggplot(aes(x = `Purchase.Date`, y = Daily_Revenue, color = Branch)) +
  geom_line() +
  labs(title = "Daily Revenue in Caravel",
       x = "Date", y = "Revenue") +
  theme_minimal()


##### attempt to see relationship between branch vs meal type
df2 %>%
  group_by(Branch, `Meal.Type`) %>%
  summarise(Num_Orders = n(), .groups = "drop") %>%
  ggplot(aes(x = `Meal.Type`, y = Branch, fill = Num_Orders)) +
  geom_tile() +
  scale_fill_gradient(low = "lightyellow", high = "steelblue") +
  labs(title = "Heatmap of Orders by Branch and Meal Type",
       x = "Meal Type", y = "Branch", fill = "Num Orders") +
  theme_minimal()


#scatterplot
df2 %>%
  group_by(Branch, `Meal Type`) %>%
  summarise(Num_Orders = n(), .groups = "drop") %>%
  ggplot(aes(x = `Meal Type`, y = Branch)) +
  geom_point(aes(size = Num_Orders, color = Num_Orders), alpha = 0.7) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Scatter Plot of Orders by Branch and Meal Type",
       x = "Meal Type", y = "Branch", size = "No. of Orders") +
  theme_minimal()


df2 %>%
  group_by(Branch, `Meal.Type`) %>%
  summarise(Num_Orders = n(), .groups = "drop") %>%
  ggplot(aes(x = `Meal.Type`, y = Branch)) +
  geom_point(aes(size = Num_Orders, color = Num_Orders), alpha = 0.7) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Scatter Plot of Orders by Branch and Meal Type",
       x = "Meal Type", y = "Branch", size = "No. of Orders") +
  theme_minimal()



#distribution of orders by branch
df2 %>%
  mutate(Order.Hour = hour(`Order.Time`)) %>%
  ggplot(aes(x = Order.Hour, fill = Branch)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5) +
  labs(title = "Distribution of Order Times by Branch", x = "Hour", y = "Number of Orders") +
  theme_minimal()

#jitterplot
df2 %>%
  mutate(Order.Hour = hour(`Order.Time`)) %>%
  ggplot(aes(x = Order.Hour, y = `Meal.Type`)) +
  geom_jitter(width = 0.3, height = 0.3, alpha = 0.5, color = "tomato") +
  labs(title = "Order Hour vs Meal Type", x = "Hour of Day", y = "Meal Type") +
  theme_minimal()


# Create contingency table
branch_meal_table <- table(df2$Branch, df2$`Meal.Type`)

# Run chi-squared test
chisq.test(branch_meal_table)




df2 %>%
  mutate(Order.Hour = hour(`Order.Time`), 
         `Meal.Type` = factor(`Meal.Type`, levels = c("Breakfast", "Lunch", "Dinner", "Other"))) %>%
  count(Branch, `Meal.Type`, Order.Hour) %>%
  ggplot(aes(x = Order.Hour, y = `Meal.Type`, fill = n)) +
  geom_tile(color = "white") +  # optional borders between tiles
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Heatmap: Orders by Hour and Meal Type by Branch",
       x = "Hour of Day", y = "Meal Type", fill = "Num Orders") +
  facet_wrap(~ Branch, ncol = 2) +
  theme_minimal()




df2 %>%
  mutate(
    Purchase.Date = as.Date(`Purchase.Date`),
    DayOfWeek = weekdays(Purchase.Date),
    IsWeekend = DayOfWeek %in% c("Saturday", "Sunday"),
    Meal.Type.Clean = case_when(
      `Meal.Type` == "N/a" ~ "Others",
      TRUE ~ as.character(`Meal.Type`)
    )
  ) %>%
  count(DayOfWeek, Meal.Type.Clean, IsWeekend) %>%
  ggplot(aes(x = DayOfWeek, y = n, fill = Meal.Type.Clean)) +
  geom_col(position = "dodge") +
  labs(
    title = "Orders by Day of Week and Meal Type",
    x = "Day of Week",
    y = "Number of Orders",
    fill = "Meal Type"
  ) +
  theme_minimal()



#logistic regression to see if lunch/dinner meal times affect weekends sales

df2 %>%
  mutate(
    Purchase.Date = as.Date(`Purchase.Date`),
    IsLunch = ifelse(`Meal.Type` == "Lunch", 1, 0),
    IsWeekend = weekdays(Purchase.Date) %in% c("Saturday", "Sunday")
  ) %>%
  group_by(Purchase.Date, IsWeekend) %>%
  summarise(LunchCount = sum(IsLunch), .groups = "drop") %>%
  lm(LunchCount ~ IsWeekend, data = .) %>%
  summary()


dinner_model <- df2 %>%
  mutate(
    IsDinner = ifelse(`Meal.Type` == "Dinner", 1, 0),
    IsWeekend = weekdays(as.Date(`Purchase.Date`)) %in% c("Saturday", "Sunday")
  ) %>%
  glm(IsDinner ~ IsWeekend, family = binomial(), data = .) 
# %>%  summary()

# checking with 95% confidence of the model
exp(cbind(Estimate = coef(dinner_model), confint(dinner_model)))

dinner_model %>% summary()


#logistic regression of lunch/dinner mealtime for weekday sales

df2 %>%
  mutate(
    Purchase.Date = as.Date(`Purchase.Date`),
    IsLunch = ifelse(`Meal.Type` == "Lunch", 1, 0),
    IsWeekday = weekdays(Purchase.Date) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  ) %>%
  glm(IsLunch ~ IsWeekday, data = .) %>%
  summary()

df2 %>%
  mutate(
    Purchase.Date = as.Date(`Purchase.Date`),
    IsDinner = ifelse(`Meal.Type` == "Dinner", 1, 0),
    IsWeekday = weekdays(Purchase.Date) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  ) %>%
  glm(IsDinner ~ IsWeekday, data = .) %>%
  summary()


### comparing all CI on dinner_model
# Base estimate
est <- coef(dinner_model)

# Confidence intervals at different levels
ci_90 <- confint.default(dinner_model, level = 0.90)
ci_95 <- confint.default(dinner_model, level = 0.95)
ci_99 <- confint.default(dinner_model, level = 0.99)

# Combine and exponentiate to get odds ratios
comparison <- list(
  "90%" = exp(cbind(Estimate = est, ci_90)),
  "95%" = exp(cbind(Estimate = est, ci_95)),
  "99%" = exp(cbind(Estimate = est, ci_99))
)

# Print result
comparison


#verify for weekdays


#linear regression of number of dinner orders during the weekends

df2 %>%
  mutate(
    Purchase.Date = as.Date(`Purchase.Date`),
    IsDinner = ifelse(`Meal.Type` == "Dinner", 1, 0),
    IsWeekend = weekdays(Purchase.Date) %in% c("Saturday", "Sunday")
  ) %>%
  group_by(Purchase.Date, IsWeekend) %>%
  summarise(DinnerCount = sum(IsDinner), .groups = "drop") %>%
  lm(DinnerCount ~ IsWeekend, data = .) %>%
  summary()



