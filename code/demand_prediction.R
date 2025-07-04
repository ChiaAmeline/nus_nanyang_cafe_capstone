# install.packages('feasts')

library(readxl)
library(fable)
library(tsibble)
library(feasts)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(hms)
library(gridExtra)
library(forcats)


### orignial translated dataset
working_directory <- getwd()
absolute_file_path <- paste(working_directory, "/raw_data/translated_transaction_dataset_2024.xlsx", sep = "")
df <- read_xlsx(path = absolute_file_path)
# summary(df)

# converting datatype of date
transactions <- df 

# summary(transactions)
# View(transactions)

# transform data type
transactions <- transactions %>%
  mutate(`Purchase Date` = as.Date(`Purchase Date`),
         `Opening time` = as_hms(`Opening time`),
         `Checkout Time` = as_hms(`Checkout Time`),
         `Order Time` = as_hms(`Order Time`)
         )


# group transactions by Dish Name and Date
daily_demand <- transactions %>%
  group_by(
    Dish = `Dishes name`,
    Date = `Purchase Date`
  ) %>%
  summarise(Quantity = n(), .groups = "drop")

demand_ts <- daily_demand %>%
  as_tsibble(index = Date, key = Dish) %>%
  fill_gaps(Quantity = 0)  # fill missing days with 0 sales

# 0. Filter out dishes with fewer than 2 occurrences
valid_dishes <- demand_ts_filtered %>%
  count(Dish) %>%
  filter(n >= 2) %>%
  pull(Dish)

demand_ts_filtered <- demand_ts_filtered %>%
  filter(Dish %in% valid_dishes)

# 1. Fit both ARIMA and ETS models per dish, with weekly seasonality
model_fit <- demand_ts_filtered %>%
  model(
    ARIMA = ARIMA(Quantity ~ trend() + season(period = "week")),
    ETS = ETS(Quantity)
  )

# 2. Evaluate model accuracy (training set)
model_accuracy <- accuracy(model_fit)

# 3. Select the best model per dish (lowest MASE)
best_models <- model_accuracy %>%
  group_by(Dish) %>%
  slice_min(MASE) %>%
  ungroup() %>%
  select(Dish, .model)

# # 4. Forecast 60 days ahead for both models
# all_forecasts <- model_fit %>%
#   forecast(h = "60 days")
# 
# # 5. Join forecasts with best model info
# best_forecasts <- all_forecasts %>%
#   as_tibble() %>%
#   inner_join(best_models, by = c("Dish", ".model")) %>%
#   select(Dish, Date, Forecast = .mean) %>%
#   arrange(Dish, Date)
# 
# View(best_forecasts)

# 4. Fit current actuals and forecast 60 days ahead for best model
# Forecast 60 days ahead
forecast_values <- model_fit %>%
  forecast(h = "60 days") %>%
  as_tibble() %>%
  inner_join(best_models, by = c("Dish", ".model")) %>%
  select(Dish, Date, Forecast = .mean)

# Get in-sample fitted values
fitted_values <- model_fit %>%
  augment() %>%
  as_tibble() %>%
  inner_join(best_models, by = c("Dish", ".model")) %>%
  select(Dish, Date, Quantity, Fitted = .fitted)

# Combine both into full timeline
full_demand_view <- full_join(fitted_values, forecast_values, by = c("Dish", "Date"))

# Plot example (optional): pick one dish
dish_to_plot <- "Hainanese Chicken Rice"

ggplot(full_demand_view %>% filter(Dish == dish_to_plot), aes(x = Date)) +
  geom_line(aes(y = Quantity, color = "Actual")) +
  geom_line(aes(y = Fitted, color = "Fitted")) +
  geom_line(aes(y = Forecast, color = "Forecast"), linetype = "dashed") +
  labs(
    title = paste("Actual vs Fitted vs Forecast:", dish_to_plot),
    y = "Quantity", color = "Legend"
  ) +
  scale_color_manual(values = c("Actual" = "black", "Fitted" = "blue", "Forecast" = "red")) +
  theme_minimal()

summary_metrics <- model_accuracy %>%
  select(
    Dish,
    .model,
    MAE,
    RMSE,
    MASE,
    ACF1
  ) %>%
  arrange(Dish, .model)

# View or export as needed
View(summary_metrics)

# library(readr)
# write_csv(summary_metrics, "summary_metrics.csv")

# check model score

# Step 1: Average metrics per dish
avg_metrics <- summary_metrics %>%
  group_by(Dish) %>%
  summarise(
    MASE = mean(MASE, na.rm = TRUE),
    MAE = mean(MAE, na.rm = TRUE),
    RMSE = mean(RMSE, na.rm = TRUE)
  )

# Step 2: Get top 10 best and worst dishes for each metric
get_top_bottom <- function(df, metric) {
  df %>%
    arrange(.data[[metric]]) %>%
    slice(c(1:10, (n() - 9):n())) %>%
    mutate(Performance = case_when(
      .data[[metric]] <= 0.9 ~ "Good",
      .data[[metric]] <= 1.1 ~ "Acceptable",
      TRUE ~ "Poor"
    ))
}

# Step 3: Plot generator
plot_metric <- function(data, metric) {
  vline_data <- if (metric == "MASE") data.frame(x = 1) else NULL
  
  p <- ggplot(data, aes(x = .data[[metric]], y = fct_reorder(Dish, .data[[metric]]), fill = Performance)) +
    geom_col(show.legend = TRUE) +
    labs(
      title = paste("Top and Bottom Dishes by", metric),
      x = metric,
      y = "Dish"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(size = 12, face = "bold"))
  
  # Add vertical line for MASE = 1 only
  if (!is.null(vline_data)) {
    p <- p + geom_vline(data = vline_data, aes(xintercept = x),
                        linetype = "dashed", color = "red", inherit.aes = FALSE)
  }
  
  return(p)
}

# Step 4: Generate each plot
mase_plot <- plot_metric(get_top_bottom(avg_metrics, "MASE"), "MASE")
mae_plot  <- plot_metric(get_top_bottom(avg_metrics, "MAE"), "MAE")
rmse_plot <- plot_metric(get_top_bottom(avg_metrics, "RMSE"), "RMSE")

# Step 5: Arrange plots in 1 row, 3 columns using gridExtra
gridExtra::grid.arrange(mase_plot, mae_plot, rmse_plot, ncol = 3)


#############################

library(ggplot2)
library(dplyr)
library(forcats)
library(gridExtra)
library(stringr)

# Step 1: Average metrics per dish
avg_metrics <- summary_metrics %>%
  group_by(Dish) %>%
  summarise(
    MASE = mean(MASE, na.rm = TRUE),
    MAE = mean(MAE, na.rm = TRUE),
    RMSE = mean(RMSE, na.rm = TRUE)
  ) %>%
  filter(!str_detect(Dish, regex("service fee|colorful pick|takeaway|delivery fee|order", ignore_case = TRUE))) %>%
  mutate(
    Performance = factor(case_when(
      MASE <= 0.9 ~ "Good",
      MASE <= 1.1 ~ "Acceptable",
      TRUE ~ "Poor"
    ), levels = c("Good", "Acceptable", "Poor"))
  )

# Step 2: Dot plot (lollipop) generator
plot_metric <- function(data, metric) {
  vline_data <- if (metric == "MASE") data.frame(x = 1) else NULL
  
  p <- ggplot(data, aes(x = .data[[metric]], y = fct_reorder(Dish, .data[[metric]]), color = Performance)) +
    geom_segment(aes(x = 0, xend = .data[[metric]], yend = Dish), color = "grey80") +
    geom_point(size = 3) +
    labs(
      title = paste("All Dishes by", metric),
      x = metric,
      y = "Dish"
    ) +
    scale_color_manual(values = c("Good" = "forestgreen", "Acceptable" = "orange", "Poor" = "red")) +
    theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(size = 12, face = "bold"))
  
  if (!is.null(vline_data)) {
    p <- p + geom_vline(data = vline_data, aes(xintercept = x),
                        linetype = "dashed", color = "black", inherit.aes = FALSE)
  }
  
  return(p)
}

# Step 3: Generate each plot
mase_plot <- plot_metric(avg_metrics, "MASE")
mae_plot  <- plot_metric(avg_metrics, "MAE")
rmse_plot <- plot_metric(avg_metrics, "RMSE")

# Step 4: Arrange plots in 1 row, 3 columns
gridExtra::grid.arrange(mase_plot, mae_plot, rmse_plot, ncol = 3)

# 
# ### heatmap
# 
# library(tidyr)
# library(ggplot2)
# library(dplyr)
# 
# # Step 1: Prepare data
# avg_metrics_long <- avg_metrics %>%
#   pivot_longer(cols = c(MASE, MAE, RMSE), names_to = "Metric", values_to = "Score")
# 
# # Step 2: Plot heatmap
# ggplot(avg_metrics_long, aes(x = Metric, y = fct_reorder(Dish, Score), fill = Score)) +
#   geom_tile(color = "white") +
#   scale_fill_gradient(low = "green", high = "red", name = "Error") +
#   labs(
#     title = "Forecast Accuracy Heatmap by Dish",
#     x = "Metric",
#     y = "Dish"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.y = element_text(size = 8),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     plot.title = element_text(size = 14, face = "bold")
#   )




# ------------------------------------------------------------------
# checking for forcasted vs actuals
# ------------------------------------------------------------------
# # For forecasted data
# forecast_janfeb <- best_forecasts %>%
#   select(Dish, Date, Forecast) %>%
#   mutate(Date = as.Date(Date)) %>%
#   filter(month(Date) %in% c(1, 2))
# 
# # For actual data
# actual_janfeb <- daily_demand %>%
#   select(Dish, Date, Quantity) %>%
#   mutate(Date = as.Date(Date)) %>%
#   filter(month(Date) %in% c(1, 2))

# Add a new column for Month-Day only
forecast_janfeb <- best_forecasts %>%
  mutate(Date = as.Date(Date),
         MonthDay = format(Date, "%m-%d")) %>%
  filter(month(Date) %in% c(1, 2)) %>%
  select(Dish, MonthDay, Forecast)

actual_janfeb <- daily_demand %>%
  mutate(Date = as.Date(Date),
         MonthDay = format(Date, "%m-%d")) %>%
  filter(month(Date) %in% c(1, 2)) %>%
  select(Dish, MonthDay, Quantity)

# Step 2: Join actual and forecast data
comparison_df <- inner_join(forecast_janfeb, actual_janfeb, by = c("Dish", "MonthDay")) %>%
  mutate(Difference = Forecast - Quantity, 
         APE = ifelse(Quantity == 0, NA, abs(Forecast - Quantity) / Quantity * 100)
  )

# Step 3: View comparison for one dish (optional)
selected_dish <- "Chicken Rice and Bak Kut Teh Set"
# 
# comparison_df %>%
#   filter(Dish == selected_dish) %>%
#   arrange(MonthDay)

comparison_df %>%
  #   filter(Dish == selected_dish) %>%
    arrange(MonthDay)
  
# plot demand
library(ggplot2)

forecast_60d %>%
  filter(Dish == "Beef Rendang") %>%
  ggplot(aes(x = Date, y = .mean)) +
  geom_line(color = "black") +
  labs(
    title = "60-Day Demand Forecast for Beef Laksa",
    x = "Date",
    y = "Forecasted Quantity"
  ) +
  theme_minimal()

components(model_fit) %>%
  autoplot()





# -----------------------
# Start analysis for inventory management 
# -----------------------

# Load recipe dataset
recipe_path <- paste(working_directory, "/raw_data/all_recipes.xlsx", sep = "")
recipe_df <- read_xlsx(path = recipe_path)

# Join forecast with recipe per serving
#   One day's demand for one ingredient, based on dish forecast * amount per serving

ingredient_demand_df <- forecast_values %>%
  rename(dish_name_english = Dish) %>%
  left_join(recipe_df, by = "dish_name_english", relationship = "many-to-many") %>%
  mutate(
    Total_Ingredient_Required = Forecast * amount_per_serving
  )

# Aggregate total ingredient demand needed per day
# this shows all the ingredients needed to prepare for the forecasted day
daily_ingredient_needs <- ingredient_demand_df %>%
  group_by(Date, ingredient_name_english) %>%
  summarise(
    Total_Required_grams = sum(Total_Ingredient_Required, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Date, ingredient_name_english)

dish_level_ingredient_plan <- forecast_values %>%
  rename(dish_name_english = Dish) %>%
  left_join(recipe_df, by = "dish_name_english", relationship = "many-to-many") %>%
  mutate(
    Total_Required = Forecast * amount_per_serving
  ) %>%
  select(Date, Dish = dish_name_english, Ingredient = ingredient_name_english,
         Forecast, amount_per_serving, Total_Required) %>%
  arrange(Date, Dish, Ingredient)


# more intuitive format
# Dish-Centric Ingredient Checklist
#   group by date > dish > ingredient
prep_checklist <- forecast_values %>%
  rename(dish_name_english = Dish) %>%
  left_join(recipe_df, by = "dish_name_english", relationship = "many-to-many") %>%
  mutate(
    Total_Required = Forecast * amount_per_serving
  ) %>%
  select(
    Date,
    Dish = dish_name_english,
    Ingredient = ingredient_name_english,
    Forecasted_Dishes = Forecast,
    Qty_Per_Serving = amount_per_serving,
    Total_Required
  ) %>%
  arrange(Date, Dish, Ingredient)

library(readr)
write_csv(prep_checklist, "prep_checklist.csv")


# 
# library(plotly)
# 
# plot_data <- prep_checklist %>%
#   filter(Date == as.Date("2025-01-01"))  # Change date as needed
# 
# plot_ly(plot_data,
#         x = ~Total_Required,
#         y = ~Ingredient,
#         color = ~Dish,
#         type = 'bar',
#         orientation = 'h') %>%
#   layout(title = "Ingredient Requirements by Dish",
#          xaxis = list(title = "Total Quantity Needed"),
#          yaxis = list(title = "Ingredient"),
#          barmode = "stack")
# 

library(DT)

datatable(prep_checklist,
          filter = "top",
          options = list(pageLength = 10, autoWidth = TRUE),
          rownames = FALSE)

# all data in the datatable
datatable(prep_checklist,
          filter = "top",
          extensions = 'Buttons',
          options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel'),
            pageLength = 20,
            scrollX = TRUE
          ),
          rownames = FALSE)

# by weekly dish-ingredient
prep_checklist_weekly <- prep_checklist %>%
  mutate(Week = lubridate::floor_date(Date, unit = "week")) %>%
  group_by(Week, Dish, Ingredient) %>%
  summarise(Total_Required = sum(Total_Required, na.rm = TRUE), .groups = "drop")

# write_csv(prep_checklist_weekly, "prep_checklist_weekly.csv")

# by monthly dish-ingredient
prep_checklist_monthly <- prep_checklist %>%
  mutate(Month = lubridate::floor_date(Date, unit = "month")) %>%
  group_by(Month, Dish, Ingredient) %>%
  summarise(Total_Required = sum(Total_Required, na.rm = TRUE), .groups = "drop")

# write_csv(prep_checklist_monthly, "prep_checklist_monthly.csv")

datatable(weekly_summary, options = list(pageLength = 10, scrollX = TRUE))
datatable(monthly_summary, options = list(pageLength = 10, scrollX = TRUE))

# without dishes, ingredients only
# by weekly dish-ingredient
prep_checklist_weekly_ing <- prep_checklist %>%
  mutate(Week = lubridate::floor_date(Date, unit = "week")) %>%
  group_by(Week, Ingredient) %>%
  summarise(Total_Required = sum(Total_Required, na.rm = TRUE), .groups = "drop")


# by monthly dish-ingredient
prep_checklist_monthly_ing <- prep_checklist %>%
  mutate(Month = lubridate::floor_date(Date, unit = "month")) %>%
  group_by(Month, Ingredient) %>%
  summarise(Total_Required = sum(Total_Required, na.rm = TRUE), .groups = "drop")

datatable(prep_checklist_monthly_ing,
          filter = "top",
          options = list(pageLength = 10, autoWidth = TRUE),
          rownames = FALSE,
          colnames = c("Month", "Ingredient", "Total Required (g)")) %>%
  formatCurrency("Total_Required", currency = "", interval = 3, mark = ",")

