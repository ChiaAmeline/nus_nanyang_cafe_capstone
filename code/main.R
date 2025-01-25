# Import libraries
install.packages("tidyverse", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("polyglotr", dependencies = TRUE)
install.packages("readxl", dependencies = TRUE)
install.packages("numberize", dependencies = TRUE)
install.packages("data.table", dependencies = TRUE)
install.packages("writexl", dependencies = TRUE)

library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(polyglotr)
library(numberize)
library(data.table)
library(writexl)

# Read Data from excel and store as variable
working_directory <- getwd()
absolute_file_path <-paste(working_directory, "/raw_data/data.xlsx", sep = "")
data <- read_xlsx(path = absolute_file_path)

# 1. Data cleaning
## Translate data from Chinese to English
translate_fun <- function(text) {
  text <- trimws(text)    
  translated_text <- google_translate(text, target_language = "en", source_language = "zh-CN")
  return(translated_text)
}
data <- sapply(data, translate_fun)
write_xlsx(data, "transaction_order_data.csv")

## Reformat the data frame and assign the translated column names to the dataset
a <- as.data.frame(data)
### Checking for NA / NAN / empty values
is_row_empty <- function(row){
  all(sapply(row, function(x) is.na(x) || x == "" || (is.numeric(x) && is.nan(x))))
}
a <- a[!apply(a, 1, is_row_empty), ]
a <- a[-1, ]
colnames(a) <- as.character(unlist(a[1,])) 
setnames(a, old = c("date", "Turnover", "Turnover amount"), new = c("Purchase Date", "Quantity", "Total Price"))
a <- a[-1, ]

## Remove unnecessary columns and empty rows
cols_to_drop <- c("Cashier", "Flights", "Number of Auxiliary", "Specification") 
cols_to_drop_regex <- grep("Remark|Ordering", colnames(a))
a <- a %>% select(-all_of(c(cols_to_drop_regex, cols_to_drop)))

rows_to_remove <- grep("total", a$`Serial number`, ignore.case = TRUE)
is_empty <- function(variable) {
  is.null(variable) || length(variable) == 0 || all(is.na(variable))
}
if (!is_empty(rows_to_remove)) {
  a <- a[-rows_to_remove, ]
}

## Clean up dirty data that occurred due to translation
monetary_cols <- c("Price", "Total Price", "Discount amount", "Amount received", "Discount") 
check_invalid_money_fun <- function(value) {
  value <- tolower(value)
  ### To extract values that are not a valid monetary data type (e.g. whole numbers / decimals)
  is_invalid_data_type <- grepl("^(?!\\d+(\\.\\d+)?$).+$", value, perl = T)
  return(is_invalid_data_type)
}

filtered_dataset <- sapply(a, check_invalid_money_fun)
columns_with_string <- filtered_dataset[, apply(filtered_dataset, 2, any)]
monetary_cols_with_invalid_values <- intersect(colnames(columns_with_string), monetary_cols)

convert_number_word_to_numerical_value_fun <- function(string_text) {
  string_text <- tolower(string_text)
  converted_value <- numberize(string_text, lang = "en")
  return(converted_value)
}
a[monetary_cols_with_invalid_values] <- sapply(a[monetary_cols_with_invalid_values], convert_number_word_to_numerical_value_fun)
### Updating Single Point values into "Ala Cart" so that the data is more meaningful
a$Dishes[a$Dishes == "Single point"] <- "Ala Cart"
a$`Store Name` <- gsub("\\(Kaer Branch\\)", "(Kam Cheong Hou - Caravel Hotel)", a$`Store Name`)
a$`Store Name` <- gsub("\\(Broadway Store\\)", "(Broadway Macau Food Street)", a$`Store Name`)

## Modifying the data types of each columns
a$`Serial number` <- as.numeric(a$`Serial number`)
a$`Purchase Date` <- as.Date(as.character(a$`Purchase Date`), format = "%Y-%m-%d")
a$`Store Name` <- as.character(a$`Store Name`)
a$`Dish Code` <- as.character(a$`Dish Code`)
a$`Dish name` <- as.factor(unlist(a$`Dish name`))
a$Dishes <- as.factor(unlist(a$Dishes))
a$`Opening time` <- format(as.POSIXct( as.character(a$`Opening time`), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%H:%M:%S")
a$`Checkout Time` <- format(as.POSIXct( as.character(a$`Checkout Time`), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%H:%M:%S")
a$`Order Time` <- format(as.POSIXct( as.character(a$`Order Time`), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%H:%M:%S")
a$Price <- as.numeric(a$Price)
a$Quantity <- as.numeric(a$Quantity)
a$`Total Price` <- as.numeric(a$`Total Price`)
a$`Discount amount` <- as.numeric(a$`Discount amount`)
a$`Amount received` <- as.numeric(a$`Amount received`)
a$Discount <- as.factor(unlist(a$Discount))
a$`Production Department` <- as.factor(unlist(a$`Production Department`))
a$`Food revenue items` <- as.factor(unlist(a$`Food revenue items`))
a$`Bill Type` <- as.factor(unlist(a$`Bill Type`))
a$channel <- as.factor(unlist(a$channel))
a$taste <- as.character(a$taste)
a$practice <- as.character(a$practice)
a$`Region Name` <- as.factor(unlist(a$`Region Name`))
a$`Table Name` <- as.factor(unlist(a$`Table Name`))
a$`Bill Number` <- as.factor(unlist(a$`Bill Number`))

# 2. Data transformation
## Feature Engineering 
### Creating new column to store the exact location of the branch
a$Branch <- ifelse(
  grepl("Nanyang Coffee \\(Broadway Macau Food Street\\)", a$`Store Name`),
  "Broadway Macau Food Street",
  "Kam Cheong Hou - Caravel Hotel"
)
a$Branch <- as.factor(a$Branch)

### Creating new column to store the months to determine seasonal changes
convert_numerical_month_to_month_name_func <- function(numerical_month){
  char_months <- switch (numerical_month,
    "01" = "January", 
    "02" = "February",
    "03" = "March", 
    "04" = "April",
    "05" = "May", 
    "06" = "June",
    "07" = "July", 
    "08" = "August",
    "09" = "September", 
    "10" = "October", 
    "11" = "November", 
    "December"
  )
  return(char_months)
}
a$Month <- sapply(format(a$`Purchase Date`, "%m"), convert_numerical_month_to_month_name_func)
a$Month <- as.factor(a$Month)

### Creating new column to store the season of the month
convert_months_to_seasons_func <- function(month){
  month <- tolower(month)
  season <- switch (month,
    "march" = "Spring", 
    "april" = "Spring",
    "may" = "Spring", 
    "june" = "Summer",
    "july" = "Summer", 
    "august" = "Summer",
    "september" = "Autumn", 
    "october" = "Autumn", 
    "november"= "Autumn",
    "december" = "Winter",
    "january" = "Winter",
    "february" = "Winter",
    NA
  )
  return(season)
}
a$Season <- sapply(a$Month, convert_months_to_seasons_func)
a$Season <- as.factor(a$Season)

### Creating new column to determine if its breakfast / lunch / dinner (opening hours: 08:00 - 21:00 Mon to Sun)
categorize_meal_type_func <- function(time){
  hour <- as.numeric(substr(time, 1, 2))
  if (hour >= 8 & hour < 11) {
    return("Breakfast")
  } else if (hour >= 11 & hour < 14) {
    return("Lunch")
  } else if (hour >= 18 & hour < 21) {
    return("Dinner")
  } else {
    return("Other")
  }
}
a$`Meal Type` <- sapply(a$`Order Time`, categorize_meal_type_func)
a$`Meal Type` <- as.factor(a$`Meal Type`)

# 3. Exploratory Data Analysis (EDA)
## How much does each outlet 


# 4. Modeling
## Apriori to create set meals

