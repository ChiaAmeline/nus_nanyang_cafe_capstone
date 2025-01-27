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
## Translate data from Chinese to English (will create own df of chinese characters to reduce time taken to process the translation when the function hits google translate's API)
### Remove any rows that are entirely empty or more than 70% empty
rows_to_delete <- function(row, threshold = 0.7){
  is_empty <- function(x){
    is.na(x) || x == "" || (is.numeric(x) && is.nan(x))
  }
  empty_count <- sum(sapply(row, is_empty))
  total_cols <- ncol(data)
  return(empty_count > (total_cols * threshold))
}
data <- data[!apply(data, 1, rows_to_delete), ]

contains_chinese_characters_func <- function(data, check_row){
  data <- as.character(data)
  data <- trimws(data)  
  chinese_character_regex <- grepl("[\u4e00-\u9fff]", data)
  if (check_row) {
    return(all(chinese_character_regex))
  } else {
    !is.na(data)
    return(any(chinese_character_regex))
  }
}
first_three_rows <- data[1:5, ]           ## To sieve out the title row from the top 5 rows (might not know where the title row is)
title_row_index <- which(apply(first_three_rows, 1, function(row) contains_chinese_characters_func(row, check_row = TRUE)))
title_row_chinese_character <- data[title_row_index, ]
data_without_title <- data[-title_row_index,]

columns_index_with_chinese_character <- which(apply(data_without_title, 2, function(column) contains_chinese_characters_func(column, check_row = FALSE)))
columns_with_chinese_character <- data_without_title[,columns_index_with_chinese_character]

get_unique_chinese_values_func <- function(all_chinese_character_data){
  all_chinese_character_data <- unlist(all_chinese_character_data)
  unique_chinese_character <- unique(all_chinese_character_data)
  return(unique_chinese_character)
}
all_chinese_data <- c(unlist(title_row_chinese_character), unlist(columns_with_chinese_character))
chinese_words <- get_unique_chinese_values_func(all_chinese_data)
chinese_words_dictionary_df <- data.frame(chinese_words = chinese_words, stringsAsFactors = FALSE)

contains_english_characters_func <- function(data, check_row){
  data <- as.character(data)
  data <- trimws(data)  
  eng_num_character_regex <- grepl("^[a-zA-Z0-9]+$", data)
  if (check_row) {
    return(all(eng_num_character_regex))
  } else {
    return(any(eng_num_character_regex))
  }
}
chinese_words_dictionary_df <- data.frame(chinese_words =   chinese_words_dictionary_df[!sapply(chinese_words_dictionary_df$chinese_words, function(word) {
  contains_english_characters_func(word, check_row = TRUE)}), ], stringsAsFactors = FALSE )

translate_fun <- function(text) {
  text <- trimws(text)    
  translated_text <- google_translate(text, target_language = "en", source_language = "zh-CN")
  return(translated_text)
}
chinese_words_dictionary_df$translated_english_word <- sapply(chinese_words_dictionary_df$chinese_words, translate_fun)

### Map the associated English words in the df
translation_mapping <- setNames(chinese_words_dictionary_df$translated_english_word, chinese_words_dictionary_df$chinese_words)
translated_transaction_order_data <- as.data.frame(lapply(data, function(column) {
  ifelse(column %in% names(translation_mapping), 
         translation_mapping[column], 
         column)
}))

write_xlsx(translated_transaction_order_data, "translated_transaction_order_data.xlsx")   ## To export so that we dont have to rerun the translation all the time
translated_transaction_order_data <- read_xlsx(path = paste(working_directory, "/translated_transaction_order_data.xlsx", sep = ""))

## Reformat the data frame and assign the translated column names to the dataset
### Checking for NA / NAN / empty values
colnames(translated_transaction_order_data) <- as.character(unlist(translated_transaction_order_data[1,])) 
setnames(translated_transaction_order_data, old = c("date", "Turnover", "Turnover amount"), new = c("Purchase Date", "Quantity", "Total Price"))
translated_transaction_order_data <- translated_transaction_order_data[-1, ]

## Remove unnecessary columns and empty rows
cols_to_drop_regex <- grep("Remark|Ordering", colnames(translated_transaction_order_data))
translated_transaction_order_data <- translated_transaction_order_data %>% select(-all_of(cols_to_drop_regex))
cols_to_drop <- c("Cashier", "Flights", "Number of Auxiliary", "Specification") 
translated_transaction_order_data <- translated_transaction_order_data %>% select(-all_of(cols_to_drop))

## Clean up dirty data that occurred due to translation
### Updating Single Point values into "Ala Cart" so that the data is more meaningful
translated_transaction_order_data$Dishes[translated_transaction_order_data$Dishes == "Single point"] <- "Ala Cart"
translated_transaction_order_data$`Store Name` <- gsub("\\(Kaer Branch\\)", "(Kam Cheong Hou - Caravel Hotel)", translated_transaction_order_data$`Store Name`)
translated_transaction_order_data$`Store Name` <- gsub("\\(Broadway Store\\)", "(Broadway Macau Food Street)", translated_transaction_order_data$`Store Name`)

## Modifying the data types of each columns
translated_transaction_order_data$`Serial number` <- as.numeric(translated_transaction_order_data$`Serial number`)
translated_transaction_order_data$`Purchase Date` <- as.Date(as.character(translated_transaction_order_data$`Purchase Date`), format = "%Y-%m-%d")
translated_transaction_order_data$`Store Name` <- as.character(translated_transaction_order_data$`Store Name`)
translated_transaction_order_data$`Dish Code` <- as.character(translated_transaction_order_data$`Dish Code`)
translated_transaction_order_data$`Dish name` <- as.factor(unlist(translated_transaction_order_data$`Dish name`))
translated_transaction_order_data$Dishes <- as.factor(unlist(translated_transaction_order_data$Dishes))
translated_transaction_order_data$`Opening time` <- format(as.POSIXct( as.character(translated_transaction_order_data$`Opening time`), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%H:%M:%S")
translated_transaction_order_data$`Checkout Time` <- format(as.POSIXct( as.character(translated_transaction_order_data$`Checkout Time`), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%H:%M:%S")
translated_transaction_order_data$`Order Time` <- format(as.POSIXct( as.character(translated_transaction_order_data$`Order Time`), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%H:%M:%S")
translated_transaction_order_data$Price <- as.numeric(translated_transaction_order_data$Price)
translated_transaction_order_data$Quantity <- as.numeric(translated_transaction_order_data$Quantity)
translated_transaction_order_data$`Total Price` <- as.numeric(translated_transaction_order_data$`Total Price`)
translated_transaction_order_data$`Discount amount` <- as.numeric(translated_transaction_order_data$`Discount amount`)
translated_transaction_order_data$`Amount received` <- as.numeric(translated_transaction_order_data$`Amount received`)
translated_transaction_order_data$Discount <- as.factor(unlist(translated_transaction_order_data$Discount))
translated_transaction_order_data$`Production Department` <- as.factor(unlist(translated_transaction_order_data$`Production Department`))
translated_transaction_order_data$`Food revenue items` <- as.factor(unlist(translated_transaction_order_data$`Food revenue items`))
translated_transaction_order_data$`Bill Type` <- as.factor(unlist(translated_transaction_order_data$`Bill Type`))
translated_transaction_order_data$channel <- as.factor(unlist(translated_transaction_order_data$channel))
translated_transaction_order_data$taste <- as.character(translated_transaction_order_data$taste)
translated_transaction_order_data$practice <- as.character(translated_transaction_order_data$practice)
translated_transaction_order_data$`Region Name` <- as.factor(unlist(translated_transaction_order_data$`Region Name`))
translated_transaction_order_data$`Table Name` <- as.factor(unlist(translated_transaction_order_data$`Table Name`))
translated_transaction_order_data$`Bill Number` <- as.factor(unlist(translated_transaction_order_data$`Bill Number`))

# 2. Data transformation
## Feature Engineering 
### Creating new column to store the exact location of the branch
translated_transaction_order_data$Branch <- ifelse(
  grepl("Nanyang Coffee \\(Broadway Macau Food Street\\)", translated_transaction_order_data$`Store Name`),
  "Broadway Macau Food Street",
  "Kam Cheong Hou - Caravel Hotel"
)
translated_transaction_order_data$Branch <- as.factor(translated_transaction_order_data$Branch)

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
translated_transaction_order_data$Month <- sapply(format(translated_transaction_order_data$`Purchase Date`, "%m"), convert_numerical_month_to_month_name_func)
translated_transaction_order_data$Month <- as.factor(translated_transaction_order_data$Month)

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
translated_transaction_order_data$Season <- sapply(translated_transaction_order_data$Month, convert_months_to_seasons_func)
translated_transaction_order_data$Season <- as.factor(translated_transaction_order_data$Season)

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
translated_transaction_order_data$`Meal Type` <- sapply(translated_transaction_order_data$`Order Time`, categorize_meal_type_func)
translated_transaction_order_data$`Meal Type` <- as.factor(translated_transaction_order_data$`Meal Type`)

# 3. Exploratory Data Analysis (EDA)


# 4. Modeling
## Apriori to create set meals

