# Import libraries
install.packages("DBI", dependencies = TRUE)
install.packages("RMySQL", dependencies = TRUE)
install.packages("tidyverse", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("polyglotr", dependencies = TRUE)
install.packages("readxl", dependencies = TRUE)
install.packages("numberize", dependencies = TRUE)
install.packages("data.table", dependencies = TRUE)
install.packages("writexl", dependencies = TRUE)
install.packages("openxlsx", dependencies = TRUE)

library(DBI)
library(RMySQL)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(polyglotr)
library(numberize)
library(data.table)
library(writexl)
library(openxlsx)

# Read Data from excel and store as variable
working_directory <- getwd()
absolute_file_path <- paste(working_directory, "/raw_data/data.xlsx", sep = "")
data <- read_xlsx(path = absolute_file_path)

recipe_file_path <- paste(working_directory, "/raw_data/recipe_data.xlsx", sep = "")

### For recipe dataset, we will be reading the tab sheet and storing it in a col
dishes_names <- excel_sheets(recipe_file_path)
# Creating a DB connection to integrate MySql into R
### We need to securely store MySql password. So to do this, you can set the password in your own OS 
mysql_password <- Sys.getenv("MYSQL_PASSWORD")
db_connection <- dbConnect(RMySQL::MySQL(), 
                           dbname = "nanyangCafe", 
                           host = "localhost", 
                           port = 3306, 
                           user = "root", 
                           ## Please input your own MySql root password for your local 
                           password = "Password",
                           allowLoadLocalInfile = TRUE)

dbSendQuery(db_connection, "SET GLOBAL local_infile = 'ON';")

# Creating schema to store all data
dbSendQuery(db_connection, "CREATE SCHEMA IF NOT EXISTS `nanyangCafe`;")
  
# 1. Data cleaning
# Transaction Dataset
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

### Creating new df using the transaction records
chinese_words_dictionary_df <- data.frame(chineseWords = chinese_words, stringsAsFactors = FALSE)
all_chinese_data <- c(unlist(title_row_chinese_character), unlist(columns_with_chinese_character))
chinese_words <- get_unique_chinese_values_func(all_chinese_data)

contains_english_num_characters_func <- function(data, check_row){
  data <- as.character(data)
  data <- trimws(data)  
  eng_num_character_regex <- grepl("^([a-zA-Z0-9]+|[+-]?\\d*\\.?\\d+(?:[eE][+-]?\\d+)?)$", data)
  if (check_row) {
    return(all(eng_num_character_regex))
  } else {
    return(any(eng_num_character_regex))
  }
}

chinese_words_dictionary_df <- data.frame(chineseWords = chinese_words_dictionary_df[!sapply(chinese_words_dictionary_df$chineseWords, function(word) {
  contains_english_num_characters_func(word, check_row = TRUE)}), ], stringsAsFactors = FALSE )

translate_fun <- function(text) {
  text <- trimws(text)    
  translated_text <- google_translate(text, target_language = "en", source_language = "zh-CN")
  return(translated_text)
}
chinese_words_dictionary_df$translatedEnglishWords <- sapply(chinese_words_dictionary_df$chineseWords, translate_fun)

# Recipes Dataset
### Appending newly translated Chinese words behind transaction records
for (d in dishes_names) {
  dish_ingredients <- read_excel(recipe_file_path, sheet = d)
  cols_with_chinese_values <- get_unique_chinese_values_func(dish_ingredients[,contains_chinese_characters_func(dish_ingredients, check_row = FALSE)])
  cols_with_chinese_values <- cols_with_chinese_values[!is.na(cols_with_chinese_values) & !sapply(cols_with_chinese_values, function(val) {
    contains_english_num_characters_func(val, check_row = FALSE)
  })]
  unique_chinese_characters <- sapply(cols_with_chinese_values, function(val) {
    val <- as.character(val)
    #### Will retain this pair of characters and translate it together
    if (grepl("??????", val)) {
      return(val)
    } else {
      chinese_characters <- str_extract_all(val, "[\u4e00-\u9fff]+") |> unlist()
      result <- paste(chinese_characters, collapse = "")
      return(result)
    }
  }, USE.NAMES = FALSE)
  
  #### Check if chinese words exists in existing Chinese words dictionary df
  unique_chinese_characters <- unique_chinese_characters[!unique_chinese_characters %in% chinese_words_dictionary_df$chineseWords]
  chinese_words_recipes_dictionary_df <- data.frame(chineseWords = unique_chinese_characters)
  chinese_words_recipes_dictionary_df$translatedEnglishWords <- sapply(chinese_words_recipes_dictionary_df$chineseWords, translate_fun)
  
  #### Appending each tabs to the existing Chinese words dictionary df
  chinese_words_dictionary_df <- rbind(chinese_words_dictionary_df, chinese_words_recipes_dictionary_df)
}

#### During translation, there are still duplicated records, so removing them
chinese_words_dictionary_df <- chinese_words_dictionary_df[!duplicated(chinese_words_dictionary_df),]

### Exporting the translation
#### write_xlsx(chinese_words_dictionary_df, paste(working_directory, "/raw_data/translated_chinese_english_dictionary.xlsx", sep = ""))
#### chinese_words_dictionary_df <- read_xlsx(path = paste(working_directory, "/raw_data/translated_chinese_english_dictionary.xlsx", sep = ""))
chinese_words_dictionary_df <- chinese_words_dictionary_df[,-1]
#### To improve processing time and reduce repeated translation, will be storing the chinese-english df into the DB
dbSendQuery(db_connection, "DROP TABLE IF EXISTS nanyangCafe.nc_chineseEnglishTranslation; ")
dbSendQuery(db_connection, "CREATE TABLE nc_chineseEnglishTranslation (
      pid INT AUTO_INCREMENT PRIMARY KEY,
      chineseWords varchar(255) NOT NULL,
      translatedEnglishWords varchar(255) NOT NULL);")

dbWriteTable(db_connection, value = chinese_words_dictionary_df, name = "nc_chineseEnglishTranslation", append = TRUE, row.names = FALSE) 

### The DB will always be the source of truth for all translation, so will query from the db
#### chinese_words_dictionary_df <- dbGetQuery(db_connection, "SELECT * FROM nanyangCafe.nc_chineseEnglishTranslation")

### Transaction dataset - Map the associated English words in the df
translation_mapping <- setNames(chinese_words_dictionary_df$translatedEnglishWords, chinese_words_dictionary_df$chineseWords)
translated_transaction_order_data <- as.data.frame(lapply(data, function(index) {
  ifelse(index %in% names(translation_mapping), 
         translation_mapping[index], 
         index)
}))

write_xlsx(chinese_words_dictionary_df, paste(working_directory, "/raw_data/translated_chinese_english_dictionary.xlsx", sep = ""))

### Recipe dataset - Map the associated English words in the df


## Reformat the data frame and assign the translated column names to the dataset
### Checking for NA / NAN / empty values
colnames(translated_transaction_order_data) <- as.character(unlist(translated_transaction_order_data[1,])) 
translated_transaction_order_data <- translated_transaction_order_data[-1, ]

## Remove unnecessary columns and empty rows
cols_to_drop <- c("Remark", "Cashier", "shift", "Number of Auxiliary", "Specification", "Orderer", "Food revenue items") 
translated_transaction_order_data <- translated_transaction_order_data %>% select(-all_of(cols_to_drop))

## Modifying the data types of each columns
translated_transaction_order_data$`Serial number` <- as.numeric(translated_transaction_order_data$`Serial number`)
translated_transaction_order_data$`Purchase Date` <- as.Date(as.character(translated_transaction_order_data$`Purchase Date`), format = "%Y-%m-%d")
translated_transaction_order_data$`Store Name` <- as.character(translated_transaction_order_data$`Store Name`)
translated_transaction_order_data$`Dishes code` <- as.character(translated_transaction_order_data$`Dishes code`)
translated_transaction_order_data$`Dishes name` <- as.factor(unlist(translated_transaction_order_data$`Dishes name`))
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
translated_transaction_order_data$`Bill Type` <- as.factor(unlist(translated_transaction_order_data$`Bill Type`))
translated_transaction_order_data$channel <- as.factor(unlist(translated_transaction_order_data$channel))
translated_transaction_order_data$`Flavor preference` <- as.character(translated_transaction_order_data$`Flavor preference`)
translated_transaction_order_data$practice <- as.character(translated_transaction_order_data$practice)
translated_transaction_order_data$`Region Name` <- as.factor(unlist(translated_transaction_order_data$`Region Name`))
translated_transaction_order_data$`Table No` <- as.character(translated_transaction_order_data$`Table No`)
translated_transaction_order_data$`Bill Number` <- as.factor(unlist(translated_transaction_order_data$`Bill Number`))
translated_transaction_order_data$`Remark` <- as.character(translated_transaction_order_data$`Remark`)

# 2. Data transformation
## Feature Engineering 
### Creating new column to store the exact location of the branch
translated_transaction_order_data$Branch <- ifelse(
  grepl("Nanyang Kopi \\(Broadway Branch\\)", translated_transaction_order_data$`Store Name`),
  "Broadway",
  "Caravel"
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
top_dishes <- translated_transaction_order_data %>%
  group_by(Branch, `Meal Type`, `Dishes name`) %>%
  summarise(Order_Count = n(), .groups = "drop") %>%
  arrange(Branch, `Meal Type`, desc(Order_Count)) %>%
  group_by(Branch, `Meal Type`) %>%
  slice_head(n = 3)

# Plot
ggplot(top_dishes, aes(x = reorder(`Dishes name`, Order_Count), y = Order_Count, fill = `Meal Type`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_grid(Branch ~ `Meal Type`, scales = "free_y") +
  labs(title = "Top 3 Dishes Ordered per Meal Type by Branch",
       x = "Dish Name",
       y = "Order Count") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

# 4. Modeling
## Apriori to create set meals

