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
install.packages("ggcorrplot", dependencies = TRUE)
install.packages("ggfortify", dependencies = TRUE)  
install.packages("lubridate", dependencies = TRUE)  
install.packages("https://cran.r-project.org/src/contrib/Archive/arules/arules_1.7-10.tar.gz", repos = NULL, type = "source", dependencies = TRUE)
install.packages("stringr", dependencies = TRUE)  
install.packages("scales", dependencies = TRUE) 
install.packages("viridisLite", dependencies = TRUE) 
install.packages("broom", dependencies = TRUE) 
install.packages("gt", dependencies = TRUE) 

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
library(ggcorrplot)
library(ggfortify)
library(lubridate)
library(arules)
library(stringr)
library(scales)
library(viridisLite)
library(broom)
library(gt)

# Read Data from excel and store as variable
working_directory <- getwd()
absolute_file_path <- paste(working_directory, "/raw_data/data.xlsx", sep = "")
data <- read_xlsx(path = absolute_file_path)

recipe_file_path <- paste(working_directory, "/raw_data/recipe_data.xlsx", sep = "")

full_transaction_file_path <- paste(working_directory, "/raw_data/transaction_dataset_2024.xlsx", sep = "")
data <- read_xlsx(path = full_transaction_file_path,  sheet = "Report")
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
                           password = "Dragonee%1997",
                           allowLoadLocalInfile = TRUE)

dbSendQuery(db_connection, "SET GLOBAL local_infile = 'ON';")

nrow(data)
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
   <- cols_with_chinese_values[!is.na(cols_with_chinese_values) & !sapply(cols_with_chinese_values, function(val) {
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

#### Since dishes names was used to extract each individual sheets, it was not part of the translation in the above function. So separately translating and appending it
dishes_names <- as.vector(dishes_names)
##### Removing some values as not all are dish names
dishes_names <- dishes_names[1:25]
chinese_words_dishes_names_df <- data.frame(chineseWords = dishes_names)
chinese_words_dishes_names_df$translatedEnglishWords <- sapply(chinese_words_dishes_names_df$chineseWords, translate_fun)
chinese_words_dictionary_df <- rbind(chinese_words_dictionary_df, chinese_words_dishes_names_df)

#### During translation, there are still duplicated records, so removing them
chinese_words_dictionary_df <- chinese_words_dictionary_df[!duplicated(chinese_words_dictionary_df),]

#### Adding missing records of dishes
missing_dishes_chinese <- c("???????????????", "?????????", "????????????")
missing_dishes_chinese <- as.vector(missing_dishes_chinese)
missing_dishes_english <- c("Mee Soto", "Prawn Noodle", "Mee Rebus")
missing_dishes_english <- as.vector(missing_dishes_english)

missing_dishes_df <- data.frame(
  chineseWords = missing_dishes_chinese,
  translatedEnglishWords = missing_dishes_english,
  stringsAsFactors = FALSE   
)

chinese_words_dictionary_df <- rbind(chinese_words_dictionary_df, missing_dishes_df)

### Exporting the translation
#### write_xlsx(chinese_words_dictionary_df, paste(working_directory, "/raw_data/translated_chinese_english_dictionary.xlsx", sep = ""))
#### chinese_words_dictionary_df <- read_xlsx(path = paste(working_directory, "/raw_data/translated_chinese_english_dictionary.xlsx", sep = ""))
chinese_words_dictionary_df <- chinese_words_dictionary_df[,-1]
#### To improve processing time and reduce repeated translation, will be storing the chinese-english df into the DB

##### Chinese English Translation
####### Transaction dataset - Map the associated English words in the df
translation_mapping <- setNames(chinese_words_dictionary_df$translatedEnglishWords, chinese_words_dictionary_df$chineseWords)
translated_transaction_order_data <- as.data.frame(lapply(data, function(index) {
  ifelse(index %in% names(translation_mapping), 
         translation_mapping[index], 
         index)
}))

####### (Optional) To bypass the above codes by pumping data from excel into the tables 
translated_dictionary_file_path <- paste(working_directory, "/raw_data/translated_chinese_english_dictionary.xlsx", sep = "")
chinese_words_dictionary_df <- read_excel(translated_dictionary_file_path)
chinese_words_dictionary_df <- chinese_words_dictionary_df[,-1]
dbWriteTable(db_connection, value = chinese_words_dictionary_df, name = "nc_chineseEnglishTranslation", append = TRUE, row.names = FALSE) 

##### Dish Name
####### Recipe dataset (Dish Name) - Map the associated English words in the df
dishes_names <- dishes_names[-1] ### "Ingredients price" is not a dish name, so removing the row

translated_dish_name_df <- data.frame(
  dishName  = character(), 
  dishComponents   = character(), 
  stringsAsFactors = FALSE        
)

for (d in dishes_names) {
  cur_sheet <- read_excel(recipe_file_path, sheet = d)
  translated_dish_name <- chinese_words_dictionary_df$translatedEnglishWords[match(d, chinese_words_dictionary_df$chineseWords)]
  ####### The dish components are all located on the first col with no other values in other cols
  all_dish_component_index <- which( !is.na(cur_sheet[[1]]) & apply(cur_sheet[-1], 1, function(x) all(is.na(x) | trimws(x) == "")))
  all_dish_component <- cur_sheet[all_dish_component_index, 1]
  all_dish_component <- apply(all_dish_component, 1, function(x) gsub("[[:punct:]]+", "", x))
  all_dish_component <- chinese_words_dictionary_df$translatedEnglishWords[match(all_dish_component, chinese_words_dictionary_df$chineseWords)]
  all_dish_component <- as.vector(all_dish_component)
  num_dish_component_cur_sheet <- length(all_dish_component)
  
  if (length(all_dish_component) == 0) {
    row <- data.frame(
      dishName = translated_dish_name,
      dishComponents = NA_character_,
      stringsAsFactors = FALSE
    )
    translated_dish_name_df <- rbind(translated_dish_name_df, row)
    next
  } else {
    for (counter in 1:num_dish_component_cur_sheet) {
      row <- data.frame(
        dishName = translated_dish_name,
        dishComponents = all_dish_component[counter],
        stringsAsFactors = FALSE
      )
      translated_dish_name_df <- rbind(translated_dish_name_df, row)
    }
  }
}

##### Manually adding some of the records as the data showcase the main ingredients to cook the dish itself and its not a component
additional_dishes <- c("Bak Kut Teh", "Hainan Chicken Kow Teh Set Meal", "Big shrimp laksa")
additional_dishes <- as.vector(additional_dishes)

for (a in additional_dishes) {
  row <- data.frame(
    dishName = a,
    dishComponents = NA_character_,
    stringsAsFactors = FALSE
  )
  translated_dish_name_df <- rbind(translated_dish_name_df, row)
}

dbWriteTable(db_connection, value = translated_dish_name_df, name = "nc_dishName", append = TRUE, row.names = FALSE) 
write_xlsx(translated_dish_name_df, paste(working_directory, "/raw_data/dish_name_table.xlsx", sep = ""))

####### (Optional) To bypass the above codes by pumping data from excel into the tables 
dish_names_file_path <- paste(working_directory, "/raw_data/dish_name_table.xlsx", sep = "")
translated_dish_name_df <- read_excel(dish_names_file_path)
translated_dish_name_df <- translated_dish_name_df[,-1]
dbWriteTable(db_connection, value = translated_dish_name_df, name = "nc_dishName", append = TRUE, row.names = FALSE) 

##### Inventory
inventory_df <- data.frame(
  ingredientName  = character(),  
  packagingSize   = numeric(),   
  packagingPrice  = numeric(),     
  unitPrice       = numeric(),    
  stringsAsFactors = FALSE        
)

for (d in dishes_names) {
  dish_ingredients <- read_excel(recipe_file_path, sheet = d)
  ##### Remove title and header rows
  dish_ingredients <- dish_ingredients[-c(1, 2),]
  ##### Removing rows that doesnt give much information
  rows_less_than_threshold <- !apply(
    dish_ingredients, 1,
    function(r) rows_to_delete(as.list(r))
  )
  cur_sheet <- dish_ingredients[rows_less_than_threshold, , drop = FALSE]
  ##### Reading the ingredient col to translate and append to dataframe
  translated_english_dish_name <- translation_mapping[ cur_sheet[[1]] ]
  row <- data.frame(
    ingredientName  = translated_english_dish_name,
    packagingSize   = cur_sheet[[2]],
    packagingPrice  = cur_sheet[[3]],
    unitPrice       = cur_sheet[[4]],
    stringsAsFactors = FALSE
  )
  inventory_df <- rbind(inventory_df, row)
}  
##### Removing rows with NA values and ingredient Name = "Tangshan" as it doesnt value add
inventory_df <- inventory_df[ complete.cases(inventory_df), ]
inventory_df <- inventory_df %>% mutate(ingredientName = trimws(ingredientName)) %>% filter(ingredientName != "Tangshan")

##### Removing duplicated rows
inventory_df <- inventory_df[ !duplicated(inventory_df), ]

dbWriteTable(db_connection, value = inventory_df, name = "nc_inventory", append = TRUE, row.names = FALSE) 
write_xlsx(inventory_df, paste(working_directory, "/raw_data/inventory_table.xlsx", sep = ""))

####### (Optional) To bypass the above codes by pumping data from excel into the tables 
inventory_file_path <- paste(working_directory, "/raw_data/inventory_table.xlsx", sep = "")
inventory_df <- read_excel(inventory_file_path)
inventory_df <- inventory_df[,-1]
dbWriteTable(db_connection, value = inventory_df, name = "nc_inventory", append = TRUE, row.names = FALSE) 

##### Recipe 
###### Manually created the excel, so inserting it into the DB 
translated_recipe_data_file_path <- paste(working_directory, "/raw_data/recipes.xlsx", sep = "")
translated_recipe_df <- read_excel(translated_recipe_data_file_path)
dbWriteTable(db_connection, value = translated_recipe_df, name = "nc_recipes", append = TRUE, row.names = FALSE) 

## Transaction dataset: Reformat the data frame and assign the translated column names to the dataset
### Checking for NA / NAN / empty values
colnames(translated_transaction_order_data) <- as.character(unlist(translated_transaction_order_data[1,])) 
translated_transaction_order_data <- translated_transaction_order_data[-1, ]

## Remove unnecessary columns and empty rows
cols_to_drop <- c("Remark", "Cashier", "shift", "Number of Auxiliary", "Specification", "Orderer", "Food revenue items") 
translated_transaction_order_data <- translated_transaction_order_data %>% select(-all_of(cols_to_drop))
translated_transaction_order_data <- translated_transaction_order_data[,-24]
translated_transaction_order_data <- translated_transaction_order_data[,-19]

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

## Inventory Recipe dataset: Reformat the data frame and assign the translated column names to the dataset
inventory_recipe_df <- dbGetQuery(db_connection, "SELECT NCD.dishName, NCD.dishComponents, NCI.ingredientName, NCI.packagingSize, NCI.packagingPrice, NCI.unitPrice, NCR.inventoryUsed, NCR.totalCost, NCR.servingPortion FROM nanyangCafe.nc_recipes NCR
                                                  INNER JOIN nanyangCafe.nc_dishName NCD ON NCR.dishId = NCD.pid
                                                  INNER JOIN nanyangCafe.nc_inventory NCI ON NCR.inventoryId = NCI.pid;")

## NA values are included as there isnt any component mentioned within the orginal dataset. As such, will copy over the dish name as its component
inventory_recipe_df <- inventory_recipe_df %>% mutate(dishComponents = ifelse(is.na(dishComponents), dishName, dishComponents))

## Modifying the data types of each columns
inventory_recipe_df$servingPortion <- round(as.numeric(inventory_recipe_df$`servingPortion`))

# 2. Data transformation
## Transaction Data: Feature Engineering 
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

write_xlsx(translated_transaction_order_data, paste(working_directory, "/raw_data/translated_transaction_dataset_2024.xlsx", sep = ""))

## Inventory Recipe Data: Feature Engineering 
### Creating new columns to determine the food wastes caused by packaging
#### Pack to Use ratio
inventory_recipe_df$packToUseRatio <- inventory_recipe_df$packagingSize / inventory_recipe_df$inventoryUsed
#### Total serving by package
inventory_recipe_df$totalServingByPackage <- inventory_recipe_df$packToUseRatio * inventory_recipe_df$servingPortion
#### Package waste quantity
inventory_recipe_df$packageWasteQty <- inventory_recipe_df$packagingSize %% inventory_recipe_df$inventoryUsed
#### Package waste cost price
inventory_recipe_df$packageWasteCost <- inventory_recipe_df$packageWasteQty * inventory_recipe_df$unitPrice
#### write_xlsx(inventory_recipe_df, paste(working_directory, "/raw_data/recipe_inventory.xlsx", sep = ""))

# 3. Exploratory Data Analysis (EDA)
## Transaction Data
## Top 3 Dishes Ordered per Meal Type by Branch (Horizontal Bar Chart)
top_dishes <- translated_transaction_order_data %>%
  group_by(Branch, `Meal Type`, `Dishes name`) %>%
  summarise(Order_Count = n(), .groups = "drop") %>%
  arrange(Branch, `Meal Type`, desc(Order_Count)) %>%
  group_by(Branch, `Meal Type`) %>%
  slice_head(n = 3)


ggplot(top_dishes, aes(x = reorder(`Dishes name`, Order_Count), y = Order_Count, fill = `Meal Type`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_grid(Branch ~ `Meal Type`, scales = "free_y") +
  labs(title = "Top 3 Dishes Ordered per Meal Type by Branch",
       x = "Dish Name",
       y = "Order Count") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))


## Top 5 Dishes Ordered per Season per Bill Type
transaction_order_data_eda_df <- translated_transaction_order_data %>% filter(!str_detect(`Dishes name`, regex("\\b(service|delivery|takeaway)\\s*fee\\b|\\bbag\\b|\\bbox\\b",, ignore_case = TRUE)))
top5 <- transaction_order_data_eda_df %>% 
  group_by(Season, `Bill Type`, `Dishes name`) %>% 
  summarise(order_count = n(), .groups = "drop") %>% 
  arrange(Season, `Bill Type`, desc(order_count)) %>% 
  group_by(Season, `Bill Type`) %>% 
  slice_head(n = 5) %>% 
  mutate(pct = order_count / sum(order_count)) %>% 
  ungroup()

dishes <- sort(unique(top5$`Dishes name`))         
n_dishes <- length(dishes)

pal_dishes  <- scales::hue_pal(l = 65, c = 100)(n_dishes)  
names(pal_dishes) <- dishes         

ggplot(top5,aes(y =`Bill Type`, x = order_count, fill = `Dishes name`)) +
  geom_col(width = 0.8) +  facet_wrap(~ Season, nrow = 1) +
  scale_fill_manual(values = pal_dishes, guide = guide_legend(ncol = 3)) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Top 5 Dishes by Season For Year 2024",
       x = "Order Count", y = NULL, fill = "Dish") +
  theme_minimal(base_size = 12) +
  theme(legend.position   = "bottom",
        legend.key.height = unit(0.4, "cm"),
        legend.text = element_text(size = 8))


ggplot(top5,aes(y = `Bill Type`, x = pct, fill = `Dishes name`)) +
  geom_col(width = 0.8) +
  geom_text(aes(label = ifelse(pct >= 0.03, percent(pct, 1), "")),
            position = position_stack(vjust = 0.5),
            colour   = "white",
            size     = 3) +
  facet_wrap(~ Season, nrow = 1) +
  scale_fill_manual(values = pal_dishes, guide = guide_legend(ncol = 3)) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Proportion of Top 5 Dishes by Season For Year 2024",
       x = "Proportion of Order Count", y = NULL, fill = "Dish") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.key.height = unit(0.4, "cm"),
        legend.text = element_text(size = 8))

## Inventory Recipe Data
## Correlation for inventory wastage (Heatmap)
reduced_df <- subset(inventory_recipe_df, select = c(inventoryUsed, servingPortion, packageWasteQty, packageWasteCost))
corr_matrix = round(cor(reduced_df), 2)
ggcorrplot(corr_matrix, lab = TRUE, lab_size = 3, outline.color = "white", colors  = c("#b2182b", "white", "#2166ac"),
           title = "Correlation of inventory metrics", ggtheme = theme_minimal(base_size = 12))

## Package size cost vs quantity wastage (Scatterplot and linear graph)
ggplot(inventory_recipe_df, aes(x = packageWasteQty, y = packageWasteCost)) +
  geom_point(alpha = 0.6, colour = "#2C77B0") +
  labs(title = "Package size cost vs. quantity wastage",
       x = "Quantity wastage (g / ml)",
       y = "Cost wastage (MOP)") +
  theme_minimal(base_size = 12)

combined_quads <- inventory_recipe_df %>%
  mutate(
    quadrant = case_when(
      packageWasteQty  >= 500 & packageWasteCost >= 10 & packageWasteCost <  20  ~ "Mid-Qty / Mid-Cost",
      packageWasteQty  >= 500 & packageWasteCost >  20  ~ "High-Qty / High-Cost",
      TRUE                                               ~ NA_character_
    )
  ) %>%
  filter(!is.na(quadrant)) %>% distinct(ingredientName, dishName, .keep_all = TRUE)

combined_quads %>%
  select(Dish = dishName,
        Ingredient = ingredientName,
         `Quantity wastage (g / ml)`    = packageWasteQty,
         `Cost Wasted (MOP)` = packageWasteCost,
         Quadrant            = quadrant) %>%
  gt() %>%
  tab_header(
    title    = "Ingredients in Mid-Qty/Mid-Cost & High-Qty/High-Cost Quadrants",
  ) %>%
  fmt_number(
    columns = vars(`Quantity wastage (g / ml)`, `Cost Wasted (MOP)`),
    decimals = 1
  ) %>%
  opt_row_striping() 


# 4. Modeling
translated_transaction_order_data <- read_excel(path = paste(working_directory, "/raw_data/translated_transaction_dataset_2024.xlsx", sep = ""))
## Inventory Management
### Linear regression on package size cost vs quantity wastage
inventory_recipe_df <- read_excel(path = paste(working_directory, "/raw_data/recipe_inventory.xlsx", sep = ""))
#### Check for normality
ggplot(data=inventory_recipe_df, mapping=aes(x=packageWasteQty))+geom_boxplot() + ggtitle("Outlier checker: Package Waste Qty") + labs(x="Package Waste Qty")
#### Removal of records with 0 as its value
inventory_recipe_qt_df <-inventory_recipe_df %>% filter(packageWasteQty != 0)
ggplot(data=inventory_recipe_qt_df, mapping=aes(x=packageWasteQty)) + geom_boxplot() + ggtitle("Outlier checker: Package Waste Qty") + labs(x="Package Waste Qty")

#### Check for normality
ggplot(data=inventory_recipe_df, mapping=aes(x=packageWasteCost))+geom_boxplot() + ggtitle("Outlier checker: Package Waste Cost") + labs(x="Package Waste Cost")
#### Removal of records with 0 as its value
inventory_recipe_ct_df <-inventory_recipe_df %>% filter(packageWasteCost != 0)
ggplot(data=inventory_recipe_ct_df, mapping=aes(x=packageWasteCost))+geom_boxplot() + ggtitle("Outlier checker: Package Waste Cost") + labs(x="Package Waste Cost")

##### Log Transform
inventory_recipe_lm_df <- inventory_recipe_df
inventory_recipe_lm_df <- inventory_recipe_lm_df %>% filter(packageWasteQty != 0) 
inventory_recipe_lm_df <- inventory_recipe_lm_df %>% filter(packageWasteCost != 0)
inventory_recipe_lm_df <- mutate(inventory_recipe_lm_df, logPackageWasteQty = log(packageWasteQty))
inventory_recipe_lm_df <- mutate(inventory_recipe_lm_df, logPackageWasteCost = log(packageWasteCost))
inventory_recipe_lm_df <- inventory_recipe_lm_df %>% filter( if_all(everything(),~ !(is.infinite(.) & . < 0) ) )

linear_model <- lm(packageWasteQty ~ packageWasteCost , data = inventory_recipe_lm_df)
summary(linear_model)
#### Determining confidence and validity of the regression by plotting the distribution of the residuals
residual_df <- data.frame(
  residuals = linear_model$residuals 
)

ggplot(residual_df, aes(sample = residuals)) +
  stat_qq(alpha = 0.7, colour = "#2C77B0") +
  stat_qq_line(colour = "red") +
  labs(title = "QQ Plot of Residuals",
       x = "Theoretical quantiles",
       y = "Residuals") +
  theme_minimal(base_size = 12)

### ABC analysis
inventory_recipe_df <- inventory_recipe_df %>%  mutate(annualQty  = inventoryUsed * (365 / 30), annualCost = unitPrice * annualQty)

inventory_recipe_df %>% arrange(desc(annualCost)) %>% 
  mutate(rank = row_number(),
         cumPct = cumsum(annualCost) / sum(annualCost)) %>%   
  ggplot(aes(rank, cumPct)) +
  geom_line(colour = "#2C77B0") +
  scale_y_continuous(labels = percent_format()) +           
  labs(title = "Pareto curve of annual ingredient spend",
       x = "Ingredients ranked by spend",
       y = "Cumulative share of total cost") +
  theme_minimal(base_size = 12)

ggplot(inventory_recipe_df, aes(x = rank)) +
  # bars for annualCost
  geom_col(aes(y = annualCost), fill = "#E6553F") +
  
  # line for cumulative pct (rescaled to primary y)
  geom_line(aes(y = cumPct * max(annualCost)), colour = "#2C77B0", size = 1) +
  geom_point(aes(y = cumPct * max(annualCost)), colour = "#2C77B0", size = 1.5) +
  
  # primary y for cost; secondary for pct
  scale_y_continuous(
    name = "Annual cost (MOP)",
    labels = comma,
    sec.axis = sec_axis(
      ~ . / max(df$annualCost),
      name = "Cumulative share of total cost",
      labels = percent_format()
    )
  ) +
  
  # show only top???N ingredient names on the y axis
  scale_x_continuous(
    breaks = df$rank[1:10],             
    labels = df$ingredientName[1:10]
  ) + labs( title = "Pareto Analysis: Annual Cost and Cumulative Share by Ingredient", x = "Ingredient (ranked by annual cost)") +
  coord_flip() +          
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y.right = element_text(colour = "#2C77B0"),
    axis.text.y         = element_text(size = 9)
  )

inventory_cum_df <- inventory_recipe_df %>% arrange(desc(annualCost)) %>% mutate(
    cumPct = cumsum(annualCost) / sum(annualCost),
    grades = case_when(
    cumPct <= 0.80 ~ "A",
    cumPct <= 0.95 ~ "B",
    TRUE           ~ "C"
  )
) %>% filter(grades == "A")

top_4_A_grade <- inventory_cum_df %>% group_by(grades) %>% slice_max(order_by = annualCost, n = 5, with_ties = FALSE) %>% ungroup()

ggplot(top_4_A_grade, aes(x = reorder(ingredientName, annualCost), y = annualCost, fill = grades)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ grades, scales = "free_y", nrow = 1) +
  scale_y_continuous(labels = comma) +
  labs(title = "Top 4 Items Ingredients by Annual Spend",
       x = NULL,
       y = "Annual cost (MOP)") +
  theme_minimal(base_size = 12)

## Customer Segmentation
### Apriori to create set meals
translated_transaction_order_apriori_data <- translated_transaction_order_data
translated_transaction_order_apriori_data$`Dishes name` <- str_to_lower(translated_transaction_order_apriori_data$`Dishes name`)
translated_transaction_order_apriori_data$`Bill Type` <- str_to_lower(translated_transaction_order_apriori_data$`Bill Type`)
transaction_data <- translated_transaction_order_apriori_data %>% filter(!str_detect(`Dishes name`, regex("\\b(service\\s*fee|delivery\\s*fee|bag|box)\\b", ignore_case = TRUE)))

transaction_data <- transaction_data %>% filter(`Bill Type` %in% c("dine-in", "takeout"))
transaction_data <- cbind(transaction_data$`Bill Number`, transaction_data$`Dishes code`) %>% as.data.frame()
colnames(transaction_data) <- c("transactionID", "dishID")

write.csv(transaction_data, paste(working_directory, "/raw_data/association_transactions.csv", sep = ""), quote = FALSE, row.names = FALSE)
setwd(file.path(working_directory, "raw_data"))
association_sale_trans <- read.transactions("association_transactions.csv", format = "single", sep = ",", cols=c(1,2), header = TRUE)

mapping <- translated_transaction_order_apriori_data[,c("Dishes code", "Dishes name")]
mapping <- unique(mapping)

lookupItemDescription <- function(dishCode){
  return (paste(dishCode,"_", (mapping[mapping$`Dishes code` == dishCode, "Dishes name"][1]), sep = ""))
}

association_sale_trans <- read.transactions("association_transactions.csv", format = "single", sep = ",", cols=c(1,2), header = TRUE)
itemLabels(association_sale_trans)
itemLabels(association_sale_trans) <- sapply(itemLabels(association_sale_trans), lookupItemDescription)

#### Generating rules
association_rules <- apriori(association_sale_trans, parameter = list(supp = 0.006, conf = 0.7, target = "rules", maxlen = 10))
association_rules_df <- as(association_rules, "data.frame")
sort_association_rules_df <- association_rules_df %>% arrange(desc(confidence))
head(sort_association_rules_df)

####  Generating frequent item set
frequent_itemset <- apriori(association_sale_trans, parameter = list(supp = 0.006, conf = 0.7, target = "frequent itemsets", maxlen = 10))
frequent_itemset_df <-  as(frequent_itemset, "data.frame")
sort_frequent_itemset_df <- frequent_itemset_df %>% arrange(desc(support))
head(sort_frequent_itemset_df)
freq_vec <- itemFrequency(association_sale_trans, type = "absolute")

freq_df <- data.frame(
  item  = names(freq_vec),
  count = as.numeric(freq_vec)
) %>% arrange(desc(count)) %>% slice_head(n = 10) %>%  mutate(item = reorder(item, count))

ggplot(freq_df, aes(x = item, y = count)) +
  geom_col(fill = "#2C77B0") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = paste("Top", 10, "Items by Transaction Frequency"),
       x = NULL,
       y = "Item frequency") +
  theme_minimal(base_size = 12)








