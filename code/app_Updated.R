library(shiny)
library(shinythemes)
library(shinyjs)
library(RMySQL)
library(DBI)
library(ggplot2)
library(dplyr)


####################################
# MySQL Connection Function        #
####################################
dbConnectMySQL <- function() {
  dbConnect(
    MySQL(),
    dbname = "nanyangcafe",
    host = "127.0.0.1",
    user = "testuser",
    password = "password",
    port = 3306
  )
}


####################################
# User Interface                   #
####################################
ui <- fluidPage(
  theme = shinytheme("spacelab"),
  useShinyjs(),
  
  navbarPage("Nanyang Cafe",
             
             # --- Delivery Order Entry Tab ---
             tabPanel("Delivery Order Entry",
                      sidebarPanel(
                        HTML("<h3>Delivery Order Entry</h3>"),
                        textInput("receiptNo", "Receipt No:"),
                        selectInput("ingredient", "Ingredient:", choices = c("Select" = ""), selected = ""),
                        numericInput("packagingSize", "Packaging Size (g/ml):", value = 1, min = 0.01, step = 0.01),
                        numericInput("packagingPrice", "Packaging Price ($):", value = 0, min = 0, step = 0.01),
                        textInput("supplierName", "Supplier Name:", ""),
                        dateInput("orderReceivedDate", "Order Received Date:", value = Sys.Date(), format = "dd/mm/yyyy", max = Sys.Date()),
                        selectInput("paymentStatus", "Payment Status:", choices = list("Paid" = "Paid", "Pending" = "Pending", "Overdue" = "Overdue"), selected = "Pending"),
                        actionButton("submitbutton", "Submit", class = "btn btn-primary"),
                        width = 3
                      ),
                      mainPanel(
                        tags$label(h3("Status/Output")),
                        uiOutput("statusMessageDelivery"),
                        tableOutput("tabledata"),
                        width = 9
                      )
             ),
             
             # --- Customer Transaction Entry Tab  ---
             tabPanel("Customer Transaction Entry",
                      sidebarPanel(
                        HTML("<h3>Customer Transaction Entry</h3>"),
                        fileInput("upload", "Upload a CSV file", accept = ".csv"),
                        checkboxInput("header", "Header", TRUE),
                        actionButton("uploadbutton", "Upload", class = "btn btn-primary"),
                        tags$hr(),
                        uiOutput("uploadError"),
                        width = 3
                      ),
                      mainPanel(
                        tags$label(h3("Status/Output")),
                        uiOutput("statusMessageUpload"),
                        tableOutput("files"),
                        width = 9
                      )
             ),
             
             # --- Ingredients Analytics Tab ---
             tabPanel("Ingredients Analytics",
                      sidebarLayout(
                        sidebarPanel(
                          HTML("<h3>Ingredients Analytics</h3>"),
                          selectInput("ingredient_filter", "Select Ingredient:", choices = c("Select" = ""), selected = ""),
                          selectInput("dish_filter", "Select Dish:", choices = c("Select" = ""), selected = ""),
                          width = 3
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Top Ingredients by Usage", plotOutput("topIngredientsPlot")),
                            tabPanel("Ingredient Price Trend", plotOutput("priceTrendPlot")),
                            tabPanel("Ingredient Usage in Selected Dish", plotOutput("ingredientUsagePlot"))
                          ),
                          width = 9
                        )
                      )
             ),
             
             # --- Customers Analytics Tabs ---
             tabPanel("Customers Analytics",
                      mainPanel(tabsetPanel(
                        tabPanel("Top Selling Dishes", plotOutput("topDishesPlot")),
                        tabPanel("Sales Over Time", plotOutput("salesOverTimePlot")),
                        tabPanel("Channel Distribution", plotOutput("channelPlot"))
                      ))),
             
             # --- Forecast Tabs ---
             tabPanel("Forecast", sidebarPanel())
  )
)


####################################
# Server                           #
####################################
server <- function(input, output, session) {
  
  con <- tryCatch(dbConnectMySQL(), error = function(e) {
    showModal(modalDialog(
      title = "Database Connection Error",
      paste("Failed to connect to the database:", e$message),
      easyClose = TRUE
    ))
    NULL
  })
  
  # --- Load dropdown data ---
  ingredients_list <- reactive({
    req(con)
    tryCatch({
      dbGetQuery(con, "SELECT DISTINCT ingredientName FROM nc_inventory ORDER BY ingredientName")$ingredientName
    }, error = function(e) character(0))
  })
  
  dishes_list <- reactive({
    req(con)
    tryCatch({
      dbGetQuery(con, "SELECT DISTINCT dishName FROM nc_dishname ORDER BY dishName")$dishName
    }, error = function(e) character(0))
  })
  
  observe({
    updateSelectInput(session, "ingredient", choices = c("Select" = "", ingredients_list()))
    updateSelectInput(session, "ingredient_filter", choices = c("Select" = "", ingredients_list()))
    updateSelectInput(session, "dish_filter", choices = c("Select" = "", dishes_list()))
  })
  
  # --- Delivery Order Entry Tab ---
  observe({
    enable <- !is.null(input$receiptNo) && input$receiptNo != "" &&
      !is.null(input$ingredient) && input$ingredient != "" &&
      !is.null(input$packagingSize) && input$packagingSize > 0 &&
      !is.null(input$packagingPrice) && input$packagingPrice >= 0 &&
      !is.null(input$supplierName) && input$supplierName != ""
    if (enable) shinyjs::enable("submitbutton") else shinyjs::disable("submitbutton")
  })
  
  observeEvent(input$submitbutton, {
    req(con)
    
    validate_msg <- function(msg) {
      renderUI(tags$span(style = "color: red; font-weight: bold;", HTML(paste0("&#10060; ", msg))))
    }
    
    # Validation checks
    if (input$receiptNo == "") return(output$statusMessageDelivery <- validate_msg("Receipt No cannot be empty."))
    if (input$ingredient == "") return(output$statusMessageDelivery <- validate_msg("Please select an ingredient."))
    if (is.null(input$packagingSize) || input$packagingSize <= 0) return(output$statusMessageDelivery <- validate_msg("Packaging Size must be positive."))
    if (is.null(input$packagingPrice) || input$packagingPrice < 0) return(output$statusMessageDelivery <- validate_msg("Packaging Price must be non-negative."))
    if (input$supplierName == "") return(output$statusMessageDelivery <- validate_msg("Supplier Name cannot be empty."))
    if (!(input$paymentStatus %in% c("Paid", "Pending", "Overdue"))) return(output$statusMessageDelivery <- validate_msg("Invalid payment status."))
    
    # Check duplicate receiptNo (unique key)
    existing_receipts <- dbGetQuery(con, "SELECT receiptNo FROM tb_inventory_order")$receiptNo
    if (input$receiptNo %in% existing_receipts) {
      output$statusMessageDelivery <- validate_msg("Duplicate Receipt No already exists.")
      return()
    }
    
    orderDate <- format(input$orderReceivedDate, "%Y-%m-%d")
    clean <- function(x) gsub("'", "''", x)  # Basic escape
    
    # INSERT query
    query <- sprintf(
      "INSERT INTO tb_inventory_order (receiptNo, ingredient, packagingSize, packagingPrice, supplierName, orderReceivedDate, paymentStatus)
       VALUES ('%s', '%s', %.2f, %.2f, '%s', '%s', '%s')",
      clean(input$receiptNo), clean(input$ingredient),
      input$packagingSize, input$packagingPrice,
      clean(input$supplierName), orderDate, clean(input$paymentStatus)
    )
    
    tryCatch({
      dbExecute(con, query)
      output$statusMessageDelivery <- renderUI({
        tags$span(style = "color: green; font-weight: bold;",
                  HTML("&#9989; Inventory order submitted successfully!"))
      })
    }, error = function(e) {
      output$statusMessageDelivery <- validate_msg(paste("Error inserting data:", e$message))
    })
  })
  
  # Display recent Inventory Orders with corrected column types
  output$tabledata <- renderTable({
    req(con)
    tryCatch({
      dbGetQuery(con, "SELECT receiptNo, ingredient, packagingSize, packagingPrice, supplierName, orderReceivedDate, paymentStatus
                      FROM tb_inventory_order ORDER BY orderReceivedDate DESC LIMIT 10")
    }, error = function(e) {
      data.frame(Error = e$message)
    })
  })
  
  
  # --- Customer Transaction Entry Tab ---
  observeEvent(input$uploadbutton, {
    req(input$upload, con)
    file <- input$upload$datapath
    df <- tryCatch(read.csv(file, header = input$header, stringsAsFactors = FALSE), error = function(e) NULL)
    
    if (is.null(df) || nrow(df) == 0) {
      output$statusMessageUpload <- renderUI({
        tags$span(style = "color: red; font-weight: bold;", HTML("&#10060; Invalid or empty CSV file."))
      })
      return()
    }
    
    required_cols <- c("serialNumber", "date", "shopName", "dishCode", "dishName", "transactionAmount")
    missing <- setdiff(required_cols, names(df))
    if (length(missing) > 0) {
      output$statusMessageUpload <- renderUI({
        tags$span(style = "color: red; font-weight: bold;", HTML(paste0("&#10060; Missing required columns: ", paste(missing, collapse = ", "))))
      })
      return()
    }
    
    # Subset df to required columns only (avoid issues if CSV has extra cols)
    df <- df[, required_cols]
    
    # Validate serialNumber uniqueness against DB
    existing_serials <- dbGetQuery(con, "SELECT serialNumber FROM tb_customer_transactions")$serialNumber
    duplicates <- df$serialNumber %in% existing_serials
    
    if (any(duplicates)) {
      output$uploadError <- renderUI({
        tags$div(style = "color: red; font-weight: bold;",
                 HTML(paste0("&#10060; Duplicate serialNumbers found: ", paste(df$serialNumber[duplicates], collapse = ", "))))
      })
      output$statusMessageUpload <- renderUI({
        tags$span(style = "color: red; font-weight: bold;", HTML("&#10060; Upload failed due to duplicate serial numbers."))
      })
      return()
    }
    
    # Insert into DB
    success <- tryCatch({
      dbWriteTable(con, "tb_customer_transactions", df, append = TRUE, row.names = FALSE)
      TRUE
    }, error = function(e) {
      output$uploadError <- renderUI({
        tags$span(style = "color: red; font-weight: bold;", HTML(paste0("&#10060; Upload error: ", e$message)))
      })
      FALSE
    })
    
    if (success) {
      output$statusMessageUpload <- renderUI({
        tags$span(style = "color: green; font-weight: bold;", HTML(paste0("&#9989; Successfully uploaded ", nrow(df), " records.")))
      })
      output$files <- renderTable({ head(df, 10) })
    }
  })
  
  # --- Ingredients Analytics Tab ---
  # Plot 1 - Top Ingredients by Usage (aggregate packagingSize from orders)
  output$topIngredientsPlot <- renderPlot({
    req(con)
    req(input$ingredient_filter != "" && input$ingredient_filter != "Select")
    query <- sprintf("
      SELECT ingredient, SUM(packagingSize) AS total_usage
      FROM tb_inventory_order
      WHERE ingredient = '%s'
      GROUP BY ingredient
      ORDER BY total_usage DESC
      LIMIT 10
    ", gsub("'", "''", input$ingredient_filter))
    
    df <- tryCatch(dbGetQuery(con, query), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = reorder(ingredient, total_usage), y = total_usage, fill = ingredient)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      coord_flip() +
      theme_minimal() +
      labs(title = "Top Ingredients by Usage", x = "Ingredient", y = "Total Usage (g/ml)")
  })
  
  # Plot 2 - Ingredient Price Trend (packagingPrice over recent 6 months)
  output$priceTrendPlot <- renderPlot({
    req(con)
    req(input$ingredient_filter != "" && input$ingredient_filter != "Select")
    query <- sprintf("
      SELECT DATE_FORMAT(orderReceivedDate, '%%Y-%%m') AS month, AVG(packagingPrice) AS avg_price
      FROM tb_inventory_order
      WHERE ingredient = '%s' AND orderReceivedDate >= DATE_SUB(CURDATE(), INTERVAL 6 MONTH)
      GROUP BY month
      ORDER BY month
    ", gsub("'", "''", input$ingredient_filter))
    
    df <- tryCatch(dbGetQuery(con, query), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = month, y = avg_price, group = 1)) +
      geom_line(color = "#2C7BB6", size = 1.2) +
      geom_point(color = "#2C7BB6", size = 3) +
      theme_minimal() +
      labs(x = "Month", y = "Avg Packaging Price ($)") +
      ggtitle("Ingredient Price Trend")
  })
  
  # Plot 3 - Ingredient Usage in Selected Dish (join recipes and inventory)
  output$ingredientUsagePlot <- renderPlot({
    req(con)
    req(input$dish_filter != "" && input$dish_filter != "Select")
    query <- sprintf("
    SELECT i.ingredientName AS ingredient, r.inventoryUsed AS `usage`
    FROM nc_recipes r
    JOIN nc_inventory i ON r.inventoryId = i.pid
    JOIN nc_dishname d ON r.dishId = d.pid
    WHERE d.dishName = '%s'
                     ", gsub("'", "''", input$dish_filter)
                     )
    
    
    df <- tryCatch(dbGetQuery(con, query), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = reorder(ingredient, usage), y = usage, fill = ingredient)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Ingredient", y = "Usage (Units)") +
      ggtitle(paste("Ingredient Usage in Selected Dish:", input$dish_filter))
  })
  
  # --- Customers Analytics Tabs ---
  # Plot 1 - Top Selling Dishes (by total transactionAmount)
  output$topDishesPlot <- renderPlot({
    req(con)
    query <- "
    SELECT dishName, SUM(transactionAmount) AS total_sales
    FROM tb_customer_transactions
    GROUP BY dishName
    ORDER BY total_sales DESC
    LIMIT 10
  "
    df <- tryCatch(dbGetQuery(con, query), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = reorder(dishName, total_sales), y = total_sales, fill = dishName)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      coord_flip() +
      theme_minimal() +
      labs(title = "Top Selling Dishes", x = "Dish Name", y = "Total Sales ($)")
  })
  
  # Plot 2 - Sales Over Time (monthly total sales)
  output$salesOverTimePlot <- renderPlot({
    req(con)
    query <- "
    SELECT DATE_FORMAT(date, '%Y-%m') AS month, SUM(transactionAmount) AS total_sales
    FROM tb_customer_transactions
    GROUP BY month
    ORDER BY month
  "
    df <- tryCatch(dbGetQuery(con, query), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = month, y = total_sales, group = 1)) +
      geom_line(color = "#1f78b4", size = 1.2) +
      geom_point(color = "#1f78b4", size = 3) +
      theme_minimal() +
      labs(title = "Sales Over Time", x = "Month", y = "Total Sales ($)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Plot 3 - Channel Distribution (sales by shopName)
  output$channelPlot <- renderPlot({
    req(con)
    query <- "
    SELECT shopName, SUM(transactionAmount) AS total_sales
    FROM tb_customer_transactions
    GROUP BY shopName
    ORDER BY total_sales DESC
  "
    df <- tryCatch(dbGetQuery(con, query), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = "", y = total_sales, fill = shopName)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar(theta = "y") +
      theme_minimal() +
      labs(title = "Sales Distribution by Channel", fill = "Shop/Channel", x = NULL, y = NULL) +
      theme(axis.text = element_blank(), axis.ticks = element_blank())
  })

  # --- Disconnect on session end ---
  session$onSessionEnded(function() {
    if (!is.null(con)) dbDisconnect(con)
  })
}


####################################
# Run App                          #
####################################
shinyApp(ui = ui, server = server)
