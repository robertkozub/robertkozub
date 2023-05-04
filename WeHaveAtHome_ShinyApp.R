# Libraries

library(shiny)
library(DBI)
library(RPostgreSQL)
library(DT)

# Database Connection

con <- dbConnect(RPostgres::Postgres(),
                 dbname = "postgres",
                 host = "athomedb.cem1vkxhxwsh.us-east-2.rds.amazonaws.com",
                 port = "5432",
                 user = "bobbyk",
                 password = "bobbykdb")

# Load Tables

allrecipes_combined <- dbGetQuery(con, "SELECT * FROM allrecipes_combined;")
mcdonalds_cleaned <- dbGetQuery(con, "SELECT * FROM mcdonalds_cleaned;")
whataburger_cleaned <- dbGetQuery(con, "SELECT * FROM whataburger_cleaned;")

# UI

ui <- fluidPage(
  titlePanel("We Have...At Home!"),
  tags$style(HTML("
    .total-price {
      font-size: 24px;
      color: darkred;
      font-weight: bold;
    }
  ")),
  sidebarLayout(
    sidebarPanel(
      selectInput("table_selection", "Select a Recipe or Fast Food Chain",
                  choices = c("All Recipes" = "allrecipes_combined",
                              "McDonald's" = "mcdonalds_cleaned",
                              "Whataburger" = "whataburger_cleaned")),
      uiOutput("id_selection_ui")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Ingredients", dataTableOutput("ingredientDetails")),
        tabPanel("HEB Items",
                 fluidRow(
                   column(6, uiOutput("hebItems_ui"))
                 ),
                 dataTableOutput("filteredHebItems"),
                 tags$div(class = "total-price", textOutput("totalPrice"))),
        tabPanel("Item Details", tableOutput("itemDetails"))
      )
    )
  )
)


server <- function(input, output) {
  
  # Render the item selection dropdown based on the selected table
  output$id_selection_ui <- renderUI({
    req(input$table_selection)
    table <- input$table_selection
    id_column <- switch(table,
                        allrecipes_combined = "unique_id",
                        mcdonalds_cleaned = "mcd_id",
                        whataburger_cleaned = "wb_id")
    name_column <- switch(table,
                          allrecipes_combined = "recipe_name",
                          mcdonalds_cleaned = "mcd_name",
                          whataburger_cleaned = "wb_name")
    items <- dbGetQuery(con, paste0("SELECT DISTINCT ", id_column, ", ", name_column, " FROM ", table, " ORDER BY 1;"))
    selectInput("id_selection", "Select an item:", choices = setNames(items[[1]], items[[2]]))
  })
  
  # Render the table of ingredients for the selected item
  output$ingredientDetails <- renderDataTable({
    req(input$table_selection, input$id_selection)
    table <- input$table_selection
    id_column <- switch(table,
                        allrecipes_combined = "unique_id",
                        mcdonalds_cleaned = "mcd_id",
                        whataburger_cleaned = "wb_id")
    ingredients_table <- switch(table,
                                allrecipes_combined = "allrecipes_ingredients",
                                mcdonalds_cleaned = "mcdonalds_ingredients",
                                whataburger_cleaned = "whataburger_ingredients")
    query <- paste0("SELECT ingredient FROM ", ingredients_table, " WHERE ", id_column, " = '", input$id_selection, "';")
    ingredients <- dbGetQuery(con, query)
    datatable(ingredients, options = list(autoWidth = TRUE))
  })
  
  # Render the item details table
  output$itemDetails <- renderTable({
    req(input$table_selection, input$id_selection)
    table <- input$table_selection
    id_column <- switch(table,
                        allrecipes_combined = "unique_id",
                        mcdonalds_cleaned = "mcd_id",
                        whataburger_cleaned = "wb_id")
    query <- paste0("SELECT * FROM ", table, " WHERE ", id_column, " = '", input$id_selection, "';")
    item_details <- dbGetQuery(con, query)
    item_details
  })
  
  # Render the HEB items UI
  output$hebItems_ui <- renderUI({
    req(input$table_selection, input$id_selection)
    table <- input$table_selection
    id_column <- switch(table,
                        allrecipes_combined = "unique_id",
                        mcdonalds_cleaned = "mcd_id",
                        whataburger_cleaned = "wb_id")
    ingredients_table <- switch(table,
                                allrecipes_combined = "allrecipes_ingredients",
                                mcdonalds_cleaned = "mcdonalds_ingredients",
                                whataburger_cleaned = "whataburger_ingredients")
    query <- paste0("SELECT ", id_column, " AS ID, ingredient FROM ", ingredients_table,
                    " WHERE ", id_column, " = '", input$id_selection, "';")
    ingredients <- dbGetQuery(con, query)
    heb_query <- "SELECT item_id, item_name, item_price FROM heb_combined WHERE "
    for (i in 1:nrow(ingredients)) {
      heb_query <- paste0(heb_query, "LOWER(item_name) LIKE LOWER('%", ingredients$ingredient[i], "%')")
      if (i != nrow(ingredients)) {
        heb_query <- paste0(heb_query, " OR ")
      }
    }
    heb_query <- paste0(heb_query, ";")
    heb_items <- dbGetQuery(con, heb_query)
    heb_items_checkboxes <- paste0(heb_items$item_id, ": ", heb_items$item_name, " - $", heb_items$item_price)
    checkboxGroupInput("hebItems_selected", "Select HEB items:", choices = heb_items_checkboxes)
  })
  
  # Render the filtered HEB items table
  output$filteredHebItems <- renderTable({
    req(input$hebItems_selected)
    selected_items <- input$hebItems_selected
    if (is.null(selected_items) || length(selected_items) == 0) {
      return(NULL)
    }
    item_ids <- sapply(selected_items, function(x) unlist(strsplit(x, ":"))[1])
    query <- paste0("SELECT item_id, item_name, item_price FROM heb_combined WHERE item_id IN ('", paste(item_ids, collapse = "','"), "');")
    filtered_heb_items <- dbGetQuery(con, query)
    filtered_heb_items
  })
  
  # Render the total price text output
  output$totalPrice <- renderText({
    req(input$hebItems_selected)
    selected_items <- input$hebItems_selected
    if (is.null(selected_items) || length(selected_items) == 0) {
      return("Total price: $0")
    }
    item_prices <- sapply(selected_items, function(x) {
      price_match <- regmatches(x, regexpr("(?<=\\$)\\d+(\\.\\d{1,2})?", x, perl = TRUE))
      if (length(price_match) > 0) {
        as.numeric(price_match)
      } else {
        0
      }
    })
    total_price <- sum(item_prices)
    paste("Total price:", sprintf("$%.2f", total_price))
  })
}

# Launch Application

shinyApp(ui = ui, server = server)
