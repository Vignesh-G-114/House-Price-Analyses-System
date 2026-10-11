
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

# Sample Dataset
initial_houses <- data.frame(
  House_ID = 1:20,
  Area = c(800,1000,1200,1500,1800,2000,1100,1400,1600,2200,
           2500,900,1300,1700,1900,2100,2400,1250,1550,2300),
  Bedrooms = c(1,2,2,3,3,4,2,3,3,4,5,1,2,3,3,4,4,2,3,4),
  Age = c(5,8,10,4,6,3,12,7,5,2,4,15,9,6,8,3,2,11,5,4),
  Location = c("City","Suburb","City","City","Suburb",
               "City","Suburb","City","Suburb","City",
               "City","Suburb","City","Suburb","City",
               "City","Suburb","City","Suburb","City"),
  Price = c(1800000,2500000,3000000,4200000,4800000,
            6500000,2600000,3900000,4100000,7000000,
            8500000,1900000,3300000,4500000,5000000,
            6800000,7500000,3200000,4000000,7800000)
)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "House Price Intelligence"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard",
               icon = icon("dashboard")),
      menuItem("Price Prediction", tabName = "predict",
               icon = icon("calculator")),
      menuItem("EMI Calculator", tabName = "emi",
               icon = icon("money")),
      menuItem("Add House", tabName = "add",
               icon = icon("plus-circle")),
      menuItem("House Records", tabName = "records",
               icon = icon("home"))
    ),
    hr(),
    sliderInput("areaFilter", "Minimum Area",
                min = 300, max = 3000, value = 500),
    sliderInput("priceFilter", "Maximum Price (₹)",
                min = 1000000, max = 10000000,
                value = 10000000, step = 100000),
    sliderInput("bedFilter", "Bedrooms",
                min = 1, max = 5, value = c(1, 5)),
    selectInput("locationFilter", "Location",
                choices = c("All", "City", "Suburb"))
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background: #f2f6fc; }
      .box, .small-box { border-radius: 10px; }
    "))),
    
    tabItems(
      # DASHBOARD
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("totalHouses", width = 3),
                valueBoxOutput("averagePrice", width = 3),
                valueBoxOutput("highestPrice", width = 3),
                valueBoxOutput("modelAccuracy", width = 3)
              ),
              fluidRow(
                box(title = "Area vs Price", width = 6,
                    status = "primary",
                    plotOutput("pricePlot", height = 280)),
                box(title = "Location Comparison", width = 6,
                    status = "success",
                    plotOutput("locationPlot", height = 280))
              ),
              fluidRow(
                box(title = "House Age Analysis", width = 6,
                    status = "warning",
                    plotOutput("agePlot", height = 280)),
                box(title = "Market Insights", width = 6,
                    status = "info",
                    tableOutput("summaryTable"),
                    textOutput("marketInsight"))
              )
      ),
      
      # PREDICTION
      tabItem(tabName = "predict",
              fluidRow(
                box(title = "Predict House Price", width = 6,
                    status = "primary",
                    numericInput("predArea", "Area (sq.ft.)",
                                 1500, min = 300, max = 10000),
                    numericInput("predBedrooms", "Bedrooms",
                                 3, min = 1, max = 10),
                    numericInput("predAge", "House Age",
                                 5, min = 0, max = 100),
                    selectInput("predLocation", "Location",
                                c("City", "Suburb")),
                    actionButton("predictBtn", "Predict Price",
                                 icon = icon("calculator"),
                                 class = "btn-success"),
                    br(), br(),
                    h3(textOutput("predictedPrice")),
                    textOutput("predictionNote")
                ),
                box(title = "Model Performance", width = 6,
                    status = "info",
                    h4("Linear Regression Model"),
                    textOutput("rSquared"),
                    p("R-squared measures how much variation in the sample prices is explained by the model."),
                    p("Predictions are estimates, not guaranteed property values.")
                )
              )
      ),
      
      # EMI CALCULATOR
      tabItem(tabName = "emi",
              fluidRow(
                box(title = "Home Loan EMI Calculator", width = 6,
                    status = "primary",
                    numericInput("loan", "Loan Amount (₹)",
                                 3000000, min = 100000),
                    sliderInput("downPayment", "Down Payment (%)",
                                min = 0, max = 90, value = 20),
                    sliderInput("interest", "Annual Interest Rate (%)",
                                min = 1, max = 20, value = 8, step = 0.25),
                    sliderInput("tenure", "Loan Tenure (Years)",
                                min = 1, max = 30, value = 20)
                ),
                box(title = "EMI Results", width = 6,
                    status = "success",
                    h3(textOutput("emiResult")),
                    h4(textOutput("loanAmountResult")),
                    h4(textOutput("interestResult")),
                    h4(textOutput("totalPaymentResult"))
                )
              )
      ),
      
      # ADD HOUSE
      tabItem(tabName = "add",
              fluidRow(
                box(title = "Add New House", width = 6,
                    status = "primary",
                    numericInput("newArea", "Area (sq.ft.)",
                                 1200, min = 300),
                    numericInput("newBedrooms", "Bedrooms",
                                 2, min = 1, max = 10),
                    numericInput("newAge", "House Age",
                                 5, min = 0),
                    selectInput("newLocation", "Location",
                                c("City", "Suburb")),
                    numericInput("newPrice", "Price (₹)",
                                 3000000, min = 1),
                    actionButton("addHouse", "Add House",
                                 icon = icon("plus"),
                                 class = "btn-success")
                )
              )
      ),
      
      # RECORDS
      tabItem(tabName = "records",
              fluidRow(
                box(title = "House Records and Favourites",
                    width = 12, status = "primary",
                    p("Select a row to add or remove it from favourites."),
                    actionButton("toggleFav", "Toggle Favourite",
                                 icon = icon("heart")),
                    br(), br(),
                    DTOutput("houseTable"),
                    br(),
                    downloadButton("download", "Download Filtered CSV"),
                    downloadButton("downloadFav", "Download Favourites")
                ),
                box(title = "Saved Favourite Houses",
                    width = 12, status = "success",
                    DTOutput("favTable"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  houses <- reactiveVal(initial_houses)
  favourites <- reactiveVal(integer(0))
  prediction <- eventReactive(input$predictBtn, {
    d <- houses()
    fit <- lm(Price ~ Area + Bedrooms + Age + Location, data = d)
    
    nd <- data.frame(
      Area = input$predArea,
      Bedrooms = input$predBedrooms,
      Age = input$predAge,
      Location = factor(input$predLocation,
                        levels = levels(factor(d$Location)))
    )
    
    as.numeric(predict(fit, newdata = nd))
  })
  
  # Update filter limits as dataset grows
  observe({
    d <- houses()
    
    updateSliderInput(session, "areaFilter",
                      max = max(3000, max(d$Area)),
                      value = min(input$areaFilter,
                                  max(3000, max(d$Area))))
    
    updateSliderInput(session, "priceFilter",
                      max = max(10000000, max(d$Price)),
                      value = min(input$priceFilter,
                                  max(10000000, max(d$Price))))
  })
  
  filtered <- reactive({
    d <- houses()
    
    d <- d[d$Area >= input$areaFilter &
             d$Price <= input$priceFilter &
             d$Bedrooms >= input$bedFilter[1] &
             d$Bedrooms <= input$bedFilter[2], ]
    
    if (input$locationFilter != "All") {
      d <- d[d$Location == input$locationFilter, ]
    }
    
    d
  })
  
  # Dashboard cards
  output$totalHouses <- renderValueBox({
    valueBox(nrow(filtered()), "Houses Found",
             icon = icon("home"), color = "blue")
  })
  
  output$averagePrice <- renderValueBox({
    d <- filtered()
    v <- if (nrow(d)) mean(d$Price) else 0
    valueBox(paste0("₹", format(round(v), big.mark = ",")),
             "Average Price", icon = icon("rupee-sign"),
             color = "green")
  })
  
  output$highestPrice <- renderValueBox({
    d <- filtered()
    v <- if (nrow(d)) max(d$Price) else 0
    valueBox(paste0("₹", format(round(v), big.mark = ",")),
             "Highest Price", icon = icon("chart-line"),
             color = "purple")
  })
  
  output$modelAccuracy <- renderValueBox({
    d <- houses()
    fit <- lm(Price ~ Area + Bedrooms + Age + Location, data = d)
    r2 <- summary(fit)$r.squared * 100
    valueBox(paste0(round(r2, 1), "%"),
             "Model R-squared", icon = icon("chart-bar"),
             color = "teal")
  })
  
  # Price chart
  output$pricePlot <- renderPlot({
    d <- filtered()
    validate(need(nrow(d) > 1, "Select at least two houses."))
    
    ggplot(d, aes(Area, Price, color = Location)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", se = FALSE) +
      scale_y_continuous(labels = function(x)
        paste0("₹", format(x, big.mark = ","))) +
      labs(x = "Area (sq.ft.)", y = "Price (₹)") +
      theme_minimal()
  })
  
  # Location comparison
  output$locationPlot <- renderPlot({
    d <- filtered()
    validate(need(nrow(d) > 0, "No matching houses."))
    
    ggplot(d, aes(Location, Price, fill = Location)) +
      geom_boxplot() +
      labs(x = "Location", y = "Price (₹)") +
      theme_minimal()
  })
  
  # Age analysis
  output$agePlot <- renderPlot({
    d <- filtered()
    validate(need(nrow(d) > 0, "No matching houses."))
    
    ggplot(d, aes(Age, Price)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "House Age (years)", y = "Price (₹)") +
      theme_minimal()
  })
  
  output$summaryTable <- renderTable({
    d <- filtered()
    validate(need(nrow(d) > 0, "No matching houses."))
    
    data.frame(
      Metric = c("Minimum Price", "Maximum Price",
                 "Average Area", "Average Bedrooms"),
      Value = c(
        paste0("₹", format(min(d$Price), big.mark = ",")),
        paste0("₹", format(max(d$Price), big.mark = ",")),
        paste(round(mean(d$Area)), "sq.ft."),
        round(mean(d$Bedrooms), 1)
      )
    )
  })
  
  output$marketInsight <- renderText({
    d <- filtered()
    if (!nrow(d)) return("Change the filters to see insights.")
    
    paste("Most expensive house:",
          d$House_ID[which.max(d$Price)],
          "| Average house age:", round(mean(d$Age)), "years")
  })
  
  # Model performance
  output$rSquared <- renderText({
    d <- houses()
    fit <- lm(Price ~ Area + Bedrooms + Age + Location, data = d)
    paste("R-squared:", round(summary(fit)$r.squared, 3))
  })
  
  output$predictedPrice <- renderText({
    req(prediction())
    paste0("Estimated Price: ₹",
           format(round(max(0, prediction())), big.mark = ","))
  })
  
  output$predictionNote <- renderText({
    req(prediction())
    "Estimate based on the current sample dataset."
  })
  
  # EMI calculator
  loanDetails <- reactive({
    principal <- input$loan * (1 - input$downPayment / 100)
    rate <- input$interest / 1200
    months <- input$tenure * 12
    
    emi <- if (rate == 0) {
      principal / months
    } else {
      principal * rate * (1 + rate)^months /
        ((1 + rate)^months - 1)
    }
    
    list(
      principal = principal,
      emi = emi,
      total = emi * months,
      interest = emi * months - principal
    )
  })
  
  output$emiResult <- renderText({
    paste0("Monthly EMI: ₹",
           format(round(loanDetails()$emi), big.mark = ","))
  })
  
  output$loanAmountResult <- renderText({
    paste0("Loan after down payment: ₹",
           format(round(loanDetails()$principal), big.mark = ","))
  })
  
  output$interestResult <- renderText({
    paste0("Total interest: ₹",
           format(round(loanDetails()$interest), big.mark = ","))
  })
  
  output$totalPaymentResult <- renderText({
    paste0("Total repayment: ₹",
           format(round(loanDetails()$total), big.mark = ","))
  })
  
  # Add new house
  observeEvent(input$addHouse, {
    d <- houses()
    
    newRow <- data.frame(
      House_ID = if (nrow(d)) max(d$House_ID) + 1 else 1,
      Area = input$newArea,
      Bedrooms = input$newBedrooms,
      Age = input$newAge,
      Location = input$newLocation,
      Price = input$newPrice
    )
    
    houses(rbind(d, newRow))
    showNotification("House added successfully!")
  })
  
  # Main table
  output$houseTable <- renderDT({
    datatable(filtered(), selection = "single",
              rownames = FALSE,
              options = list(pageLength = 10))
  })
  
  # Favourite toggle
  observeEvent(input$toggleFav, {
    rows <- input$houseTable_rows_selected
    d <- filtered()
    
    if (length(rows) != 1 || rows > nrow(d)) {
      showNotification("Select a house first.", type = "error")
      return()
    }
    
    id <- d$House_ID[rows]
    fav <- favourites()
    
    if (id %in% fav) {
      favourites(setdiff(fav, id))
      showNotification("Removed from favourites.")
    } else {
      favourites(c(fav, id))
      showNotification("Added to favourites.")
    }
  })
  
  output$favTable <- renderDT({
    datatable(
      houses()[houses()$House_ID %in% favourites(), ],
      rownames = FALSE,
      options = list(pageLength = 5)
    )
  })
  
  output$download <- downloadHandler(
    filename = function() paste0("filtered_houses_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered(), file, row.names = FALSE)
  )
  
  output$downloadFav <- downloadHandler(
    filename = function() paste0("favourite_houses_", Sys.Date(), ".csv"),
    content = function(file) {
      d <- houses()
      write.csv(d[d$House_ID %in% favourites(), ],
                file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
