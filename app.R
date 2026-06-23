# setwd("C:/Users/akhiv/OneDrive/Desktop/SECRET PROJECT/FluViewPhase2Data")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(forecast)
library(DT)
library(leaflet)
library(leaflet.extras)
library(maps)
library(dplyr)

state_data <- read.csv("ILINet.csv", skip = 1, fill = TRUE)

clean_flu <- state_data[, c("REGION", "YEAR", "WEEK", "X.UNWEIGHTED.ILI")]
colnames(clean_flu) <- c("State", "Year", "Week", "ILI")
clean_flu$ILI <- as.numeric(clean_flu$ILI)
clean_flu$Year <- as.numeric(clean_flu$Year)
clean_flu$Week <- as.numeric(clean_flu$Week)

hospital_data <- tryCatch({
  read.csv(
    "https://raw.githubusercontent.com/covidcaremap/covid19-healthsystemcapacity/master/data/published/us_healthcare_capacity-facility-CovidCareMap.csv",
    check.names = FALSE
  )
}, error = function(e){
  data.frame()
})

if(nrow(hospital_data) > 0){
  hospital_data <- hospital_data[
    !is.na(hospital_data$Latitude) &
      !is.na(hospital_data$Longitude),
  ]
}

population_data <- data.frame(
  State = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
            "Connecticut","Delaware","District of Columbia","Florida","Georgia",
            "Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
            "Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
            "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire",
            "New Jersey","New Mexico","New York","North Carolina","North Dakota",
            "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina",
            "South Dakota","Tennessee","Texas","Utah","Vermont","Virginia",
            "Washington","West Virginia","Wisconsin","Wyoming"),
  Population = c(5108468,733536,7431344,3067732,38965193,5877610,
                 3617176,1031890,678972,22610726,11029227,
                 1435138,1964726,12549689,6862199,3207004,2940546,4526154,
                 4573749,1395722,6180253,7001399,10037261,5737915,
                 2939690,6196156,1132812,1978379,3194176,1402054,
                 9290841,2114371,19571216,10835491,783926,
                 11785935,4053824,4233358,12961683,1095962,5373555,
                 919318,7126489,30503301,3417734,647464,8715698,
                 7812880,1770071,5910955,584057)
)

state_centers <- data.frame(
  State = state.name,
  Longitude = state.center$x,
  Latitude = state.center$y
)

state_centers <- rbind(
  state_centers,
  data.frame(State = "District of Columbia", Longitude = -77.0369, Latitude = 38.9072)
)

risk_level <- function(x){
  if(is.na(x)){
    return("No Data")
  }
  if(x < 3){
    return("Low")
  } else if(x < 6){
    return("Moderate")
  } else{
    return("High")
  }
}

priority_level <- function(x){
  if(is.na(x)){
    return("No Data")
  }
  if(x < 10000000){
    return("Low Priority")
  } else if(x < 30000000){
    return("Medium Priority")
  } else if(x < 60000000){
    return("High Priority")
  } else{
    return("Critical Priority")
  }
}

forecast_h <- function(target){
  if(target == "next4"){
    return(4)
  } else if(target == "next12"){
    return(12)
  } else if(target == "next26"){
    return(26)
  } else if(target == "fallwinter"){
    return(26)
  } else if(target == "fullnext"){
    return(52)
  } else{
    return(4)
  }
}

forecast_summary_value <- function(fc, target){
  values <- as.numeric(fc$mean)
  
  if(length(values) == 0){
    return(NA)
  }
  
  if(target == "fallwinter"){
    return(mean(tail(values, min(13, length(values))), na.rm = TRUE))
  } else if(target == "fullnext"){
    return(mean(values, na.rm = TRUE))
  } else{
    return(values[1])
  }
}

ui <- dashboardPage(
  
  dashboardHeader(title = "Influenza Surveillance Dashboard"),
  
  dashboardSidebar(
    selectInput("state", "Select State:", choices = unique(clean_flu$State), selected = "Ohio"),
    
    selectInput(
      "year",
      "Select Historical Year:",
      choices = sort(unique(clean_flu$Year), decreasing = TRUE),
      selected = max(clean_flu$Year)
    ),
    
    sliderInput(
      "week_range",
      "Select CDC Weeks:",
      min = 1,
      max = 52,
      value = c(1, 52),
      step = 1
    ),
    
    selectInput(
      "forecast_target",
      "Select Forecast Target:",
      choices = c(
        "Next 4 Weeks" = "next4",
        "Next 12 Weeks" = "next12",
        "Next 26 Weeks" = "next26",
        "Upcoming Fall/Winter Season" = "fallwinter",
        "Next Full Flu Season" = "fullnext"
      ),
      selected = "next4"
    ),
    
    selectInput(
      "compare_states",
      "Compare States:",
      choices = unique(clean_flu$State),
      selected = c("Ohio", "California", "Texas"),
      multiple = TRUE
    ),
    
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Dashboard Summary", tabName = "summary", icon = icon("dashboard")),
      menuItem("Forecast", tabName = "forecast", icon = icon("chart-area")),
      menuItem("Model Accuracy", tabName = "accuracy", icon = icon("bullseye")),
      menuItem("Top 10 High Risk States", tabName = "top10", icon = icon("ranking-star")),
      menuItem("State Comparison", tabName = "comparison", icon = icon("chart-line")),
      menuItem("Peak Season Prediction", tabName = "peak", icon = icon("mountain")),
      menuItem("Risk Alerts", tabName = "risk", icon = icon("exclamation-triangle")),
      menuItem("Recommendations", tabName = "recommend", icon = icon("hospital")),
      menuItem("Hospital Preparedness", tabName = "hospital", icon = icon("hospital")),
      menuItem("Population Vulnerability", tabName = "vulnerability", icon = icon("users")),
      menuItem("US GIS Risk Map", tabName = "map", icon = icon("map")),
      menuItem("CDC Data", tabName = "data", icon = icon("table")),
      menuItem("Download Report", tabName = "report", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem("overview",
              fluidRow(
                valueBoxOutput("currentILI"),
                valueBoxOutput("peakILI"),
                valueBoxOutput("nextRisk")
              ),
              fluidRow(
                box(title = "Historical Influenza Trend by Selected Weeks", width = 12, plotOutput("trendPlot"))
              )
      ),
      
      tabItem("summary",
              fluidRow(
                valueBoxOutput("totalStates"),
                valueBoxOutput("lowStates"),
                valueBoxOutput("moderateStates"),
                valueBoxOutput("highStates")
              ),
              fluidRow(
                box(title = "Forecasted National Influenza Risk Summary", width = 12, DTOutput("summaryTable"))
              )
      ),
      
      tabItem("forecast",
              fluidRow(
                valueBoxOutput("forecastMeanBox"),
                valueBoxOutput("forecastPeakBox"),
                valueBoxOutput("forecastPeakWeekBox")
              ),
              fluidRow(
                box(title = "Recent Historical Trend Plus Future Forecast", width = 12, plotOutput("forecastPlot"))
              ),
              fluidRow(
                box(
                  title = "How to Interpret This Forecast",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  p("Black line: last 26 observed CDC weeks used for recent context."),
                  p("Blue line: future predicted influenza activity from the ARIMA model."),
                  p("Red dashed line: point where historical data ends and prediction begins."),
                  p("The week range slider is for viewing historical CDC data. The forecast target predicts future influenza activity from available historical data.")
                )
              )
      ),
      
      tabItem("accuracy",
              fluidRow(
                valueBoxOutput("maeBox"),
                valueBoxOutput("rmseBox"),
                valueBoxOutput("mapeBox")
              ),
              fluidRow(
                box(title = "Model Accuracy Table", width = 12, DTOutput("accuracyTable"))
              ),
              fluidRow(
                box(
                  title = "How Accuracy Is Calculated",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  p("The model is trained on older historical observations and tested on the most recent 4 observed weeks."),
                  p("MAE = average absolute prediction error."),
                  p("RMSE = square-root average squared error; larger errors are penalized more."),
                  p("MAPE = average percent error. Lower values mean better model performance.")
                )
              )
      ),
      
      tabItem("top10",
              fluidRow(
                box(title = "Top 10 Forecasted High-Risk States", width = 12, DTOutput("top10Table"))
              ),
              fluidRow(
                box(title = "Top 10 High-Risk States Chart", width = 12, plotOutput("top10Plot"))
              )
      ),
      
      tabItem("comparison",
              fluidRow(
                box(title = "State Comparison Trend", width = 12, plotOutput("comparisonPlot"))
              ),
              fluidRow(
                box(title = "State Comparison Data", width = 12, DTOutput("comparisonTable"))
              )
      ),
      
      tabItem("peak",
              fluidRow(
                valueBoxOutput("peakILIBox2"),
                valueBoxOutput("peakWeekAheadBox2"),
                valueBoxOutput("peakRiskBox2")
              ),
              fluidRow(
                box(title = "Forecasted Peak Season Table", width = 12, DTOutput("peakTable"))
              ),
              fluidRow(
                box(
                  title = "Peak Season Meaning",
                  width = 12,
                  status = "warning",
                  solidHeader = TRUE,
                  p("This section identifies the highest predicted ILI value inside the selected forecast target."),
                  p("Forecast Week Ahead means how many weeks into the future the predicted peak occurs.")
                )
              )
      ),
      
      tabItem("risk",
              fluidRow(
                box(title = "Forecast Risk Classification", width = 12, DTOutput("riskTable"))
              )
      ),
      
      tabItem("recommend",
              fluidRow(
                box(title = "Public Health Preparedness Recommendation", width = 12,
                    status = "info", solidHeader = TRUE, textOutput("recommendation"))
              )
      ),
      
      tabItem("hospital",
              fluidRow(
                valueBoxOutput("hospitalRisk"),
                valueBoxOutput("preparednessScore"),
                valueBoxOutput("forecastILI")
              ),
              fluidRow(
                box(title = "Hospital Preparedness Actions", width = 12,
                    status = "danger", solidHeader = TRUE, textOutput("hospitalActions"))
              )
      ),
      
      tabItem("vulnerability",
              fluidRow(
                valueBoxOutput("selectedPopulation"),
                valueBoxOutput("vulnerabilityScoreBox"),
                valueBoxOutput("priorityLevelBox")
              ),
              fluidRow(
                box(title = "Population Vulnerability Index by State", width = 12, DTOutput("vulnerabilityTable"))
              )
      ),
      
      tabItem("map",
              fluidRow(
                box(title = "Forecasted US Influenza Risk Heat Map", width = 12, leafletOutput("riskMap", height = 700))
              ),
              fluidRow(
                box(
                  title = "GIS Map Guide",
                  width = 12,
                  status = "success",
                  solidHeader = TRUE,
                  p("Drag the map to move left, right, up, or down."),
                  p("Blue clustered dots show hospital locations."),
                  p("Green transparent circles show population-weighted vulnerability."),
                  p("Use the layer control box to turn map layers on or off.")
                )
              )
      ),
      
      tabItem("data",
              fluidRow(
                box(title = "CDC Historical Dataset - Selected Year and Week Range", width = 12, DTOutput("dataTable"))
              )
      ),
      
      tabItem("report",
              fluidRow(
                box(
                  title = "Download Project Report",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  p("This button downloads a PDF summary of the selected state, forecast, risk level, model accuracy, hospital preparedness recommendation, and population vulnerability."),
                  p("If PDF download fails, install rmarkdown and TinyTeX using: install.packages('rmarkdown'); install.packages('tinytex'); tinytex::install_tinytex()"),
                  downloadButton("downloadReport", "Download PDF Report")
                )
              )
      )
    )
  )
)

server <- function(input, output){
  
  selected_data <- reactive({
    data <- clean_flu[
      clean_flu$State == input$state &
        clean_flu$Year == input$year,
    ]
    
    data <- data[
      data$Week >= input$week_range[1] &
        data$Week <= input$week_range[2],
    ]
    
    data <- data[!is.na(data$ILI), ]
    data
  })
  
  forecast_data_for_state <- reactive({
    data <- clean_flu[clean_flu$State == input$state, ]
    data <- data[!is.na(data$ILI), ]
    data <- data[data$Year <= input$year, ]
    data <- data[order(data$Year, data$Week), ]
    data
  })
  
  selected_model <- reactive({
    req(nrow(forecast_data_for_state()) >= 10)
    auto.arima(ts(forecast_data_for_state()$ILI))
  })
  
  selected_forecast <- reactive({
    h <- forecast_h(input$forecast_target)
    forecast(selected_model(), h = h)
  })
  
  forecast_value <- reactive({
    forecast_summary_value(selected_forecast(), input$forecast_target)
  })
  
  peak_data <- reactive({
    fc_values <- as.numeric(selected_forecast()$mean)
    peak_week <- which.max(fc_values)
    peak_ili <- max(fc_values, na.rm = TRUE)
    data.frame(
      State = input$state,
      Forecast_Target = input$forecast_target,
      Forecast_Week_Ahead = peak_week,
      Predicted_Peak_ILI = round(peak_ili, 2),
      Peak_Risk_Level = risk_level(peak_ili)
    )
  })
  
  accuracy_metrics <- reactive({
    data <- forecast_data_for_state()
    data <- data[!is.na(data$ILI), ]
    
    if(nrow(data) < 20){
      return(data.frame(
        Metric = c("MAE", "RMSE", "MAPE"),
        Value = c(NA, NA, NA)
      ))
    }
    
    test_n <- min(4, floor(nrow(data) * 0.2))
    train_data <- data[1:(nrow(data) - test_n), ]
    test_data <- data[(nrow(data) - test_n + 1):nrow(data), ]
    
    model <- auto.arima(ts(train_data$ILI))
    fc <- forecast(model, h = test_n)
    predicted <- as.numeric(fc$mean)
    actual <- test_data$ILI
    
    mae <- mean(abs(actual - predicted), na.rm = TRUE)
    rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
    mape <- mean(abs((actual - predicted) / actual), na.rm = TRUE) * 100
    
    data.frame(
      Metric = c("MAE", "RMSE", "MAPE"),
      Value = round(c(mae, rmse, mape), 2)
    )
  })
  
  summary_data <- reactive({
    states <- unique(clean_flu$State)
    results <- data.frame(State = character(), Predicted_ILI = numeric(), Risk_Level = character())
    
    for(st in states){
      data <- clean_flu[clean_flu$State == st, ]
      data <- data[!is.na(data$ILI), ]
      data <- data[data$Year <= input$year, ]
      data <- data[order(data$Year, data$Week), ]
      
      if(nrow(data) >= 10){
        model <- auto.arima(ts(data$ILI))
        h <- forecast_h(input$forecast_target)
        fc <- forecast(model, h = h)
        pred <- forecast_summary_value(fc, input$forecast_target)
        
        results <- rbind(
          results,
          data.frame(
            State = st,
            Predicted_ILI = round(pred, 2),
            Risk_Level = risk_level(pred)
          )
        )
      }
    }
    
    results
  })
  
  top10_data <- reactive({
    data <- summary_data()
    data <- data[order(-data$Predicted_ILI), ]
    head(data, 10)
  })
  
  comparison_data <- reactive({
    data <- clean_flu[
      clean_flu$State %in% input$compare_states &
        clean_flu$Year == input$year,
    ]
    
    data <- data[
      data$Week >= input$week_range[1] &
        data$Week <= input$week_range[2],
    ]
    
    data <- data[!is.na(data$ILI), ]
    data
  })
  
  vulnerability_data <- reactive({
    data <- merge(summary_data(), population_data, by = "State", all.x = TRUE)
    data$Vulnerability_Index <- round(data$Population * (data$Predicted_ILI / 100), 0)
    data$Priority_Level <- sapply(data$Vulnerability_Index, priority_level)
    data <- data[order(-data$Vulnerability_Index), ]
    data
  })
  
  selected_population_info <- reactive({
    data <- vulnerability_data()
    data <- data[data$State == input$state, ]
    
    if(nrow(data) == 0){
      data <- data.frame(
        State = input$state,
        Predicted_ILI = NA,
        Risk_Level = "No Data",
        Population = NA,
        Vulnerability_Index = NA,
        Priority_Level = "No Data"
      )
    }
    
    data
  })
  
  output$totalStates <- renderValueBox({
    valueBox(nrow(summary_data()), "States Analyzed", icon = icon("map"), color = "blue")
  })
  
  output$lowStates <- renderValueBox({
    valueBox(sum(summary_data()$Risk_Level == "Low"), "Low Risk States", icon = icon("check-circle"), color = "green")
  })
  
  output$moderateStates <- renderValueBox({
    valueBox(sum(summary_data()$Risk_Level == "Moderate"), "Moderate Risk States", icon = icon("exclamation-circle"), color = "yellow")
  })
  
  output$highStates <- renderValueBox({
    valueBox(sum(summary_data()$Risk_Level == "High"), "High Risk States", icon = icon("warning"), color = "red")
  })
  
  output$summaryTable <- renderDT({
    datatable(summary_data())
  })
  
  output$currentILI <- renderValueBox({
    req(nrow(selected_data()) > 0)
    valueBox(
      paste0(round(tail(selected_data()$ILI, 1), 2), "%"),
      paste("Historical ILI -", input$state),
      icon = icon("virus"),
      color = "blue"
    )
  })
  
  output$peakILI <- renderValueBox({
    req(nrow(selected_data()) > 0)
    valueBox(
      paste0(round(max(selected_data()$ILI, na.rm = TRUE), 2), "%"),
      paste("Historical Peak ILI -", input$state),
      icon = icon("arrow-up"),
      color = "red"
    )
  })
  
  output$nextRisk <- renderValueBox({
    valueBox(
      risk_level(forecast_value()),
      paste("Forecasted Risk -", input$state),
      icon = icon("bell"),
      color = "green"
    )
  })
  
  output$forecastMeanBox <- renderValueBox({
    valueBox(
      paste0(round(forecast_value(), 2), "%"),
      "Forecasted ILI",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$forecastPeakBox <- renderValueBox({
    fc_values <- as.numeric(selected_forecast()$mean)
    valueBox(
      paste0(round(max(fc_values, na.rm = TRUE), 2), "%"),
      "Predicted Peak ILI",
      icon = icon("arrow-up"),
      color = "red"
    )
  })
  
  output$forecastPeakWeekBox <- renderValueBox({
    fc_values <- as.numeric(selected_forecast()$mean)
    valueBox(
      which.max(fc_values),
      "Expected Peak Forecast Week",
      icon = icon("calendar"),
      color = "yellow"
    )
  })
  
  output$maeBox <- renderValueBox({
    data <- accuracy_metrics()
    valueBox(data$Value[data$Metric == "MAE"], "MAE", icon = icon("bullseye"), color = "blue")
  })
  
  output$rmseBox <- renderValueBox({
    data <- accuracy_metrics()
    valueBox(data$Value[data$Metric == "RMSE"], "RMSE", icon = icon("chart-simple"), color = "purple")
  })
  
  output$mapeBox <- renderValueBox({
    data <- accuracy_metrics()
    valueBox(paste0(data$Value[data$Metric == "MAPE"], "%"), "MAPE", icon = icon("percent"), color = "yellow")
  })
  
  output$accuracyTable <- renderDT({
    datatable(accuracy_metrics())
  })
  
  output$top10Table <- renderDT({
    datatable(top10_data())
  })
  
  output$top10Plot <- renderPlot({
    data <- top10_data()
    ggplot(data, aes(x = reorder(State, Predicted_ILI), y = Predicted_ILI)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Top 10 States by Forecasted ILI",
        x = "State",
        y = "Predicted ILI Percentage"
      ) +
      theme_minimal()
  })
  
  output$comparisonPlot <- renderPlot({
    req(length(input$compare_states) >= 1)
    ggplot(comparison_data(), aes(x = Week, y = ILI, group = State)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      facet_wrap(~State, ncol = 1, scales = "free_y") +
      labs(
        title = paste("State Comparison for", input$year),
        subtitle = paste("CDC Weeks", input$week_range[1], "to", input$week_range[2]),
        x = "CDC Week",
        y = "Unweighted ILI Percentage"
      ) +
      theme_minimal()
  })
  
  output$comparisonTable <- renderDT({
    datatable(comparison_data())
  })
  
  output$peakILIBox2 <- renderValueBox({
    data <- peak_data()
    valueBox(paste0(data$Predicted_Peak_ILI, "%"), "Forecasted Peak ILI", icon = icon("mountain"), color = "red")
  })
  
  output$peakWeekAheadBox2 <- renderValueBox({
    data <- peak_data()
    valueBox(data$Forecast_Week_Ahead, "Peak Week Ahead", icon = icon("calendar-days"), color = "yellow")
  })
  
  output$peakRiskBox2 <- renderValueBox({
    data <- peak_data()
    valueBox(data$Peak_Risk_Level, "Peak Risk Level", icon = icon("triangle-exclamation"), color = "purple")
  })
  
  output$peakTable <- renderDT({
    datatable(peak_data())
  })
  
  output$trendPlot <- renderPlot({
    req(nrow(selected_data()) > 0)
    ggplot(selected_data(), aes(x = Week, y = ILI)) +
      geom_line(color = "red", size = 1.2) +
      geom_point(size = 2) +
      labs(
        title = paste("Historical Influenza Trend in", input$state, "-", input$year),
        subtitle = paste("CDC Weeks", input$week_range[1], "to", input$week_range[2]),
        x = "CDC Week",
        y = "Unweighted ILI Percentage"
      ) +
      theme_minimal()
  })
  
  output$forecastPlot <- renderPlot({
    historical_data <- tail(forecast_data_for_state(), 26)
    fc <- selected_forecast()
    forecast_values <- as.numeric(fc$mean)
    
    historical_weeks <- 1:nrow(historical_data)
    future_weeks <- (max(historical_weeks) + 1):(max(historical_weeks) + length(forecast_values))
    
    plot(
      historical_weeks,
      historical_data$ILI,
      type = "l",
      col = "black",
      lwd = 2,
      xlim = c(1, max(future_weeks)),
      ylim = range(c(historical_data$ILI, forecast_values), na.rm = TRUE),
      xlab = "Timeline",
      ylab = "ILI Percentage",
      main = paste("Recent Historical Trend Plus Forecast for", input$state)
    )
    
    points(historical_weeks, historical_data$ILI, pch = 16, col = "black")
    lines(future_weeks, forecast_values, col = "blue", lwd = 2)
    points(future_weeks, forecast_values, pch = 16, col = "blue")
    
    abline(v = max(historical_weeks), lty = 2, col = "red")
    
    legend(
      "topright",
      legend = c("Last 26 Weeks Historical Data", "Forecasted Future Weeks", "Forecast Start"),
      col = c("black", "blue", "red"),
      lty = c(1, 1, 2),
      lwd = c(2, 2, 2),
      bty = "n"
    )
  })
  
  output$riskTable <- renderDT({
    predictions <- as.numeric(selected_forecast()$mean)
    risk_table <- data.frame(
      Forecast_Week = 1:length(predictions),
      Predicted_ILI = round(predictions, 2),
      Risk_Level = sapply(predictions, risk_level)
    )
    datatable(risk_table)
  })
  
  output$recommendation <- renderText({
    risk <- risk_level(forecast_value())
    if(risk == "Low"){
      "Low Risk: Continue routine influenza surveillance, vaccination awareness, and CDC trend monitoring."
    } else if(risk == "Moderate"){
      "Moderate Risk: Increase vaccination messaging, monitor outpatient visits, and prepare clinics for increased flu activity."
    } else if(risk == "High"){
      "High Risk: Strengthen outbreak monitoring, prepare hospital resources, and intensify prevention campaigns."
    } else{
      "No Data: Not enough data available for this selection."
    }
  })
  
  output$hospitalRisk <- renderValueBox({
    valueBox(risk_level(forecast_value()), paste("Hospital Alert -", input$state),
             icon = icon("heart-pulse"), color = "red")
  })
  
  output$preparednessScore <- renderValueBox({
    score <- round(min(forecast_value() * 15, 100))
    valueBox(paste0(score, "/100"), "Preparedness Score", icon = icon("hospital"), color = "purple")
  })
  
  output$forecastILI <- renderValueBox({
    valueBox(paste0(round(forecast_value(), 2), "%"), "Forecasted ILI",
             icon = icon("chart-line"), color = "yellow")
  })
  
  output$hospitalActions <- renderText({
    risk <- risk_level(forecast_value())
    if(risk == "Low"){
      "LOW ALERT: Continue routine surveillance. Maintain vaccination outreach. Monitor CDC updates."
    } else if(risk == "Moderate"){
      "MODERATE ALERT: Increase staffing readiness. Expand flu testing availability. Monitor emergency department visits. Prepare outpatient capacity."
    } else if(risk == "High"){
      "HIGH ALERT: Activate surge planning. Prepare additional beds. Increase testing capacity. Coordinate with local hospitals. Strengthen outbreak response."
    } else{
      "NO DATA: Not enough data available for hospital preparedness recommendation."
    }
  })
  
  output$selectedPopulation <- renderValueBox({
    data <- selected_population_info()
    valueBox(format(data$Population, big.mark = ","), paste("Population -", input$state),
             icon = icon("users"), color = "blue")
  })
  
  output$vulnerabilityScoreBox <- renderValueBox({
    data <- selected_population_info()
    valueBox(format(data$Vulnerability_Index, big.mark = ","), "Vulnerability Index",
             icon = icon("exclamation-triangle"), color = "orange")
  })
  
  output$priorityLevelBox <- renderValueBox({
    data <- selected_population_info()
    valueBox(data$Priority_Level, "Public Health Priority", icon = icon("flag"), color = "red")
  })
  
  output$vulnerabilityTable <- renderDT({
    datatable(vulnerability_data())
  })
  
  output$riskMap <- renderLeaflet({
    states_map <- map("state", fill = TRUE, plot = FALSE)
    map_ids <- sapply(strsplit(states_map$names, ":"), function(x) x[1])
    
    state_lookup <- data.frame(
      map_id = unique(map_ids),
      State = tools::toTitleCase(unique(map_ids))
    )
    
    state_lookup$State[state_lookup$State == "District Of Columbia"] <- "District of Columbia"
    
    map_data <- merge(state_lookup, summary_data(), by = "State", all.x = TRUE)
    map_data <- merge(map_data, population_data, by = "State", all.x = TRUE)
    map_data$Vulnerability_Index <- round(map_data$Population * (map_data$Predicted_ILI / 100), 0)
    
    risk_colors <- ifelse(
      is.na(map_data$Risk_Level),
      "lightgray",
      ifelse(map_data$Risk_Level == "High", "red",
             ifelse(map_data$Risk_Level == "Moderate", "yellow", "green"))
    )
    
    pop_layer <- merge(map_data, state_centers, by = "State", all.x = TRUE)
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light Map") %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Map") %>%
      addPolygons(
        data = states_map,
        fillColor = risk_colors[match(map_ids, map_data$map_id)],
        fillOpacity = 0.65,
        color = "white",
        weight = 1,
        group = "Forecasted Influenza Risk Layer",
        popup = paste0(
          "<b>State:</b> ", map_data$State[match(map_ids, map_data$map_id)],
          "<br><b>Forecast Target:</b> ", input$forecast_target,
          "<br><b>Predicted ILI:</b> ", map_data$Predicted_ILI[match(map_ids, map_data$map_id)], "%",
          "<br><b>Risk Level:</b> ", map_data$Risk_Level[match(map_ids, map_data$map_id)],
          "<br><b>Population:</b> ", format(map_data$Population[match(map_ids, map_data$map_id)], big.mark = ","),
          "<br><b>Vulnerability Index:</b> ", format(map_data$Vulnerability_Index[match(map_ids, map_data$map_id)], big.mark = ",")
        )
      ) %>%
      addCircleMarkers(
        data = pop_layer,
        lng = ~Longitude,
        lat = ~Latitude,
        radius = ~sqrt(Population) / 900,
        color = "darkgreen",
        fillColor = "green",
        fillOpacity = 0.35,
        stroke = TRUE,
        weight = 2,
        group = "Population Vulnerability Layer",
        popup = ~paste0(
          "<b>Population-Weighted Vulnerability</b>",
          "<br><b>State:</b> ", State,
          "<br><b>Population:</b> ", format(Population, big.mark = ","),
          "<br><b>Predicted ILI:</b> ", Predicted_ILI, "%",
          "<br><b>Vulnerability Index:</b> ", format(Vulnerability_Index, big.mark = ",")
        )
      ) %>%
      addCircleMarkers(
        data = hospital_data,
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 4,
        color = "blue",
        fillColor = "blue",
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1,
        group = "Hospital Locations",
        clusterOptions = markerClusterOptions(),
        popup = ~paste0(
          "<b>Hospital:</b> ", Name,
          "<br><b>State:</b> ", State
        )
      ) %>%
      addLayersControl(
        baseGroups = c("Light Map", "Street Map", "Satellite Map"),
        overlayGroups = c("Forecasted Influenza Risk Layer", "Hospital Locations", "Population Vulnerability Layer"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("green", "yellow", "red", "lightgray"),
        labels = c("Low Risk", "Moderate Risk", "High Risk", "No Data"),
        title = "Forecasted Risk"
      ) %>%
      addLegend(
        position = "topright",
        colors = c("blue", "green"),
        labels = c("Hospital Locations", "Population-Weighted Vulnerability"),
        title = "GIS Layers"
      ) %>%
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE) %>%
      addResetMapButton() %>%
      fitBounds(lng1 = -125, lat1 = 24, lng2 = -66, lat2 = 50)
  })
  
  output$dataTable <- renderDT({
    datatable(selected_data())
  })
  
  
  output$downloadReport <- downloadHandler(
    filename = function(){
      paste0("influenza_forecast_report_", input$state, ".pdf")
    },
    content = function(file){
      tempReport <- tempfile(fileext = ".Rmd")
      
      acc <- accuracy_metrics()
      pop <- selected_population_info()
      peak <- peak_data()
      top10 <- top10_data()
      fc <- selected_forecast()
      fc_values <- as.numeric(fc$mean)
      
      forecast_label <- switch(
        input$forecast_target,
        next4 = "Next 4 Weeks",
        next12 = "Next 12 Weeks",
        next26 = "Next 26 Weeks",
        fallwinter = "Upcoming Fall/Winter Season",
        fullnext = "Next Full Flu Season",
        input$forecast_target
      )
      
      ci_values <- data.frame(
        Mean = as.numeric(fc$mean),
        Lower95 = as.numeric(fc$lower[, 2]),
        Upper95 = as.numeric(fc$upper[, 2])
      )
      
      if(input$forecast_target == "fallwinter"){
        rows_to_use <- tail(seq_len(nrow(ci_values)), min(13, nrow(ci_values)))
      } else if(input$forecast_target == "fullnext"){
        rows_to_use <- seq_len(nrow(ci_values))
      } else{
        rows_to_use <- 1
      }
      
      forecast_lower95 <- round(mean(ci_values$Lower95[rows_to_use], na.rm = TRUE), 2)
      forecast_upper95 <- round(mean(ci_values$Upper95[rows_to_use], na.rm = TRUE), 2)
      
      preparedness_score <- round(min(forecast_value() * 15, 100))
      
      preparedness_category <- ifelse(
        preparedness_score <= 30,
        "Routine Monitoring",
        ifelse(
          preparedness_score <= 60,
          "Increased Readiness",
          "Surge Preparedness"
        )
      )
      
      hospital_recommendation <- ifelse(
        risk_level(forecast_value()) == "Low",
        "Continue routine influenza surveillance, maintain vaccination awareness, and monitor weekly CDC ILINet updates.",
        ifelse(
          risk_level(forecast_value()) == "Moderate",
          "Increase staffing readiness, expand flu testing availability, monitor outpatient and emergency department visits, and strengthen vaccination messaging.",
          "Activate surge planning, prepare additional beds, increase testing capacity, coordinate with local hospitals, and strengthen outbreak response."
        )
      )
      
      risk_interpretation <- ifelse(
        risk_level(forecast_value()) == "Low",
        "Predicted influenza activity remains below the moderate-risk threshold. No immediate surge planning is required, but routine surveillance should continue.",
        ifelse(
          risk_level(forecast_value()) == "Moderate",
          "Predicted influenza activity is elevated enough to justify enhanced monitoring, prevention messaging, and early healthcare resource preparation.",
          "Predicted influenza activity is high. Public health teams should prepare for increased healthcare demand and strengthen outbreak response activities."
        )
      )
      
      peak_interpretation <- ifelse(
        risk_level(peak$Predicted_Peak_ILI) == "Low",
        "The forecast suggests that influenza activity is expected to remain relatively stable during the selected forecast period, with no major outbreak signal detected.",
        ifelse(
          risk_level(peak$Predicted_Peak_ILI) == "Moderate",
          "The forecast suggests possible increasing influenza activity. Public health teams should strengthen monitoring and preparedness activities.",
          "The forecast suggests a high predicted influenza peak. Public health and hospital systems should prepare for increased healthcare demand."
        )
      )
      
      top10_md <- paste(
        "| Rank | State/Region | Predicted ILI (%) | Risk Level |",
        "|---:|---|---:|---|",
        paste0(
          "| ", seq_len(nrow(top10)), 
          " | ", top10$State,
          " | ", top10$Predicted_ILI,
          " | ", top10$Risk_Level,
          " |",
          collapse = "\n"
        ),
        sep = "\n"
      )
      
      hist_plot_file <- tempfile(fileext = ".png")
      forecast_plot_file <- tempfile(fileext = ".png")
      top10_plot_file <- tempfile(fileext = ".png")
      
      png(hist_plot_file, width = 900, height = 500)
      if(nrow(selected_data()) > 0){
        print(
          ggplot(selected_data(), aes(x = Week, y = ILI)) +
            geom_line(color = "red", linewidth = 1.1) +
            geom_point(size = 2) +
            labs(
              title = paste("Historical Influenza Trend in", input$state, "-", input$year),
              subtitle = paste("CDC Weeks", input$week_range[1], "to", input$week_range[2]),
              x = "CDC Week",
              y = "Unweighted ILI Percentage"
            ) +
            theme_minimal()
        )
      } else {
        plot.new()
        text(0.5, 0.5, "No historical data available for selected week range.")
      }
      dev.off()
      
      png(forecast_plot_file, width = 900, height = 500)
      historical_data <- tail(forecast_data_for_state(), 26)
      historical_weeks <- 1:nrow(historical_data)
      future_weeks <- (max(historical_weeks) + 1):(max(historical_weeks) + length(fc_values))
      
      plot(
        historical_weeks,
        historical_data$ILI,
        type = "l",
        col = "black",
        lwd = 2,
        xlim = c(1, max(future_weeks)),
        ylim = range(c(historical_data$ILI, fc_values, fc$lower[, 2], fc$upper[, 2]), na.rm = TRUE),
        xlab = "Timeline",
        ylab = "ILI Percentage",
        main = paste("Recent Historical Trend Plus Forecast for", input$state)
      )
      points(historical_weeks, historical_data$ILI, pch = 16, col = "black")
      polygon(
        c(future_weeks, rev(future_weeks)),
        c(as.numeric(fc$lower[, 2]), rev(as.numeric(fc$upper[, 2]))),
        col = rgb(0.2, 0.4, 1, 0.15),
        border = NA
      )
      lines(future_weeks, fc_values, col = "blue", lwd = 2)
      points(future_weeks, fc_values, pch = 16, col = "blue")
      abline(v = max(historical_weeks), lty = 2, col = "red")
      legend(
        "topright",
        legend = c("Last 26 Weeks Historical Data", "Forecasted Future Weeks", "95% Prediction Interval", "Forecast Start"),
        col = c("black", "blue", "blue", "red"),
        lty = c(1, 1, 1, 2),
        lwd = c(2, 2, 8, 2),
        bty = "n"
      )
      dev.off()
      
      png(top10_plot_file, width = 900, height = 500)
      print(
        ggplot(top10, aes(x = reorder(State, Predicted_ILI), y = Predicted_ILI)) +
          geom_col() +
          coord_flip() +
          labs(
            title = "Top 10 States/Regions by Forecasted ILI",
            x = "State/Region",
            y = "Predicted ILI Percentage"
          ) +
          theme_minimal()
      )
      dev.off()
      
      hist_plot_path <- normalizePath(hist_plot_file, winslash = "/", mustWork = TRUE)
      forecast_plot_path <- normalizePath(forecast_plot_file, winslash = "/", mustWork = TRUE)
      top10_plot_path <- normalizePath(top10_plot_file, winslash = "/", mustWork = TRUE)
      
      report_text <- paste0(
        "---
title: 'Influenza Surveillance Forecasting and Public Health Early Warning System'
subtitle: 'Forecast Report - ", input$state, "'
output:
  pdf_document:
    toc: true
    number_sections: true
geometry: margin=1in
---

# Executive Summary

This report summarizes influenza surveillance forecasts, risk assessment, model performance, hospital preparedness indicators, and population vulnerability for **", input$state, "**.

Based on the selected forecast target, the predicted influenza-like illness (ILI) percentage is **", round(forecast_value(), 2), "%**, with a 95% prediction interval of **", forecast_lower95, "% to ", forecast_upper95, "%**. This corresponds to a **", risk_level(forecast_value()), "** forecasted risk classification.

The predicted peak ILI during the selected forecast period is **", peak$Predicted_Peak_ILI, "%**, expected approximately **", peak$Forecast_Week_Ahead, " week(s)** after the latest available CDC surveillance week.

Forecast uncertainty was estimated using 95% prediction intervals generated from the ARIMA model. Actual influenza activity may vary because of emerging outbreaks, vaccination uptake, healthcare-seeking behavior, reporting delays, and changes in public health interventions.

# Project Objectives

1. Forecast short-term and seasonal influenza activity using historical surveillance data.
2. Identify states or regions with higher predicted influenza activity.
3. Support hospital preparedness and resource planning.
4. Estimate population-weighted vulnerability based on forecasted influenza activity.
5. Provide public health decision support through tables, charts, GIS mapping, and preparedness recommendations.

# Forecast Methodology

Historical CDC ILINet surveillance data were modeled using Auto ARIMA time-series forecasting.

The model automatically selected the best combination of autoregressive, differencing, and moving-average parameters based on the available surveillance trend for the selected state or region.

Forecasts were generated for the selected forecast target, and 95% prediction intervals were calculated to quantify uncertainty around the predicted influenza activity.

# Selected Dashboard Inputs

| Input | Selected Value |
|---|---|
| State/Region | ", input$state, " |
| Historical Year | ", input$year, " |
| CDC Week Range | ", input$week_range[1], " to ", input$week_range[2], " |
| Forecast Target | ", forecast_label, " |

# Historical Trend Chart

This chart shows observed CDC ILINet influenza-like illness activity for the selected state or region, year, and CDC week range.

![](", hist_plot_path, ")

# Forecast Results

| Indicator | Value |
|---|---:|
| Forecasted ILI | ", round(forecast_value(), 2), "% |
| 95% Prediction Interval | ", forecast_lower95, "% to ", forecast_upper95, "% |
| Forecasted Risk Level | ", risk_level(forecast_value()), " |
| Predicted Peak ILI | ", peak$Predicted_Peak_ILI, "% |
| Expected Peak Timing | ", peak$Forecast_Week_Ahead, " week(s) after latest available CDC week |
| Peak Risk Level | ", peak$Peak_Risk_Level, " |

**Peak Season Interpretation:** ", peak_interpretation, "

# Forecast Chart

This chart displays the last 26 observed weeks followed by the forecasted future weeks. The shaded blue band represents the 95% prediction interval.

![](", forecast_plot_path, ")

# Risk Interpretation

**Risk Category:** ", risk_level(forecast_value()), "

", risk_interpretation, "

Risk thresholds used in this dashboard:

| Risk Level | Forecasted ILI |
|---|---|
| Low | Less than 3% |
| Moderate | 3% to less than 6% |
| High | 6% or higher |

# Public Health Action Matrix

| Risk Level | Public Health Action | Hospital/Resource Action |
|---|---|---|
| Low | Continue routine influenza surveillance and vaccination awareness. | Maintain normal staffing and routine monitoring. |
| Moderate | Increase vaccination outreach, monitor outpatient trends, and communicate prevention messages. | Prepare testing supplies, review staffing readiness, and monitor ED visits. |
| High | Strengthen outbreak monitoring and intensify public health communication. | Activate surge planning, prepare additional beds, and coordinate with local hospitals. |

# Model Performance

The ARIMA model was evaluated by training on older observations and testing on the most recent observed weeks.

| Metric | Value |
|---|---:|
| MAE | ", acc$Value[acc$Metric == "MAE"], " |
| RMSE | ", acc$Value[acc$Metric == "RMSE"], " |
| MAPE | ", acc$Value[acc$Metric == "MAPE"], "% |

**Interpretation:** Lower MAE, RMSE, and MAPE values indicate better predictive performance.

# Population Vulnerability Assessment

| Indicator | Value |
|---|---:|
| Population | ", format(pop$Population, big.mark = ","), " |
| Vulnerability Index | ", format(pop$Vulnerability_Index, big.mark = ","), " |
| Public Health Priority | ", pop$Priority_Level, " |

The vulnerability index is calculated by weighting forecasted influenza activity by population size. This helps identify places where even moderate influenza activity may affect a large number of people.

# Hospital Preparedness Assessment

| Indicator | Value |
|---|---:|
| Preparedness Score | ", preparedness_score, " / 100 |
| Preparedness Category | ", preparedness_category, " |
| Hospital Alert Level | ", risk_level(forecast_value()), " |

Preparedness score interpretation:

| Score Range | Meaning |
|---|---|
| 0 to 30 | Routine Monitoring |
| 31 to 60 | Increased Readiness |
| 61 to 100 | Surge Preparedness |

**Recommended Action:** ", hospital_recommendation, "

# Top 10 Forecasted High-Risk States/Regions

", top10_md, "

# Top 10 High-Risk States/Regions Chart

![](", top10_plot_path, ")

# GIS Dashboard Interpretation

The GIS component of the dashboard visualizes forecasted influenza risk across the United States. States are classified into Low, Moderate, and High risk categories based on forecasted ILI values.

Hospital markers support preparedness planning by showing healthcare facility locations, while the population-weighted vulnerability layer helps identify areas where influenza activity could affect larger numbers of residents.

# Future Enhancements

Future versions of this dashboard can be connected directly to CDC FluView and ILINet data feeds for automated weekly surveillance updates instead of relying on manually updated CSV files.

Additional enhancements may include:

- Real-time CDC FluView or ILINet data integration
- County-level influenza forecasting
- Machine learning forecasting models
- Automated outbreak alerts
- Hospital bed utilization prediction
- Vaccination coverage integration
- Cloud deployment through Shiny Server or Posit Connect
- Automated weekly PDF report generation for public health teams

These improvements would support a fully automated public health early warning system capable of providing real-time decision support for healthcare systems and public health agencies.

# Public Health Use

This dashboard can support short-term influenza surveillance, early warning, hospital preparedness planning, resource allocation, vaccination messaging, and population vulnerability assessment.

# Project Information

| Component | Description |
|---|---|
| Data Source | CDC ILINet Influenza Surveillance Data |
| Forecasting Method | ARIMA Time Series Forecasting |
| GIS Components | Influenza risk map, hospital locations, population vulnerability layer |
| Future Automation | CDC FluView or ILINet API connection for weekly data updates |
| Project | Influenza Surveillance Forecasting and Public Health Early Warning System |
| Developer | Akhila Vanga |
| Program | MS Clinical Epidemiology, Kent State University |
"
      )
      
      writeLines(report_text, tempReport)
      rmarkdown::render(tempReport, output_file = file, quiet = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)