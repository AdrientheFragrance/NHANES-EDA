# Project 2 and 3

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(NHANES)
library(corrplot)
library(viridis)
library(shinythemes)

# Load and clean data
data("NHANESraw")

nhanes_clean <- NHANESraw |>
  filter(!is.na(Gender), !is.na(Age), !is.na(Education)) |>
  mutate(
    HHIncomeMid = ifelse(is.na(HHIncomeMid), median(HHIncomeMid, na.rm = TRUE), HHIncomeMid),
    SleepHrsNight = ifelse(is.na(SleepHrsNight), median(SleepHrsNight, na.rm = TRUE), SleepHrsNight),
    SmokeNow = ifelse(is.na(SmokeNow) | SmokeNow == "No", "No", "Yes"),
    SmokeNow = factor(SmokeNow, levels = c("No", "Yes")),
    Gender = factor(Gender),
    Education = factor(Education),
    HealthGen = factor(HealthGen, levels = c("Excellent", "Very good", "Good", "Fair", "Poor"), ordered = TRUE),
    AlcoholGroup = case_when(
      is.na(AlcoholDay) ~ NA_character_,
      AlcoholDay == 0 ~ "None",
      AlcoholDay <= 2 ~ "Light",
      AlcoholDay <= 5 ~ "Moderate",
      TRUE ~ "Heavy"
    ),
    AlcoholGroup = factor(AlcoholGroup, levels = c("None", "Light", "Moderate", "Heavy")),
    IncomeGroup = case_when(
      is.na(HHIncomeMid) ~ NA_character_,
      HHIncomeMid < 25000 ~ "Low",
      HHIncomeMid < 75000 ~ "Middle",
      TRUE ~ "High"
    ),
    IncomeGroup = factor(IncomeGroup, levels = c("Low", "Middle", "High")),
    PovertyGroup = ntile(Poverty, 3),
    PovertyGroup = factor(PovertyGroup, labels = c("Low", "Medium", "High")),
    HardDrugs = case_when(
      HardDrugs %in% c("No", "Other") ~ "No",
      is.na(HardDrugs) ~ "No",
      TRUE ~ HardDrugs
    ),
    AgeGroup = cut(Age, breaks = c(0, 29, 44, 59, Inf), labels = c("0-29", "30-44", "45-59", "60+"))
  ) |>
  filter(SleepHrsNight <= 16, AlcoholDay <= 20) |>
  filter(!is.na(IncomeGroup), !is.na(PovertyGroup))


# UI
ui <- navbarPage(
  "NHANES EDA Dashboard: The Impact of Socioeconomic Status, Lifestyle Choices, & Demographic Factors on Health",
  theme = shinytheme("flatly"),
  
  # Tab 1
  tabPanel("Socioeconomic & Substance Use",
           sidebarLayout(
             sidebarPanel(
               h4("Filters"),
               checkboxGroupInput("t1_gender", "Gender", choices = levels(nhanes_clean$Gender),
                                  selected = levels(nhanes_clean$Gender)),
               checkboxGroupInput("t1_educ", "Education", choices = levels(nhanes_clean$Education),
                                  selected = levels(nhanes_clean$Education)),
               checkboxGroupInput("t1_poverty", "Poverty Group", choices = levels(nhanes_clean$PovertyGroup),
                                  selected = levels(nhanes_clean$PovertyGroup)),
               br()
             ),
             mainPanel(
               h3("Q1: How does socioeconomic status relate to smoking, alcohol, and drug use?"),
               h4("Bar plot of smoking rates by education level"),
               plotlyOutput("t1_smoke_edu", height = "320px"),
               hr(),
               h4("Violin plot of alcohol consumption by income group"),
               plotlyOutput("t1_alcohol_income", height = "340px"),
               hr(),
               h4("Stacked bar plot of hard drug use by poverty level"),
               plotlyOutput("t1_drug_poverty", height = "320px"),
               hr()
             )
           )
  ),
  
  # Tab 2
  tabPanel("Lifestyle & Health",
           sidebarLayout(
             sidebarPanel(
               h4("Filters"),
               checkboxGroupInput("t2_gender", "Gender", choices = levels(nhanes_clean$Gender),
                                  selected = levels(nhanes_clean$Gender)),
               checkboxGroupInput("t2_smoke", "Smoking status", choices = levels(nhanes_clean$SmokeNow),
                                  selected = levels(nhanes_clean$SmokeNow)),
               checkboxGroupInput("t2_alcgrp", "Alcohol level", choices = levels(nhanes_clean$AlcoholGroup),
                                  selected = levels(nhanes_clean$AlcoholGroup)),
               radioButtons("t2_smooth", "Add smoothing to scatter/jitter?", choices = c("No", "loess", "lm"), selected = "loess"),
               br()
             ),
             mainPanel(
               h3("Q2: How do lifestyle choices relate to self-rated health and sleep duration?"),
               h4("Violin plot of sleep duration by smoking status"),
               plotlyOutput("t2_sleep_smoke", height = "360px"),
               hr(),
               h4("Box plot of self-rated health by alcohol consumption"),
               plotlyOutput("t2_health_alc", height = "360px"),
               hr(),
               h4("Correlogram of lifestyle choices"),
               plotOutput("t2_corr", height = "360px"),
               hr()
             )
           )
  ),
  
 
  # Tab 3
  tabPanel("Demographics & Health",
           sidebarLayout(
             sidebarPanel(
               h4("Filters"),
               checkboxGroupInput("t3_gender", "Gender", choices = levels(nhanes_clean$Gender),
                                  selected = levels(nhanes_clean$Gender)),
               checkboxGroupInput("t3_marital", "Marital status", choices = unique(nhanes_clean$MaritalStatus),
                                  selected = unique(nhanes_clean$MaritalStatus))
             ),
             mainPanel(
               h3("Q3: How do demographic factors influence lifestyle and health outcomes?"),
               h4("Heatmap of average daily alcohol use by age group and marital status"),
               plotlyOutput("t3_heatmap", height = "340px"),
               hr(),
               h4("Scatter plot of sleep vs age (faceted by marital status, colored by gender)"),
               plotlyOutput("t3_sleep_facet", height = "420px"),
             )
           )
  ),
  
  # Summary Tab
  tabPanel("Summary",
           sidebarLayout(
             sidebarPanel(
               selectizeInput(
                 "summary_vars", 
                 "Choose variables (up to 5):", 
                 choices = names(nhanes_clean), 
                 multiple = TRUE, 
                 options = list(maxItems = 5)
               ),
               actionButton("go_sum", "Generate summary", class = "btn-primary"),
               hr(),
               downloadButton("download_all", "Download full cleaned CSV")
             ),
             
             mainPanel(
               h4("Summary output"),
               verbatimTextOutput("summary_text"),
               hr(),
               h4("Distribution plots"),
               plotlyOutput("summary_plots") 
             )
           )
  )
)

# Server
server <- function(input, output, session) {
  
  # Tab 1
  t1_data <- reactive({
    req(input$t1_gender, input$t1_educ, input$t1_poverty)
    nhanes_clean |>
      filter(Gender %in% input$t1_gender,
             Education %in% input$t1_educ,
             PovertyGroup %in% input$t1_poverty)
  })
  
  # Plot 1A
  output$t1_smoke_edu <- renderPlotly({
    df <- t1_data()
    if(nrow(df) == 0){
      p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data for selected filters") + theme_void()
      return(ggplotly(p))
    }
    agg <- df |>
      group_by(Education, SmokeNow) |>
      summarise(n = n(), .groups = "drop") |>
      group_by(Education) |>
      mutate(pct = n / sum(n) * 100)
    
    p <- ggplot(agg, aes(x = Education, y = pct, fill = SmokeNow,
                         text = paste("Education:", Education,
                                      "<br>Smoke:", SmokeNow,
                                      "<br>Percent:", round(pct, 1), "%"))) +
      scale_fill_manual(values = c("No" = "bisque2", "Yes" = "lightsalmon3")) +
      geom_col(position = "stack") +
      labs(x = "Education Level", y = "Proportion of Group (%)", fill = "Smokes") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 35, hjust = 1))
    ggplotly(p, tooltip = "text") |> layout(legend = list(orientation = "h", x = 0.2, y = -0.15))
  })
  
  # Plot 1B
  output$t1_alcohol_income <- renderPlotly({
    df <- t1_data() |> filter(!is.na(AlcoholDay) & AlcoholDay > 0)
    if(nrow(df) == 0){
      p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data for selected filters") + theme_void()
      return(ggplotly(p))
    }
    
    p <- ggplot(df, aes(x = IncomeGroup, y = AlcoholDay)) +
      geom_violin(fill = "indianred", alpha = 0.5) +
      stat_summary(fun = median, geom = "point", size = 3, color = "indianred") +
      labs(x = "Income Group", y = "Alcohol Drinks per Day") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Plot 1C
  output$t1_drug_poverty <- renderPlotly({
    df <- t1_data()
    if(nrow(df) == 0){
      p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data for selected filters") + theme_void()
      return(ggplotly(p))
    }
    
    agg <- df |>
      group_by(PovertyGroup, HardDrugs) |>
      summarise(n = n(), .groups = "drop") |>
      group_by(PovertyGroup) |>
      mutate(pct = n / sum(n) * 100)
    
    p <- ggplot(agg, aes(x = PovertyGroup, y = pct, fill = HardDrugs,
                         text = paste("Poverty:", PovertyGroup, "<br>HardDrugs:", HardDrugs, "<br>Percent:", round(pct, 1), "%"))) +
      scale_fill_manual(values = c("No" = "thistle", "Yes" = "plum4")) + 
      geom_col(position = "stack") +
      labs(x = "Poverty Group", y = "Proportion of Group (%)", fill = "Hard drug use") +
      theme_minimal()
    ggplotly(p, tooltip = "text") |> layout(legend = list(orientation = "h", x = 0.2, y = -0.15))
  })
  
  # Tab 2 
  t2_data <- reactive({
    req(input$t2_gender, input$t2_smoke, input$t2_alcgrp)
    nhanes_clean |>
      filter(Gender %in% input$t2_gender,
             SmokeNow %in% input$t2_smoke,
             AlcoholGroup %in% input$t2_alcgrp)
  })
  
  # Sample points for jitter
  sample_for_points <- function(df, max_n = 2000){
    if(nrow(df) > max_n){
      df |>
        sample_n(max_n)
    } else df
  }
  
  # Plot 2A 
  output$t2_sleep_smoke <- renderPlotly({
    df_full <- t2_data() |> filter(!is.na(SleepHrsNight))
    if(nrow(df_full) == 0){
      p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data for selected filters") + theme_void()
      return(ggplotly(p))
    }
    
    df_points <- sample_for_points(df_full, 1500)
    
    p <- ggplot() +
      geom_violin(data = df_full, aes(x = SmokeNow, y = SleepHrsNight),
                  fill = "lightblue2", trim = FALSE, alpha = 0.7) +
      geom_jitter(data = df_points, aes(x = SmokeNow, y = SleepHrsNight,
                                        text = paste("Gender:", Gender, "<br>Age:", Age)),
                  width = 0.15, alpha = 0.4, size = 0.8) +
      stat_summary(data = df_full, aes(x = SmokeNow, y = SleepHrsNight),
                   fun = median, geom = "point", size = 3, color = "lightblue4")
    
    # add smoothing if requested
    if(input$t2_smooth != "No"){
      p <- p +
        geom_smooth(data = df_full,
                    aes(x = as.numeric(SmokeNow), y = SleepHrsNight),
                    method = input$t2_smooth, se = TRUE, color = "lightblue4")
    }
    
    p <- p + labs(x = "Smoking Status", y = "Sleep hours per night") +
      theme_minimal() + theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Plot 2B
  output$t2_health_alc <- renderPlotly({
    answered <- c("Poor","Fair","Good","Excellent")
    df <- t2_data() |>
      filter(HealthGen %in% answered, !is.na(AlcoholYear))
    
    if(nrow(df) == 0){
      p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data for selected filters") + theme_void()
      return(ggplotly(p))
    }
    
    p <- ggplot(df, aes(x = HealthGen, y = AlcoholYear, fill = HealthGen,
                        text = paste("Health:", HealthGen, "<br>Days Drinking/Year:", AlcoholYear,
                                     "<br>Gender:", Gender))) +
      geom_boxplot(alpha = 0.7) +
      coord_flip() +
      scale_fill_brewer(palette = "YlGnBu", direction = -1) +
      labs(x = "Self-Reported Health", y = "Number of Days Drinking Alcohol in Past Year") +
      theme_minimal() + theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Plot 2C
  output$t2_corr <- renderPlot({
    df <- t3_data() |>
      mutate(SmokeNum = ifelse(SmokeNow == "Yes", 1, 0),
             HealthNum = as.numeric(HealthGen)) |>
      select(SleepHrsNight, AlcoholDay, SmokeNum, HealthNum) |>
      na.omit()
    if(nrow(df) < 3){
      plot.new(); text(0.5, 0.5, "Not enough data for correlogram")
      return()
    }
    corr <- cor(df)
    corrplot(corr, method = "color", col = colorRampPalette(c("darkorange4", "white", "darkgreen"))(200),
             type = "upper", tl.col = "black", tl.cex = 0.9, tl.srt = 45)
  })
  
  # Tab 3 
  t3_data <- reactive({
    req(input$t3_gender, input$t3_marital)
    nhanes_clean |>
      filter(Gender %in% input$t3_gender,
             MaritalStatus %in% input$t3_marital)
  })
  
  # Plot 3A
  output$t3_heatmap <- renderPlotly({
    df <- t3_data() |>
      filter(!is.na(AgeGroup), !is.na(AlcoholDay)) |>
      group_by(AgeGroup, MaritalStatus) |>
      summarise(AvgAlcohol = mean(AlcoholDay, na.rm = TRUE), n = n(), .groups = "drop")
    
    if(nrow(df) == 0){
      p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data for selected filters") + theme_void()
      return(ggplotly(p))
    }
    
    p <- ggplot(df, aes(x = MaritalStatus, y = AgeGroup, fill = AvgAlcohol,
                        text = paste("Marital:", MaritalStatus, "<br>AgeGroup:", AgeGroup, "<br>Avg drinks/day:", round(AvgAlcohol,2), "<br>n:", n))) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "pink", high = "violetred4") +
      labs(x = "Marital status", y = "Age group", fill = "Avg drinks/day") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 25, hjust = 1))
    ggplotly(p, tooltip = "text")
  })
  
  # Plot 3B
  output$t3_sleep_facet <- renderPlotly({
    df_full <- t3_data() |> filter(!is.na(SleepHrsNight), !is.na(Age))
    if(nrow(df_full) == 0){
      p <- ggplot() + annotate("text", x = 1, y = 1, label = "No data for selected filters") + theme_void()
      return(ggplotly(p))
    }
    df_points <- if(nrow(df_full) > 3000) df_full |> sample_n(3000) else df_full
    
    p <- ggplot() +
      geom_jitter(data = df_points, aes(x = Age, y = SleepHrsNight, color = Gender,
                                        text = paste("Age:", Age, "<br>Sleep:", SleepHrsNight, "<br>Gender:", Gender)),
                  width = 0.8, height = 0.25, alpha = 0.5, size = 0.8) +
      geom_smooth(data = df_full, aes(x = Age, y = SleepHrsNight), method = "loess", se = TRUE) +
      facet_wrap(~MaritalStatus) +
      labs(x = "Age", y = "Sleep hours per night") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Summary tab
  observeEvent(input$go_sum, {
    req(input$summary_vars)
    df <- nhanes_clean |> select(all_of(input$summary_vars))
    
    # Summary stats
    output$summary_text <- renderPrint({
      lapply(df, function(x) {
        if(is.numeric(x)) {
          stats <- c(
            Min = min(x, na.rm = TRUE),
            `1st Qu.` = quantile(x, 0.25, na.rm = TRUE),
            Median = median(x, na.rm = TRUE),
            Mean = mean(x, na.rm = TRUE),
            `3rd Qu.` = quantile(x, 0.75, na.rm = TRUE),
            Max = max(x, na.rm = TRUE),
            SD = sd(x, na.rm = TRUE)
          )
          return(stats)
        } else {
          # counts and proportions
          tbl <- table(x)
          prop <- round(prop.table(tbl) * 100, 1)
          cbind(Count = as.vector(tbl), Percent = as.vector(prop))
        }
      })
    })
    
    # Distribution plots
    output$summary_plots <- renderPlotly({
      req(nrow(df) > 0)
      
      plot_list <- lapply(names(df), function(var) {
        if(is.numeric(df[[var]])) {
          p <- ggplot(df, aes(x = .data[[var]])) +
            geom_histogram(bins = 30, fill = "lightsteelblue", color = "steelblue4") +
            labs(x = var, y = "Count") +
            theme_minimal()
        } else {
          p <- ggplot(df, aes(x = .data[[var]])) +
            geom_bar(fill = "lightsteelblue") +
            labs(x = var, y = "Count") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 30, hjust = 1))
        }
        ggplotly(p)
      })
      
      # Arrange multiple plots in one output using subplot
      subplot(plot_list, nrows = ceiling(length(plot_list)/2), shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE)
    })
  })
  
  
  # Download full cleaned dataset
  output$download_all <- downloadHandler(
    filename = function() { paste0("nhanes_cleaned_", Sys.Date(), ".csv") },
    content = function(file) {
      write.csv(nhanes_clean, file, row.names = FALSE)
    }
  )
}

# Run app
shinyApp(ui = ui, server = server)
