# Load required libraries
pacman::p_load(
  shiny, shinyjs, shinydashboard, tidyverse, ggcorrplot, car, ggplot2, dplyr, plotly,
  summarytools, forcats, patchwork, treemap, RColorBrewer, DT, 
  caret, pROC, ROSE, smotefamily, gbm, randomForest, e1071, MASS, coefplot
)



# Define UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Resale HDB Analysis",
    titleWidth = 300
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Univariate Analysis",
        tabName = "univariate",
        icon = icon("chart-bar")  
      ),
      menuItem(
        "Bivariate Analysis",
        tabName = "bivariate",
        icon = icon("line-chart")  
      ),
      menuItem(
        "Correlation Analysis",
        tabName = "correlation",
        icon = icon("link")  
      ),
      menuItem(
        "Resale Price Prediction",
        tabName = "prediction",
        icon = icon("dollar-sign")  
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(
        "
          body {
            font-family: 'Poppins'; 
            font-size: 12px;
          }
          .main-header .logo {
            font-family: 'Poppins';
            font-size: 14px;
          }
          .box-title {
            font-family: 'Poppins';
            font-size: 8px;
          }
          .my-tab-header {
            background-color: #007bff;
            color: white;
            padding: 12px;
          }
          "
      ))
    ),
    
    tabItems(
      # Univariate Analysis Tab------------------------------------------------------------------------------------------------------
      tabItem(
        tabName = "univariate",
        fluidRow(
          tabBox(
            width = 12,
            title = "Univariate Analysis of Data in 2023",
            tabPanel(
              "Observe Multiple Variables",
              fluidRow(
                column(width = 12,
                       fluidRow(
                         column(width = 3,
                                box(
                                  width = NULL,
                                  status = "warning",
                                  radioButtons("display_option", "Display Options",
                                               choices = c("Display by Count" = "count",
                                                           "Display by Density" = "density"))
                                )
                         ),
                         column(width = 9,
                                plotOutput("show_multiple", height = "600px")
                         )
                       )
                )
              )
            ),
            tabPanel(
              "Observe Individual Variable",
              fluidRow(
                column(
                  width = 4,
                  box(
                    width = 12,
                    status = "warning",
                    solidHeader = FALSE,
                    selectInput(
                      "univariate_var",
                      "Select Variable:",
                      choices = c(
                        "Month" = "month",
                        "Town" = "town",
                        "Remaining Lease" = "remaining_lease",
                        "Flat Type" = "flat_type",
                        "Storey Range" = "storey_range",
                        "Flat Model" = "flat_model",
                        "Floor Area (sqm)" = "floor_area_sqm",
                        "Lease Commence Date" = "lease_commence_date",
                        "Resale Price" = "resale_price"
                      ),
                      selected = "town"
                    ),
                    checkboxInput("show_stats", "Display Statistical Summary", value = FALSE)
                  )
                ),
                column(
                  width = 8,
                  box(
                    width = 12,
                    plotlyOutput("univariate_plot", height = "100%"),
                    conditionalPanel(
                      condition = "input.show_stats == true",
                      verbatimTextOutput("summary_stats")
                    )
                  )
                )
              )
            )
          )
        )
      ),
      
      # Bivariate Analysis Tab----------------------------------------------------------------------------------------------------------
      tabItem(
        tabName = "bivariate",
        fluidRow(
          tabBox(
            width = 12,
            title = "Bivariate Analysis",
            tabPanel(
              "Observe Price by Time",
              plotlyOutput("price_by_time")
            ),
            tabPanel(
              "Observe Price by Non-time",
              h2("Observe Data in 2023", style = "text-align: center; color: #007bff; font-family: 'Poppins';"), 
              fluidRow(
                column(3,
                       div(
                         style = "background-color: #f8f9fa; border-radius: 5px; padding: 15px;",
                         h4("Boxplot", style = "text-align:left ; color: #007bff; font-family: 'Poppins';"),
                         radioButtons("boxplot_var", NULL,
                                      choices = c("Town" = "town",
                                                  "Flat Type" = "flat_type",
                                                  "Storey Range" = "storey_range",
                                                  "Flat Model" = "flat_model",
                                                  "Floor Area (sqm)" = "floor_area_sqm",
                                                  "Lease Commence Date" = "lease_commence_date",
                                                  "Remaining Lease" = "remaining_lease"),
                                      selected = "town")
                       )
                ),
                column(9, plotlyOutput("boxplot"))
              ),
              br(),
              fluidRow(
                column(3,
                       div(
                         style = "background-color: #f8f9fa; border-radius: 5px; padding: 15px;",
                         h4("Treemap", style = "text-align:left; color: #007bff; font-family: 'Poppins';"),
                         checkboxGroupInput("treemap_var", NULL,
                                            choices = c("Town" = "town",
                                                        "Flat Type" = "flat_type",
                                                        "Storey Range" = "storey_range",
                                                        "Flat Model" = "flat_model",
                                                        "Floor Area (sqm)" = "floor_area_sqm",
                                                        "Lease Commence Date" = "lease_commence_date",
                                                        "Remaining Lease" = "remaining_lease"),
                                            selected = c("town"))
                       )
                ),
                column(9, plotOutput("treemap"))
              ),
              br(),
              fluidRow(
                column(3,
                       div(
                         style = "background-color: #f8f9fa; border-radius: 5px; padding: 15px;",
                         h4("Heatmap", style = "text-align:left; color: #007bff; font-family: 'Poppins';"),
                         radioButtons("heatmap_var", NULL,
                                      choices = c("Town" = "town", 
                                                  "Flat Type" = "flat_type",
                                                  "Storey Range" = "storey_range",
                                                  "Flat Model" = "flat_model",
                                                  "Floor Area (sqm)" = "floor_area_sqm",
                                                  "Lease Commence Date" = "lease_commence_date",
                                                  "Remaining Lease" = "remaining_lease"),
                                      selected = "town")
                       )
                ),
                column(9, plotlyOutput("heatmap"))
              )
            )
          )
        )
      ),
      
      # Correlation Analysis Tab---------------------------------------------------------------------------------------------------------
      tabItem(
        tabName = "correlation",
        fluidPage(
          fluidRow(
            column(3,
                   wellPanel(
                     style = "background-color: #f5f5f5; padding: 20px;",
                     checkboxGroupInput("variables", "Variables:",
                                        choices = c("Month" = "month",
                                                    "Town" = "town",
                                                    "Flat Type"= "flat_type",
                                                    "Block" = "block",
                                                    "Street Name" = "street_name",
                                                    "Storey Range" = "storey_range",
                                                    "Floor Area (sqm)" = "floor_area_sqm",
                                                    "Flat Model" = "flat_model",
                                                    "Lease Commence Date" = "lease_commence_date",
                                                    "Resale Price" = "resale_price",
                                                    "Remaining Lease" = "remaining_lease"),
                                        selected = c("month", "town", "flat_type", "block", "street_name",
                                                     "storey_range", "floor_area_sqm", "flat_model",
                                                     "lease_commence_date", "resale_price", "remaining_lease"))
                   )
            ),
            column(9,
                   tabsetPanel(
                     tabPanel("Correlation",
                              plotOutput("corrplot", height = "600px")
                     ),
                     tabPanel("Multicollinearity",
                              dataTableOutput("vifTable"),
                              tags$div(
                                style = "font-size: 12px; margin-top: 10px; color: red;",
                                "VIF values were log scaled.",
                                tags$br(),
                                "low collinearity: VIF < 5",
                                tags$br(),
                                "moderate collinearity: 5 < VIF < 10", 
                                tags$br(),
                                "high collinearity: 10 < VIF"
                              )
                     )
                   )
            )
          )
        )
      ),
      # prediction tab----------------------------------------------------------------------------------------------------------------------
      tabItem(
        tabName = "prediction",
        fluidRow(
          column(12, align = "center",
                 h1("Predictive Modeling", style = "color: #007bff; font-family: 'Poppins';")
          )
        ),
        fluidRow(
          column(12,
                 box(
                   title = "Prediction Settings",
                   width = NULL,
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   fluidRow(
                     column(4,
                            checkboxGroupInput("predicting_algorithm", "Predicting Algorithm",
                                               choices = c("Multiple Linear Regression",
                                                           "Random Forest",
                                                           "Boosted Tree"),
                                               selected = NULL)
                     ),
                     column(8,
                            sliderInput("split_ratio", "Training/Test Set Splitting", min = 0.5, max = 1, value = 0.7, step = 0.1)
                     )
                   ),
                   fluidRow(
                     column(4,
                            conditionalPanel(
                              condition = "input.predicting_algorithm.includes('Multiple Linear Regression')",
                              selectInput("step_direction", "Step Direction (Multiple Linear Regression)",
                                          choices = c("both", "backward", "forward"),
                                          selected = "forward")
                            )
                     ),
                     column(4,
                            conditionalPanel(
                              condition = "input.predicting_algorithm.includes('Random Forest') || input.predicting_algorithm.includes('Boosted Tree')",
                              sliderInput("tree_depth", "Tree Depth (Random Forest & Boosted Tree)", min = 1, max = 15, value = 5, step = 1)
                            )
                     ),
                     column(4,
                            conditionalPanel(
                              condition = "input.predicting_algorithm.includes('Random Forest') || input.predicting_algorithm.includes('Boosted Tree')",
                              sliderInput("trees", "Trees (Random Forest & Boosted Tree)", min = 1, max = 100, value = 20, step = 1)
                            )
                     )
                   ),
                   fluidRow(
                     column(4,
                            checkboxGroupInput("model_variables", "Variables",
                                               choices = c("Town" = "town",
                                                           "Flat Type" = "flat_type",
                                                           "Storey Range" = "storey_range",
                                                           "Flat Model" = "flat_model",
                                                           "Floor Area (sqm)" = "floor_area_sqm",
                                                           "Remaining Lease" = "remaining_lease"),
                                               selected = c("town", "flat_type", "storey_range",
                                                            "flat_model", "floor_area_sqm", "remaining_lease"))
                     ),
                     column(8,
                            sliderInput("", "Correlations", min = 0.1, max = 1, value = 0.5, step = 0.1)
                     )
                   ),
                   fluidRow(
                     column(12, offset = 4,
                            actionButton("start_prediction", "Start Prediction")
                     )
                   )
                 )
          )
        ),
        fluidRow(
          column(12,
                 verbatimTextOutput("status_msg")
          )
        ),
        fluidRow(
          column(12,
                 box(
                   title = "Output",
                   width = NULL,
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   conditionalPanel(
                     condition = "input.start_prediction > 0",
                     fluidRow(
                       column(4,
                              conditionalPanel(
                                condition = "input.predicting_algorithm.includes('Multiple Linear Regression')",
                                box(
                                  title = "Multiple Linear Regression",
                                  width = NULL,
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  dataTableOutput("metric_mlr"),
                                  plotOutput("residuals_mlr"),
                                  plotOutput("fit_curve_mlr"),
                                  plotOutput("feature_importance_mlr",width = "100%"),
                                  plotOutput("top_10_feature_plot_mlr"),
                                  tableOutput("top_10_feature_table_mlr")
                                )
                              )
                       ),
                       column(4,
                              conditionalPanel(
                                condition = "input.predicting_algorithm.includes('Random Forest')",
                                box(
                                  title = "Random Forest",
                                  width = NULL,
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  dataTableOutput("metric_rf"),
                                  plotOutput("residuals_rf"),
                                  plotOutput("fit_curve_rf"),
                                  plotOutput("feature_importance_rf",width = "100%"),
                                  plotOutput("top_10_feature_plot_rf"),
                                  tableOutput("top_10_feature_table_rf")
                                )
                              )
                       ),
                       column(4,
                              conditionalPanel(
                                condition = "input.predicting_algorithm.includes('Boosted Tree')",
                                box(
                                  title = "Boosted Tree",
                                  width = NULL,
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  dataTableOutput("metric_bt"),
                                  plotOutput("residuals_bt"),
                                  plotOutput("fit_curve_bt"),
                                  plotOutput("top_10_feature_plot_bt"),
                                  dataTableOutput("top_10_feature_table_bt")
                                )
                              )
                       )
                     )
                   )
                 )
          )
        )
      )
      
    )
  )
)
                                                    



# Define server logic---------------------------------------------------------------------------------------------------------------------------------
server <- function(input, output) {
  # Univariate Analysis: Observe Multiple Variables--------------------------------------------------------------------
  
  # Create reactive values
  rv <- reactiveValues(plot = NULL)
  
  # output for Update reactive values based on radio button selection
  observe({
    if (input$display_option == "count") {
      
      library(ggplot2)
      library(dplyr)
      library(forcats)
      library(patchwork)
      
      # Importing data
      resale_hdb <- readRDS("data/resale_hdb.rds")
      data <- resale_hdb[format(resale_hdb$month, "%Y") == "2023", ]
      
      
      # Grouping by month and calculating quantity of sales for each month
      data_summarized_month <- data %>%
        group_by(month) %>%
        summarise(quantity = n())
      
      # Reordering towns and calculating quantity of sales for each town
      data$town <- fct_reorder(data$town, data$town, function(x) -length(x))
      data_summarized_town <- data %>%
        group_by(town) %>%
        summarise(quantity = n())
      
      # Reordering flat types and calculating quantity of sales for each flat type
      data$flat_type <- fct_reorder(data$flat_type, data$flat_type, function(x) -length(x))
      data_summarized_flat_type <- data %>%
        group_by(flat_type) %>%
        summarise(quantity = n())
      
      # Reordering flat models and calculating quantity of sales for each flat model
      data$flat_model <- fct_reorder(data$flat_model, data$flat_model, function(x) -length(x))
      data_summarized_flat_model <- data %>%
        group_by(flat_model) %>%
        summarise(quantity = n())
      
      # Grouping by storey range and calculating quantity of sales for each storey range
      data_summarized_storey_range <- data %>%
        group_by(storey_range) %>%
        summarise(quantity = n())
      
      # Creating histogram for months 
      p_month <- ggplot(data_summarized_month, aes(x = month, y = quantity)) +
        geom_bar(stat = "identity", fill = "steelblue", color = "black", alpha = 0.5) +
        labs(x = "Month", y = "Count")+
        theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5)) 
      
      # Creating histogram for towns 
      p_town <- ggplot(data_summarized_town, aes(x = town, y = quantity)) +
        geom_bar(stat = "identity", fill = "steelblue", color = "black", alpha = 0.5) +
        labs(x = "Town", y = "Count") + 
        theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5)) 
      
      # Creating histogram for flat types 
      p_flat_type <- ggplot(data_summarized_flat_type, aes(x = flat_type, y = quantity)) +
        geom_bar(stat = "identity", fill = "steelblue", color = "black", alpha = 0.5) +
        labs(x = "Flat Type", y = "Count")+
        theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5)) 
      
      # Creating histogram for storey ranges 
      p_storey_range <- ggplot(data_summarized_storey_range, aes(x = storey_range, y = quantity)) +
        geom_bar(stat = "identity", fill = "steelblue", color = "black", alpha = 0.5) +
        labs(x = "Storey Range", y = "Count") + 
        theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5)) 
      
      # Creating histogram for flat models
      p_flat_model <- ggplot(data_summarized_flat_model, aes(x = flat_model, y = quantity)) +
        geom_bar(stat = "identity", fill = "steelblue", color = "black", alpha = 0.5) +
        labs(x = "Flat Model", y = "Count") + 
        theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5)) 
      
      
      # Grouping by remaining lease and calculating quantity of sales for each remaining lease
      data_summarized_remaining_lease <- data %>%
        group_by(remaining_lease) %>%
        summarise(quantity = n())
      
      # Creating histogram for remaining lease
      p_remaining_lease <- ggplot(data_summarized_remaining_lease, aes(x = remaining_lease, y = quantity)) +
        geom_bar(stat = "identity", fill = "steelblue", color = "black", alpha = 0.5) +
        labs(x = "Remaining Lease", y = "Count") +
        theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5))
      
      # Grouping by floor area sqm and calculating quantity of sales for each floor area sqm
      data_summarized_floor_area_sqm <- data %>%
        group_by(floor_area_sqm) %>%
        summarise(quantity = n())
      
      # Creating histogram for floor area sqm
      p_floor_area_sqm <- ggplot(data_summarized_floor_area_sqm, aes(x = floor_area_sqm, y = quantity)) +
        geom_bar(stat = "identity", fill = "grey", color = "steelblue", alpha = 0.5) +
        labs(x = "Floor Area (sqm)", y = "Count") +
        theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5))
      
      # Grouping by lease commence date and calculating quantity of sales for each lease commence date
      data_summarized_lease_commence_date <- data %>%
        group_by(lease_commence_date) %>%
        summarise(quantity = n())
      
      # Creating histogram for lease commence date
      p_lease_commence_date <- ggplot(data_summarized_lease_commence_date, aes(x = lease_commence_date, y = quantity)) +
        geom_bar(stat = "identity", fill = "steelblue", color = "black", alpha = 0.5) +
        labs(x = "Lease Commence Date", y = "Count") +
        theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5))
      
      
      # Adding the new plots to p_combined
      p_combined <- p_month + p_town + p_flat_type + p_storey_range + p_flat_model + p_remaining_lease + p_floor_area_sqm + p_lease_commence_date
      
      # Assigning the combined plot to rv$plot
      rv$plot <- p_combined
      
      
    } else if (input$display_option == "density") {
      
      resale_hdb <- readRDS("data/resale_hdb.rds")
      data <- resale_hdb[format(resale_hdb$month, "%Y") == "2023", ]
      
      # Create density plots
      p_remaining_lease <- ggplot(data, aes(x = remaining_lease)) +
        geom_density(fill = "forestgreen", alpha = 0.5) +
        labs(x = "Remaining Lease", y = "Density")
      
      p_floor_area_sqm <- ggplot(data, aes(x = floor_area_sqm)) +
        geom_density(fill = "forestgreen", alpha = 0.5) +
        labs(x = "Floor Area (sqm)", y = "Density")
      
      p_lease_commence_date <- ggplot(data, aes(x = lease_commence_date)) +
        geom_density(fill = "forestgreen", alpha = 0.5) +
        labs(x = "Lease Commencement Date", y = "Density")
      
      p_resale_price <- ggplot(data, aes(x = resale_price)) +
        geom_density(fill = "forestgreen", alpha = 0.5) +
        labs(x = "Resale Price", y = "Density")
      
      p_month <- ggplot(data, aes(x = month)) +
        geom_density(fill = "forestgreen", alpha = 0.5) +
        labs(x = "Month", y = "Density")
      
      # Combine density plots
      density_combined <- p_month + p_remaining_lease + p_floor_area_sqm + p_lease_commence_date + p_resale_price
      
      rv$plot <- density_combined
      
      
    }
  })
  
  # Render the plot based on reactive values
  output$show_multiple <- renderPlot({
    rv$plot
  })
  
  #output for Univariate Analysis: Observe individual Variable--------------------------------------------------------------------
  output$univariate_plot <- renderPlotly({
    if (input$univariate_var == "month") {
      library(dplyr)
      library(plotly)
      # group data by month and count quantity for each month
      resale_hdb <- readRDS("data/resale_hdb.rds")
      data <- resale_hdb[format(resale_hdb$month, "%Y") == "2023", ]
      data_summarized <- data %>%
        group_by(month) %>%
        summarise(quantity = n())
      
      # create plotly figure
      fig <- plot_ly(data_summarized, x = ~month, y = ~quantity, type = "scatter", mode = "lines+markers",
                     line = list(color = "blue", width = 1),
                     marker = list(color = "red", size = 8))
      
      # add range slider and customize layout
      fig <- fig %>%
        layout(title = "Resale Quantity by Month",
               xaxis = list(title = "Month in 2023",
                            rangeslider = list(visible = TRUE),
                            tickformat = "%Y-%m",
                            tickangle = 90,
                            tickfont = list(size = 8)),
               yaxis = list(title = "Quantity",
                            tickmode = "linear",
                            tick0 = 0,
                            dtick = 500))
      
      # display the plot
      fig
      
    } else if (input$univariate_var == "town") {
      resale_hdb <- readRDS("data/resale_hdb.rds")
      resale_hdb <- resale_hdb[format(resale_hdb$month, "%Y") == "2023", ]
      
      resale_hdb$town <- fct_reorder(resale_hdb$town, resale_hdb$town, function(x) -length(x))
      
      p2 <- ggplot(resale_hdb, aes(x = town)) +
        geom_bar(fill = "steelblue", color = "black", alpha = 0.5) +
        labs(title = "Resale Quantity by Town", x = "Town", y = "Quantity") +
        theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5))
      
      ggplotly(p2)
      
    } else if (input$univariate_var %in% c("remaining_lease", "floor_area_sqm", "lease_commence_date", "resale_price")) {
      resale_hdb <- readRDS("data/resale_hdb.rds")
      resale_hdb <- resale_hdb[format(resale_hdb$month, "%Y") == "2023", ]
      var_name <- input$univariate_var
      
      p3 <- ggplot(resale_hdb, aes(x = "", y = resale_hdb[[var_name]])) +
        geom_boxplot(color = "blue") +
        geom_violin(fill = "forestgreen", color = "forestgreen", alpha = 0.3) +
        stat_summary(fun = "mean", geom = "point", shape = 18, size = 4, color = "red", position = position_dodge(width = 0.75)) +
        labs(title = paste0("Violin Plot of ", var_name, " (Red dot: Mean Value)"), y = var_name, x = "")
      
      ggplotly(p3)
      
    } else if (input$univariate_var %in% c("flat_type", "storey_range", "flat_model")) {
      resale_hdb <- readRDS("data/resale_hdb.rds")
      resale_hdb <- resale_hdb[format(resale_hdb$month, "%Y") == "2023", ]
      var_name <- input$univariate_var
      resale_hdb[[var_name]] <- fct_reorder(resale_hdb[[var_name]], resale_hdb[[var_name]], function(x) -length(x))
      
      p4 <- ggplot(resale_hdb, aes(x = resale_hdb[[var_name]])) +
        geom_bar(fill = "steelblue", color = "black", alpha = 0.5) +
        labs(title = paste0("Resale Quantity by ", var_name), x = var_name, y = "Quantity") +
        theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5))
      
      ggplotly(p4)
    }
  })
  
  output$summary_stats <- renderPrint({
    resale_hdb <- readRDS("data/resale_hdb.rds")
    resale_hdb <- resale_hdb[format(resale_hdb$month, "%Y") == "2023", ]
    req(input$univariate_var)
    var_name <- input$univariate_var
    
    if (is.numeric(resale_hdb[[var_name]])) {
      descr(resale_hdb[[var_name]])
    } else {
      summary(resale_hdb[[var_name]])
    }
  })
  
  # output for bivariate analysis: observe price by time-------------------------------------------------
  output$price_by_time <- renderPlotly({
    resale_hdb <- readRDS("data/resale_hdb.rds")
    data <- resale_hdb

      median_data <- data %>%
        group_by(month) %>%
        summarise(median_resale_price = median(resale_price))

      fig <- plot_ly(median_data, x = ~month, y = ~median_resale_price, type = "scatter", mode = "lines+markers",
                     line = list(color = "blue", width = 0.3),
                     marker = list(color = "red", size = 3))
      
      fig <- fig %>% 
        layout(title = "Median Resale Price by Time (2013-2023)",
               xaxis = list(title = "Time", rangeslider = list(visible = TRUE)),
               yaxis = list(title = "Median Resale Price"))
      
      fig
  })
  #output for bivariate analysis: observe price by non-time-------------------------------------------------------------------------------------------
  #boxplot------------------------------------------------------------------------------------------------------------------
  output$boxplot <- renderPlotly({
    
    resale_hdb <- readRDS("data/resale_hdb.rds")
    data <- resale_hdb[format(resale_hdb$month, "%Y") == "2023", ]
    
    if (input$boxplot_var %in% c("town", "flat_type", "storey_range", "flat_model")) {
      # Bivariate Analysis: resale price by selected variable
      
      # Calculate median resale price for each category
      median_prices <- data %>%
        group_by(!!sym(input$boxplot_var)) %>%
        summarise(median_price = median(resale_price))
      
      # Reorder the levels of the categorical variable based on the median resale price
      data[[input$boxplot_var]] <- factor(data[[input$boxplot_var]], levels = median_prices[[input$boxplot_var]][order(median_prices$median_price)])
      
      p <- ggplot(data, aes_string(x = input$boxplot_var, y = "resale_price", fill = input$boxplot_var)) +
        geom_boxplot(color = "black", alpha = 0.8) +
        scale_fill_viridis_d(name = input$boxplot_var, option = "plasma") +
        labs(title = paste("Boxplot of Resale Price by", input$boxplot_var, "(sorted by median)"), x = input$boxplot_var, y = "Resale Price") +
        theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5), legend.position = "none")
      
      ggplotly(p)
    }
    else if (input$boxplot_var %in% c("floor_area_sqm", "lease_commence_date", "remaining_lease")) {
      # Create bins for the numerical variable
      data$bins <- cut(data[[input$boxplot_var]], breaks = seq(0, max(data[[input$boxplot_var]]), by = 10))
      # Calculate median resale price for each bin
      median_prices <- data %>%
        group_by(bins) %>%
        summarise(median_price = median(resale_price))
      
      # Reorder the levels of the bins based on the median resale price
      data$bins <- factor(data$bins, levels = median_prices$bins[order(median_prices$median_price)])
      
      # Create the box plot using ggplot2
      p <- ggplot(data, aes(x = bins, y = resale_price, fill = bins)) +
        geom_boxplot(color = "black", alpha = 0.8) +
        scale_fill_viridis_d(name = input$boxplot_var, option = "plasma") +
        labs(
          title = paste("Distribution of Resale Price by", input$boxplot_var),
          x = input$boxplot_var,
          y = "Resale Price"
        ) +
        theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5), legend.position = "none")
      
      ggplotly(p)
    }
    
  })
  
  #heatmap------------------------------------------------------------------------------------------------------------
  output$heatmap <- renderPlotly({
    
    resale_hdb <- readRDS("data/resale_hdb.rds")
    data <- resale_hdb[format(resale_hdb$month, "%Y") == "2023", ]
    
    if (input$heatmap_var %in% c("town", "flat_type", "storey_range", "flat_model")) {
      p <- ggplot(data, aes(x = !!sym(input$heatmap_var), y = resale_price)) +
        geom_bin2d(binwidth = c(5, 10000)) +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        labs(
          title = paste("Heatmap of Resale Price by", input$heatmap_var),
          x = input$heatmap_var,
          y = "Resale Price"
        ) +
        theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5))
      
      ggplotly(p)
    }
    else if (input$heatmap_var %in% c("floor_area_sqm", "lease_commence_date", "remaining_lease")) {
      # Create bins for the numerical variable
      data$bins <- cut(data[[input$heatmap_var]], breaks = seq(0, max(data[[input$heatmap_var]]), by = 10))
      # Calculate median resale price for each bin
      median_prices <- data %>%
        group_by(bins) %>%
        summarise(median_price = median(resale_price))
      
      # Reorder the levels of the bins based on the median resale price
      data$bins <- factor(data$bins, levels = median_prices$bins[order(median_prices$median_price)])
      p <- ggplot(data, aes(x = !!sym(input$heatmap_var), y = resale_price)) +
        geom_bin2d(binwidth = c(5, 10000)) +
        scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Count") +
        scale_x_continuous(breaks = seq(0, max(data[[input$heatmap_var]]), by = 10)) +
        labs(
          title = paste("Heatmap of Resale Price by", input$heatmap_var),
          x = input$heatmap_var,
          y = "Resale Price"
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "right")
      
      ggplotly(p)
      
    }
    
  })
  
  #treemap-----------------------------------------------------------------------------------------------------------
  output$treemap <- renderPlot({
    
    resale_hdb <- readRDS("data/resale_hdb.rds")
    data <- resale_hdb[format(resale_hdb$month, "%Y") == "2023", ]
    
    if (length(input$treemap_var) >= 1) {
      #Choose the top three variables as hierarchical structure
      treemap_vars <- input$treemap_var[1:min(3, length(input$treemap_var))]
      
      #Create a dataframe containing the selected variables, resale_price, and floor_area_sqm
      treemap_data <- data[, c(treemap_vars, "resale_price", "floor_area_sqm")]
      
      #Calculate the total resale_price and median price_per_sqm for each combination
      treemap_data <- treemap_data %>%
        group_by(across(all_of(treemap_vars))) %>%
        summarise(
          Total_Resale_Price = sum(resale_price),
          Median_Price_Per_Sqm = median(resale_price / floor_area_sqm)
        )
      
      # create treemap
      treemap(treemap_data,
              index = treemap_vars,
              vSize = "Total_Resale_Price",
              vColor = "Median_Price_Per_Sqm",
              type = "value",
              title = "HDB Resale Flats by Selected Variables",
              title.legend = "Median Price per Square Meter (S$)"
      )
    }
  })
  
  
  #Output for correlation-----------------------------------------------------------------------------------------------------------------
  resale_hdb <- readRDS("data/resale_hdb.rds")
  data <- resale_hdb[format(resale_hdb$month, "%Y") == "2023", ]
  
  output$corrplot <- renderPlot({
    selected_vars <- input$variables
    
    if (length(selected_vars) > 1) {
      df_selected <- resale_hdb[, selected_vars]
      df_numeric <- as.data.frame(sapply(df_selected, as.numeric))
      corr_matrix <- cor(df_numeric, method = "pearson")
      
      ggcorrplot(corr_matrix, 
                 type = "lower",
                 lab = TRUE,
                 lab_size = 3,
                 method = "circle",
                 colors = c("#6D9EC1", "white", "#E46726"),
                 title = "Correlogram",
                 ggtheme = theme_bw)
    } else {
      plot.new()
      text(0.5, 0.5, "Please select at least two variables.")
    }
  })
  
  
  resale_hdb <- readRDS("data/resale_hdb.rds")
  data <- resale_hdb[format(resale_hdb$month, "%Y") == "2023", ]
  
  output$vifTable <- renderDataTable({
    selected_vars <- input$variables
    if (length(selected_vars) > 1) {
      df_selected <- resale_hdb[, selected_vars]
      df_numeric <- as.data.frame(sapply(df_selected, function(x) log(as.numeric(x) + 1)))  # log-scaled
      vif_values <- car::vif(lm(resale_price ~ ., data = df_numeric[-length(df_numeric)]))
      vif_data <- data.frame(Variable = names(vif_values),
                             VIF = formatC(vif_values, format = "f", digits = 2))
      
    
      vif_data$Evaluation <- cut(vif_values, breaks = c(0, 5, 10, Inf),
                                 labels = c("low collinearity", "moderate collinearity", "high collinearity"), right = FALSE)
      
      vif_data <- dplyr::arrange(vif_data, VIF)  
      
      datatable(vif_data,
                rownames = FALSE,
                options = list(
                  pageLength = 5,
                  searching = TRUE,
                  lengthMenu = list(c(3, 5, 10, -1), c('3', '5', '10', 'All')),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#f5f5f5', 'color': '#333333', 'font-weight': 'bold'});",
                    "}"
                  )
                )
      )
    }
  })
  #Output for prediction-----------------------------------------------------------------------------------------------------------------------------------
  
  # start prediction button--------------------------------------------------------------------------------------
  
  # Respond to the click event of the "Start Prediction" button
  observeEvent(input$start_prediction, {
    
    # Show "modeling..." message
    output$status_msg <- renderUI({
      showNotification("Modeling...", duration = NULL, type = "message")
    })
    
    # Get user-selected algorithms, variables, and other input values
    algorithms <- input$predicting_algorithm
    model_variables <- input$variables
    split_ratio <- input$split_ratio
    step_direction <- input$step_direction
    tree_depth <- input$tree_depth
    trees <- input$trees
    correlations <- input$correlations
    
    
    data_cleaned <- readRDS("data/data_model.rds")
    
    # Split the dataset ---------------------------------------------------------------------------------------------------------------------
    set.seed(123)  # Set random seed to ensure reproducibility
    # Split the dataset into training and testing sets
    train_index <- sample(1:nrow(data_cleaned), split_ratio * nrow(data_cleaned))  
    train_data <- data_cleaned[train_index, ]
    test_data <- data_cleaned[-train_index, ]
    
    # Perform predictive modeling based on user-selected algorithms
    # Multiple Linear Regression--------------------------------------------------------------------------------------------------------------
    if ("Multiple Linear Regression" %in% algorithms) {
      MLR <- lm(resale_price ~ ., data = train_data)
      MLR <- stepAIC(MLR, direction = step_direction)
      
      output$metric_mlr <- renderDataTable({
        # Calculate predicted values
        preds <- predict(MLR, newdata = train_data)
        
        # Use postResample to calculate regression metrics
        reg_metrics <- postResample(preds, train_data$resale_price)
        
        # Convert reg_metrics to a data frame
        reg_metrics_df <- data.frame(Metric = names(reg_metrics),
                                     Value = as.vector(reg_metrics))
        
        # Set number format, keep 4 decimal places
        reg_metrics_df$Value <- format(round(reg_metrics_df$Value, 4), nsmall = 4)
        
        # Use datatable function to present the table
        datatable(reg_metrics_df,
                  options = list(dom = 'i', scrollX = TRUE, autoWidth = TRUE))
      })
      
      output$residuals_mlr <- renderPlot({
        # Residuals vs. Predicted Values plot
        # Use the model to predict the training set data
        predictions <- predict(MLR, train_data)
        
        # Calculate residuals
        residuals <- train_data$resale_price - predictions
        
        # Plot residuals vs. predicted values
        plot(predictions, residuals,
             xlab = "Predicted Values",
             ylab = "Residuals",
             main = "Residuals vs. Predicted Values")
        abline(h = 0, col = "red")  # Add horizontal reference line
      })
      
      output$fit_curve_mlr <- renderPlot({
        # Model fit (scatter plot of observed vs. predicted values, and the fit curve)
        plot(test_data$resale_price, predict(MLR, newdata = test_data), 
             xlab = "Observed", ylab = "Predicted", main = "Observed vs Predicted")
        abline(0, 1, col = "red")  # Add fit curve
      })
      
      output$feature_importance_mlr <- renderPlot({
        coef_df <- data.frame(
          Variable = names(coef(MLR))[-1],  
          Coefficient = coef(MLR)[-1]       
        )
        
        ggplot(coef_df, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          coord_flip() +
          labs(x = "Feature", y = "Coefficient") +
          theme(text = element_text(size = 8),
                axis.text.y = element_text(size = 6))
      })
      
      output$top_10_feature_plot_mlr <- renderPlot({
        coefficients <- coef(MLR)[-1]  # 去掉截距项
        sorted_coefficients <- sort(abs(coefficients), decreasing = TRUE)
        top_10 <- sorted_coefficients[1:10]
        df <- data.frame(Coefficient = names(top_10), Magnitude = top_10)
        df <- df[order(df$Coefficient, decreasing = TRUE), ]
        
        ggplot(df, aes(x = reorder(Coefficient, -Magnitude), y = Magnitude)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          labs(title = "Top 10 Important Features", x = "feature", y = "Coefficient") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      })
      
      output$top_10_feature_table_mlr <- renderTable({
        coefficients <- coef(MLR)[-1] 
        sorted_coefficients <- sort(abs(coefficients), decreasing = TRUE)
        top_10 <- sorted_coefficients[1:10]
        df <- data.frame(Feature = names(top_10), Coefficient = top_10)
        df <- df[order(-df$Coefficient), ]
        colnames(df) <- c("Feature", "Coefficient")  
        df
      })
      
    }
    
    # Random Forest---------------------------------------------------------------------------------------------------------------------
    
    if ("Random Forest" %in% algorithms) {
      RF <- randomForest(resale_price ~ ., 
                         data = train_data, 
                         ntree = trees, 
                         mtry = sqrt(ncol(train_data)),
                         maxnodes = NULL,
                         importance = TRUE)
      
      output$metric_rf <- renderDataTable({
        # Calculate predicted values
        preds <- predict(RF, newdata = train_data)
        
        # Use postResample to calculate regression metrics
        reg_metrics <- postResample(preds, train_data$resale_price)
        
        # Convert reg_metrics to a data frame
        reg_metrics_df <- data.frame(Metric = names(reg_metrics),
                                     Value = as.vector(reg_metrics))
        
        # Set number format, keep 4 decimal places
        reg_metrics_df$Value <- format(round(reg_metrics_df$Value, 4), nsmall = 4)
        
        # Use datatable function to present the table
        datatable(reg_metrics_df,
                  options = list(dom = 'i', scrollX = TRUE, autoWidth = TRUE))
      })
      
      output$residuals_rf <- renderPlot({
        # Residuals vs. Predicted Values plot
        # Use the model to predict the training set data
        predictions <- predict(RF, train_data)
        
        # Calculate residuals
        residuals <- train_data$resale_price - predictions
        
        # Plot residuals vs. predicted values
        plot(predictions, residuals,
             xlab = "Predicted Values",
             ylab = "Residuals",
             main = "Residuals vs. Predicted Values")
        abline(h = 0, col = "red")  # Add horizontal reference line
      })
      
      output$fit_curve_rf <- renderPlot({
        # Model fit (scatter plot of observed vs. predicted values, and the fit curve)
        plot(test_data$resale_price, predict(RF, newdata = test_data), 
             xlab = "Observed", ylab = "Predicted", main = "Observed vs Predicted")
        abline(0, 1, col = "red")  # Add fit curve
      })
      
      output$feature_importance_rf <- renderPlot({
        importance <- importance(RF)

        df <- data.frame(Variable = row.names(importance),
                         Importance = importance[,1])

        df <- df[order(-df$Importance),]

        ggplot(df, aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
          geom_bar(stat = "identity", alpha = 0.8, fill = "steelblue") +
          coord_flip() +
          theme(plot.title = element_text(size = 9, face = "bold"),
                axis.title = element_text(size = 7),
                axis.text = element_text(size = 5),
                legend.position = "none") +
          labs(title = "Variable Importance Plot",
               x = "Variable",
               y = "Importance")
      })
      
      output$top_10_feature_plot_rf <- renderPlot({
        # Get feature importance
        imp <- importance(RF)
        
        # Convert importance scores to a data frame
        imp_df <- data.frame(Feature = row.names(imp), 
                             Importance = imp[, "%IncMSE"])
        
        # Sort by importance scores in descending order
        imp_df <- imp_df[order(-imp_df$Importance), ]  
        
        # Extract the top 10 most important features
        top10 <- head(imp_df, 10) 
        
        # Create a bar plot
        plot <- ggplot(top10, aes(x = reorder(Feature, Importance), y = Importance)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          labs(x = "Feature", y = "Importance", title = "Top 10 Important Features") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        # Display the bar plot
        print(plot)
      })
      
      output$top_10_feature_table_rf <- renderTable({
        # Get feature importance
        imp <- importance(RF)
        
        # Convert importance scores to a data frame
        imp_df <- data.frame(Feature = row.names(imp),
                             Importance = imp[, "%IncMSE"])
        
        # Sort by importance scores in descending order
        imp_df <- imp_df[order(-imp_df$Importance), ]
        
        # Extract the top 10 most important features
        top10 <- head(imp_df, 10)
        colnames(top10)[2] <- "Importance (%IncMSE)"
        # Display the top 10 features in a table
        top10
      })
    }
    
    #boosted tree------------------------------------------------------------------------------------------------------------------
    
    if ("Boosted Tree" %in% algorithms) {

      data_cleaned <- readRDS("data/data_model.rds")
      set.seed(123)
      train_index <- sample(1:nrow(data_cleaned), split_ratio * nrow(data_cleaned))  
      train_data <- data_cleaned[train_index, ]
      test_data <- data_cleaned[-train_index, ]
      
      # Fit Boosted Tree model
      BT <- gbm(resale_price ~ ., data = train_data, distribution = "gaussian",
                         n.trees = trees, interaction.depth = tree_depth, shrinkage = 0.01)
      
      output$metric_bt <- renderDataTable({
        preds <- predict(BT, newdata = train_data)
        reg_metrics <- postResample(preds, train_data$resale_price)
        reg_metrics_df <- data.frame(Metric = names(reg_metrics), 
                                     Value = as.vector(reg_metrics))
        reg_metrics_df$Value <- format(round(reg_metrics_df$Value, 4), nsmall = 4)
        datatable(reg_metrics_df,
                  options = list(dom = 'i', scrollX = TRUE, autoWidth = TRUE))
      })
      
      output$residuals_bt <- renderPlot({
        # Residuals vs. Predicted Values plot
        # Use the model to predict the training set data
        predictions <- predict(BT, train_data)
        
        # Calculate residuals
        residuals <- train_data$resale_price - predictions
        
        # Plot residuals vs. predicted values
        plot(predictions, residuals,
             xlab = "Predicted Values",
             ylab = "Residuals",
             main = "Residuals vs. Predicted Values")
        abline(h = 0, col = "red")  # Add horizontal reference line
      })
      
      output$fit_curve_bt <- renderPlot({
        # Model fit (scatter plot of observed vs. predicted values, and the fit curve)
        plot(test_data$resale_price, predict(BT, newdata = test_data), 
             xlab = "Observed", ylab = "Predicted", main = "Observed vs Predicted")
        abline(0, 1, col = "red")  # Add fit curve
      })
      
      output$top_10_feature_plot_bt <- renderPlot({
        
        var_imp <- summary(BT, plotit = FALSE)
        top_10_var_imp <- head(var_imp[order(var_imp$rel.inf, decreasing = TRUE),], 10)
        
        ggplot(data = top_10_var_imp, aes(x = reorder(row.names(top_10_var_imp), -rel.inf), y = rel.inf)) +
          geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
          labs(title = "Top 10 Important Features",
               x = "Feature",
               y = "Relative Importance") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
      })
      
      
      
      output$top_10_feature_table_bt <- renderDataTable({
        
        var_imp <- summary(BT, plotit = FALSE)
        top_10_var_imp <- head(var_imp, 10)
        datatable(top_10_var_imp, rownames = FALSE)
      })
    }
    
    output$status_msg <- renderText("")
    showNotification("Predictive modeling is doing...", duration = 5, type = "message")
    
  })
  
  
}

# Run the app
shinyApp(ui = ui, server = server)