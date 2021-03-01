# Load packages
library(shiny)
library(tidyverse)

# Data source
icu_cohort = readRDS("icu_cohort.rds")

# User interface ---- 
ui <- fluidPage(

    # Application title
    titlePanel("MIMIC-IV ICU Cohort Data Explorer"),


    tabsetPanel(
        tabPanel("Demographics",
           sidebarLayout(
             sidebarPanel(
                radioButtons("dem", "Demographic variables:",
                             c("age_at_adm", "anchor_age", "gender", "language",
                               "marital_status", "ethnicity"), 
                             selected = "age_at_adm")

                
                ),
             mainPanel(
               tabsetPanel(type = "tabs",
                            tabPanel("Plot", plotOutput("plot")),
                            tabPanel("Summary", verbatimTextOutput("summary")),
                            tabPanel("Table", tableOutput("table")))
                         )
                     )
    ),
       tabPanel("Lab Measurements",
          sidebarLayout(
            sidebarPanel(
                 selectInput("labvar", label = "Lab item:",
                     c("bicarbonate", "calcium", "chloride", 
                       "creatinine", "glucose", "hematocrit", 
                       "lactate", "magnesium", "potassium", 
                       "sodium", "white blood cell"),
                     selected = "bicarbonate"),
                 
                 
                 sliderInput("labbins",
                             label = "Number of bins:",
                             min = 1,
                             max = 400,
                             value = 40)
                     
                 ),
            mainPanel(
                plotOutput("BoxPlot1"),
                plotOutput("labHist"))
       )
    ),
    
       tabPanel("Vitals",
          sidebarLayout(
              sidebarPanel(
                  selectInput("vitalvar", label = "Vitals item:",
                              c("heart rate", 
                                "noninvasive blood pressure_systolic",
                                "noninvasive blood pressure_mean",
                                "respiratory rate",
                                "temperature (°F)",
                                "arterial blood pressure_systolic",
                                "arterial blood pressure_mean"),
                              selected = "heart rate"),
                  
                  sliderInput("vitalbins",
                              label = "Number of bins:",
                              min = 1,
                              max = 400,
                              value = 40)   
                 ),
              mainPanel(
                  plotOutput("BoxPlot2"),
                  plotOutput("vitalHist")
              )
                )
    )
))
    

    
# Server logic ----
server <- function(input, output) {

    
#Generate plot, summary, and table for demographics   
    output$plot <- renderPlot({
        dem <- switch(input$dem,
                      "age_at_adm" = icu_cohort$age_at_adm, 
                      "anchor_age" = icu_cohort$anchor_age, 
                      "gender" = icu_cohort$gender, 
                      "language" = icu_cohort$language,
                      "marital_status" = icu_cohort$marital_status, 
                      "ethnicity" = icu_cohort$ethnicity)
        
        ggplot() +
            geom_bar(mapping = aes(x = dem, fill =dem )) +
            labs(x= input$dem, y = "Count")
    })    
    output$summary <- renderPrint({
        summary()
    })
    output$table <- renderTable({
       
    })
    
#Generate a boxplot and histogram for lab measurements      
    output$BoxPlot1 <- renderPlot({
         labvar <- switch(input$labvar,
                          "creatinine" = icu_cohort$creatinine, 
                          "potassium" = icu_cohort$potassium, 
                          "sodium" = icu_cohort$sodium, 
                          "chloride" = icu_cohort$chloride, 
                          "bicarbonate" = icu_cohort$bicarbonate, 
                          "hematocrit" = icu_cohort$hematocrit, 
                          "white blood cell" = icu_cohort$wbc, 
                          "glucose" = icu_cohort$glucose, 
                          "magnesium" = icu_cohort$magnesium, 
                          "calcium" = icu_cohort$calcium, 
                          "lactate" = icu_cohort$lactate)
            
         ggplot() + 
             geom_boxplot(mapping = aes(x = input$labvar, y = labvar))  +
             labs(x= input$labvar, y = "Value")+
             theme(axis.text.x = element_blank()) 
             
    })
    
   output$labHist <- renderPlot({
       labvar <- switch(input$labvar,
                        "creatinine" = icu_cohort$creatinine, 
                        "potassium" = icu_cohort$potassium, 
                        "sodium" = icu_cohort$sodium, 
                        "chloride" = icu_cohort$chloride, 
                        "bicarbonate" = icu_cohort$bicarbonate, 
                        "hematocrit" = icu_cohort$hematocrit, 
                        "white blood cell" = icu_cohort$wbc, 
                        "glucose" = icu_cohort$glucose, 
                        "magnesium" = icu_cohort$magnesium, 
                        "calcium" = icu_cohort$calcium, 
                        "lactate" = icu_cohort$lactate)
       
        ggplot(data = icu_cohort) +
            geom_histogram(mapping = aes (x = labvar), bins = input$labbins,
                           fill = "cornflowerblue", color = "black") +
            scale_x_log10() +
            labs(x= "Lab measurement distribution in log scale", y = "Count")+
            theme_minimal()
    })
   
#Generate a boxplot and histogram for vitals
   
   output$BoxPlot2 <- renderPlot({
       vitalvar <- switch(input$vitalvar,
"heart rate" = icu_cohort$heart_rate, 
"noninvasive blood pressure_systolic" = icu_cohort$non_invasive_blood_pressure_systolic,
"noninvasive blood pressure_mean" = icu_cohort$non_invasive_blood_pressure_mean,
"respiratory rate" = icu_cohort$respiratory_rate,
"temperature (°F)" = icu_cohort$temperature_fahrenheit,
"arterial blood pressure_systolic" = icu_cohort$arterial_blood_pressure_systolic,
"arterial blood pressure_mean" = icu_cohort$arterial_blood_pressure_mean)
       
       ggplot() + 
           geom_boxplot(mapping = aes(x = input$vitalvar, y = vitalvar))  +
           labs(x= input$vitalvar, y = "Value")+
           theme(axis.text.x = element_blank())
       
   })
   
   output$vitalHist <- renderPlot({
       vitalvar <- switch(input$vitalvar,
"heart rate" = icu_cohort$heart_rate, 
"noninvasive blood pressure_systolic" = icu_cohort$non_invasive_blood_pressure_systolic,
"noninvasive blood pressure_mean" = icu_cohort$non_invasive_blood_pressure_mean,
"respiratory rate" = icu_cohort$respiratory_rate,
"temperature (°F)" = icu_cohort$temperature_fahrenheit,
"arterial blood pressure_systolic" = icu_cohort$arterial_blood_pressure_systolic,
"arterial blood pressure_mean" = icu_cohort$arterial_blood_pressure_mean)
       
       ggplot(data = icu_cohort) +
           geom_histogram(mapping = aes (x = vitalvar), bins = input$vitalbins,
                          fill = "coral2", color = "black") +
           labs(x= "Vital distribution", y = "Count")+
           theme_minimal()
   })
}


# Run the application ----
shinyApp(ui = ui, server = server)
