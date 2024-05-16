library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(maps)
library(scales)
#library(showtext)
#font_add_google("Open Sans", "Open Sans")

main_font = "Open Sans"

# Your data

#data <- read.csv("IHME_GBD_2010_MORTALITY_1970_2010.csv")
#data <- data |> mutate(death_abs = as.numeric(gsub(",", "", death_abs)))

#data1 <- data |> filter(age_name == "All ages"& sex_name == "Both"& region_name != " Global")

#data2 <- data |> filter(age_name == "All ages"& sex_name == "Both"& region_name == " Global")

#age_levels <- c("1-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years",
#                "35-39 years", "40-44 years","45-49 years", "50-54 years", "55-59 years", "60-64 years", 
#                "65-69 years", "70-74 years", "75-79 years", "80+ years")

#data3 <- data |> filter(age_name != "All ages"& age_name != "0-6 days"& age_name != "7-27 days"& age_name != "28-364 days"& sex_name != "Both"& region_name == " Global") |> mutate(age_name = factor(age_name, levels = age_levels, ordered = TRUE))



# Define the UI
ui <- fluidPage(
  titlePanel("The Global Burden of Disease Study 2010 (GBD 2010) "),
  #sidebarLayout(
  #  sidebarPanel(
      # Use tabPanel directly within tabsetPanel
  #  ),
    #mainPanel(
  fluidRow(
    column(6,
           plotOutput("totaldeathLineChart")
    ),
     column(6,
            tabsetPanel(
              id = "tabs",
              tabPanel("Global Death by Age Group and Gender",
                       tabsetPanel(type = "tab",
                                   id = "yearTabs1",
                                   tabPanel("1970", value = "1970"),
                                   tabPanel("1980", value = "1980"),
                                   tabPanel("1990", value = "1990"),
                                   tabPanel("2000", value = "2000"),
                                   tabPanel("2010", value = "2010")
                       ),
                       plotOutput("AgeGenderDeath")
              )
            )
     )
  ),
  fluidRow(
    column(6,
           tabsetPanel(
             id = "tabs",
             tabPanel("Death Rate by Region",
                      tabsetPanel(type = "tab",
                                  id = "yearTabs",
                                  tabPanel("1970", value = "1970"),
                                  tabPanel("1980", value = "1980"),
                                  tabPanel("1990", value = "1990"),
                                  tabPanel("2000", value = "2000"),
                                  tabPanel("2010", value = "2010")
                      ),
                      plotOutput("yearPlot")
             )
           )
    ),
    column(6,
           tabsetPanel(
             id = "tabs",
             tabPanel("Global Death Rate by Country",
                      tabsetPanel(type = "tab",
                                  id = "yearTabs2",
                                  tabPanel("1970", value = "1970"),
                                  tabPanel("1980", value = "1980"),
                                  tabPanel("1990", value = "1990"),
                                  tabPanel("2000", value = "2000"),
                                  tabPanel("2010", value = "2010")
                      ),
                      plotOutput("DeathRate")
             )
           )
    )
  ),
  fluidRow(
    column(6,tags$img(src = "female.gif", width = "480px", height = "480px", deleteFile=FALSE)),
    column(6,tags$img(src = "male.gif", width = "480px", height = "480px", deleteFile=FALSE))
  )
    #)
)



# Run the app
#shinyApp(ui = ui, server = server)
