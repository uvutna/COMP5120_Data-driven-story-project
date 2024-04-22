library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(maps)
library(scales)

# Your data

data <- read.csv("IHME_GBD_2010_MORTALITY_1970_2010.csv")
data <- data |> mutate(death_abs = as.numeric(gsub(",", "", death_abs)))

data1 <- data |> filter(age_name == "All ages"& sex_name == "Both"& region_name != " Global")

data2 <- data |> filter(age_name == "All ages"& sex_name == "Both"& region_name == " Global")

age_levels <- c("1-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years",
                "35-39 years", "40-44 years","45-49 years", "50-54 years", "55-59 years", "60-64 years", 
                "65-69 years", "70-74 years", "75-79 years", "80+ years")

data3 <- data |> filter(age_name != "All ages"& age_name != "0-6 days"& age_name != "7-27 days"& age_name != "28-364 days"& sex_name != "Both"& region_name == " Global") |> mutate(age_name = factor(age_name, levels = age_levels, ordered = TRUE))



# Define the UI
ui <- fluidPage(
  titlePanel("The Global Burden of Disease Study 2010 (GBD 2010) "),
  #sidebarLayout(
  #  sidebarPanel(
      # Use tabPanel directly within tabsetPanel
  #  ),
    #mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Number of Death by Region",
          tabsetPanel(type = "tab",
            id = "yearTabs",
            tabPanel("1970", value = "1970"),
            tabPanel("1980", value = "1980"),
            tabPanel("1990", value = "1990"),
            tabPanel("2000", value = "2000"),
            tabPanel("2010", value = "2010")
          ),
          plotOutput("yearPlot")
        ),
      ),
      plotOutput("totaldeathLineChart"),
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
      ),
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
    #)
)

# Define the server logic
server <- function(input, output) {
  output$yearPlot <- renderPlot({
    Year <- input$yearTabs
    yearData <- data1 |> filter(year == Year)
    # Generate bar chart (same as before)
    ggplot(yearData, aes(x = region_name, y = death_abs)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = paste0("Number of Death by Region (", Year, ")"), x = "Region", y = "Total Death")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  output$totaldeathLineChart <- renderPlot({
    # Generate line chart showing the growth of USA's GDP
    ggplot(data2, aes(x = year, y = death_abs)) +
      geom_line(color = "blue") +
      labs(title = "Global Death Growth", x = "Year", y = "Total Death") +
      theme_minimal()
  })
  output$AgeGenderDeath <- renderPlot({
    Year1 <- input$yearTabs1
    yearData1 <- data3 |> filter(year == Year1)
    data_long <- yearData1 |> mutate( death_abs = ifelse(sex_name=="Male", death_abs*(-1), death_abs*1))
    male_death <- data_long |> filter(sex_name == 'Male')
    female_death <- data_long |> filter(sex_name == 'Female')
    
    ggplot(data_long, aes(x = 0,y = age_name)) +  
        geom_col(data=male_death, aes(y = age_name, x = death_abs,fill = "Male"))+
        geom_col(data=female_death,aes(y = age_name, x = death_abs,fill = "Female"))+
        scale_fill_manual(values = c("#109466","#112e80"))+
      theme_minimal(
        base_family = main_font,
        base_size = 13
      )+ 
      theme(
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        plot.subtitle = element_text(hjust = 0.5, face="bold"),
        legend.position = "bottom",
        panel.grid.major.x = element_line(color = "grey", linewidth = 0.2),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = rel(0.7)),
        axis.text.x = element_text(face="bold",colour = "#66635d"), 
        axis.text.y = element_text(face="bold",colour = "#66635d"), 
        legend.text = element_text(size = rel(0.7))
      )+
      labs(
        title = paste0("Global Death by Age Group and Gender (", Year1, ")"),
        y = "Age Group",
        x = "Number of Death",
        fill="Gender"
      )
      })
  
  output$DeathRate <- renderPlot({
    Year2 <- input$yearTabs2
    
    data <- read.csv("IHME_GBD_2010_MORTALITY_AGE_SPECIFIC_BY_COUNTRY_1970_2010_1.csv")
    data1 <- data |> filter(Age_Group == "All ages"& Sex == "Both") |> 
      mutate(Death_Rate = as.numeric(gsub(",", "", Death_Rate)))
    yearData2 <- data1 |> filter(Year == Year2)

    world_map <- map_data("world")
    world_map <- subset(world_map, region!="Antarctica")
    
    
    ggplot(yearData2) +
      geom_map(
        dat = world_map, map = world_map, aes(map_id = region),
        fill = "white", color = "#7f7f7f", size = 0.25
      ) +
      geom_map(map = world_map, aes(map_id = Country_Name, fill = Death_Rate), size = 0.25) +
      scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = "Death Rate per 100K") +
      expand_limits(x = world_map$long, y = world_map$lat)+
      labs(
        title = paste0("Global Death Rate by Country (", Year2, ")")
      )
      })
}

# Run the app
shinyApp(ui = ui, server = server)
