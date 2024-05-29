library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(maps)
library(scales)
library(emojifont)
library(emoGG)
library(gganimate)
library(plotly)
#library(showtext)
#font_add_google("Open Sans", "Open Sans")

main_font = "Open Sans"

# Your data

data <- read.csv("IHME_GBD_2010_MORTALITY_1970_2010.csv")
data <- data |> mutate(death_abs = as.numeric(gsub(",", "", death_abs)))
data_1 <- data |> mutate(death_rate = as.numeric(gsub(",", "", death_rate)))

data1 <- data_1 |> filter(age_name == "All ages"& sex_name == "Both"& region_name != " Global")

data2 <- data |> filter(age_name == "All ages"& sex_name == "Both"& region_name == " Global")

age_levels <- c("1-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years",
                "35-39 years", "40-44 years","45-49 years", "50-54 years", "55-59 years", "60-64 years", 
                "65-69 years", "70-74 years", "75-79 years", "80+ years")

data3 <- data |> filter(age_name != "All ages"& age_name != "0-6 days"& age_name != "7-27 days"& age_name != "28-364 days"& sex_name != "Both"& region_name == " Global") |> mutate(age_name = factor(age_name, levels = age_levels, ordered = TRUE))



# Define the server logic
server <- function(input, output) {
  output$yearPlot <- renderPlotly({
    Year <- input$yearTabs
    yearData <- data1 |> filter(year == Year)
    q <- ggplot(yearData, aes(x = region_name, y = death_rate)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = paste0("Death Rate per 100K by Region (", Year, ")"), x = "Region", y = "Death Rate")+
      theme_minimal(base_family = main_font,
                    base_size = 13)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      ylim(0,1875)
    ggplotly(q, tooltip = "y")
  })
  
  output$totaldeathLineChart <- renderPlot({
    ggplot(data2, aes(x = year, y = death_abs)) +
      geom_line(color = "blue") +
      geom_text(aes(label = fontawesome('fa-globe')), family='fontawesome-webfont', size=5)+
      labs(title = "Global Death Growth", x = "Year", y = "Total Death") +
      theme_minimal(
        base_family = main_font,
        base_size = 13
      )
    
  })
  output$AgeGenderDeath <- renderPlotly({
    Year1 <- input$yearTabs1
    yearData1 <- data3 |> filter(year == Year1)
    data_long <- yearData1 |> mutate( death_abs = ifelse(sex_name=="Male", death_abs*(-1)/1000, death_abs*1/1000))
    male_death <- data_long |> filter(sex_name == 'Male')
    female_death <- data_long |> filter(sex_name == 'Female')
    
    p <- ggplot(data_long, aes(x = 0,y = age_name)) +  
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
    ggplotly(p, tooltip = "x")
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
      theme(
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()
      )+
      labs(
        title = paste0("Global Death Rate by Country (", Year2, ")"),
        x = NULL,
        y = NULL
      )
  })
}