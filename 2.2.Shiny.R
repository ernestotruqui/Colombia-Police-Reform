library(tidyverse)
library(shiny)
library(plotly)
library(lubridate)
library(sf)
library(ggplot2)
#install.packages("ggmap")
library(ggmap)
#install.packages('rsconnect')
library(rsconnect)

setwd("C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/Data")

ui <- fluidPage(
  fluidRow(
    column(width = 12,
           align = "center",
           tags$h1("Fighting Crime in Medellin, COL"),
           tags$h5("Data from 2018-2021"),
           tags$hr()
    )
  ),
  fluidRow(
    column(width = 12,
           align = "center",
           tags$h5("This website is the product of a Harris School of Public Policy project, in conjuection with the National Police of Colombia, to analyze police patrol allocation in the city of Medellin and recommend improvements. The graph on the left will show the amount of police officers per quadrant (equivalent to police beats in the US) while the one on the right will show the number of crimes per officer by quadrant. After analyzing data from 2018-2021, we mapped the status quo of Medellin's crime and police distribution and from that, recommended an optimal allocation strategy based on incidences of crime rather than a uniform distribution. You can interact with the plots by selecting the shifts you would like to visualize or by choosing to view the status quo or the hypothetical results of our suggested distribution."),
    )
  ),
  fluidRow(
    column(width = 6,
           align = "center",
           tags$h3("Police patrol distribution"),
           tags$hr()),
    column(width = 6,
           align = "center",
           tags$h3("Crime distribution"),
           tags$hr())
  ),
  fluidRow(
    column(width = 3,
           align = "center",
           selectInput(inputId = "shift_pol",
                       label = "Select a shift",
                       choices = c("morning", "afternoon", "night"))),
    column(width = 3,
           align = "center",
           radioButtons(inputId = "time_pol",
                        label = "Before or after redistribution?",
                        choices = c("Before", "After"),
                        selected = "Before")),
    column(width = 3,
           align = "center",
           selectInput(inputId = "shift_cri",
                        label = "Select a shift",
                        choices = c("morning", "afternoon", "night"))),
    column(width = 3,
           align = "center",
           radioButtons(inputId = "time_cri",
                        label = "Before or after redistribution?",
                        choices = c("Before", "After"),
                        selected = "Before"))
  ),
  fluidRow(
    column(width = 6,
           align = "center",
           plotlyOutput(outputId = "police")),
      column(width = 6,
             align = "center",
             plotlyOutput(outputId = "crimes"))
  )
)

server <- function(input, output) {
  df_shift <- st_read("df_shifts_avg.shp")
  
  output$police <- renderPlotly({
    ifelse(input$time_pol == "Before",
           plt <- ggplot() +
             geom_sf(data = df_shift[df_shift$shift == ifelse(input$shift_pol == "morning", "5-13", ifelse(input$shift_pol == "afternoon", "13-21", "21-5")),],
                     #aes(fill = n_of_police)) +
                     aes(fill = n_f_plc)) +
             labs(title = "Officers per Quadrant",
                  subtitle = "Uniform distribution - 2 per quadrant",
                  fill = "Officers per Quadrant") +
             theme(plot.title = element_text(hjust = 0.5, size = 15),
                   plot.subtitle = element_text(hjust = 0.5, size = 10)) +
             scale_fill_viridis_c(option = "mako", limits = c(1, 8)),
           plt <- ggplot() +
             geom_sf(data = df_shift[df_shift$shift == ifelse(input$shift_pol == "morning", "5-13", ifelse(input$shift_pol == "afternoon", "13-21", "21-5")),],
                     #aes(fill = rn_of_police)) +
                     aes(fill = rn_f_pl)) +
             labs(title = "Officers per Quadrant",
                  fill = "Officers per Quadrant") +
             theme(plot.title = element_text(hjust = 0.5, size = 15),
                   plot.subtitle = element_text(hjust = 0.5, size = 10)) +
             scale_fill_viridis_c(option = "mako", limits = c(1, 8)) 
           )
    ggplotly(plt)
  
  })
  
  output$crimes <- renderPlotly({
    ifelse(input$time_cri == "Before",
           plt <- ggplot() +
             geom_sf(data = df_shift[df_shift$shift == ifelse(input$shift_cri == "morning", "5-13", ifelse(input$shift_cri == "afternoon", "13-21", "21-5")),],
                     aes(fill = cpp)) +
             labs(title = "Crimes per Officer",
                  fill = "Crimes per Officer",
                  color = "Crimes per Officer") +
             theme(plot.title = element_text(hjust = 0.5, size = 15)) +
             scale_fill_viridis_c(option = "inferno", limits = c(0, 80)) +
             scale_color_viridis_c(option = "inferno", limits = c(0, 80)) ,
           plt <-  ggplot() +
             geom_sf(data = df_shift[df_shift$shift == ifelse(input$shift_cri == "morning", "5-13", ifelse(input$shift_cri == "afternoon", "13-21", "21-5")),],
                     aes(fill = rcpp)) +
             labs(title = "Crimes per Officer",
                  fill = "Crimes per Officer",
                  color = "Crimes per Officer") +
             theme(plot.title = element_text(hjust = 0.5, size = 15)) +
             scale_fill_viridis_c(option = "inferno", limits = c(0, 80)) +
             scale_color_viridis_c(option = "inferno", limits = c(0, 80)) 
    )
    ggplotly(plt)
    
  })
  
}
shinyApp(ui = ui, server = server)


