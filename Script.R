#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(sf)
library(dplyr)
library(shiny)
library(DT)
library(ggplot2)
library(ggiraph)
library(plotly)
library(shinydashboard)
library(shinyjs)

source('tableau-in-shiny-v1.2.R')

# patronage data
data <- read.csv("data/average_patronage.csv")
data <- subset(data, Year >= 2021 & Year <= 2023)

# tax_data
tax_data <- read.csv("data/public_transport_tax.csv")
tax_data <- subset(tax_data, Year >= 2021 & Year <= 2023)

# vehicle registration data
register_data <- read.csv("data/vehicle_registration.csv")
register_data <- subset(register_data, Year >= 2021 & Year <= 2023)

# vehicle registration data
pd_acc_data <- read.csv("data/total_pedestrian_accident.csv")
pd_acc_data <- subset(pd_acc_data, Year >= 2021 & Year <= 2023)

# vehicle motive data
vehicle_motive_data <- read.csv("data/vehicle_motive.csv")
vehicle_motive_data <- subset(vehicle_motive_data, Year >= 2021 & Year <= 2023)

# Define the order of days of the week
days_of_week_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
data$Day_of_week <- factor(data$Day_of_week, levels = days_of_week_order)


# overview tab
overview_tab <- tabPanel("Data Overview",
       
       fluidRow(
         column(width = 12,
                div(class = "rowTitle",
                    tags$h3("Public Transport", style = "text-align: left;")
                )
         )
       ),
       fluidRow(
         column(6,
            verticalLayout(   
              plotlyOutput("patronageLineChart",height= "300px", width = "90%"),
              plotlyOutput("taxLineChart",height= "300px",width = "90%")
            )
                ),
         column(6,
          tableauPublicViz(
              id ='PublicTransport',
              url = 'https://public.tableau.com/views/PublicTransportMap/PublicTransportMap?:language=en-US&publish=yes&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link',
              height="600px"
            )
         )
       )
       ,
       fluidRow(
         column(12, 
              div(style = "text-align: center;",
                actionButton("public_transport_analysis", "Click to view detailed public transport analysis", 
                             style = "margin-top: 20px;margin-bottom: 20px;background-color: #667;color:white;")
                )
         )
       ),
       
       fluidRow(
         column(width = 12,
                div(class = "rowTitle",
                    tags$h3("Private Transport", style = "text-align: left;")
                )
         )
       ),
       
       fluidRow(
         column(6, plotlyOutput("vehicleLineChart",height= "350px")),
         column(6, plotlyOutput("vehicleMotiveLineChart",height= "350px"))
       ),
       
       fluidRow(
         column(12, 
            div(style = "margin-left: 200px;margin-right: 200px;margin-top: 20px;",
                plotlyOutput("accidentPedestrianLineChart",height= "350px"))
         )
       ),
       
       fluidRow(
         column(12, 
                div(style = "text-align: center;margin-top: 20px;margin-bottom: 20px;",
                    actionButton("pedestrian_analysis", "Click to view detailed pedestrian analysis", 
                                 style = "margin-right:50px;background-color: #667;color:white;"),
                    actionButton("accident_analysis", "Click to view detailed accident analysis", 
                                 style = "margin-left: 50px;background-color: #667;color:white;")
                )
         )
       )
    )

# Public transport tab
public_tab <- tabPanel("Public Transport Analysis",

      fluidRow(
        column(6, selectInput("year", "Select Year:", choices = c( unique(data$Year)), selected = "2021")),
        column(6, selectInput("month", "Select Month:", choices = c( unique(data$Month_name)), selected = "Jan")),
      ),
      fluidRow(
        column(5, girafeOutput("pieChartWeekday",height= "400px", width = "90%")),
        column(7, plotlyOutput("barChart",height= "350px", width = "90%"))
      ),
      fluidRow(
        column(5, girafeOutput("pieChartWeekend", height= "400px", width = "90%")),
        column(7, plotlyOutput("taxBarChart", height= "350px", width = "90%"))
      )
    )

# Pedestrian tab
pedestrian_tab <- tabPanel("Pedestrian Analysis",
   
     fluidRow(
       column(6, selectInput("pedestrian_year", "Select Year:", choices = c("All", unique(data$Year)), selected = "All")),
       column(6, selectInput("pedestrian_month", "Select Month:", choices = c("All", month.name), selected = "All"))
       
     ),                        
                             
     fluidRow(
       
        column(6, height="720px",
               
           div(style = "margin-left: 40px;margin-right: 20px;margin-bottom: 20px;",   
               
              verticalLayout(  
                
                  tableauPublicViz(
                    id ='Pedestrian_heatmap',
                    url = 'https://public.tableau.com/views/Pedestrianheatmap/PedHeatMap?:language=en-US&publish=yes&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link',
                    height="350px"
                  ),
                  
                  tableauPublicViz(
                    id ='Pedestrian_count',
                    url = 'https://public.tableau.com/views/Pedestriancountshourday/Pedbarchart?:language=en-US&publish=yes&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link',
                    height="350px"
                  ),
              )  
            )
                  
           ),
           
           column(6,  height="720px",
              div(style = "margin-left: 20px;margin-right: 40px;margin-bottom: 20px;",      
                  
                  tableauPublicViz(
                    id ='Pedestrian_map',
                    url = 'https://public.tableau.com/shared/G273HCKNP?:display_count=n&:origin=viz_share_link',
                    height="700px"
                  )
              )
           )
     )
  )

# Accident tab
accident_tab <- tabPanel("Accident Analysis",
                        
     fluidRow(
       column(6, selectInput("accident_year", "Select Year:", choices = c( "All",unique(data$Year)), selected = "All")),
       column(6, selectInput("accident_month", "Select Month:", choices = c("All", month.name), selected = "All"))
  
     ),   
    
     fluidRow(
       
         column(6, height="380px",
              
            div(style = "margin-left: 40px;margin-bottom: 20px;",
                
              tableauPublicViz(
                id ='AccidentSpeedZone',
                url = 'https://public.tableau.com/views/AccidentbySpeedZone/AccidentsbySpeedZones?:language=en-US&publish=yes&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link',
                height="350px"
              )
            )
              
          ),
       
        column(6, height="380px",
           
           div(style = "margin-right: 40px;margin-bottom: 20px;",
              
              tableauPublicViz(
                id ='Accident_map',
                url = 'https://public.tableau.com/shared/H5W5YKZZM?:display_count=n&:origin=viz_share_link',
                height="350px"
              )
           )
        )
     ),
     
     fluidRow(
       
       column(6, height="400px",
               
            div(style = "margin-top: 10px;margin-left: 40px;margin-bottom: 10px;",
                      
              tableauPublicViz(
                id ='Accident_weather',
                url = 'https://public.tableau.com/views/AtmosphereCondition/weather?:language=en-US&publish=yes&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link',
                height="380px"
              )
            )
       ),
       
       column(6, height="400px",
              
            div(style = "margin-top: 10px;margin-right: 40px;margin-bottom: 10px;",
                
              tableauPublicViz(
                id ='Accident_road_surface',
                url = 'https://public.tableau.com/views/SurfaceCondition/roadsurface?:language=en-US&publish=yes&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link',
                height="380px"
              ) ,
       )
       )
     )
              
)

# UI
ui <- navbarPage(
  
  id="mypage",
  
  header = setUpTableauInShiny(),
  
  # Custom style for row title
  tags$style(HTML("
  .rowTitle{
      color: white;
      background-color: #667; 
      padding: 10px; 
      border-radius: 5px; 
      margin-bottom: 10px;
    }
  ")),
  
  title="Transport Patronage and Tax Analysis",
  overview_tab,
  public_tab,
  accident_tab,
  pedestrian_tab
)

# server logic
server <- function(input, output, session) {
  
  # Data filtering for patronage & operation payments
  filter_patronage <- reactive({
    df <- data
    
    df <- df[df$Year == input$year, ]
    df <- df[df$Month_name == input$month, ]
    df
  })
  
  filter_tax <- reactive({
    df <- tax_data
    
    df <- df[df$Year == input$year, ]
    df <- df[df$Month_name == input$month, ]
    
    df
  })
  
  # Line chart - Patronage over time
  output$patronageLineChart <- renderPlotly({
    
    data_with_date <- data %>%
      mutate(Date = as.Date(paste(Year, Month_name, "1"), format = "%Y %B %d")) %>%
      group_by(Date, Mode) %>%  # Group by Date and Mode
      summarise(Pax_daily = sum(Pax_daily, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(data_with_date, aes(x = Date, y = Pax_daily, color = Mode)) +
    
      geom_point(size = 0.7,aes(text = paste("Date:", format(Date,"%Y %B"), "\nMode:", Mode, "<br>Total Patronage: ", format(Pax_daily, big.mark = ",")))) +
      geom_line(size = 0.5) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Public Transport Patronage from 2021 to 2023", x = "Time", y = "Total Patronage") +
      scale_color_brewer(palette = "Blues") + 
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Line chart - Operation payment over time
  output$taxLineChart <- renderPlotly({
    # Create a Date column without summarizing the tax
    tax_long <- tidyr::gather(tax_data, key = "Mode", value = "Tax", -Year, -Month_name) %>%
      mutate(Date = as.Date(paste(Year, Month_name, "1"), format = "%Y %B %d")) %>%
      group_by(Date, Mode) 
    
    p <- ggplot(tax_long, aes(x = Date, y = Tax, color = Mode, group=Mode)) +
      geom_line(size = 0.5) +
      geom_point(size = 0.7) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Operation Payment from 2021 to 2023", x = "Time", y = "Tax Paid") +
      scale_color_brewer(palette = "Blues") + 
      theme_minimal()+
      aes(text = paste("Date:", format(Date,"%Y %B"),"\nMode:", Mode, "<br>Operation Payment: ", format(Tax, big.mark = ",")))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Line chart - Vehicle Registration by vehicle type over time
  output$vehicleLineChart <- renderPlotly({
    
    p <- ggplot(register_data, aes(x = Year, y = VehicleNum, fill = VehicleType)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_brewer(palette = "Blues") +
      scale_y_sqrt(labels = scales::comma) + 
      labs(title = "Vehicle Numbers by Type Over the Years", x = "Year", y = "Number of Vehicles") +
      theme_minimal() +
      aes(text = paste("Year:", Year, "\nVehicle Type:", VehicleType,"<br>Total:", format(VehicleNum, big.mark = ",")))
    
      ggplotly(p, tooltip = "text")
  })
  
  # Line chart - Vehicle Registration by motive type over time
  output$vehicleMotiveLineChart <- renderPlotly({
    
    vehicle_motive <- vehicle_motive_data %>%
      tidyr::gather(key = "MotiveType", value = "VehicleNum", -Year)
    
    p <- ggplot(vehicle_motive, aes(x = Year, y = VehicleNum, fill = MotiveType)) +
      geom_bar(stat = "identity",position = "dodge") +
      labs(title = "Vehicle Numbers by Motive Type Over the Years", x = "Year", y = "Number of Vehicles") +
      scale_fill_brewer(palette = "Blues") + 
      scale_y_sqrt(labels = scales::comma) + 
      theme_minimal()+
      aes(text = paste("Year:", Year, "\nMotive Type:", MotiveType,"<br>Total:", format(VehicleNum, big.mark = ",")))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Line chart - Total accidents and pedestrians over the Years
  output$accidentPedestrianLineChart <- renderPlotly({
    
    pd_acc_data <- pd_acc_data %>%
      mutate(Date = as.Date(paste(Year, Month_name, "1"), format = "%Y %B %d"),
             tooltip_accidents = paste("Date:", format(Date, "%Y %B"), "<br>",
                                      "Total Accidents:", TotalAccident),
             tooltip_pedestrians = paste("Date:", format(Date, "%Y %B"), "<br>",
                                         "Total Pedestrians :", format(TotalPedestrian, big.mark = ",")))
    
    p <- ggplot(pd_acc_data) +
      geom_line(aes(x = Date, y = TotalAccident, color = "Total Accidents"), size = 0.5) +
      geom_point(aes(x = Date, y = TotalAccident, color = "Total Accidents", text = tooltip_accidents), size = 0.7) +
      geom_line(aes(x = Date, y = TotalPedestrian / 10000, color = "Total Pedestrians (x10000)"), size = 0.5) +
      geom_point(aes(x = Date, y = TotalPedestrian / 10000, color = "Total Pedestrians (x10000)", text = tooltip_pedestrians), size = 0.7) +
      scale_color_manual(values = c("Total Accidents" = "red", "Total Pedestrians (x10000)" = "blue")) + 
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Total Accidents and Pedestrians Over the Years", 
           x = "Year", 
           y = "Count", 
           color = "Legend") +
      theme_minimal() 
   
    ggplotly(p, tooltip = "text")
  })
  
  # Pie chart - Proportion of patronage by mode (weekday)
  output$pieChartWeekday <- renderGirafe({
    
    data <- filter_patronage()
    filtered_data <- subset(data, Day_type %in% c("Normal Weekday", "School Holiday Weekday"))
    
    mode_sums <- aggregate(Pax_daily ~ Mode, data = filtered_data, sum)
    
    p <- ggplot(mode_sums, aes(x = "", y = Pax_daily, fill = Mode)) +
      geom_bar_interactive(aes(tooltip = paste(Mode, ": ", Pax_daily), data_id = Mode), 
                           width = 1, stat = "identity") +
      coord_polar("y") +
      scale_fill_brewer(palette = "Blues") +
      labs(title = paste("Proportion of Patronage by Mode for Weekday in", input$month, input$year)) +
      theme_void()
    
    girafe(ggobj = p,
           options = list(
             opts_hover(css = "fill:grey;")
           ))
  })
  
  # Pie chart - Proportion of patronage by mode (weekend)
  output$pieChartWeekend <- renderGirafe({
    
    data <- filter_patronage()
    filtered_data <- subset(data, Day_type == "Weekend" )
    
    mode_sums <- aggregate(Pax_daily ~ Mode, data = filtered_data, sum)
    
    p <- ggplot(mode_sums, aes(x = "", y = Pax_daily, fill = Mode)) +
      geom_bar_interactive(aes(tooltip = paste(Mode, ": ", Pax_daily), data_id = Mode), 
                           width = 1, stat = "identity") +
      coord_polar("y") +
      scale_fill_brewer(palette = "Blues") +
      labs(title = paste("Proportion of Patronage by Mode for Weekend in", input$month, input$year)) +
      theme_void()
    
    girafe(ggobj = p,
           options = list(
             opts_hover(css = "fill:grey;")
           ))
  })
  
  # Bar chart - Average daily patronage by mode
  output$barChart <- renderPlotly({
    
    filtered_data <- filter_patronage()
    
    avg_patronage <- aggregate(Pax_daily ~ Day_of_week + Mode, data = filtered_data, FUN = mean)
    
    p <- ggplot(avg_patronage, aes(x = Day_of_week, y = Pax_daily, fill = Mode)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_brewer(palette = "Blues") +
      scale_y_continuous(labels = scales::comma) +
      labs(title = paste("Average Daily Patronage by Mode for", input$month, input$year),
           x = "Day of Week", y = "Average Patronage") +
      theme_minimal()+
      aes(text = paste("Mode:", Mode, "<br>Average Patronage: ", format(Pax_daily, big.mark = ",")))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Bar chart - Public transport operation payment by Mode
  output$taxBarChart <- renderPlotly({
    
    filtered_tax <- filter_tax()
    
    # Reshape the data from wide to long format for easier plotting
    tax_long <- tidyr::gather(filtered_tax, key = "Mode", value = "Tax", -Year, -Month_name)
    
    p <- ggplot(tax_long, aes(x = Mode, y = Tax, fill = Mode)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Blues") +
      scale_y_continuous(labels = scales::comma) +
      labs(title = paste("Operation Payment by Mode for", input$month, input$year),
           x = "Transport Mode", y = "Tax Amount") +
      theme_minimal() +
      aes(text = paste("Mode:", Mode, "<br>Operation Payment: $", format(Tax, big.mark = ",")))
    
    ggplotly(p, tooltip = "text")
  })
  
  # button click - Public transport analysis
  observeEvent(input$public_transport_analysis, {
    updateNavbarPage(session, "mypage", selected = "Public Transport Analysis")
  })
  
  # button click - Pedestrian analysis
  observeEvent(input$pedestrian_analysis, {
    updateNavbarPage(session, "mypage", selected = "Pedestrian Analysis")
  })
  
  # button click - Accident analysis
  observeEvent(input$accident_analysis, {
    updateNavbarPage(session, "mypage", selected = "Accident Analysis")
  })
  
  # ------------------------ Accident Filter Control -------------------------------
  observeEvent(input$accident_year, {
    
    year <- input$accident_year
  
    if (year == "All") {
      runjs('
        let sviz = document.getElementById("AccidentSpeedZone");
        sviz.workbook.activeSheet.clearFilterAsync("YEAR(Accident Date)");
        
        let mviz = document.getElementById("Accident_map");
        mviz.workbook.activeSheet.clearFilterAsync("YEAR(Accident Date)");
        
        let rviz = document.getElementById("Accident_road_surface");
        rviz.workbook.activeSheet.clearFilterAsync("YEAR(Accident Date)");
        
        let wviz = document.getElementById("Accident_weather");
        wviz.workbook.activeSheet.clearFilterAsync("YEAR(Accident Date)");
            
      ')
      
    } else {
    
      runjs(sprintf('
          
          let SpeedZoneViz = document.getElementById("AccidentSpeedZone");
          SpeedZoneViz.workbook.activeSheet.applyFilterAsync("YEAR(Accident Date)", ["%s"], FilterUpdateType.Replace);', year))
      
      runjs(sprintf('
          let viz = document.getElementById("Accident_map");
          viz.workbook.activeSheet.applyFilterAsync("YEAR(Accident Date)", ["%s"], FilterUpdateType.Replace);', year))
      
      runjs(sprintf('
          let viz = document.getElementById("Accident_road_surface");
          viz.workbook.activeSheet.applyFilterAsync("YEAR(Accident Date)", ["%s"], FilterUpdateType.Replace);', year))
      
      runjs(sprintf('
          let viz = document.getElementById("Accident_weather");
          viz.workbook.activeSheet.applyFilterAsync("YEAR(Accident Date)", ["%s"], FilterUpdateType.Replace);', year))
    }
  })
  
  observeEvent(input$accident_month, {
    
    month <- input$accident_month
    
    if (month == "All") {
      runjs('
        let sviz = document.getElementById("AccidentSpeedZone");
        sviz.workbook.activeSheet.clearFilterAsync("MONTH(Accident Date)");
        
        let mviz = document.getElementById("Accident_map");
        mviz.workbook.activeSheet.clearFilterAsync("MONTH(Accident Date)");
        
        let rviz = document.getElementById("Accident_road_surface");
        rviz.workbook.activeSheet.clearFilterAsync("MONTH(Accident Date)");
        
        let wviz = document.getElementById("Accident_weather");
        wviz.workbook.activeSheet.clearFilterAsync("MONTH(Accident Date)");
            
      ')
      
    } else {
    
    runjs(sprintf('
        let viz = document.getElementById("AccidentSpeedZone");
        viz.workbook.activeSheet.applyFilterAsync("MONTH(Accident Date)", ["%s"], FilterUpdateType.Replace);', month))
    
    runjs(sprintf('
        let viz = document.getElementById("Accident_map");
        viz.workbook.activeSheet.applyFilterAsync("MONTH(Accident Date)", ["%s"], FilterUpdateType.Replace);', month))
    
    runjs(sprintf('
        let viz = document.getElementById("Accident_road_surface");
        viz.workbook.activeSheet.applyFilterAsync("MONTH(Accident Date)", ["%s"], FilterUpdateType.Replace);', month))
    
    runjs(sprintf('
        let viz = document.getElementById("Accident_weather");
        viz.workbook.activeSheet.applyFilterAsync("MONTH(Accident Date)", ["%s"], FilterUpdateType.Replace);', month))
    }
  })
  
  # ------------------------ Pedestrian Filter Control -------------------------------
  
  observeEvent(input$pedestrian_year, {
    
    year <- input$pedestrian_year
    
    if (year == "All") {
      runjs('
        let sviz = document.getElementById("Pedestrian_heatmap");
        sviz.workbook.activeSheet.clearFilterAsync("YEAR(Sensing Date)");
        
        let mviz = document.getElementById("Pedestrian_map");
        mviz.workbook.activeSheet.clearFilterAsync("YEAR(Sensing Date)");
        
        let rviz = document.getElementById("Pedestrian_count");
        rviz.workbook.activeSheet.clearFilterAsync("YEAR(Sensing Date)");
      ')
      
    } else {
    
    runjs(sprintf('
        let SpeedZoneViz = document.getElementById("Pedestrian_heatmap");
        SpeedZoneViz.workbook.activeSheet.applyFilterAsync("YEAR(Sensing Date)", ["%s"], FilterUpdateType.Replace);', year))
    
    runjs(sprintf('
        let viz = document.getElementById("Pedestrian_map");
        viz.workbook.activeSheet.applyFilterAsync("YEAR(Sensing Date)", ["%s"], FilterUpdateType.Replace);', year))
    
    runjs(sprintf('
        let viz = document.getElementById("Pedestrian_count");
        viz.workbook.activeSheet.applyFilterAsync("YEAR(Sensing Date)", ["%s"], FilterUpdateType.Replace);', year))
    }
  })
  
  observeEvent(input$pedestrian_month, {
    
    month <- input$pedestrian_month
    
    if (month == "All") {
      runjs('
        let sviz = document.getElementById("Pedestrian_heatmap");
        sviz.workbook.activeSheet.clearFilterAsync("MONTH(Sensing Date)");
        
        let mviz = document.getElementById("Pedestrian_map");
        mviz.workbook.activeSheet.clearFilterAsync("MONTH(Sensing Date)");
        
        let rviz = document.getElementById("Pedestrian_count");
        rviz.workbook.activeSheet.clearFilterAsync("MONTH(Sensing Date)");
      ')
      
    } else {
    
    runjs(sprintf('
        let SpeedZoneViz = document.getElementById("Pedestrian_heatmap");
        SpeedZoneViz.workbook.activeSheet.applyFilterAsync("MONTH(Sensing Date)", ["%s"], FilterUpdateType.Replace);', month))
    
    runjs(sprintf('
        let viz = document.getElementById("Pedestrian_map");
        viz.workbook.activeSheet.applyFilterAsync("MONTH(Sensing Date)", ["%s"], FilterUpdateType.Replace);', month))
    
    runjs(sprintf('
        let viz = document.getElementById("Pedestrian_count");
        viz.workbook.activeSheet.applyFilterAsync("MONTH(Sensing Date)", ["%s"], FilterUpdateType.Replace);', month))
    }
  })
}

# Run the app
shinyApp(ui, server, options=list(launch.browser=TRUE))