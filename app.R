#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#### load libraries ####
library(plotly)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(stringr)
library(lubridate)
library(magrittr)
#library(RColorBrewer)
library(leaflet)
library(htmltools)
#### end ####

#### load data ####
listing_dates <- read.csv("calendar.csv")
listings <- read.csv("listings-summary.csv")
reviews <- read.csv("reviews.csv")

#names(listing_dates)
#names(listings)
#names(reviews)

#filter for only available listings
listing_dates <- listing_dates[listing_dates$available == 't',]

#pre-process listings_data to create derived variables, and remove time series data
#derive 6 variables listing_start_date, listing_end_date, starting_price, ending_price,
# minimum_nights, maximum_nights

listing_dates <- listing_dates %>%
    group_by(listing_id) %>%
    mutate(
        listing_start_date = min(date),
        listing_end_date = max(date),
        min_price = min(adjusted_price),
        max_price = max(adjusted_price),
        minimum_nights = max(minimum_nights),
        maximum_nights = min(maximum_nights)
    )

listing_dates <- listing_dates %>%
    select(-c('date', 'available', 'price', 'adjusted_price')) %>%
    unique()



#join data together using the listing_id ('id' for listings.csv) across datasets
listings_merged <-
    merge(listing_dates, listings, by.x = 'listing_id', by.y = 'id')

#we will avoid joining reviews with the listings, as there are multiple reviews
# for a listing and that will create duplicate rows. we will use 'reviews' directly in the visuals, based on selected listings

#colSums(is.na(listings_merged))

#remove 'neighbourhood_group' column as it has only NA

#there are 95 missing values in maximum_nights and minimum nights.
#It might be that this information for these listings is not available. we will leave it as-is

#review_per_month has 2631 missing values. this info not available for certain listings. leave it as-is

#This section is used to load transformed data for the purpose of building the dashboard.
#This is commented for the purpose of the final r code


#save derived data to csv
#write.csv(listings_merged, "listings_merged.csv")

#### load merged data from csv ####
#listings_merged <- read.csv("listings_merged.csv")


#str_replace_all(listings_merged$min_price, ".00", "")

listings_merged$min_price <-
  as.double(gsub("^.{1}", "", as.character(listings_merged$min_price)))
listings_merged$max_price <-
  as.double(gsub("^.{1}", "", as.character(listings_merged$max_price)))

#convert dates from string to date type
listings_merged$listing_start_date <-
  as.Date(listings_merged$listing_start_date, format = "%Y-%m-%d")
listings_merged$listing_end_date <-
  as.Date(listings_merged$listing_end_date, format = "%Y-%m-%d")
listings_merged$last_review <-
  as.Date(listings_merged$last_review, format = "%Y-%m-%d")



#transform reviews data 
reviews$date <-
  as.Date(reviews$date, format = "%Y-%m-%d")

#### end ####


#Define shiny application and design visuals for dashboard

#### ui ####

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Airbnb Listings In Melbourne In 2020 ", titleWidth = "1100"),
  dashboardSidebar(
    tags$style(HTML(" .main-sidebar{ width: 230px; }")),
    dateRangeInput(
      "date_range",
      "Select Date range:",
      start = min(listings_merged$listing_start_date),
      end = max(listings_merged$listing_end_date) ,
      width = 250
    ),
    #### filter selection ####
    selectInput(
      "neighbourhood",
      label = "Select neighbourhood",
      choices = NULL,
      #choices = listings_merged$neighbourhood,
      multiple = TRUE,
      width = 250
    ),
    
    sliderInput(
      "price_range",
      "Select price range",
      min = min(listings_merged$min_price, na.rm = TRUE),
      max = max(listings_merged$max_price, na.rm = TRUE),
      value =  c(
        min(listings_merged$min_price, na.rm = TRUE),
        max(listings_merged$max_price, na.rm = TRUE)
      ),
      step = NULL,
      round = FALSE,
      ticks = TRUE,
      width = 250
    ),
    
    sliderInput(
      "number_of_reviews",
      "No. Of Reviews",
      min = min(listings_merged$number_of_reviews, na.rm = TRUE),
      max = max(listings_merged$number_of_reviews, na.rm = TRUE),
      value =  c(
        min(listings_merged$number_of_reviews, na.rm = TRUE),
        max(listings_merged$number_of_reviews, na.rm = TRUE)
      ),
      step = NULL,
      round = FALSE,
      ticks = TRUE,
      width = 250
    ),
    
    sliderInput(
      "min_nights",
      "Minimum Stay Duration \n (show listings that have this restriction)",
      min = min(listings_merged$'minimum_nights.y', na.rm = TRUE),
      max = max(listings_merged$'minimum_nights.y', na.rm = TRUE),
      value = min(listings_merged$'minimum_nights.y', na.rm = TRUE),
      step = NULL,
      round = FALSE,
      ticks = TRUE,
      width = 250
    ),
    
    selectInput(
      "room_type",
      label = "Select room type",
      choices=NULL,
      #choices = listings_merged$room_type,
      multiple = TRUE,
      width = 250
    ),
    
      actionButton('reset_filters1', "Reset Filters and Map")
  ),
  #### end ####
  
  dashboardBody(tags$style(
    HTML(
      ".box.box-solid.box-primary>.box-header {
                    color:#fff;
                    background:#3c8dbc
                    }
                    .box.box-solid.box-primary{
                    background:#3c8dbc
                    }"
    )
  ), 
    
    # Show a plot of the generated distribution
    fluidPage(
      tabsetPanel(type = "tabs",
          
      tabPanel("Read Me",
               fluidRow(box(width=30, height=50, title="This is the R Shiny dashboard to explore the trends, 
                        patterns and information on the Airbnb listings in Melbourne in 2020")),
               
               fluidRow(box(width=50, height=50, title="There are 3 tabs in this dashboard:", lty = 'solid')),
               fluidRow(box(width=50, height=50, title= "          1. Read Me (This Tab)  -  Explains the purpose and usability of the dashboard")),
               fluidRow(box(width=50, height=70, title= "          2. 'Summary View'  -  Provides a summary of the listings with a high level summary of important metrics such as 
                          total listings, min/max price, average price, total reviews and listings with 365 availibility, 
                          distribution and trends across improtant attributes such as neighbourhoos, room types,
                          distribution of prices and location of the listings on the map of Melbourne")),
               fluidRow(box(width=50, height=50, title="          3. 'Explore The Listings In More Detail' - Select a listing to explore its full details such as the 
                          reviews, reviewes, review date, host, listing start and end date, neighbourhood, price, room type, availibility type etc.  ")),
               fluidRow(box(width=50, height=70, title="Select the filters in the filter pane to view the summary, trends and details for the chosen filters 
                            (date range, neighbourhood, price range, no. of reviews, minimum stay duration, room type). CHoose 'reset filters' at any time to reset views to default setting")),
               fluidRow(box(width=50, height=50, title="Data can be found on kaggle at     https://www.kaggle.com/datasets/nadyafed/melbourne-airbnb-2020"))
               ),            
                          
#### tabset1 starts ####  
      tabPanel("Summary View", 
      fluidRow(
        height = 10,
        box(width= 30, height=150, title="Summary of Airbnb listings in Melbourne (for the selected filters)",
        column(2, uiOutput('total_listings')),
        column(2, uiOutput('min_price')),
        column(2, uiOutput('max_price')),
        column(2, uiOutput('avg_price')),
        column(2, uiOutput('number_of_reviews')),
        column(2, uiOutput('availability_365')),
      )),
      
#### visuals and plots ####
      
      fluidRow(
        height = 550,
        box(solidHeader = TRUE,
          width = 4,
          title = 'No. Of Listings By Neighbourhood',
          plotlyOutput('listing_count_by_neighbourhood'),
          height = 200
        ),
        box(solidHeader = TRUE,
          width = 4,
         title = 'No. Of Listings By Room Type',
          plotlyOutput('listing_count_by_room_type'),
          height = 200
        ),
        box(
          width = 4,
          title = 'No. Of New Listing By Date',
          plotlyOutput('new_listings_by_date'),
          height = 200
        )
      ),
      fluidRow(
        box(
          width = 4,
          title = 'No. Of Listings Across The Price Range (on a logarithmic Scale)',
          plotlyOutput('distribution_of_price_range'),
          height = 450
        ),
        box(
          width = 8,
          title = 'Location of The Listings On The Map Of Melbourne (click the circles to zoom in and get the \'listing_id\' on click or \'listing_name\' on hover)',
          leafletOutput('map1'),
          height = 450
        )),
      ), 

#### tabset2 starts ####      
  tabPanel("Explore The Listings In More Detail",
           fluidRow(
             box(title = 'Explore Listings on map; (Note either the \'listing_id\' on click, or \'listing_name\' on hover on the map and apply as filters on the right)',
                 leafletOutput("map2", height = "50vh")
             ),
             box(height = 500,
                 style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                 
                 fluidRow(
                   column(2, offset=4,
                   actionButton('reset_filters2', "Reset Listing Filters"))
                   ),
               column(
                 4,
                 title = 'Choose a listing ID',
                 selectInput(
                   "listing_id",
                   label = "Select Listing ID",
                   choices=NULL,
                   #choices = listings_merged$listing_id,
                   multiple = TRUE,
                   width = 250
                 )),
               column(
                 4,
                 selectInput(
                   "listing_name",
                   label = "Select a listing name",
                   choices=NULL,
                   #choices = listings_merged$listing_name,
                   multiple = TRUE,
                   width = 250
                 )),
               column(
                 4,
                 selectInput(
                   "host_name",
                   label = "Select a host name",
                   choices=NULL,
                   #choices = listings_merged$host_name,
                   multiple = TRUE,
                   width = 250
                 )
               ),
               fluidRow( 
                 box(title="Customer reviews for the selected listings",
                     DT::dataTableOutput('reviews'), width = 12,
                     style = "height:350px; overflow-y: scroll;overflow-x: scroll;"
                 ))
             )
           ),

           fluidRow(
             box(
               title = "Full details Of Listings",
               DT::dataTableOutput('table'),
               height = 300,
               width = 500
             ))
        ) #tabpanel 2

    ) #tabsets 

  ) #fluid page
    
  ) #dashboard body
) #dashboard page
  
  #### end ####
  
  
  
  
  #### end ####
  
  ### server ####
 
#begin <- 0  

# Define server logic required to draw a histogram
  server <- function(input, output, session)
  {
    neighbourhood <- listings_merged$neighbourhood
    room_type <- listings_merged$room_type
    price_range <- listings_merged$min_price
    number_of_reviews <- listings_merged$number_of_reviews
    min_nights <- listings_merged$'minimum_nights.y'
    
    observe({
      updateSelectInput(session, "neighbourhood", choices=selected_data()$neighbourhood, selected=input$neighbourhood)
      updateSelectInput(session, "room_type", choices=selected_data()$room_type, selected=input$room_type)
    })

    observeEvent(input$reset_filters1,{
      updateSelectInput(session, "listing_id", choices=selected_listings()$listing_id, selected=NULL)
      updateSelectInput(session, "listing_name", choices=selected_listings()$name, selected=NULL)
      updateSelectInput(session, "host_name", choices=selected_listings()$host_name, selected=NULL)
      updateSelectInput(session, "neighbourhood", choices=selected_data()$listing_id, selected=NULL)
      updateSelectInput(session, "room_type", choices=selected_data()$name, selected=NULL)
      updateSliderInput(session, "price_range", value=c(min(listings_merged$min_price, na.rm = TRUE),
                                                        max(listings_merged$max_price, na.rm = TRUE)))
      updateSliderInput(session, "number_of_reviews", value=c(min(listings_merged$number_of_reviews, na.rm = TRUE),
                                                              max(listings_merged$number_of_reviews, na.rm = TRUE)))
      updateSliderInput(session, "min_nights", value=min(listings_merged$'minimum_nights.y', na.rm = TRUE))
      updateDateRangeInput(session, "date_range", start = min(listings_merged$listing_start_date), 
                           end = max(listings_merged$listing_end_date))
    })
    
    selected_data <- reactive({
      df <- listings_merged %>%
        {
          if (!is.null(input$neighbourhood)) {
            filter(., neighbourhood %in% input$neighbourhood)
          }
          else
            select_all(., )
        } %>%
        {
          if (!is.null(input$room_type)) {
            filter(., room_type %in% input$room_type)
          }
          else
            select_all(., )
        } %>%
        {
          filter(
            .,
            !(
              listing_start_date > input$date_range[2] |
                listing_end_date < input$date_range[1]
            )
          )
        } %>%
        filter(., price >= input$price_range[1] &
                 price <= input$price_range[2]) %>%
        filter(
          .,
          number_of_reviews >= input$number_of_reviews[1] &
            number_of_reviews <= input$number_of_reviews[2]
        ) %>%
        filter(., minimum_nights.y >= input$min_nights)
      
      return(df)
    })

    observeEvent(input$reset_filters1,
                 {
                   selected_data <- listings_merged
                 })
    
    output$total_listings <- renderValueBox({
      valueBox(
        value = tags$p(
           selected_data() %>%
            count() %>% unlist %>% format(., big.mark = ",", scientific =
                                            FALSE),
          style = "font-size: 50%;"
        ),
        #listings_merged %>% count() %>% unlist %>% format(.,big.mark=",",scientific=FALSE),
        "Listings",
        # icon = icon("home"),
        icon = tags$i(icon("home"), style = "font-size: 48px"),
        color = "green",
        width = 10
      )
    })
    
    output$min_price <- renderValueBox({
      valueBox(
        value = tags$p(
          paste0(
            "$ ",
            selected_data() %>% select(min_price) %>%
              min(na.rm = TRUE)
          ),
          style = "font-size: 50%;"
        ),
        "Min Price",
        icon = tags$i(icon("dollar"), style = "font-size: 48px"),
        color = "blue",
        width = 10
      )
    })
    
    output$max_price <- renderValueBox({
      valueBox(
        value = tags$p(
          paste0(
            "$ ",
            selected_data() %>% select(max_price) %>% max(na.rm = TRUE)
          ),
          style = "font-size: 50%;"
        ),
        "Max Price",
        #icon = icon("dollar"),
        icon = tags$i(icon("dollar"), style = "font-size: 48px"),
        color = "olive",
        width = 10
      )
    })
    
    output$avg_price <- renderValueBox({
      valueBox(
        value = tags$p(
          paste0(
            "$ ",
            selected_data() %>% 
             select(price) %>% unlist() %>% as.numeric() %>% mean(na.rm = TRUE) %>% round(0)
          ),
          style = "font-size: 50%;"
        ),
        subtitle = tags$p("Avg. Price",
                          style = "font-size: 100%;"),
        #icon = icon("dollar"),
        icon = tags$i(icon("dollar"), style = "font-size: 48px"),
        color = "orange",
        width = 10
      )
    })
    
    output$number_of_reviews <- renderValueBox({
      valueBox(
        value = tags$p(
          selected_data() %>% select(number_of_reviews) %>%
            sum(na.rm = TRUE) %>%
            format(., big.mark = ",", scientific = FALSE),
          style = "font-size: 50%;"
        ),
        "Reviews",
        #icon = icon("pen"),
        icon = tags$i(icon("pen"), style = "font-size: 48px"),
        color = "teal",
        width = 10
      )
    })
    
    output$availability_365 <- renderValueBox({
      valueBox(
        value = tags$p(
          selected_data() %>%
            .[.$availability_365 == 365, ] %>% count()
          %>%
            format(., big.mark = ",", scientific = FALSE),
          style = "font-size: 50%;"
        ),
        "Available 365",
        #icon = icon("home"),
        icon = tags$i(icon("home"), style = "font-size: 48px"),
        
        color = "green",
        width = 10
      )
    })

    
    output$listing_count_by_neighbourhood <- renderPlotly({
      selected_data() %>% group_by(neighbourhood) %>%
        summarise(listing_count = n()) %>% .[order(-.$listing_count), ] %>%
        plot_ly(
          height = 145,
          x = .$neighbourhood,
          y = .$listing_count,
          type = "bar",
          #colors = 'blue',
          text = .$listing_count,
          textposition = 'auto'
        ) %>%   #, source = "heatplot") %>%
        layout(
          xaxis = list(categoryorder = "total descending", tickangle=-40,  automargin=TRUE),
          yaxis = list(title = "No. Of Listings")
        )
    })

    output$listing_count_by_room_type <- renderPlotly({
      selected_data()  %>% group_by(room_type) %>%
        summarise(listing_count = n()) %>% .[order(-.$listing_count), ] %>%
        plot_ly(
          height = 135,
          x = .$room_type,
          y = .$listing_count,
          type = "bar",
          colors = "red",
          text = .$listing_count,
          textposition = 'auto'
        ) %>%   #, source = "heatplot") %>%
        layout(
          xaxis = list(title = "Room Type", categoryorder = "total descending", tickangle=-20, automargin=TRUE),
          yaxis = list(title = "No. Of Listings")#, title="No. Of Listings By Room Type"#,          
          #paper_bgcolor='#e5ecf6'
        )
    })
    
    output$new_listings_by_date <- renderPlotly({
      selected_data() %>% group_by(listing_start_date) %>% summarise(listing_count = n()) %>%
        plot_ly(
          height = 135,
          x = .$listing_start_date,
          y = .$listing_count,
          type = 'scatter',
          mode = 'lines',
          colors = 'teal',
          text = .$listing_count,
          textposition = 'auto'
        ) %>% layout(xaxis = list(title = "Date Range", tickangle=-20, automargin=TRUE), 
                     yaxis = list(title = "No. of New Listings", type = 'log'))
    })
    
    
    output$distribution_of_price_range <- renderPlotly({
      selected_data() %>%
        plot_ly(
          height = 400,
          x = .$price,
          #y = distribution_of_price_range$price_count,
          type = 'histogram',
          nbinsx = 100
        ) %>%
        layout(xaxis = list(title = 'Prices', tickangle=-45),
          yaxis = list(title = "No. of Listings (Log Scale)", type = 'log'))
    })
    
    output$map1 <- renderLeaflet({
      leaflet(#options = leafletOptions(preferCanvas = FALSE),
        height = "40%px",
        width = 200,
        selected_data() %>%
          .[, c("listing_id", "name", "host_name", "longitude", "latitude")]
      ) %>%
        addTiles() %>% addMarkers(lat=~latitude, lng=~longitude, label = ~htmlEscape(name), popup = ~htmlEscape(listing_id),
                                  clusterOptions = markerClusterOptions(spiderfyOnMaxZoom=TRUE, 
                                                                        zoomToBoundsOnClick=TRUE,
                                                                        showCoverageOnHover=TRUE))
    })

    #respond to new data
    ## respond to the filtered data
    observe({
      
      leafletProxy(mapId = "map1", data = selected_data()) %>%
        clearMarkers() %>%   ## clear previous markers
        addMarkers(lat=~latitude, lng=~longitude, label = ~htmlEscape(name), popup = ~htmlEscape(listing_id),
                   clusterOptions = markerClusterOptions(spiderfyOnMaxZoom=TRUE, 
                                                         zoomToBoundsOnClick=TRUE,
                                                         showCoverageOnHover=TRUE))
    })
   
#### second tab plots render ####    
    
    observe({
      updateSelectInput(session, "listing_id", choices=selected_listings()$listing_id, selected=input$listing_id)
      updateSelectInput(session, "listing_name", choices=selected_listings()$name, selected=input$listing_name)
      updateSelectInput(session, "host_name", choices=selected_listings()$host_name, selected=input$host_name)
    })

     observeEvent(input$reset_filters2,{
       updateSelectInput(session, "listing_id", choices=selected_listings()$listing_id, selected=NULL)
       updateSelectInput(session, "listing_name", choices=selected_listings()$name, selected=NULL)
       updateSelectInput(session, "host_name", choices=selected_listings()$host_name, selected=NULL)
     })
    
    selected_listings <- reactive ({
      df <- selected_data() %>%
        {
          if (!is.null(input$listing_id)) {
            filter(., listing_id %in% input$listing_id)
          }
          else
            select_all(., )
        } %>%
        
        {
          if (!is.null(input$listing_name)) {
            filter(., name %in% input$listing_name)
          }
          else
            select_all(., )
        } %>%
        
        {
          if (!is.null(input$host_name)) {
            filter(., host_name %in% input$host_name)
          }
          else
            select_all(., )
        } 
    })

    
    observe({
    output$map2 <- renderLeaflet({
      leaflet(#options = leafletOptions(preferCanvas = FALSE),
        height = "30%px",
        width = 200,
        selected_listings() %>%
          .[, c("listing_id", "name", "host_name",  "longitude", "latitude")]
      ) %>%
        addTiles() %>% addMarkers(lat=~latitude, lng=~longitude, label = ~htmlEscape(name), popup = ~htmlEscape(listing_id),
                                  clusterOptions = markerClusterOptions(spiderfyOnMaxZoom=TRUE, 
                                                                        zoomToBoundsOnClick=TRUE,
                                                                        showCoverageOnHover=TRUE))
    })  
    })
    
    
    
    ## respond to the filtered data
    observe({
      
      leafletProxy(mapId = "map2", data = selected_listings()%>%
                     .[, c("listing_id", "name", "host_name", "longitude", "latitude")]) %>%
        clearMarkers() %>%   ## clear previous markers
        addMarkers(lat=~latitude, lng=~longitude, label = ~htmlEscape(name), popup = ~htmlEscape(listing_id),
                   clusterOptions = markerClusterOptions(spiderfyOnMaxZoom=TRUE, 
                                                         zoomToBoundsOnClick=TRUE,
                                                         showCoverageOnHover=TRUE))
    })
    
    
    output$table <- renderDataTable({
      selected_listings() %>% select(listing_id, name, listing_start_date, listing_end_date,
                                     host_name, neighbourhood, price, room_type, number_of_reviews,
                                     availability_365)
    }, options=list(pageLength = 5, lengthChange = FALSE, info = FALSE, dom='tp'), rownames = FALSE)
    
    
    #render reviews data
    
    output$reviews <- DT::renderDataTable({
      
      if((selected_listings() %>% count())>100)
      
        {reviews %>% 
            filter(.,listing_id %in% (selected_listings()$listing_id)) %>% 
        select(listing_id, reviewer_name, date, comments) %>% 
        
        head(300)} 
      
      else
      {
        reviews %>% 
          filter(.,listing_id %in% (selected_listings()$listing_id)) %>% 
          select(listing_id, reviewer_name, date, comments)
      }
      
    }, options = list(#dom='t', #pageLength = 3#, 
      paging = FALSE,
      searching=FALSE
      #processing=FALSE #scrollX = TRUE
      ), rownames = FALSE)
    #### end ####
  }

  #### end ####
  
  
  #### Run the application ####
  shinyApp(ui = ui, server = server)
  
  #### end ####