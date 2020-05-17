### COMMUTE ANALYZER ####

# import trips data
trips_pa <- read_csv("data/trips_pa_apr2.csv")
trips_sz <- read_csv("data/trips_sz_apr2.csv")

trips_pa_n <- read_csv("data/trips_pa_apr3_n.csv")
trips_sz_n <- read_csv("data/trips_sz_pa_ap4.csv")

pa_id <- read_csv("data/pa_id.csv")
sz_id <- read_csv("data/sz_id.csv")

# import shape files
pa_shp <- st_read(dsn = "data/geospatial", layer = "MP14_PLNG_AREA_WEB_PL")
sz_shp <- st_read(dsn = "data/geospatial", layer = "MP14_SUBZONE_WEB_PL")

# rename shape files col header to view name on map
pa_shp1 <- pa_shp %>% rename("name" = "PLN_AREA_N")
sz_shp1 <- sz_shp %>% rename("name" = "SUBZONE_N")

# current bounding box
bbox_new <- st_bbox(pa_shp1)

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[1] <- bbox_new[1] - (0.15 * xrange) # xmin - left
bbox_new[2] <- bbox_new[2] + (0.15 * yrange) # ymin - bottom

# new bounding box
bbox_new <- bbox_new %>%  
st_as_sfc()

# read modified intramax code
source("Intramax_v2.R")

# switch on interactive mode
tmap_mode("view")

# server.R
function(input,output, session){
  
  output$plot1 <- renderPlotly({
    
    # conditional statement to assign the relevant boundary object
    if(input$boundary == "PLANNING AREA"){
      select_boundary_n <- trips_pa_n
    } 
    
    ## plotting adjacency matrix - including intra regions
    if(input$intraconnect == "in")
    {
      select_trips_n <- select_boundary_n %>%
        filter (DAY_TYPE == input$day,
                TIME_PER_HOUR == input$hour) %>%
        select(from, to, trips)
    }
    ## plotting adjacency matrix - including intra regions
    else if(input$intraconnect == "out")
    {
      select_trips_n <- select_boundary_n %>%
        filter (DAY_TYPE == input$day,
                TIME_PER_HOUR == input$hour, 
                from != to) %>%
        select(from, to, trips)
    }
    
    ##Converting to matrix
    # reshape2::acast() creates the correct matrix without "from" col header
    select_trips_n_matrix <- acast(select_trips_n, from ~ to, value.var = "trips")
    
    # replace NA with 0
    select_trips_n_matrix[is.na(select_trips_n_matrix)] <- 0
    
    # remove the "NA" col which is at the last col
    select_trips_n_matrix <- select_trips_n_matrix[,-ncol(select_trips_n_matrix)]
    
    Blues <- colorRampPalette(brewer.pal(11,"Blues"))
    
    # Plot heatmap
    if(input$normalize == "yes")
    {
      heatmaply(round(normalize(select_trips_n_matrix),3),
                Colv=NA,
                Rowv = NA, 
                plot_method= "plotly",
                label_names = c("ORIGIN", "DESTINATION","Normalized Trip Counts"),
                xlab = "DESTINATION",
                ylab = "ORIGIN",
                colors = Blues,
      )
    }
    else if(input$normalize == "no")
    {
      heatmaply(select_trips_n_matrix,
                Colv=NA,
                Rowv = NA, 
                plot_method= "plotly",
                label_names = c("ORIGIN", "DESTINATION","Trip Counts"),
                xlab = "DESTINATION",
                ylab = "ORIGIN",
                colors = Blues,
              
      )
    }
  })
  
  output$allocation <- renderUI({

     if(input$boundary == "PLANNING AREA"){
       select_boundary <- trips_pa
       select_shp <- pa_shp1
     } else if (input$boundary == "SUBZONE"){
       select_boundary <- trips_sz
       select_shp <- sz_shp1
      }

     # filter trips data based on UI
     select_trips <- select_boundary %>%
       filter (DAY_TYPE == input$day,
               TIME_PER_HOUR == input$hour) %>%
       select(from, to, trips)

     for(i in unique(select_trips$from)){
       df = select_trips %>% filter (to == i,from == i)
       if(is.data.frame(df) && nrow(df) == 0){
         select_trips <- rbind(select_trips,data.frame(from = i, to = i, trips = 0))
       }
     }

     for(i in unique(select_trips$to)){
       df = select_trips %>% filter (from == i,to == i)
       if(is.data.frame(df) && nrow(df) == 0){
         select_trips <- rbind(select_trips,data.frame(from = i, to = i, trips = 0))
       }
     }

     # prepare trips matrix
     ## reshape2::acast() creates the correct matrix without "from" col header
     matrix <- acast(select_trips, from ~ to, value.var = "trips")

     ## replace NA with 0
     matrix[is.na(matrix)] <- 0

     # run intramax
     Intramax(matrix)
     
     trips <- read_csv("trips.csv")
     
     values <- trips$`Cumulative Intrazonal Interaction`[which((trips$`Cumulative Intrazonal Interaction`) >= 75 & (trips$`Cumulative Intrazonal Interaction`) < 100)]
     values_final <- unique(values)
     
     #Create the select input for percent
     selectInput(inputId = "percent", #name of input
                       label = "Please select the percentage (%) of total flows to visualize functional transport area clusters:", #label displayed in ui
                       choices = values_final,
                       selected = values_final[1])
     
  })

  output$map <- renderLeaflet({

    # conditional statement to assign the relevant boundary object
    if(input$boundary == "PLANNING AREA"){
      select_boundary <- trips_pa
      select_shp <- pa_shp1
    } else if (input$boundary == "SUBZONE"){
      select_boundary <- trips_sz
      select_shp <- sz_shp1
    }

    # filter trips data based on UI
    select_trips <- select_boundary %>%
      filter (DAY_TYPE == input$day,
              TIME_PER_HOUR == input$hour) %>%
      select(from, to, trips)

    for(i in unique(select_trips$from)){
      df = select_trips %>% filter (to == i,from == i)
      if(is.data.frame(df) && nrow(df) == 0){
        select_trips <- rbind(select_trips,data.frame(from = i, to = i, trips = 0))
      }
    }

    for(i in unique(select_trips$to)){
      df = select_trips %>% filter (from == i,to == i)
      if(is.data.frame(df) && nrow(df) == 0){
        select_trips <- rbind(select_trips,data.frame(from = i, to = i, trips = 0))
      }
    }

    # prepare trips matrix
    ## reshape2::acast() creates the correct matrix without "from" col header
    matrix <- acast(select_trips, from ~ to, value.var = "trips")

    ## replace NA with 0
    matrix[is.na(matrix)] <- 0

    # run intramax
    Intramax(matrix)

    alloc <- read_csv("alloc.csv") %>%
      # transform alloc number from dbl to char for viz on map
     mutate_all(as.factor)

    ## Join alloc to shp file
    alloc_shp <- select_shp %>%
      # alloc_shp <- pa_shp %>%
      # transform OBJECTID as char to align with alloc
      mutate_at(vars(OBJECTID), as.factor) %>%
      # join alloc to pa shp file for viz
      left_join(alloc, c("OBJECTID" = "X1"))
    
    # Read trips data
    trips <- read_csv("trips.csv")
    
    for (i in 1:length(trips$`Cumulative Intrazonal Interaction`)){
      allocs <- trips %>%
        filter(trips$`Cumulative Intrazonal Interaction`== input$percent)
    }
    allocid <- allocs$X1[nrow(allocs)]

    # Visualize allocation number on map
    ## plot map
      tmmap <- tm_shape(alloc_shp, bbox = bbox_new) +
      tm_fill(allocid, style = "cat", palette = "Paired", alpha = 0.7, id = "name", title = "Cluster", showNA = FALSE) +
      tm_borders(alpha = 0.5) +
      tmap_options(max.categories = 100)
    
      
      tmap_leaflet(tmmap)

  })
  
  output$dendro <- renderPlot({
    
    req(input$boundary,
        input$day,
        input$hour)
    
    # conditional statement to assign the relevant boundary object
    if(input$boundary == "PLANNING AREA"){
      select_boundary <- trips_pa
      select_shp <- pa_shp1
      select_id <- pa_id
    } else if (input$boundary == "SUBZONE"){
      select_boundary <- trips_sz
      select_shp <- sz_shp1
      select_id <- sz_id
    }
    
    # filter trips data based on UI
    select_trips <- select_boundary %>%
      filter (DAY_TYPE == input$day,
              TIME_PER_HOUR == input$hour) %>%
      select(from, to, trips)
    
    # code to create a symmetrical matrix (for hour of day with asymmetrical trips matrix)
    for(i in unique(select_trips$from)){
      df = select_trips %>% filter (to == i,from == i)
      if(is.data.frame(df) && nrow(df) == 0){
        select_trips <- rbind(select_trips,data.frame(from = i, to = i, trips = 0))
      }
    }
    
    for(i in unique(select_trips$to)){
      df = select_trips %>% filter (from == i,to == i)
      if(is.data.frame(df) && nrow(df) == 0){
        select_trips <- rbind(select_trips,data.frame(from = i, to = i, trips = 0))
      }
    }
    
    # prepare trips matrix
    ## reshape2::acast() creates the correct matrix without "from" col header
    matrix <- acast(select_trips, from ~ to, value.var = "trips")
    
    ## replace NA with 0
    matrix[is.na(matrix)] <- 0
    
    # run intramax
    Intramax(matrix)
    
    ## import data
    pair <- read_csv("pair.csv") 
    
    # transform dataframe to edge list format, note that instead of "from" in col1, "to" is in col1 for the dendrogram to work
    pair1 <- melt(pair, id="X1")
    pair2 <- dcast(pair1, variable ~ X1)
    pair3 <- pair2 %>% select(c(2,3,1))
    
    vertex1 <- pair3 %>% 
      select(c(1,2))
    
    vertex2 <- melt(vertex1)
    
    vertex3 <- vertex2 %>%
      left_join(select_id, c("value" = "id")) %>%
      rename(id = value) %>%
      select(c(2,3,4)) %>%
      distinct(id, name, REGION) %>%
      arrange(id)
    
    # convert dataframe to igraph and merge with id-name list
    mygraph <- graph_from_data_frame(pair3,
                                     directed = TRUE,
                                     vertices = vertex3)
    
    # plot dendrogram
    ggraph(mygraph, layout = "dendrogram", circular = TRUE) +
      geom_edge_diagonal() +
      geom_node_point() +
      geom_node_label(aes(label = name, col = REGION), size = 4, repel = TRUE) +
      theme_void() +
      theme(legend.position="bottom", legend.box = "horizontal")

  })

  output$from_c <- renderLeaflet({

    req(input$boundary,
        input$day,
        input$hour)

    # conditional statement to assign the relevant boundary object
    if(input$boundary == "PLANNING AREA"){
      select_boundary <- trips_pa
      select_shp <- pa_shp1
      select_id <- pa_id
    } else if (input$boundary == "SUBZONE"){
      select_boundary <- trips_sz
      select_shp <- sz_shp1
      select_id <- sz_id
    }

    # filter trips data based on UI
    select_trips <- select_boundary %>%
      filter (DAY_TYPE == input$day,
              TIME_PER_HOUR == input$hour) %>%
      select(from, to, trips)

    # group trips by "from" id
    trips_fr <- select_trips %>%
      group_by(from) %>%
      summarise(trips = sum(trips)) %>%
      ungroup()

    # join trips to shp file
    trips_fr_shp <- select_shp  %>%
      # join alloc to shp file for viz
      left_join(trips_fr, c("OBJECTID" = "from"))

    # plot choropleth map "from"
    tmmap_fr <-
      tm_shape(trips_fr_shp, bbox = bbox_new) +
      tm_fill("trips", palette = "Greens", alpha = 0.7, id = "name", style = "order", showNA = FALSE) +
      tm_borders(alpha = 0.5)

    # render leaflet
    tmap_leaflet(tmmap_fr)

  })

  output$to_c <- renderLeaflet({

    req(input$boundary,
        input$day,
        input$hour)

    # conditional statement to assign the relevant boundary object
    if(input$boundary == "PLANNING AREA"){
      select_boundary <- trips_pa
      select_shp <- pa_shp1
      select_id <- pa_id
    } else if (input$boundary == "SUBZONE"){
      select_boundary <- trips_sz
      select_shp <- sz_shp1
      select_id <- sz_id
    }

    # filter trips data based on UI
    select_trips <- select_boundary %>%
      filter (DAY_TYPE == input$day,
              TIME_PER_HOUR == input$hour) %>%
      select(from, to, trips)

    # group trips by "to" id
    trips_to <- select_trips  %>%
      group_by(to) %>%
      summarise(trips = sum(trips)) %>%
      ungroup()

    # join trips to shp file
    trips_to_shp <- select_shp  %>%
      # join alloc to shp file for viz
      left_join(trips_to, c("OBJECTID" = "to"))

    # plot choropleth map "to"
    tmmap_to <-
      tm_shape(trips_to_shp, bbox = bbox_new) +
      tm_fill("trips", palette = "Greens", alpha = 0.7, id = "name", style = "order", showNA = FALSE) +
      tm_borders(alpha = 0.7)

    # render leaflet
    tmap_leaflet(tmmap_to)

  })
  
  output$pdf_viewer <- renderUI ({
    tags$iframe(style="height:600px; width:100%; scrolling=yes",
                src="USER GUIDE FOR COMMUTE ANALYZER.pdf")
    
  })

}
