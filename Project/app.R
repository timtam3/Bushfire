# Load library
library(shiny)
library(dplyr)
library(leaflet)
library(readr)
library(KernSmooth)
library(shinyWidgets)
library(plotly)
library(sp)
library(mapview)
library(leafem)
library(rgdal)
library(maptools)
library(raster)
library(DT)


# Load in training data
mydata <- read_csv("mydata1.csv")
mydata1 <- mydata
mydata2<- read_csv("mydata2.csv")
mydata4<- read_csv("mydata4.csv")
prediction <- read_csv("prediction.csv")
simulation <- read_csv("simulation.csv")
ystart = -39.08246
yend = -34.03690
xstart = 140.6923
xend = 149.8976
y = seq(ystart+0.1/2,yend -0.1/2 ,.1)
x = seq(xstart + 0.181/2, xend - 0.181/2, 0.181)
png <- "https://cdn.cfa.vic.gov.au/o/cfa-theme/images/cfa-logo.png"

if (interactive()) {
    # User interface ----
    ui <- fluidPage(
        navbarPage("VICfire", id="main",
                   
                   tabPanel("Historical data map",
                            fluidRow(
                                
                                column(8,align="left",
                                       sidebarLayout(
                                           sidebarPanel(
                                               helpText("Create demographic maps of fire in Victoria"),
                                               
                                               sliderInput("year", label = "Choose Year",
                                                           value = c(2010,2017),min = min(mydata$year), max=max(mydata$year),step = 1),
                                               
                                               checkboxGroupButtons("month", label = "Choose Month:",
                                                                    choices = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                                                                    individual = TRUE,justified = FALSE,
                                                                    width = "100%"),
                                               
                                               checkboxGroupInput("reason", label = "Choose Reason:",
                                                                  choices = levels(factor(mydata$new_cause)), selected = "Any"),
                                               actionButton("showd","Show density plot"),
                                               actionButton("cleard","Clear density plot")),
                                           
                                           mainPanel(
                                               leafletOutput(outputId = "map",height = 580)))),
                                
                                
                                column(4, 
                                       conditionalPanel(#condition = "output.condition1 == 0",
                                           br(),tags$h4("About",style='color:blue'),p('This Shiny App helps visualise fires in Victoria for last two decades.
                            You can choose different conditions to check fires happened in different years, months for different starting reasons.')
                                       ),
                                       wellPanel(
                                           tabsetPanel(
                                               tabPanel(tags$em("Percentage",style="font-size:100%"),
                                                        tags$hr(style="border-color: #ffc266;"),
                                                        plotlyOutput("p1"),
                                                        plotlyOutput("p2")
                                               ),
                                               
                                               tabPanel(tags$em("Rainfall",style="font-size:100%"),
                                                        tags$hr(style="border-color:  #d27979;"),
                                                        plotlyOutput("rain")),
                                               
                                               tabPanel(tags$em("Temperature",style="font-size:100%"),
                                                        tags$hr(style="border-color:  #ffc266;"),
                                                        plotlyOutput("temp")
                                               )
                                           )
                                       )
                                       
                                )
                            )
                   ),
                   
                   tabPanel("Predicted data map", 
                            fluidRow(
                                column(8,align="left",
                                       sidebarLayout(
                                           sidebarPanel(
                                               helpText("Predicted demographic maps of fire in Victoria"),
                                               checkboxGroupButtons("month1", label = "Choose Month:",
                                                                    choices = c("Jan","Feb","Mar","Oct","Nov","Dec"),
                                                                    individual = TRUE,justified = FALSE,
                                                                    width = "100%"),
                                               checkboxGroupInput("reason1", label = "Choose Reason:",
                                                                  choices = levels(factor(prediction$new_cause)), selected = "Any")),
                                           mainPanel(leafletOutput(outputId = "map2",height = 330),leafletOutput(outputId = "map3",height = 330))
                                           
                                       )),
                                wellPanel(
                                    tabsetPanel(
                                        # tabPanel(tags$em("Percentage",style="font-size:100%"),
                                        #          tags$hr(style="border-color: #ffc266;"),
                                        #          plotlyOutput("p3"),
                                        #          plotlyOutput("p4")
                                        # ),
                                        
                                        tabPanel(tags$em("Rainfall",style="font-size:100%"),
                                                 tags$hr(style="border-color:  #d27979;"),
                                                 plotlyOutput("rain1")),
                                        
                                        tabPanel(tags$em("Temperature",style="font-size:100%"),
                                                 tags$hr(style="border-color:  #ffc266;"),
                                                 plotlyOutput("temp1")
                                        )
                                    )
                                )
                                
                            )
                   ),
                   
                   
                   
                   tabPanel("Data", DT::dataTableOutput("data")),
                   tabPanel("Read Me")
        )
    )
    
    
    # Server logic ----
    server <- function(input, output) {
        
        pal <- colorFactor(pal = c("#E69F00","#000000","#0072B2","#009E73", "#F0E442","#CC79A7"), domain = c("arson","lightning","burningoff","accident","relight","other"))
        pal1 <- colorFactor(pal = c("#E69F00","#000000","#0072B2","#009E73"), domain = c("arson","lightning","burningoff","accident"))
        
        
        
        
        dataselected_1 <- reactive({
            mydata <- subset(mydata,year >= input$year[1] & year <= input$year[2])
        }) 
        
        dataselected_2 <- reactive({
            mydata <- subset(dataselected_1(), month %in% input$month)
        }) 
        
        
        dataselected <- reactive({if(is.null(input$reason)){subset(dataselected_2(), new_cause == 1)}
            else{dataselected_2() %>% filter(new_cause %in% input$reason )}
        })
        
        
        pre_1 <- reactive({
            subset(prediction, month %in% input$month1)
        })
        
        pre_2 <- reactive({if(is.null(input$reason1)){subset(pre_1(), new_cause == 1)}
            else{pre_1() %>% filter(new_cause %in% input$reason1)}
        })
        
        d2d <- bkde2D(cbind(mydata$lon,mydata$lat),bandwidth=c(.0045, .0068), gridsize = c(100,100))
        # datasimulation <- simulation
        # lonsimulation = datasimulation[,1]
        # latsimulation = datasimulation[,2]
        # numlonsimulation = length(lonsimulation)
        # numlatsimulation = length(latsimulation)
        # lat <- seq(ystart, yend, 0.1)
        # lon <- seq(xstart, xend, 0.181)
        # latsimulation_pos = rep(0, length(lat)-1)
        # lonsimulation_pos = rep(0, length(lon)-1)
        # 
        # countsimulation = matrix(0,nrow = length(latsimulation_pos), ncol = length(lonsimulation_pos))
        # for (i in 1:numlonsimulation){
        #   print(i)
        #   lonsimulation_1 = lonsimulation[i]
        #   latsimulation_1 = latsimulation[i]
        #   if ((latsimulation_1 >= min(lat)) && lonprediction_1>= min(lon)&& (lonsimulation_1 <= max(lon)) && (latsimulation_1 <= max(lat)))
        #   {
        #     lon_pos = max(which(lonsimulation_1 >= lon))
        #     lat_pos = max(which(latsimulation_1 <= lat))
        #     countsimulation[lat_pos,lon_pos] = countsimulation[lat_pos,lon_pos] + 1
        #   }
        # }
        # 
        # dataprediction <- prediction
        # lonprediction = dataprediction[,2]
        # latprediction = dataprediction[,3]
        # numlonprediction = length(lonprediction)
        # numlatprediction = length(latprediction)
        # 
        # countprediction = matrix(0,nrow = length(latsimulation_pos), ncol = length(lonsimulation_pos))
        # for (i in 1:numlonprediction){
        #   print(i)
        #   lonprediction_1 = lonprediction[i]
        #   latprediction_1 = latprediction[i]
        #   if ((latprediction_1 >= min(lat)) && lonprediction_1>= min(lon) && (lonprediction_1 <= max(lon)) && (latprediction_1 <= max(lat)))
        #   {lon_pos = max(which(lonprediction_1 >= lon))
        #   lat_pos = max(which(latprediction_1 <= lat))
        #   countprediction[lat_pos,lon_pos] = countprediction[lat_pos,lon_pos] + 1
        #   }
        # }
        # ratio = countprediction/countsimulation
        # ratio[is.nan(ratio)] = 0
        # ratio[!is.finite(ratio)] = 0
        # 
        # 
        # 
        # ratio <- apply(ratio, 1, rev)
        # ls <- list("x1" = x, "x2" = y, "fhat" = ratio)
        # KernelDensityRaster <- raster(list(x=ls$x1 ,y=ls$x2 ,z = ls$fhat))
        KernelDensityRaster <- raster(list(x=d2d$x1 ,y=d2d$x2 ,z = d2d$fhat))
        KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA
        palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values, na.color = "transparent")
        
        
        
        #density polygons
        selectedarson <- reactive({dataselected() %>% filter(new_cause == "arson")})
        d2d_arson <- reactive({bkde2D(cbind(selectedarson()$lon, selectedarson()$lat), bandwidth=c(0.15,0.15))})
        lines_arson<- reactive({contourLines(d2d_arson()$x1,d2d_arson()$x2,d2d_arson()$fhat)})
        dd1_arson <- reactive({sapply(1:length(lines_arson()), function(i) Polygon(as.matrix(cbind(lines_arson()[[i]]$x,lines_arson()[[i]]$y))))})
        dd2_arson <- reactive({sapply(1:length(lines_arson()), function(i) Polygons(list(dd1_arson()[[i]]),i))})
        poly_data_arson <- reactive({data.frame(Value = sapply(1:length(lines_arson()), function(i) lines_arson()[[i]]$level))})
        dd3_arson <- reactive({SpatialPolygonsDataFrame(SpatialPolygons(dd2_arson()),data = poly_data_arson())})
        
        selectedlightning <- reactive({dataselected() %>% filter(new_cause == "lightning")})
        d2d_lightning <- reactive({bkde2D(cbind(selectedlightning()$lon,selectedlightning()$lat),bandwidth=c(0.15,0.15))})
        lines_lightning<- reactive({contourLines(d2d_lightning()$x1,d2d_lightning()$x2,d2d_lightning()$fhat)})
        dd1_lightning <- reactive({sapply(1:length(lines_lightning()), function(i) Polygon(as.matrix(cbind(lines_lightning()[[i]]$x,lines_lightning()[[i]]$y))))})
        dd2_lightning <- reactive({sapply(1:length(lines_lightning()), function(i) Polygons(list(dd1_lightning()[[i]]),i))})
        poly_data_lightning <- reactive({data.frame(Value = sapply(1:length(lines_lightning()), function(i) lines_lightning()[[i]]$level))})
        dd3_lightning <- reactive({SpatialPolygonsDataFrame(SpatialPolygons(dd2_lightning()),data = poly_data_lightning())})
        
        selectedburningoff <- reactive({dataselected() %>% filter(new_cause == "burningoff")})
        d2d_burningoff <- reactive({bkde2D(cbind(selectedburningoff()$lon, selectedburningoff()$lat), bandwidth=c(0.15,0.15))})
        lines_burningoff<- reactive({contourLines(d2d_burningoff()$x1,d2d_burningoff()$x2,d2d_burningoff()$fhat)})
        dd1_burningoff <- reactive({sapply(1:length(lines_burningoff()), function(i) Polygon(as.matrix(cbind(lines_burningoff()[[i]]$x,lines_burningoff()[[i]]$y))))})
        dd2_burningoff <- reactive({sapply(1:length(lines_burningoff()), function(i) Polygons(list(dd1_burningoff()[[i]]),i))})
        poly_data_burningoff <- reactive({data.frame(Value = sapply(1:length(lines_burningoff()), function(i) lines_burningoff()[[i]]$level))})
        dd3_burningoff <- reactive({SpatialPolygonsDataFrame(SpatialPolygons(dd2_burningoff()),data = poly_data_burningoff())})
        
        selectedaccident <- reactive({dataselected() %>% filter(new_cause == "accident")})
        d2d_accident <- reactive({bkde2D(cbind(selectedaccident()$lon,selectedaccident()$lat),bandwidth=c(0.15,0.15))})
        lines_accident<- reactive({contourLines(d2d_accident()$x1,d2d_accident()$x2,d2d_accident()$fhat)})
        dd1_accident <- reactive({sapply(1:length(lines_accident()), function(i) Polygon(as.matrix(cbind(lines_accident()[[i]]$x,lines_accident()[[i]]$y))))})
        dd2_accident <- reactive({sapply(1:length(lines_accident()), function(i) Polygons(list(dd1_accident()[[i]]),i))})
        poly_data_accident <- reactive({data.frame(Value = sapply(1:length(lines_accident()), function(i) lines_accident()[[i]]$level))})
        dd3_accident <- reactive({SpatialPolygonsDataFrame(SpatialPolygons(dd2_accident()),data = poly_data_accident())})
        
        
        selectedrelight <- reactive({dataselected() %>% filter(new_cause == "relight")})
        d2d_relight <- reactive({bkde2D(cbind(selectedrelight()$lon,selectedrelight()$lat),bandwidth=c(0.15,0.15))})
        lines_relight<- reactive({contourLines(d2d_relight()$x1,d2d_relight()$x2,d2d_relight()$fhat)})
        dd1_relight <- reactive({sapply(1:length(lines_relight()), function(i) Polygon(as.matrix(cbind(lines_relight()[[i]]$x,lines_relight()[[i]]$y))))})
        dd2_relight <- reactive({sapply(1:length(lines_relight()), function(i) Polygons(list(dd1_relight()[[i]]),i))})
        poly_data_relight <- reactive({data.frame(Value = sapply(1:length(lines_relight()), function(i) lines_relight()[[i]]$level))})
        dd3_relight <- reactive({SpatialPolygonsDataFrame(SpatialPolygons(dd2_relight()),data = poly_data_relight())})
        
        
        selectedother <- reactive({dataselected() %>% filter(new_cause == "other")})
        d2d_other <- reactive({bkde2D(cbind(selectedother()$lon,selectedother()$lat),bandwidth=c(0.15,0.15))})
        lines_other<- reactive({contourLines(d2d_other()$x1,d2d_other()$x2,d2d_other()$fhat)})
        dd1_other <- reactive({sapply(1:length(lines_other()), function(i) Polygon(as.matrix(cbind(lines_other()[[i]]$x,lines_other()[[i]]$y))))})
        dd2_other <- reactive({sapply(1:length(lines_other()), function(i) Polygons(list(dd1_other()[[i]]),i))})
        poly_data_other <- reactive({data.frame(Value = sapply(1:length(lines_other()), function(i) lines_other()[[i]]$level))})
        dd3_other <- reactive({SpatialPolygonsDataFrame(SpatialPolygons(dd2_other()),data = poly_data_other())})
        
        clicked_map <- reactiveValues(clickedMarker=NULL)
        observeEvent(input$map_marker_click,{
            clicked_map$clickedMarker <- input$map_marker_click
        })         
        
        selected_coordinates <- reactive(({
            c(clicked_map$clickedMarker$lng,clicked_map$clickedMarker$lat)
        }))
        
        
        clicked<- reactive(({
            subset(dataselected(),lon == as.numeric(selected_coordinates()[1]) & lat == as.numeric(selected_coordinates()[2]))
        }))
        
        
        output$rain <- renderPlotly({
            rain=clicked()
            if(is.null(rain))
                return(NULL)
            plot_ly(
                x = c(" 7day","14day","28day"),
                y = c(rain$arf7,rain$arf14,rain$arf28),
                name = "rain fall",
                type = "bar") %>%
                layout(title = '',
                       xaxis = list(title = "Period Average rain fall"),
                       yaxis = list(title = "mm"))
        })
        
        
        output$temp <- renderPlotly({
            temp=clicked()
            if(is.null(temp))
                return(NULL)
            plot_ly(
                x = c(" 7day","14day","28day"),
                y = c(temp$amaxt7,temp$amaxt14,temp$amaxt28),
                type = 'scatter', mode = 'lines',name="max") %>%
                add_trace(y = c(temp$amint7,temp$amint14,temp$amint28),name="min")%>%
                layout(title = '',
                       xaxis = list(title = "Period Average Max/Min Temperature"),
                       yaxis = list(title = "°C"))
        })
        
        
        
        
        clicked_map2 <- reactiveValues(clickedMarker=NULL)
        observeEvent(input$map2_marker_click,{
            clicked_map2$clickedMarker <- input$map2_marker_click
        })
        
        selected_coordinates1 <- reactive(({
            c(clicked_map2$clickedMarker$lng,clicked_map2$clickedMarker$lat)
        }))
        
        
        clicked1<- reactive(({
            subset(pre_2(),lon == as.numeric(selected_coordinates1()[1]) & lat == as.numeric(selected_coordinates1()[2]))
        }))
        
        
        output$rain1 <- renderPlotly({
            rain=clicked1()
            if(is.null(rain))
                return(NULL)
            plot_ly(
                x = c(" 7day","14day","28day"),
                y = c(rain$arf7,rain$arf14,rain$arf28),
                name = "rain fall",
                type = "bar") %>%
                layout(title = '',
                       xaxis = list(title = "Period Average rain fall"),
                       yaxis = list(title = "mm"))
        })
        
        
        output$temp1 <- renderPlotly({
            temp=clicked1()
            if(is.null(temp))
                return(NULL)
            plot_ly(
                x = c(" 7day","14day","28day"),
                y = c(temp$amaxt7,temp$amaxt14,temp$amaxt28),
                type = 'scatter', mode = 'lines',name="max") %>%
                add_trace(y = c(temp$amint7,temp$amint14,temp$amint28),name="min")%>%
                layout(title = '',
                       xaxis = list(title = "Period Average Max/Min Temperature"),
                       yaxis = list(title = "°C"))
        })
        
        
        
        # initiate a plotly object
        
        tx <- highlight_key(mydata2,~new_cause)
        
        # initiate a plotly object
        base <- plot_ly(tx, color = ~new_cause) %>% group_by(year)
        
        # create a time series of median house price
        t <- base %>%
            group_by(new_cause) %>%
            add_lines(x = ~year, y = ~Total,legendgroup = ~new_cause)
        
        hist <- add_histogram(base, x= ~Total, histnorm = "probability density")
        
        
        gig <- ggplot(mydata2, aes(x = year, y = Total, fill = new_cause)) +
            geom_bar(stat = "identity")
        
        fig <- plot_ly(mydata4, 
                       x = ~year, 
                       y = ~accident, 
                       type = 'bar', 
                       name = 'accident') %>%
            add_trace(y = ~arson, name = 'arson')%>% 
            add_trace(y = ~burningoff, name = 'burningoff')%>%
            add_trace(y = ~lightning, name = 'lightning')%>%
            add_trace(y = ~other, name = 'other')%>%
            add_trace(y = ~relight, name = 'relight')%>%
            layout(yaxis = list(title = 'Count'), barmode = 'stack')
        
        
        output$p1 <- renderPlotly({
            subplot(t,fig,nrows=2,shareX = T)%>% layout(legend = list(tracegroupgap=6),yaxis2=list(ticks = "outside",dtick = 50,  tick0 = 0,tickmode = "array" ))
        })
        
        
        output$percentage <- renderPlot({
            ggplot() + 
                geom_bar(data=filter(dataselected(), new_cause==input$reason),
                         aes(x=year,y=fire,fill = new_cause,label= "fire"),stat='identity') + 
                scale_fill_manual("legend", values = c("arson" = "#000000","lightning" ="#009E73", "relight" = "#CC79A7","other"="#F0E442","accident"="#E69F00","burningoff"="#0072B2"))
            
        })
        
        output$map <- renderLeaflet({
            leaflet() %>% 
                addTiles() %>%
                addLegend(pal=pal, values=mydata$new_cause) %>%
                addLayersControl(overlayGroups =c("show all fire"))%>%
                setView(lng= 144.7852, lat = -36.3913 , zoom = 6.3)%>%
                addMouseCoordinates()%>%
                leafem::addLogo(png, url = "https://www.cfa.vic.gov.au/home")%>%
                addCircles(data = mydata1, lat =  ~lat, lng =~lon,
                           radius = 100, color="#C0C0C0",
                           stroke = FALSE, fillOpacity = 0.7, group="show all fire")%>%
                hideGroup("show all fire")
            
        })
        
        
        
        observe({leafletProxy("map") %>%
                clearMarkers() %>%
                addCircleMarkers(data = dataselected(), lat =  ~lat, lng =~lon, 
                                 radius = 3, 
                                 color = ~pal(new_cause),
                                 stroke = FALSE, fillOpacity = 20, 
                                 popup= ~paste0("Fire starts at: ", FIRE_START, "<br/>",
                                                "Wind speed:", round(ws,2),"<br/>",
                                                "Fire reason: ", new_cause,"<br/>",
                                                "Forest types: ",FOR_TYPE,"<br/>",
                                                "Distance to road: ",round(dist_road)
                                 ))
            
        })
        
        
        observeEvent(input$showd,
                     {
                         leafletProxy("map") %>%
                             addTiles() %>%clearGroup(group="density")
                         
                         if ("accident" %in% input$reason)
                         { 
                             leafletProxy("map") %>%
                                 addTiles() %>%
                                 addPolygons(data=dd3_accident(),col="#E69F00",group="plot density",stroke = FALSE)}
                         
                         if ("arson" %in% input$reason){
                             leafletProxy("map") %>%
                                 addTiles() %>%
                                 addPolygons(data=dd3_arson(),col="#000000",group="plot density",stroke = FALSE)}
                         
                         if ("burningoff" %in% input$reason){
                             leafletProxy("map")  %>%
                                 addTiles() %>%
                                 addPolygons(data=dd3_burningoff(),col="#0072B2",group="plot density",stroke = FALSE)}
                         
                         if ("lightning" %in% input$reason){
                             leafletProxy("map") %>%
                                 addTiles() %>%
                                 addPolygons(data=dd3_lightning(),col="#009E73",group="plot density",stroke = FALSE)}
                         
                         if ("other" %in% input$reason){
                             leafletProxy("map")%>%
                                 addTiles() %>%
                                 addPolygons(data=dd3_other(),col="#F0E442",group="plot density",stroke = FALSE)}
                         
                         if ("relight" %in% input$reason){
                             leafletProxy("map")  %>%
                                 addTiles() %>%
                                 addPolygons(data=dd3_relight(),col="#CC79A7",group="plot density",stroke = FALSE)}
                         
                         
                     }
        )
        
        
        observeEvent(input$cleard,{
            leafletProxy("map") %>% addTiles() %>%
                clearGroup(group="plot density")
            
        })
        
        
        output$map2 <- renderLeaflet({
            leaflet() %>%
                addTiles() %>%
                addLegend(pal=pal1, values=prediction$new_cause) %>%
                setView(lng= 144.7852, lat = -36.3913 , zoom = 6.3)%>%
                addMouseCoordinates()
            
        })
        
        observe({leafletProxy("map2") %>%
                clearMarkers() %>%
                addCircleMarkers(data = pre_2(), lat =  ~lat, lng =~lon,
                                 radius = 3,
                                 color = ~pal1(new_cause),
                                 stroke = FALSE, fillOpacity = 20,
                                 popup= ~paste0("Fire reason: ", new_cause,"<br/>",
                                                "Forest types: ",FOR_TYPE,"<br/>",
                                                "Distance to road: ",round(dist_road))
                )
        })
        
        output$map3 <- renderLeaflet({
            leaflet() %>%
                addTiles() %>%
                setView(lng= 144.7852, lat = -36.3913 , zoom = 6.3)
            
        })
        
        output$map3 <- renderLeaflet({
            leaflet() %>% addTiles() %>% 
                addRasterImage(KernelDensityRaster, 
                               colors = palRaster, 
                               opacity = .4) %>%
                addLegend(pal = palRaster, 
                          values = KernelDensityRaster@data@values, 
                          title = "Fire Probability")
            
        })
        
        observe({leafletProxy("map3") %>%
                addRasterImage(KernelDensityRaster, 
                               colors = palRaster, 
                               opacity = .4) %>%
                addLegend(pal = palRaster, 
                          values = KernelDensityRaster@data@values, 
                          title = "Fire Probability")
        })
        
        
        output$data <-DT::renderDataTable(datatable(
            mydata[,c(2:4,7,9,10,12,13,64)],filter = 'top',
            colnames = c("EventID","Fire name","Fire district","Fire Start","Longitude","Latitude","Forest Type","Forest Category",
                         "Cause")
        ))
        
        
    }
    # Run app ----
    shinyApp(ui, server)
    
}

