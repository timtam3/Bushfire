
# Load library
library(shiny)
library(dplyr)
library(leaflet)
library(readr)
library(KernSmooth)
library(shinyWidgets)


# Load in training data
mydata <- read_csv("mydata.csv")
mydata1 <- mydata
arsondata <- mydata %>% filter(new_cause == "arson" )
lightningdata <- mydata %>% filter(new_cause == "lightning" )
burningdata <- mydata %>% filter(new_cause == "burning_off_human" )
otherdata <- mydata %>% filter(new_cause == "other")
accidentdata <- mydata %>% filter(new_cause == "acidental_human")
relightdata <- mydata %>% filter(new_cause == "relight")


d2d <- bkde2D(cbind(mydata$lon,mydata$lat),bandwidth=c(0.15,0.15))
lines<- contourLines(d2d$x1,d2d$x2,d2d$fhat)
dd1 <- sapply(1:length(lines),function(i) Polygon(as.matrix(cbind(lines[[i]]$x,lines[[i]]$y))))
dd2 <- sapply(1:length(lines),function(i) Polygons(list(dd1[[i]]),i))
poly_data <- data.frame(Value = sapply(1:length(lines),function(i) lines[[i]]$level))
dd3 <- SpatialPolygonsDataFrame(SpatialPolygons(dd2),data = poly_data)

d2d_arson <- bkde2D(cbind(arsondata$lon,arsondata$lat),bandwidth=c(0.15,0.15))
lines_arson<- contourLines(d2d_arson$x1,d2d_arson$x2,d2d_arson$fhat)
dd1_arson <- sapply(1:length(lines_arson),function(i) Polygon(as.matrix(cbind(lines_arson[[i]]$x,lines_arson[[i]]$y))))
dd2_arson <- sapply(1:length(lines_arson),function(i) Polygons(list(dd1_arson[[i]]),i))
poly_data_arson <- data.frame(Value = sapply(1:length(lines_arson),function(i) lines_arson[[i]]$level))
dd3_arson <- SpatialPolygonsDataFrame(SpatialPolygons(dd2_arson),data = poly_data_arson)


d2d_lightning <- bkde2D(cbind(lightningdata$lon,lightningdata$lat),bandwidth=c(0.15,0.15))
lines_lightning<- contourLines(d2d_lightning$x1,d2d_lightning$x2,d2d_lightning$fhat)
dd1_lightning <- sapply(1:length(lines_lightning),function(i) Polygon(as.matrix(cbind(lines_lightning[[i]]$x,lines_lightning[[i]]$y))))
dd2_lightning <- sapply(1:length(lines_lightning),function(i) Polygons(list(dd1_lightning[[i]]),i))
poly_data_lightning <- data.frame(Value = sapply(1:length(lines_lightning),function(i) lines_lightning[[i]]$level))
dd3_lightning <- SpatialPolygonsDataFrame(SpatialPolygons(dd2_lightning),data = poly_data_lightning)


if (interactive()) {
    # User interface ----
    ui <- fluidPage(
        navbarPage("VICfire", id="main",
                   

        tabPanel("Historical data map",strong("Buscador"),value="panel2",
                 tags$head(tags$style(HTML(".multicol{font-size:12px;
                                                  height:auto;
                                                  -webkit-column-count: 2;
                                                  -moz-column-count: 2;
                                                  column-count: 2;
                                                  }

                                                  div.checkbox {margin-top: 0px;}"))),
        
        sidebarLayout(
            sidebarPanel(
                helpText("Create demographic maps of fire in Victoria"),
                
                sliderInput("year", label = "Choose Year",
                            value = c(2010,2017),min = min(mydata$year), max=max(mydata$year),step = 1),
                
                checkboxGroupInput("reason", label = "Choose Reason:",
                                   choices = levels(factor(mydata$new_cause)), selected = "Any"),
                
                checkboxGroupButtons("month", label = "Choose Month:",
                            choices = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), individual = TRUE,
                            checkIcon = list(
                                yes = tags$i(class = "fa fa-circle", 
                                             style = "color: steelblue",),
                                no = tags$i(class = "fa fa-circle-o", 
                                            style = "color: steelblue")))
            ),
            
            mainPanel(
                leafletOutput(outputId = "map",height=450)
            )
        )
    ),
    tabPanel("Predicted data map"),
    tabPanel("Tables")
    )
    )
    
    
    
    
    
    # Server logic ----
    server <- function(input, output) {
        
        pal <- colorFactor(pal = c("#E69F00","#000000","#0072B2","#009E73", "#F0E442","#CC79A7"), domain = c("arson","lightning","burning_off_human","acidental_human","relight","other"))
        
        dataselected_1 <- reactive({
            mydata <- subset(mydata,year >= input$year[1] & year <= input$year[2])
        }) 
        
        dataselected_2 <- reactive({
                mydata <- subset(dataselected_1(), month %in% input$month)
        }) 
        
        
        dataselected <- reactive({if(is.null(input$reason)){mydata <- subset(dataselected_2(), new_cause == 1)}
            else{mydata <- dataselected_2() %>% filter(new_cause %in% input$reason )}
        })
        
        #filtered <- reactive({dataselected()[dataselected()$year >= input$range[1] & dataselected()$year <= input$range[2],]})
       
        output$map <- renderLeaflet({
            leaflet() %>% 
                addTiles() %>%
                addLegend(pal=pal, values=mydata$new_cause) %>%
                addLayersControl(overlayGroups =c("all fire","arson","lightning"))%>%
                setView(lng= 144.7852, lat = -36.3913 , zoom = 6)%>%
                addPolygons(data=dd3,group = "all fire",weight=1,col="#56B4E9")%>%
                addPolygons(data=dd3_arson,group = "arson",col="#000000")%>%
                addPolygons(data=dd3_lightning, group = "lightning",weight=1,col="#009E73")%>%
                hideGroup(c("all fire","arson","lightning"))
        
        })
        
        observe({leafletProxy("map") %>%
            clearMarkers()%>%
            addCircleMarkers(data = dataselected(), lat =  ~lat, lng =~lon, 
                             radius = 2, 
                             color = ~pal(new_cause),
                             stroke = FALSE, fillOpacity = 20)

            
        })
        
            
        
            
        
    }
    
    # Run app ----
    shinyApp(ui, server)
    
}
