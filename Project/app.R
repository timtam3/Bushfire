
# Load library
library(shiny)
library(dplyr)
library(leaflet)
library(readr)
library(KernSmooth)

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
        titlePanel("VICfire"),
        
        sidebarLayout(
            sidebarPanel(
                helpText("Create demographic maps of fire in Victoria"),
                
                selectInput("year", label = "Choose Year",
                            choices = c("Any", levels(factor(mydata$year))),selected = "Any"),
                checkboxGroupInput("reason", label = "Choose Reason:",
                                   choices = levels(factor(mydata$new_cause)), selected = "Any"),
                
                selectInput("month", label = "Choose Month:",
                            choices = c("Any",levels(factor(mydata$month))), selected = "Any")
            ),
            
            mainPanel(
                leafletOutput(outputId = "map")
            )
        )
    )
    
    # Server logic ----
    server <- function(input, output) {
        
        pal <- colorFactor(pal = c("#E69F00","#000000","#0072B2","#009E73", "#F0E442","#CC79A7"), domain = c("arson","lightning","burning_off_human","acidental_human","relight","other"))
        
        dataselected_1 <- reactive({
            if(input$year =="Any"){mydata <- mydata}
            else{
                mydata <- subset(mydata, year == input$year)}
        }) 
        
        dataselected_2 <- reactive({
            if(input$month =="Any"){mydata <- dataselected_1()}
            else{
                mydata <- subset(dataselected_1(), month == input$month)}
        }) 
        
        
        dataselected <- reactive({if(is.null(input$reason)){mydata <- subset(dataselected_2(), new_cause == 1)}
            else{mydata <- dataselected_2() %>% filter(new_cause %in% input$reason )}
            })
        
       
        
        
        output$map <- renderLeaflet({
            leaflet() %>% 
                addTiles() %>%
                addLegend(pal=pal, values=mydata$new_cause) %>%
                setView(lng= 144.7852, lat = -36.3913 , zoom = 6) %>%
            addCircleMarkers(data = dataselected(), lat =  ~lat, lng =~lon, 
                             radius = 2, 
                             color = ~pal(new_cause),
                             stroke = FALSE, fillOpacity = 20) %>%
                addCircleMarkers(data = mydata1, lat =  ~lat, lng =~lon, 
                                 radius = 2, color="#56B4E9",
                                 stroke = FALSE, fillOpacity = 0.2, group="all fire")%>%
                addPolygons(data=dd3,group = "fire",col="#56B4E9")%>%
                addPolygons(data=dd3_arson,group = "arson",col="#000000")%>%
                addPolygons(data=dd3_lightning,group = "lightning",col="#009E73")%>%
                addLayersControl(overlayGroups =c("all fire","fire","arson","lightning"))
        })
    }
    
    # Run app ----
    shinyApp(ui, server)
    
}
