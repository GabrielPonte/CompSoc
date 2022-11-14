library(shiny)
library(leaflet)
library(fontawesome)
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

#greenLeafIcon <- makeIcon(fa_png(name = "map-pin"),18,18)
greenLeafIcon <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
  iconWidth = 12.5, iconHeight = 20.5,
  iconAnchorX = 6, iconAnchorY = 20.5,
  shadowUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png",
  shadowWidth = 0.5, shadowHeight = -17,
  shadowAnchorX = 20.5, shadowAnchorY = 20.5
)
ui <- fluidPage(
  leafletOutput("mymap"),
  p()
)
server <- function(input, output, session) {
  my_table <- read.csv("https://raw.githubusercontent.com/GabrielPonte/CompSoc/main/bancos_python.csv")
  points <- cbind(my_table$longitude,my_table$latitude)
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points,popup = my_table$banco,icon=greenLeafIcon)
      #addMarkers(data = points,popup = my_table$banco)
      #addCircleMarkers(data=points,popup = my_table$banco,fillOpacity = 1,fillColor = "goldenrod",stroke = F,radius = 2)
  })
}
shinyApp(ui, server)
