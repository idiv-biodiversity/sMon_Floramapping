# wrap leaflet proxy chain for easier maintainability
my_leaflet_proxy <- function(map,
                             provider,
                             url,
                             mapdata,
                             colorset,
                             opac,
                             bundeslander) {

  leafletProxy(map) %>%

  clearImages() %>%
  clearControls() %>%
  clearShapes() %>%

  addProviderTiles(provider) %>%

  addWMSTiles(
    baseUrl = url,
    layers = "Naturraeume",
    options = WMSTileOptions(transparent = TRUE, format = "image/png"),
    attribution = "Bundesamt für Naturschutz (BfN)"
  ) %>%

  addRasterImage(
    mapdata,
    colors = colorset,
    opacity = opac
  ) %>%

  addPolygons(data = bundeslander,
    color = "black",
    fillColor = NA,
    fillOpacity = 0,
    weight = 0.8
  )
}
