library(shiny)
library(stringr)
library(sf)
library(leaflet)
library(gridExtra)
library(leaflet.minicharts)
library(manipulateWidget)
library(viridis)
library(pals)
library(raster)
library(htmltools)
library(htmlwidgets)
library(scales)
library(cowplot)
library(ggplot2)
library(png)



myLabelFormat = function(..., reverse_order = FALSE){ 
  if(reverse_order){ 
    function(type = "numeric", cuts){ 
      cuts <- sort(cuts, decreasing = T)
    } 
  }else{
    labelFormat(...)
  }
}

# Define palette for empty map (i.e. transparent)
emptypal<-colorRampPalette(c("#ffffff00","#ffffff00"),interpolate="spline")


#list all data sets
filelist<-list.files("./datasets/")
filetab<- data.frame("Taxon"=paste(word(filelist,2,sep="_"),word(filelist,3,sep="_"), sep="_")
                     )
filetab$Taxon<- as.character(filetab$Taxon)
filetab$Taxon[filetab$Taxon=="empty_ts"]<-"empty"
filetab<-data.frame("Taxon"=filetab[!duplicated(filetab$Taxon),])

speciesnames<- readRDS("./datasets/speciesnames.rds")
speciesnames$Species<- as.character(speciesnames$Species)
Encoding(speciesnames$Species)<- c("UTF-8")

filetab$Species<- speciesnames$Species[match(filetab$Taxon,speciesnames$Taxon)]
filetab$Species[filetab$Taxon=="empty"]<-c("empty")


filetab<-filetab[-which(is.na(filetab$Species)),]

spec_choices<- sort(unique(filetab$Species)[-which(unique(filetab$Species)=="empty")])

bundeslander<- readRDS("./datasets/bundeslander.rds")

## Overview on Species SOP and changes
species_summary<- readRDS("./datasets/speciessummary.rds")

sMon_image<- readPNG("./sMon_image.png")

species_summary$Species<- as.character(species_summary$Species)
Encoding(species_summary$Species)<-c("UTF-8")

germanmap<- leaflet() %>%
  addTiles() %>% 
  addPolygons(data=bundeslander, color="black", fillColor = NA, fillOpacity = 0, weight=0.8) %>%
  addScaleBar(position=c("bottomright")) %>% syncWith(groupname="germanmaps")


## 
ui<- fluidPage(
  sidebarPanel(
    radioButtons(inputId="Basemap",
                 label="Basemap",
                 choices=list("Open Street Map"="OpenStreetMap.Mapnik",
                              "Satellite (ESRI)"="Esri.WorldImagery"),
                 selected="OpenStreetMap.Mapnik"),
    br(),
    selectInput(inputId = "url",
                label="Natural Regions",
                choices=list("On"= "http://geodienste.bfn.de/ogc/wms/gliederungen?",
                             "Off"= NA),
                selected = "On"),
    br(),
    selectizeInput("Species",
                   "Select a Species",
                   choices=c("choose"="",spec_choices)
    ),
    br(),
    sliderInput(inputId="Opacity",
                label= "Opacity of active layer",
                value=0.8,
                min=0, max=1, step=0.1)
    , width=2,
    br(),
    plotOutput(outputId = "legends",height = "120px"),
    HTML("<font size=1> <p align='justify'> <em> We are grateful to the tremendous effort of the countless 
         people who contributed their plant occurrence records to the different sources that underlie this visualization!</em> </p><font>"),
    fluidRow(HTML("<font size=1> <p align='center'> <em>Project Links:</em>"),
             br(),
             column(4,tags$a(imageOutput("image1"),
                             href="https://www.idiv.de/en/smon",
                             target="_blank",
                             alt="Go to sMon Homepage")),
             column(8,tags$a(imageOutput("image2"),
                             href="https://www.idiv.de/",
                             target="_blank"))
           )),
    mainPanel(tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                                  Shiny.onInputChange("innerWidth", window.innerWidth);
                                  });
                                  $(window).resize(function(e) {
                                  Shiny.onInputChange("innerWidth", window.innerWidth);
                                  });
                                  ')),
            h4("Maps of occurrence probability (OP)"),
            HTML("<font size=2>This site is a companion to the publication of Eichenberg et al. (2020). <br> 
                 Occurrence probabilities are estimates of species occurrence probability 
                 corrected for potential biases due to incomplete reporting ('reporting bias') based 
                 on the Frescalo algorithm 
                 (<a href='https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.2041-210X.2011.00146.x'>Hill, 2012</a>).
                 For detailed information on the Frescalo algorithm and its specifications see Eichenberg et al (2020, Supplementary Information I).</font>"),
            br(),
            tabsetPanel(type = "tabs",
                        tabPanel("Occurence probabilities",
                                 fluidRow(column(4,h5("1960-1987 (t1)",align="center"),leafletOutput(outputId="Map1")),
                                          column(4,h5("1988-1996 (t2)",align="center"),leafletOutput(outputId="Map2")),
                                          column(4,h5("1997-2017 (t3)",align="center"),leafletOutput(outputId="Map3")))
                        )
                        ,
                        tabPanel("Uncertainty (SD)",
                                 fluidRow(column(4,h5("1960-1987 (t1)",align="center"),leafletOutput(outputId="Map4")),
                                          column(4,h5("1988-1996 (t2)",align="center"),leafletOutput(outputId="Map5")),
                                          column(4,h5("1997-2017 (t3)",align="center"),leafletOutput(outputId="Map6")))
                        )),
            HTML("<font size=1> <em>The information displayed here is based on the collection of approx. 29 mio occurrence records in Germany. <br>
                 Despite careful checking and statistical processing, the maps shown here come <b>without liability for completeness or incompleteness</b>. 
                 For detailed information on data sources see Eichenberg et al. (2020).</em>
                 <font>"),
            br(),
            br(),
            HTML("<font size=3>The table below shows SOP<sub>Spec</sub> of the 
                 respective species as well as the absolute changes across the 
                 three study periods.</font>"),
            br(),
            HTML("<font size=3> For details on the calculation of SOP<sub>Spec</sub> 
                 see Methods section in Eichenberg et al. (2020). </font>"),
            br(),
            tableOutput(outputId = "table"),
            width=10)
            )

server<- function(input, output) {
  output$Map1 = renderLeaflet({germanmap})
  output$Map2 = renderLeaflet({germanmap})
  output$Map3 = renderLeaflet({germanmap})
  output$Map4 = renderLeaflet({germanmap})
  output$Map5 = renderLeaflet({germanmap})
  output$Map6 = renderLeaflet({germanmap})
  
  output$image1<- renderImage({
      list(src = "./sMon_image.png",
           contentType = "image/png",
           width = "41px",
           height = "40px"
           )}, deleteFile = FALSE)
  
  output$image2<- renderImage({
    list(src = "./iDiv_image.png",
         contentType = "image/png",
         width = "60px",
         height = "24px"
    )}, deleteFile = FALSE)
  
  observeEvent(c(input$Basemap,input$url,input$Species,input$Opacity),{
    
    Species<- if(input$Species=="") {c("empty")} else{input$Species}
    url<- input$url
    
    opac<- if(Species=="empty") {c(0)} else{input$Opacity}
    
    
    line<- which(filetab$Species==Species)[1]
    
    mapdata1<- readRDS(paste0("./datasets/","ras_",filetab$Taxon[line],"_ts_1_OP.rds"))
    mapdata2<- readRDS(paste0("./datasets/","ras_",filetab$Taxon[line],"_ts_2_OP.rds"))
    mapdata3<- readRDS(paste0("./datasets/","ras_",filetab$Taxon[line],"_ts_3_OP.rds"))
    mapdata4<- readRDS(paste0("./datasets/","ras_",filetab$Taxon[line],"_ts_1_sd.rds"))
    mapdata5<- readRDS(paste0("./datasets/","ras_",filetab$Taxon[line],"_ts_2_sd.rds"))
    mapdata6<- readRDS(paste0("./datasets/","ras_",filetab$Taxon[line],"_ts_3_sd.rds"))
    
    
    
    val_mdat1<- values(mapdata1)[-which(is.na(values(mapdata1)))]
    val_mdat1<- val_mdat1[order(val_mdat1)]
    val_mdat2<- values(mapdata2)[-which(is.na(values(mapdata2)))]
    val_mdat2<- val_mdat2[order(val_mdat2)]
    val_mdat3<- values(mapdata3)[-which(is.na(values(mapdata3)))]
    val_mdat3<- val_mdat3[order(val_mdat3)]
    
    val_mdat4<- values(mapdata4)[-which(is.na(values(mapdata4)))]
    val_mdat4<- val_mdat4[order(val_mdat4)]
    val_mdat5<- values(mapdata5)[-which(is.na(values(mapdata5)))]
    val_mdat5<- val_mdat5[order(val_mdat5)]
    val_mdat6<- values(mapdata6)[-which(is.na(values(mapdata6)))]
    val_mdat6<- val_mdat6[order(val_mdat6)]
    
    allvalsop<-data.frame("values"=c(val_mdat1,val_mdat2,val_mdat3))
    allvalsop$values<- allvalsop$values[order(allvalsop$values)]
    allvalssd<-data.frame("values"=c(val_mdat4,val_mdat5,val_mdat6))
    allvalssd$values<- allvalssd$values[order(allvalssd$values)]
    
    k<-which(c(max(val_mdat1),max(val_mdat2),max(val_mdat3))==max(allvalsop$values))[1]
    l<-which(c(max(val_mdat4),max(val_mdat5),max(val_mdat6))==max(allvalssd$values))[1]
    
    op_all_colors<- colorNumeric(viridis(800),
                                 domain=allvalsop$values,
                                 na.color = "transparent")
    
    colors_op1<- colorRampPalette(op_all_colors(val_mdat1),interpolate="linear")
    colors_op2<- colorRampPalette(op_all_colors(val_mdat2),interpolate="linear")
    colors_op3<- colorRampPalette(op_all_colors(val_mdat3),interpolate="linear")
    
    sd_all_colors<- colorNumeric(cividis(800),
                                 domain=allvalssd$values,
                                 na.color = "transparent")
    
    colors_sd1<- colorRampPalette(sd_all_colors(val_mdat4),interpolate="linear")
    colors_sd2<- colorRampPalette(sd_all_colors(val_mdat5),interpolate="linear")
    colors_sd3<- colorRampPalette(sd_all_colors(val_mdat6),interpolate="linear")
    
    legdat_OP<- data.frame("dummy"=seq(from=0,to=1,length.out = 800),
                           "OP"=seq(from=0,to=1,length.out = 800))
    legdat_sd<- data.frame("dummy"=seq(from=0,to=1,length.out = 800),
                           "sd"=seq(from=min(get(paste0("val_mdat",l+3))),
                                    to=max(get(paste0("val_mdat",l+3))),
                                    length.out = 800))
    
    legplot_OP<- get_legend(ggplot()+geom_point(data=legdat_OP,aes(x=dummy,y=OP,fill=OP))+
                              scale_fill_gradientn(colors = viridis(800),name=c("Occurrence \n probability"))+
                              theme(legend.title = element_text(size = 9,hjust=0.5),
                                    legend.text = element_text(size=8)))
    
    legplot_sd<- get_legend(ggplot()+geom_point(data=legdat_sd,aes(x=dummy,y=sd,fill=sd))+
                              scale_fill_gradientn(colors = cividis(800),name=c("Standard \n deviation"))+
                              theme(legend.title = element_text(size = 9,hjust=0.5),
                                    legend.text = element_text(size=8)))
    
    legends<-plot_grid(legplot_OP,legplot_sd,ncol=2)
    
    palette_OP <- colorNumeric(
      rev(viridis(800)), 
      domain = rescale(values(get(paste0("mapdata",k))),to=c(0,1)), 
      na.color = "transparent"
    )
    
    palette_sd <- colorNumeric(
      rev(cividis(800)), 
      domain = values(get(paste0("mapdata",l+3))), 
      na.color = "transparent")
    
    leafletProxy("Map1") %>% 
      clearImages() %>%
      clearControls() %>%
      clearShapes() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution="Bundesamt für Naturschutz (BfN)") %>% 
      addRasterImage(mapdata1,colors = colors_op1(800),opacity = opac) %>% 
      addPolygons(data=bundeslander, color="black", fillColor = NA, fillOpacity = 0, weight=0.8)
    
    leafletProxy("Map2") %>% 
      clearImages() %>%
      clearControls() %>%
      clearShapes() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution="Bundesamt für Naturschutz (BfN)")%>% 
      addRasterImage(mapdata2,colors = colors_op2(800),opacity = opac)%>% 
      addPolygons(data=bundeslander, color="black", fillColor = NA, fillOpacity = 0, weight=0.8)
    
    
    leafletProxy("Map3") %>% 
      clearImages() %>%
      clearControls() %>%
      clearShapes() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution="Bundesamt für Naturschutz (BfN)")%>% 
      addRasterImage(mapdata3,colors = colors_op3(800),opacity = opac)%>% 
      addPolygons(data=bundeslander, color="black", fillColor = NA, fillOpacity = 0, weight=0.8)
    
    
    
    leafletProxy("Map4") %>% 
      clearImages() %>%
      clearControls() %>%
      clearShapes() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution="Bundesamt für Naturschutz (BfN)") %>% 
      addRasterImage(mapdata4,colors = colors_sd1(800),opacity = opac)%>% 
      addPolygons(data=bundeslander, color="black", fillColor = NA, fillOpacity = 0, weight=0.8)
    
    leafletProxy("Map5") %>% 
      clearImages() %>%
      clearControls() %>%
      clearShapes() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution="Bundesamt für Naturschutz (BfN)")%>% 
      addRasterImage(mapdata5,colors = colors_sd2(800),opacity = opac) %>% 
      addPolygons(data=bundeslander, color="black", fillColor = NA, fillOpacity = 0, weight=0.8)
    
    
    leafletProxy("Map6") %>% 
      clearImages() %>%
      clearControls() %>%
      clearShapes() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution="Bundesamt für Naturschutz (BfN)")%>% 
      addRasterImage(mapdata6,colors = colors_sd3(800),opacity = opac)%>% 
      addPolygons(data=bundeslander, color="black", fillColor = NA, fillOpacity = 0, weight=0.8)
    
    
    output$legends = renderPlot({plot(legends)})
    
    output$table = renderTable({
      species_summary[species_summary$Species==Species,]
    })
  })
}        


shinyApp(ui,server)
