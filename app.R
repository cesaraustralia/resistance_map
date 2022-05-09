# data preparation --------------------------------------------------------
library(tidyverse)
library(shiny)
library(shinythemes)
library(leaflet)
library(sf)
library(DT)


# reading and cleaning map for plotting
aussmap <- sf::st_read("spatial_data/regions.gpkg", quiet = TRUE)

# reading processed data
resist_data <- read_csv("app_data/resist_data.csv") %>%
  mutate(CHEM.GROUP = paste0(tolower(CHEM.GROUP)," (",CHEM.IRAC,")")) %>%
  mutate(SOURCE = paste(AUTHOR, YEAR)) %>%
  # placeholder !!!
  mutate(LEARN.MORE =
           paste0("<a href='", "https://reshub.miek.com.au/pests/","'>", "ResHub","</a>")) %>%
  complete(REGION = unique(aussmap$NRM_REGION),
           nesting(SPECIES, CHEM.GROUP, COMMON.ACTIVES),
           fill = list(SOURCE="", SEVERITY="NO KNOWN RESISTANCE"))

pesticide <- read_csv("app_data/IRAC_DATA_Chemical_Codes.csv") %>%
  mutate(ACTIVE = tolower(ACTIVE))
pests = read_csv("app_data/pest_synonyms.csv")
pests = pests %>%
  mutate(PEST = str_split(PEST, "; ")) %>%
  unnest(PEST) %>%
  bind_rows(tibble(SPECIES = pests$SPECIES, PEST = pests$SPECIES)) %>%
  distinct

# columns to show in the table
select_fields <-  c("SPECIES", "REGION", "CHEM.GROUP", "COMMON.ACTIVES",
                    "SEVERITY", "SOURCE")
main_fields <-  c("REGION", "CHEM.GROUP", "COMMON.ACTIVES", "SEVERITY")


#  aggregate the resistance cases per region
resist_region <- resist_data$REGION %>%
  table() %>%
  as.data.frame() %>%
  setNames(c("NRM_REGION", "Resistance"))
# calculate the pest numbers
pest_num <- resist_data %>%
  filter(SEVERITY != "NO KNOWN RESISTANCE") %>%
  group_by(REGION) %>%
  summarise(npest = length(unique(SPECIES)), .groups="drop") %>%
  select(NRM_REGION = REGION, npest)
# generate the default values for all resistance
aussmap <- aussmap %>%
  # dplyr::select(- Resistance) %>%
  left_join(resist_region, by = "NRM_REGION") %>%
  left_join(pest_num, by = "NRM_REGION")


# default palette for the map
mypal <- colorBin(palette = viridis::viridis(option = "D", n = 6, direction = -1),
                  na.color = "white",
                  domain = aussmap$Resistance,
                  bins = c(0, 1, 5, 10, 50, 100, 500))
# default label format
labels <- sprintf(
  "<strong>%s</strong><br/> Resistance cases: %s<br/> Number of pests: %s",
  aussmap$NRM_REGION,
  ifelse(is.na(aussmap$Resistance), "No known resistance", aussmap$Resistance),
  ifelse(is.na(aussmap$npest), "No known resistance", aussmap$npest)
) %>% lapply(htmltools::HTML)


# default legend title
# legend_title <- "For all pests and <br> chemical groups"
legend_title <- "Resistance cases"

#
# User Interface (UI) -----------------------------------------------------
# Define UI
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  fluidRow(style = "height:500px",

           # sidebarLayout(
           # sidebarPanel(
           #   width = 4,
           #     # style = "height:500px",
           #     #
           #     selectizeInput(
           #       "pest", "Insect pest", choices = sort(unique(pests$PEST)),
           #       options = list(placeholder = 'Please search for a pest',
           #                      onInitialize = I('function() { this.setValue(""); }'))
           #     ),
           #
           #     selectizeInput(
           #       "active", "Active ingredient", choices = sort(unique(pesticide$ACTIVE)),
           #       options = list(placeholder = 'Please search for an active',
           #                      onInitialize = I('function() { this.setValue(""); }'))
           #     # )
           #   ),
           # # )
           mainPanel(width = 12, leafletOutput("map", height = 500))

  ),

  fluidRow(DT::dataTableOutput("mytable"))

)


# Server ------------------------------------------------------------------
# Define server logic
server <- function(input, output, session) {

  # observeEvent(input$reset_input, {
  #   updateSelectizeInput(
  #     session, inputId = "pest", label = "Insect pest",
  #     choices = sort(unique(pests$PEST)),
  #     options = list(placeholder = 'Please search for a pest',
  #                    onInitialize = I('function() { this.setValue(""); }'))
  #   )
  #
  #   updateSelectizeInput(
  #     session,inputId = "active", label = "Active ingredient",
  #     choices = sort(unique(pesticide$ACTIVE)),
  #     options = list(placeholder = 'Please search for an active',
  #                    onInitialize = I('function() { this.setValue(""); }'))
  #   )
  #
  # })
  pest <- NULL
  active <- NULL

  # the url query
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['pest']])) {
      # updateTextInput(session, "pest", value = query[['pest']])
      if(query[['pest']] %in% unique(pests$PEST)){
        pest <<- query[['pest']]
      } else{
        pest <<- NULL
      }
    }
    if (!is.null(query[['active']])) {
      # updateNumericInput(session, "active", value = query[['active']])
      active <<- query[['active']]
    }
    print(query[['pest']])
  })


  ## the default leaflet map
  output$map <- renderLeaflet({
    ## Create map object and add tiles and polygon layers to it
    # browser()
    leaflet(data = aussmap) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lat = -28, lng = 135, zoom = 4) %>%
      addPolygons(fillColor = ~ mypal(Resistance),
                  color="grey",
                  weight = 2,
                  dashArray = "3",
                  highlight =
                    highlightOptions(
                      weight = 3,
                      color = "red",
                      fillOpacity = 0.8,
                      bringToFront = TRUE),
                  layerId = ~ NRM_REGION,
                  # label =  ~ NRM_REGION,
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "12px",
                    direction = "auto")) %>%
      addLegend(pal = mypal,
                values = ~ Resistance,
                na.label = "No known resistance",
                title = legend_title,
                opacity = 0.5,
                layerId = "legend",
                position = "bottomleft") %>%
      leafem::addHomeButton(st_bbox(aussmap),
                            group = "Go full extent",
                            position = "bottomright",
                            add = TRUE)

  })


  # updating the map based on click or clear button
  feature <- NULL
  # observeEvent(input$reset_input, {
  #   feature <<- NULL
  # })

  observeEvent(input$map_shape_click, {
    feature <<- input$map_shape_click$id
  })


  # change the status of the app based on user reaction
  observe({

    # update the app if any of these changes
    input$map_shape_click
    # input$reset_input

    # update the extent based on user's reaction
    if(is.null(feature)){
      # return to the full-extent
      bbox <- as.numeric(sf::st_bbox(aussmap))
    } else{
      # get the feature and update the box
      bbox <- as.numeric(sf::st_bbox(aussmap[aussmap$NRM_REGION == feature, ]))
    }


    # no table to show
    outtable <- resist_data
    mapdata  <- resist_data

    # # get input values
    # product_input <- ifelse(is.null(input$product), "", input$product)
    # pest_input <- ifelse(is.null(pest), "", pest)

    # filter if pest is selected
    if(is.null(pest) || pest==""){
      # do nothing
    } else {

      species <- pests %>%
        filter(PEST==pest) %>%
        pull(SPECIES) %>%
        unique()

      mapdata <- outtable <- outtable %>%
        filter(SPECIES==species)

      # mapdata <- mapdata %>%
      #   filter(SPECIES==species)

      # update map title
      legend_title <- "Cases for pest"

    }

    # # filter base on product
    # if(is.null(active) || active==""){
    #   # do nothing
    # } else{
    #   chem_irac = pesticide %>%
    #     filter(ACTIVE==active) %>%
    #     pull(CHEM.IRAC) %>%
    #     unique()
    #
    #   outtable = outtable %>%
    #     filter(CHEM.IRAC==chem_irac)
    #
    #   mapdata = mapdata %>%
    #     filter(CHEM.IRAC==chem_irac)
    #
    #   # update map title
    #   legend_title <- "Cases for <br> chemical group"
    #
    # }

    # update the legend title
    if((!is.null(active) && active!="") && (!is.null(pest) && pest!="")){
      legend_title <- "For selected pest <br> and chemical group"
    }

    # # filter the table if a region is selected
    # if(is.null(feature)){
    #   # do nothing
    # } else{
    #   # grepstr = paste(croped_map$NRM_REGION, collapse="|")
    #   outtable = outtable %>%
    #     filter(REGION == feature) %>%
    #     # mutate(REGION = if_else(REGION == feature,
    #     #                         paste("<b>",REGION,"</b>"), REGION)) %>%
    #     # # dplyr::select(RESISTANCE, select_fields) %>%
    #     # # distinct() %>%
    #     # # group_by(across(c(-REGION))) %>%
    #     # # summarise(REGION = paste(REGION, collapse="; "), .groups="drop") %>%
    #     # mutate(regres = grepl(feature, REGION)) %>%
    #     # arrange(desc(regres)) %>%
    #     # dplyr::select(-regres) %>%
    #     identity()
    #
    #   # change the colours in the map based on the click
    #   mapdata <- mapdata %>%
    #     filter(REGION == feature)
    # }


    ## create empty table but let the map to be with full data
    if((is.null(pest) || pest=="") && (is.null(active) || active=="") && is.null(feature)){
      outtable <- outtable %>%
        dplyr::select(RESISTANCE, main_fields) %>%
        filter(RESISTANCE == -99999) # create an empty table
    }

    # add the interactive table
    output$mytable = DT::renderDataTable({
      outtable %>%
        dplyr::select(RESISTANCE, select_fields) %>%
        distinct() %>%
        group_by(across(c(-REGION, -SOURCE))) %>%
        summarise(
          REGION = paste(unique(REGION), collapse="; "),
          SOURCE = paste(unique(SOURCE), collapse="; "),
          .groups="drop") %>%
        dplyr::select(main_fields) %>%
        distinct() %>%
        # mutate(SPECIES =
        #          if_else(pest!=SPECIES,
        #                  paste("<i>", SPECIES, "</i> <br> <b>", pest, "</b>"),
        #                  paste("<i>", SPECIES, "</i>"))) %>%
        DT::datatable(style = 'bootstrap4' ,
                      escape=FALSE,
                      options = list(pageLength = 10)
        ) %>%
        DT::formatStyle(
          # table,
          columns = main_fields,
          # valueColumns = columns,
          # target = c("cell", "row"),
          # fontWeight = NULL,
          # color = "black"
          # backgroundColor = "black",
          # background = "black",
          # ...
        ) %>%
        # DT::formatStyle(
        #   columns = "LEARN.MORE",
        #   backgroundColor = "grey"
        # ) %>%
        formatStyle(
          'SEVERITY',
          backgroundColor = styleEqual(c("NONE", "LOW","MODERATE","HIGH"),
                                       c('white', '#ffff75', "#ffc875", "#ff7575")),
          color = styleEqual(c("NONE", "LOW","MODERATE","HIGH"),
                             c("white", "black", "black", "black"))
        )
    })


    # update the colour of the map
    if(all(is.na(mapdata$RESISTANCE))){
      aussmap$Resistance <- NA
      aussmap$npest <- NA
    } else{
      resist_region <- mapdata %>%
        filter(SEVERITY != "NO KNOWN RESISTANCE") %>%
        group_by(REGION) %>%
        summarise(Resistance = n(), .groups="drop") %>%
        dplyr::select(NRM_REGION = REGION, Resistance)
      pest_num <- mapdata %>%
        filter(SEVERITY != "NO KNOWN RESISTANCE") %>%
        group_by(REGION) %>%
        summarise(npest = length(unique(SPECIES)), .groups="drop") %>%
        select(NRM_REGION = REGION, npest)
      aussmap <- aussmap %>%
        dplyr::select(- c(Resistance, npest)) %>%
        left_join(resist_region, by = "NRM_REGION") %>%
        left_join(pest_num, by = "NRM_REGION")
    }


    ## update leaflet map with each click
    if(is.null(feature)){
      ausmapsub <- aussmap
    } else{
      ausmapsub <- aussmap
      # ausmapsub <- aussmap %>% filter(NRM_REGION == feature) # change this to the colour of map with no label
    }

    # # re-create palette for the map
    # mypal <- colorBin(palette = viridis::viridis(option = "D", n = 6, direction = -1),
    #                   na.color = "white",
    #                   domain = ausmapsub$Resistance,
    #                   bins = c(0, 1, 5, 10, 50, 100, 500))
    # re-create labels
    # labels <- sprintf(
    #   "<strong>%s</strong><br/> Resistance cases: %s<br/> Number of pests: %s",
    #   ausmapsub$NRM_REGION,
    #   ifelse(is.na(ausmapsub$Resistance), "No known resistance", ausmapsub$Resistance),
    #   ifelse(is.na(ausmapsub$npest), "No known resistance", ausmapsub$npest)
    # ) %>% lapply(htmltools::HTML)

    labels <- sprintf(
      "<strong>%s</strong><br/> Resistance cases: %s<br/> Pests: %s",
      ausmapsub$NRM_REGION,
      ifelse(is.na(ausmapsub$Resistance), "No known resistance", ausmapsub$Resistance),
      ifelse(is.na(ausmapsub$npest), "No known resistance", ifelse(is.null(pest), "All", pest))
    ) %>% lapply(htmltools::HTML)


    leafletProxy("map", data = ausmapsub) %>%
      flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
      clearShapes() %>%
      removeControl(layerId = "legend")  %>%
      addPolygons(fillColor = ~ mypal(Resistance),
                  color="grey",
                  weight = 2,
                  dashArray = "3",
                  highlight = highlightOptions(weight = 3,
                                               color = "black",
                                               fillOpacity = 0.8,
                                               bringToFront = TRUE),
                  layerId = ~ NRM_REGION,
                  # label =  ~ NRM_REGION,
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "12px",
                    direction = "auto")) %>%
      addLegend(pal = mypal,
                values = ~ Resistance,
                na.label = "No known resistance",
                title = legend_title,
                opacity = 0.5,
                layerId = "legend",
                position = "bottomleft")


  })

}


# Run the app -------------------------------------------------------------
shinyApp(ui = ui, server = server)

# The end -----------------------------------------------------------------
