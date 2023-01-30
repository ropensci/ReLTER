# library(shiny)
# require(magrittr)
#library(sf)
#library(sp)
#library(leaflet)
#library(leaflet.extras)
#' Select sites by choosing a 3d bounding box through an interactive GUI.
#' @description Open a GUI (a shiny widget) to interact with the user for the 
#' selection of a 3D bounding box. The user can visualise the sites within the selected
#' bounding box and ask for returning the contained sites or simply the bounding box.
#' @return `tibble` with selected sites or 
#' a `list` with selected bounding box (with slots `bbx` and `elevation_range`).
#' @author Paolo Tagliolato
get_site_interactive <- function(){
  gui = TRUE
  if (gui) {
    #nocov start
    gui_deps <- c("leaflet", "shiny",
                  "leaflet.extras",
                  "magrittr")
    gui_deps_missing <- !sapply(gui_deps, requireNamespace, quietly = TRUE)
    
    if (sum(gui_deps_missing) > 0) {
      stop("You need to install the following Suggested packages to use this function.
           Please install them with:
           install.packages(c(\"leaflet\", \"shiny\",\"leaflet.extras\", \"magrittr\"))")
    } else {
      requireNamespace("leaflet")
      requireNamespace("shiny")
      requireNamespace("leaflet.extras")
      requireNamespace("magrittr")
    }
    #nocov end
  }
#}

bbxSelectorUI <- function(id, label = "select bounding box", height=400) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    leaflet::leafletOutput(ns("mapleaflet"), height=height)
  )
  
}

bbxSelector <- function(input, output, session, reactiveToClearTheMap) {
  
  output$mapleaflet <- leaflet::renderLeaflet({
    leaflet::leaflet() %>% leaflet::addTiles(group="OpenStreetMap") %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group="CartoDB") %>%
      leaflet::setView(0, 0, 1) %>% leaflet.extras::addSearchOSM() %>%
      leaflet.extras::addDrawToolbar(
        targetGroup = "draw",
        polylineOptions = FALSE,
        markerOptions = FALSE,
        circleOptions = FALSE,
        circleMarkerOptions = FALSE,
        polygonOptions = FALSE,
        rectangleOptions = leaflet.extras::drawRectangleOptions(
          shapeOptions = leaflet.extras::drawShapeOptions(
          fillOpacity = 0,
          color = 'white',
          weight = 3
        )),
        singleFeature = TRUE
      )%>%
      leaflet::addLayersControl(baseGroups=c("OpenStreetMap","CartoDB"))
    
  })
  
  # RV reactive values. It contains the slots "layers" (list of downloaded layers - sp objects)
  # and a boolean flag "layersPresent" indicating if layers are already present in the layers list slot.
  RV <-
    shiny::reactiveValues(layers = list(),
                   layersPresent = FALSE,
                   queries = list())
  
  
  # BOUNDING BOX
  {
    # BBX reactive values. It contains "N" "S" "E" "W" slots.
    BBX <- shiny::reactiveValues(N = NULL,
                          S = NULL,
                          E = NULL,
                          W = NULL)
    
    # bbx reactive. It returns the concatenation (array) of BBX slots.
    # [the following is lazy and cached. It is reactive: it is notified when its dependencies change]
    bbx <- shiny::reactive({
      # it is the same to explicitly return the value:
      # return(c(BBX$W, BBX$S, BBX$E, BBX$N))
      # or to simply end the expression with:
      c(BBX$W, BBX$S, BBX$E, BBX$N)
    })
    
    bbx_df<-shiny::reactive({
      xmin=as.numeric(BBX$W)
      xmax=as.numeric(BBX$E)
      ymin=as.numeric(BBX$S)
      ymax=as.numeric(BBX$N)
      return(data.frame(x=c(xmin,xmax),y=c(ymin,ymax)))
    })
    
    crs_string<-reactive({
      bbox_dfl<-bbx_df()
      sf::st_crs(sf:st_sfc(sf::st_point(as.numeric(bbox_dfl[1,])), 
                           sf::st_point(as.numeric(bbox_dfl[2,])), 
                           crs=4326))$proj4string
    })
    
    
    # concatenation of bbx with "_" separator
    bbx_concat <- shiny::reactive({
      paste(bbx(), sep = "_", collapse = "_")
    })
    
    # # print bounding box bbx (reactive) in label
    # output$bbx <- renderText({
    #   bbx()
    # })
    
    # intercept new bounding box drawn by the user and store it in BBX reactive values
    # (note: its within the pattern: observer->no return value, but side effects)
    observeEvent(input$mapleaflet_draw_new_feature, {
      shape <- input$mapleaflet_draw_new_feature
      
      polygon_coordinates <- shape$geometry$coordinates
      
      feature_type <- shape$properties$feature_type
      if (feature_type == "rectangle") {
        NW = polygon_coordinates[[1]][[2]]
        SE = polygon_coordinates[[1]][[4]]
        
        BBX$N <- paste(NW[[2]])
        BBX$W <- paste(NW[[1]])
        BBX$S <- paste(SE[[2]])
        BBX$E <- paste(SE[[1]])
      }
    })
  

    }
  
  proxymap<-shiny::reactive({leaflet::leafletProxy("mapleaflet")})
  
  # RESET LAYERS
  # reset the layers (in the map and in the RV layers slot) when bbx() changes 
  {
    clearTheMap<-function(){
      cat("bbxSelector - ClearTheMap\n")
      proxy <- proxymap()#leafletProxy("mapleaflet")
      leaflet::clearShapes(proxy)
      leaflet::clearImages(proxy)
      RV$layers = list()
      RV$layersPresent = FALSE
      proxy %>% leaflet::addLayersControl(baseGroups=c("OpenStreetMap","CartoDB"))
      #%>% leaflet::removeLayersControl()
    }
    
    observeEvent(bbx(), {
      clearTheMap()
    })
    
    # ...any other observeEvent=>reset layers needed? 
    #  is there any way to reset layers even from the including app?
    observeEvent(reactiveToClearTheMap(),{
      req(proxymap())
      cat("bbxSelector: triggered reactiveToClearTheMap")
      clearTheMap()
    }, ignoreInit = TRUE)
    
  }
  
  
  return(list("bbx"=bbx,
              "proxymap"=proxymap, 
              "bbx_df"=bbx_df, 
              "crs_string"=crs_string))
  
  # NOTE: When invoking the module, accessing the return values
  # must be done like:
  # bbxSelector1$bbx()
  # bbxSelector1$proxymap()
  # i.e. access the list element as reactive, NOT the list as a reactive!!
}

plotInBbxSelector<-function(features,proxy){
  #browser()
  #proxy<-bbxSelector1$proxymap()
  #raster2plot<- raster::resample(currentInputRaster(),fact=4,expand=FALSE)
  #rasterResampled<-
  proxy %>%
    leaflet::clearShapes() %>%
    leaflet::clearImages() %>%
    #addRasterImage(raster2plot, opacity=0.7, group="inputDEM")  %>%
    leaflet::addCircles(data=features,
                        popup = paste0("<a href='",features$deimsid,"'>",features$name,">")
                        ) %>% 
    leaflet::addLayersControl(baseGroups=c("OpenStreeMap","CartoDB"),
                     overlayGroups="sites")
}

define_bbx <- function(){
  ui <- shiny::fluidPage(
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::actionButton("clearMap", "clear the map"),
          shiny::textOutput("bbxText")),
        
        shiny::mainPanel(
          bbxSelectorUI("bbxSelectorId","Bounding Box", height="20em"),
          shiny::sliderInput(inputId = "altitude",label = "Altitude", 
                             min = -10000, max = 9000, value=c(-10000, 9000),
                             step=100, dragRange = TRUE),
          shiny::actionButton('getSites', 'Retrieve LTER sites in selection'),
          shiny::actionButton('ok','Return Bounding Box'),
          shiny::actionButton('returnSites', "Return sites dataframe")
        )
      )
  )
  
  server <- function(input, output, session) {
    bbxSelector1<-shiny::callModule(bbxSelector, "bbxSelectorId", shiny::reactive(input$clearMap))
    output$bbxText <- shiny::renderText(bbxSelector1$bbx())
    
    sites <- shiny::reactiveValues()
    
    observeEvent(input$getSites, {
      sites$sites <- get_sites_within_3d_bounding_box(list(bbx=bbxSelector1$bbx_df(), 
                                            elevation_range=input$altitude), show_map=F)
      plotInBbxSelector(sites$sites, bbxSelector1$proxymap())
    })
    
    shiny::observeEvent(input$ok, {
      shiny::stopApp(list(bbx=bbxSelector1$bbx_df(), 
                          elevation_range=input$altitude))
    })
    
    shiny::observeEvent(input$returnSites, {shiny::stopApp(sites$sites)})
  }
  
  shiny::runGadget(ui, server)
}
  define_bbx()
}

#get_site_interactive()
#bbx3d<-define_bbx()

#get_sites_within_3d_bounding_box(define_bbx(), elevation_range = c(4000, 8000))

