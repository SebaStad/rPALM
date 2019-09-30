# source("base_functions.R")
#
#
# Building   <- "Building"
# Pavement   <- "Pavement"
# Vegetation <- "Vegetation"
# Water      <- "Water"
#
#
# Buildtypes <- c("Wohn bis 1950", "Wohn 1950 bis 2000", "Wohn seit 2000",
#                 "Buero bis 1950", "Buero 1950 bis 2000", "Buero seit 2000")
# Pavetypes  <- c("Nutzerdefiniert", "Unbekannter Asphalt", "Asphalt",
#                 "Beton", "Pflaster", "Pflastersteine (Paving)", "Pflastersteine (Cobble)",
#                 "Metal", "Holz", "Kies", "Feiner Kies", "Kieselstein", "Hackschnitzel",
#                 "Tartan", "Kunstrasen", "Lehm", "Gebaeude (Dummy)")
# Vegtypes   <- c("Nutzerdefiniert", "Erdboden", "Feld (Getreide)", "Kurzes Gras", "Immergrüne Nadelbäume",
#                 "Laubabwerfender Nadelbaum", "Immergrüner Laubbaum", "Laubbaum", "Hohes Gras",
#                 "Wüste", "Tundra", "Bewässertes Feld", "Halbwüste", "Gletscher (funktioniert nicht)",
#                 "Suempfe und Marsche", "Immergrüne Straeucher", "Laubabwerfende Straeucher", "Mischwald",
#                 "Unterbrochener Wald")
# Wattypes  <-  c("Nutzerdefiniert", "See", "Fluss", "Ozean", "Teich", "Brunnen")
#
#
# library(shiny)
# library(shinyTree)
# library(DT)


options(shiny.maxRequestSize=1000*1024^2) #Increase limit for file-upload to 1 GB

# https://stackoverflow.com/questions/39209411/shinytree-set-variable-to-value-if-checkbox-is-checked
# !!! Hilfreiche SO !!!
server <- shiny::shinyServer(function(input, output, session) {

  session$onSessionEnded(function() {
    stopApp()
  })

  output$tree <- shinyTree::renderTree({
    treelist <- list(
      "Global attributes" = structure("",                #Name
                                      sticon = "globe",   #Icon
                                      stselected=TRUE),  #Vorausgewaehlte Node
      "Topography" = structure("",stopened = TRUE, sticon = "image"),
      "Buildings" = structure(
        list(
        "Building Height" = structure("", sticon = "file"),
        "Building ID" = structure("", sticon = "file"),
        "Building Type" = structure("", sticon = "file")
        ),
      stopened=TRUE, sticon = "home"),
      "Vegetation" = structure(
        list(
          "Vegetation Type"=structure("", sticon="file"),
          # "LeafAreaIndex"=structure("", sticon="file"),
          "Vegetation Height"=structure("", sticon="file")
        ),
        stopened=TRUE, sticon = "tree"),
      "Water" = structure("", sticon = "tint"),
      "Pavement" = structure("", sticon = "road"),
      "Settings" = structure("", sticon = "cogs")#,
      #root1 = structure("", stselected=TRUE, sticon="tree"),
      #root2 = structure(list(
      #  SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
      #  SubListB = structure(list(leafA = "", leafB = ""), stdisabled=TRUE)
      #),
      #stopened=TRUE, #open tree at start
      #sticon = "tint"
      #)
    )
    treelist
  })

  output$treeselect <-reactive({
    tree <- input$tree
    unlist(get_selected(tree,format = "names"))
  })
  outputOptions(output, "treeselect", suspendWhenHidden = FALSE)



  #### PALM Globale ####
  PALMGlobale <- eventReactive(input$palmglobal,{
    if(!all(
      nchar(input$palmtitle) > 0 ,
      nchar(input$palmcreator) > 0 ,
      nchar(input$palminstitute) > 0 ,
      nchar(input$palmlocation) > 0 ,
      nchar(input$palmursprungx) > 0 ,
      nchar(input$palmursprungy) > 0 ,
      nchar(input$palmuHeightAMSL) > 0 ,
      nchar(input$palmlatitude) > 0 ,
      nchar(input$palmlongitude) > 0
    )){
      showNotification("Bitte füllen Sie die Globalen Attribute aus!", duration = 3, closeButton = TRUE, type = "error")
    }

    palmglob  <- palm_global$new(title = input$palmtitle, author = input$palmcreator,
                                 institute = input$palminstitute, location = input$palmlocation,
                                 x0 = as.numeric(input$palmursprungx),
                                 y0 = as.numeric(input$palmursprungy),
                                 z0 = as.numeric(input$palmuHeightAMSL),
                                 t0 = input$palmTime, #"2018-06-21 21:00:00 +00",
                                 lat = as.numeric(input$palmlatitude),
                                 lon = as.numeric(input$palmlongitude))
    palmglob$changeVar(variable = "resolution",
                       input = as.numeric(input$palmgrid))


    if(is.null(input$palmursprungx) | is.null(input$palmursprungy)){
      lat_grid <- as.data.frame(expand.grid( palmglob$head$origin_lon,
                                             palmglob$head$origin_lat))
      sputm2 <- SpatialPoints(lat_grid, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs") )# Defining Gauss Krüger)
      #spgeo2 <- spTransform(sputm2, CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
      spgeo2 <- spTransform(sputm2, CRS("+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
      palmglob$changeVar(variable = "origin_x",
                         input = as.numeric(as.data.frame(spgeo2)[1]))
      palmglob$changeVar(variable = "origin_y",
                         input = as.numeric(as.data.frame(spgeo2)[2]))
    }
    palmglob
  })

   PALMGlobale_Summary <- reactiveValues(title = "Eingabe fehlt",
                                         author = "Eingabe fehlt",
                                         institute = "Eingabe fehlt",
                                         location = "Eingabe fehlt",
                                         latitude = "Eingabe fehlt",
                                         longitude = "Eingabe fehlt",
                                         resolution = "Eingabe fehlt")
   PALMDATA <- reactiveValues(class = NULL)
   Bckup    <- reactiveValues(palmdata_1 = NULL)

   SortedFunction <- reactiveValues(HasBeen = FALSE)


   observeEvent(PALMGlobale(), {
     PALMGlobale_Summary$title      <- PALMGlobale()$head$title
     PALMGlobale_Summary$author     <- PALMGlobale()$head$author
     PALMGlobale_Summary$institute  <- PALMGlobale()$head$institution
     PALMGlobale_Summary$location   <- PALMGlobale()$head$location
     PALMGlobale_Summary$latitude   <- PALMGlobale()$head$origin_lat
     PALMGlobale_Summary$longitude  <- PALMGlobale()$head$origin_lon
     PALMGlobale_Summary$resolution <- PALMGlobale()$head$resolution

     # output$palmglSummary     <- renderText({paste("<b>Titel:</b>", PALMGlobale_Summary$title, "<br>",
     #  "<b>Author:</b>",   PALMGlobale_Summary$author,"<br>",
     #  "<b>Institut:</b>", PALMGlobale_Summary$institute,"<br>",
     #  "<b>Ort:</b>",      PALMGlobale_Summary$location,"<br>",
     #  "<b>Breite:</b>",   PALMGlobale_Summary$latitude,"<br>",
     #  "<b>Länge:</b>",    PALMGlobale_Summary$longitude,"<br>",
     #  "<b>Auflösung:</b>",PALMGlobale_Summary$resolution)})
   })


   #### Summary ####

   PALM_Summary <- reactiveValues(topography ="Eingabe fehlt",
                                  building2d ="Eingabe fehlt",
                                  buildingid ="Eingabe fehlt",
                                  buildingtype = "Eingabe fehlt",
                                  vegetationtype = "Eingabe fehlt",
                                  #leafareaindex = "Eingabe fehlt",
                                  vegetationheight = "Eingabe fehlt",
                                  water = "Eingabe fehlt",
                                  pavement="Eingabe fehlt",
                                  filling="kein Filling angewandt"
   )


   # output$palmSummary <- renderText({paste("<b>Titel:</b>", PALMGlobale_Summary$title, "<br>",
   #                                         "<b>Author:</b>",   PALMGlobale_Summary$author,"<br>",
   #                                         "<b>Institut:</b>", PALMGlobale_Summary$institute,"<br>",
   #                                         "<b>Ort:</b>",      PALMGlobale_Summary$location,"<br>",
   #                                         "<b>Breite:</b>",   PALMGlobale_Summary$latitude,"<br>",
   #                                         "<b>Länge:</b>",    PALMGlobale_Summary$longitude,"<br>",
   #                                         "<b>Auflösung:</b>",PALMGlobale_Summary$resolution,"<br>",
   #                                         "<b>Topografie:</b>",  PALM_Summary$topography, "<br>",
   #                                         "<b>Gebäude 2D:</b>",  PALM_Summary$building2d, "<br>",
   #                                         "<b>Gebäude ID:</b>",  PALM_Summary$buildingid, "<br>",
   #                                         "<b>Gebäudetyp:</b>",  PALM_Summary$buildingtype, "<br>",
   #                                         "<b>Vegetation Typ:</b>",  PALM_Summary$vegetationtype, "<br>",
   #                                         "<b>Vegetation Leaf Area Index:</b>",  PALM_Summary$leafareaindex, "<br>",
   #                                         "<b>Vegetation Höhe:</b>",  PALM_Summary$vegetationheight, "<br>",
   #                                         "<b>Wasser:</b>",  PALM_Summary$water, "<br>",
   #                                         "<b>Strasse:</b>",  PALM_Summary$pavement, "<br>",
   #                                         "<b>Filling:</b>",  PALM_Summary$filling, "<br>"
   # )})

   output$palmSummary <- renderDT({
     datatable(data.frame(
       Parameter = c("Titel:", "Autor:", "Institut:", "Ort:", "Breite:", "Länge:", "Auflösung:",
                     "Topografie:", "Gebäude 2D:", "Gebäude ID:", "Gebäudetyp:", "Vegetation Typ:", #"Vegetation Leaf Area Index:",
                      "Vegetation Höhe:", "Wasser:", "Strasse:", "Filling"),
       Eingabe = c(PALMGlobale_Summary$title,PALMGlobale_Summary$author,PALMGlobale_Summary$institute,
                  PALMGlobale_Summary$location,PALMGlobale_Summary$latitude,PALMGlobale_Summary$longitude,
                  PALMGlobale_Summary$resolution,PALM_Summary$topography,PALM_Summary$building2d,
                  PALM_Summary$buildingid,PALM_Summary$buildingtype,PALM_Summary$vegetationtype, #PALM_Summary$leafareaindex,
                  PALM_Summary$vegetationheight,PALM_Summary$water,PALM_Summary$pavement,
                  PALM_Summary$filling
                  )),
       rownames=F, selection = c("none"),
       options = list(dom="t",
                      pageLength=20))%>%formatStyle("Eingabe",target="row",backgroundColor=styleEqual("Eingabe fehlt", "red"))
   }
   )


  #### Topografie ####
  output$palmplot_topo <- renderPlot({
    if("zt" %in% names(PALMDATA$class$data)){
      PALMDATA$class$quickplot("zt")
    } else{
      NULL
    }
  })

   observeEvent(input$palmtopo_upload,{
     if(input$palmGIS=="ArcGIS"){
       arcgis <- TRUE
     } else {
       arcgis <- FALSE
     }
     PALMDATA$class <- tryCatch({palm_ncdf_shiny$new(topofile = input$palmtopography$datapath,
                                          headclass = PALMGlobale() ,
                                          gui.arcgis = arcgis)},
                                 error = function(e){
                                   showNotification("Globale Attribute noch nicht ausefüllt oder keine Datei ausgewählt!", duration = 2, closeButton = TRUE, type = "error")
                                 } )

     if(is.character(PALMDATA$class)){
       PALMDATA$class  <- NULL
     }
     if(length(PALMDATA$class)>1){
      PALM_Summary$topography <- input$palmtopography$name
     }
   })


  # PALMCLASS <- eventReactive(input$palmtopo_upload,{
  #   palm_ncdf_shiny$new(topofile = input$palmtopography,
  #                      headclass = PALMGlobale())
  #})



  #### Gebäude ####
   observeEvent(input$palmbuilding_upload,{
     if(is.null(PALMDATA$class)){
       showNotification("Bitte füllen Sie die Globalen Attribute aus und laden die Topografie als erstes hoch!", duration = 3, closeButton = TRUE, type = "error")
     }
     req(!is.null(PALMDATA$class))
     tryCatch({
     PALMDATA$class$importbuildings_DUMMY(filepath = input$palmbuildings2d$datapath)},
     error = function(e){
       showNotification("Keine Datei ausgewählt!", duration = 3, closeButton = TRUE, type = "error")
     } )
    #  print(names(PALMDATA$class$data))
     if("buildings_2d" %in% names(PALMDATA$class$data)){
       PALM_Summary$building2d <- input$palmbuildings2d$name
     }

     #print(names(PALMDATA$data))
     #print(PALMDATA$data)

   })

  output$palmplot_buildings2d <- renderPlot({
    if("buildings_2d" %in% names(PALMDATA$class$data)){
      PALMDATA$class$quickplot("buildings_2d")
    } else{
      NULL
    }
  })

  observeEvent(input$palmbuildingID_upload,{
    if(is.null(PALMDATA$class)){
      showNotification("Bitte füllen Sie die Globalen Attribute aus und laden die Topografie als erstes hoch!.", duration = 3, closeButton = TRUE, type = "error")
    }
    req(!is.null(PALMDATA$class))
    tryCatch({
    PALMDATA$class$getBuildingID(input$palmbuildingID$datapath, TRUE)},
    error = function(e){
      showNotification("Keine Datei ausgewählt!", duration = 3, closeButton = TRUE, type = "error")
    } )
    if("building_id" %in% names(PALMDATA$class$data)){
      PALM_Summary$buildingid <- input$palmbuildingID$name
    }
  })

  observeEvent(input$palmbuildingtype_upload,{
    if(is.null(PALMDATA$class)){
      showNotification("Bitte füllen Sie die Globalen Attribute aus und laden die Topografie als erstes hoch!.", duration = 3, closeButton = TRUE, type = "error")
    }
    req(!is.null(PALMDATA$class))
    if(input$palmbuildingtype_select == 1){#Upload
     ncfile       <-  tryCatch({
        nc_open(input$palmbuildingtype$datapath)},
        error = function(e){
          1
          showNotification("Keine Datei ausgewählt!", duration = 3, closeButton = TRUE, type = "error")
        } ,
        warning = function(w){
          1
          showNotification("Keine Datei ausgewählt!", duration = 3, closeButton = TRUE, type = "error")

        })
      if(is.character(ncfile)){
       # print(ncfile)
      } else {
       # print(ncfile)
        data_type       <- arcgischeck(
          ncvar_get(ncfile, "Band1"),
          PALMDATA$class$arcgis)
        data_type[data_type==0]   <- -127
        if("buildings_2d" %in% names(PALMDATA$class$data)){
          PALMDATA$class$data$building_type$vals <- data_type
          PALM_Summary$buildingtype <- input$palmbuildingtype$name
        } else{
          showNotification("Laden Sie zuerst eine Buildings_2d Datei hoch!", duration = 3, closeButton = TRUE, type = "error")

        }

        nc_close(ncfile)


      }

    }
    else{
      showNotification("Standardwert angesetzt.", duration = 3, closeButton = TRUE, type = "message")

      PALM_Summary$buildingtype <- paste0("Standardwert ",
                                          switch(as.numeric(input$buildingtype_select),
                                                 "Wohngebäude, bis 1950",
                                                 "Wohngebäude, 1950 - 2000",
                                                 "Wohngebäude, ab 2000",
                                                 "Bürogebäude, bis 1950",
                                                 "Bürogebäude, 1950-2000",
                                                 "Bürogebäude, ab 2000"),
                                          " vorbelegt")
      PALMDATA$class$data$building_id$vals[PALMDATA$class$data$building_id$vals>0] <- as.numeric(input$buildingtype_select)
    }
  })


  #### Vegetation ####
  observeEvent(input$palmvegetationtype_upload,{
    if(is.null(PALMDATA$class)){
      showNotification("Bitte füllen Sie die Globalen Attribute aus und laden die Topografie als erstes hoch!.", duration = 3, closeButton = TRUE, type = "error")
    }
    req(!is.null(PALMDATA$class))
    tryCatch({
    PALMDATA$class$import_data(v.file = input$palmvegetation$datapath, palmtype = "vegetation_type", typeid = 1)},
    error = function(e){
      showNotification("Keine Datei ausgewählt!", duration = 3, closeButton = TRUE, type = "error")
    } )
    if("vegetation_type" %in% names(PALMDATA$class$data)){
      PALM_Summary$vegetationtype <- input$palmvegetation$name
    }
  })

  observeEvent(input$leafareaindex_upload,{
    if(input$leafareaindex_select==1){ ## Dateiupload
      PALM_Summary$leafareaindex <- input$leafareaindex$name
    }
    else{              ## Standardwert
     # PALMDATA$class$generate_lai_array(dz = PALMGlobale_Summary$resolution
                                          # output$palmSummary$resolution
     #                                   )
      PALM_Summary$leafareaindex <- "Standardwerte gemäß Vegetation Typ vorbelegt"
    }
  })


  observeEvent(input$palmvegetationheight_upload,{
    tempcheck <- input$palmvegetationheight$name
    if(is.null(PALMDATA$class)){
      showNotification("Bitte füllen Sie die Globalen Attribute aus und laden die Topografie als erstes hoch!.", duration = 3, closeButton = TRUE, type = "error")
    }
    req(!is.null(PALMDATA$class))
    if(input$palmvegetationheight_select == 1){ ##Dateiupload
      if(!is.null(tempcheck)){
        PALM_Summary$vegetationheight <- input$palmvegetationheight$name
      } else {
        showNotification("Keine Datei ausgewählt!", duration = 3, closeButton = TRUE, type = "error")
      }

    }
    else{               ##Standardwert
      showNotification("Standardwert angesetzt.", duration = 3, closeButton = TRUE, type = "message")

      PALM_Summary$vegetationheight <- "Standardwerte gemäß Vegetation Typ vorbelegt"
    }
  })

  #### Wasser ####
  observeEvent(input$palmwater_upload,{
    if(is.null(PALMDATA$class)){
      showNotification("Bitte füllen Sie die Globalen Attribute aus und laden die Topografie als erstes hoch!.", duration = 3, closeButton = TRUE, type = "error")
    }
    req(!is.null(PALMDATA$class))
    tryCatch({
    PALMDATA$class$import_data(v.file = input$palmwater$datapath, palmtype = "water_type", typeid = 1)},
    error = function(e){
      showNotification("Keine Datei ausgewählt!", duration = 3, closeButton = TRUE, type = "error")
    } )
    if("water_type" %in% names(PALMDATA$class$data)){
      PALM_Summary$water <- input$palmwater$name
    }
  })


  #### Straßen ####
  observeEvent(input$palmpavement_upload,{
    if(is.null(PALMDATA$class)){
      showNotification("Bitte füllen Sie die Globalen Attribute aus und laden die Topografie als erstes hoch!.", duration = 3, closeButton = TRUE, type = "error")
    }
    req(!is.null(PALMDATA$class))
    tryCatch({
    PALMDATA$class$import_data(v.file = input$palmpavement$datapath, palmtype = "pavement_type", typeid = 1)},
    error = function(e){
      showNotification("Keine Datei ausgewählt!", duration = 3, closeButton = TRUE, type = "error")
    } )
    if("pavement_type" %in% names(PALMDATA$class$data)){
      PALM_Summary$pavement <- input$palmpavement$name
    }
  })

  #### Einstellungen ####
  observeEvent(input$palm_sort,{
    # print(paste(input$sort_select, collapse=""))
    if(length(input$sort_select)==4){
      tryCatch({
      PALMDATA$class$SortOverlayingdata(paste(input$sort_select, collapse=""))
        },
      error = function(e){
        showNotification("Nocht nicht alle Daten hochgeladen!", duration = 3, closeButton = TRUE, type = "error")
      },
      warning = function(w){
        showNotification("Nocht nicht alle Daten hochgeladen!", duration = 3, closeButton = TRUE, type = "error")
      })
      req(isolate(PALMDATA$class$data$vegetation_type$vals))
      req(isolate(PALMDATA$class$data$pavement_type$vals))
      req(isolate(PALMDATA$class$data$water_type$vals))
      req(isolate(PALMDATA$class$data$buildings_2d$vals))
      SortedFunction$HasBeen <- TRUE
    } else {
      showNotification("Alle Datentypen muessen angegeben werden", duration = 3, closeButton = TRUE, type = "error")
    }

   })


  output$selTxt <- renderText({
    tree <- input$tree
    if (is.null(tree)){
      "None"
    } else{
      unlist(get_selected(tree, format = "names"))
    }
  })

  outVar =  reactive({
    mydata = get(input$palmtype)

    if(mydata=="Building"){
      output = Buildtypes
    } else if(mydata=="Pavement"){
      output  = Pavetypes
    } else if(mydata=="Vegetation"){
      output   =  Vegtypes
    } else if(mydata=="Water"){
      output   = Wattypes
    }

    output
  })

  preSelected =  reactive({
    mydata = get(input$palmtype)

    if(mydata=="Building"){
      output = Buildtypes[2]
    } else if(mydata=="Pavement"){
      output  = Pavetypes[3]
    } else if(mydata=="Vegetation"){
      output   =  Vegtypes[4]
    } else if(mydata=="Water"){
      output   = Wattypes[3]
    }

    output
  })


  observe({
    updateSelectInput(session, "palmid",
                      choices = outVar(),
                      selected = preSelected()
    )})


  #### Filling ####

  observeEvent(input$palmtype_fill,{
    if(input$palmtype=="Vegetation"){
      whichnumber <- which(outVar()==input$palmid)-1
      PALMDATA$class$data$vegetation_type$vals[PALMDATA$class$data$vegetation_type$vals<0] <- whichnumber
      if(SortedFunction$HasBeen){
        PALMDATA$class$SortOverlayingdata("BWPV")
      }
    } else if(input$palmtype=="Pavement"){
      whichnumber <- which(outVar()==input$palmid)-1
      PALMDATA$class$data$pavement_type$vals[PALMDATA$class$data$pavement_type$vals<0] <- whichnumber
      if(SortedFunction$HasBeen){
        PALMDATA$class$SortOverlayingdata("BWVP")
      }
    }
     PALM_Summary$filling <- paste0("Lücken wurden mit Standardwert ",
                                     input$palmtype,
                                    " vorbelegt")
  })



  # #### Plot ####
  # pp <- eventReactive(
  #   c(input$redraw,
  #                       isolate(PALMDATA$class$data$vegetation_type$vals),
  #                       isolate(PALMDATA$class$data$pavement_type$vals),
  #                       isolate(PALMDATA$class$data$water_type$vals),
  #                       isolate(PALMDATA$class$data$buildings_2d$vals),
  #                       input$palmtype_fill),{
  #             PALMDATA$class$plot_area(1,1,
  #                                      dim(PALMDATA$class$data$zt$vals)[1]-1,
  #                                      dim(PALMDATA$class$data$zt$vals)[2]-1)
  # })

  #Neuer Ansatz matw: Plot nur nachdem die Daten hochgeladen wurden:

  pp <- reactive(
{
  input$redraw
  req(isolate(PALMDATA$class$data$vegetation_type$vals))
  req(isolate(PALMDATA$class$data$pavement_type$vals))
  req(isolate(PALMDATA$class$data$water_type$vals))
  req(isolate(PALMDATA$class$data$buildings_2d$vals))
  input$palmtype_fill

        PALMDATA$class$plot_area(1,1,
                                 dim(PALMDATA$class$data$zt$vals)[1]-1,
                                 dim(PALMDATA$class$data$zt$vals)[2]-1)
      })




  output$plot1 <- renderPlot({
      pp()
  })

  output$brush_info <- renderPrint({
   xmin <- round(input$plot1_brush$xmin)
   xmax <- round(input$plot1_brush$xmax)
   ymin <- round(input$plot1_brush$ymin)
   ymax <- round(input$plot1_brush$ymax)
  })

  output$hover_info <- renderPrint({
    if(is.null(input$plot1_hover)) {
      cat("Keine Auswahl")
    } else {

    xp <- round(input$plot1_hover$x)
    yp <- round(input$plot1_hover$y)

    cat(paste("X:", xp, "\n", sep = " "))
    cat(paste("Y:", yp, "\n", sep = " "))
    #print(xp)
    #print(yp)

    if(PALMDATA$class$data$buildings_2d$vals[xp,yp]>0){
      cat("Gebaeudehoehe:\n")
      str(PALMDATA$class$data$buildings_2d$vals[xp,yp])
      cat("Gebaeudetyp:\n")
      str(Buildtypes[PALMDATA$class$data$building_type$vals[xp,yp]])
    }
    if(PALMDATA$class$data$vegetation_type$vals[xp,yp]>0){
      cat("Vegetationstyp:\n")
      str(Vegtypes[PALMDATA$class$data$vegetation_type$vals[xp,yp]+1])
    }
    if(PALMDATA$class$data$water_type$vals[xp,yp]>0){
      cat("Wassertyp:\n")
      str(Wattypes[PALMDATA$class$data$water_type$vals[xp,yp]+1])
    }
    if(PALMDATA$class$data$pavement_type$vals[xp,yp]>0){
      cat("Strassentyp:\n")
      str(Pavetypes[PALMDATA$class$data$pavement_type$vals[xp,yp]+1])
    }

    }



  })


  #### Download ####



#  observeEvent(input$file_download,{
#    PALMDATA$class$exportname <- input$exportfile
#    PALMDATA$class$createbuilding3D(TRUE, TRUE)
#    PALMDATA$class$addsoilandsurfacefraction()
#    PALMDATA$class$exportncdf()
#
#  })

  output$file_download <- downloadHandler(
    filename <- function(){
      input$exportfile
    },
    content = function(file){
      PALMDATA$class$exportname <- input$exportfile
      PALMDATA$class$generate_lai_array(dz = PALMGlobale_Summary$resolution)
      # Hier Fix für Baeueme
      if(is.null(input$palmvegetationheight$name)){
        additionaltrees = FALSE
      } else if (PALM_Summary$vegetationheight == input$palmvegetationheight$name){
        additionaltrees = TRUE
      } else {
        additionaltrees = FALSE
      }
      print(additionaltrees)
      if(additionaltrees){
        PALMDATA$class$generate_lai_array(dz = PALMGlobale_Summary$resolution,
                                          additional_array = input$palmvegetationheight$datapath)
      }
      PALMDATA$class$createbuilding3D(TRUE, TRUE)
      PALMDATA$class$addsoilandsurfacefraction()

      # Fix for rausgefilterte Buildings
      # Fix: na.rm =TRUE für max Befehl
      # PALMDATA$class$data$building_id$vals[PALMDATA$class$data$buildings_2d$vals<=0] <- -9999.9
      # PALMDATA$class$data$building_id$vals[PALMDATA$class$data$buildings_2d$vals>0 & PALMDATA$class$data$building_id$vals<=0] <- max(PALMDATA$class$data$building_id$vals, na.rm = T) + 1
      #

      PALMDATA$class$exportncdf(EPSGCode = input$palmuEPSG)
      file.copy(paste0(getwd(),"/",input$exportfile), file)
    }
  )

  observeEvent(input$palmglobal, {
    if(all(
      nchar(input$palmtitle) > 0 ,
      nchar(input$palmcreator) > 0 ,
      nchar(input$palminstitute) > 0 ,
      nchar(input$palmlocation) > 0 ,
      nchar(input$palmursprungx) > 0 ,
      nchar(input$palmursprungy) > 0 ,
      nchar(input$palmuHeightAMSL) > 0 ,
      nchar(input$palmlatitude) > 0 ,
      nchar(input$palmlongitude) > 0
    )){
    showNotification("Eingaben gespeichert", duration = 3, closeButton = TRUE, type = "message")
    }
  })

  observeEvent(input$palmtopo_upload, {
    if(length(PALMDATA$class)>1){
    showNotification("Eingaben gespeichert", duration = 3, closeButton = TRUE, type = "message")
    }
  })

  observeEvent(input$palmbuilding_upload, {
    if("buildings_2d" %in% names(PALMDATA$class$data)){
      showNotification("Eingaben gespeichert", duration = 3, closeButton = TRUE, type = "message")
    }
  })

  observeEvent(input$palmbuildingID_upload, {
    if("building_id" %in% names(PALMDATA$class$data)){
      showNotification("Eingaben gespeichert", duration = 3, closeButton = TRUE, type = "message")
    }
  })

  observeEvent(input$palmbuildingtype_upload, {
    tempcheck <- input$palmbuildingtype$name
    if(!is.null(tempcheck) & "buildings_2d" %in% names(PALMDATA$class$data)){
      showNotification("Eingaben gespeichert", duration = 3, closeButton = TRUE, type = "message")
    }

  })

  observeEvent(input$palmvegetationtype_upload, {
    if("vegetation_type" %in% names(PALMDATA$class$data)){
      showNotification("Eingaben gespeichert", duration = 3, closeButton = TRUE, type = "message")
    }
  })

  observeEvent(input$palmvegetationheight_upload, {
    tempcheck <- input$palmvegetationheight$name
    if(!is.null(tempcheck) & "zt" %in% names(PALMDATA$class$data)){
      PALM_Summary$vegetationheight <- input$palmvegetationheight$name
      showNotification("Eingaben gespeichert", duration = 3, closeButton = TRUE, type = "message")
    } else {
       # showNotification("Keine Datei ausgewählt!", duration = 3, closeButton = TRUE, type = "error")
    }

  })

  observeEvent(input$palmwater_upload, {
    if("water_type" %in% names(PALMDATA$class$data)){
      showNotification("Eingaben gespeichert", duration = 3, closeButton = TRUE, type = "message")
    }
  })

  observeEvent(input$palmpavement_upload, {
    if("pavement_type" %in% names(PALMDATA$class$data)){
      showNotification("Eingaben gespeichert", duration = 3, closeButton = TRUE, type = "message")
    }
  })

  observeEvent(input$palm_sort, {
    req(isolate(PALMDATA$class$data$vegetation_type$vals))
    req(isolate(PALMDATA$class$data$pavement_type$vals))
    req(isolate(PALMDATA$class$data$water_type$vals))
    req(isolate(PALMDATA$class$data$buildings_2d$vals))
    showNotification("Eingaben gespeichert", duration = 2, closeButton = TRUE, type = "message")
  })

  observeEvent(input$palmtype_fill, {
    showNotification("Filling mit Standardwert erfolgreich.", duration = 2, closeButton = TRUE, type = "message")
  })

  observeEvent(input$redraw, {
    showNotification("Grafik aktualisiert", duration = 2, closeButton = TRUE, type = "message")
  })



})
