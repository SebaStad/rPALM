# source("base_functions.R")
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
# library(shiny)
# library(shinyTree)
# library(DT)
#library(XML)



# Courtesy of:
# https://stackoverflow.com/questions/20637248/shiny-4-small-textinput-boxes-side-by-side
textInput3<-function (inputId, label, value = "",...)
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId),
      tags$input(id = inputId, type = "text", value = value,...))
}









library(shiny)
library(shinyTree)


ui <- shiny::shinyUI(
  fluidPage(

  # Application title
  #titlePanel("PALM-4U: Static Driver Creator"),

  navbarPage("PALM-4U: Static Driver Creator",
    tabPanel("File Upload", #icon = "upload",
             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(
                 # sliderInput("bins",
                 #             "Number of bins:",
                 #             min = 1,
                 #             max = 50,
                 #             value = 30)
                 h4("Input:"),
                 shinyTree("tree", checkbox = FALSE)
               ),

               mainPanel(
                 h3(htmlOutput("selTxt")),
                 br(),
                 conditionalPanel(
                   condition="output.treeselect == 'Global attributes'",
                    textInput("palmtitle", "Titel", placeholder = "PALM-4U Schulungscase"),
                    textInput("palmcreator", "Ersteller", placeholder = "Ersteller"),
                    textInput("palminstitute", "Institut", placeholder = "Institut"),
                    textInput("palmlocation", "Ort", placeholder = "Ort"),

                    textInput("palmlatitude", "Breitengrad des Projekts", placeholder = 52.50836),
                    textInput("palmlongitude", "Längengrad des Projekts", placeholder = 13.31343),
                    textInput("palmursprungx", "Koordinatensystem: Ursprung X", placeholder = 385536.4),
                    textInput("palmursprungy", "Koordinatensystem: Ursprung Y", placeholder = 5818919),
                    #textInput("palmuEPSG", "Koordinatensystem: EPSG",
                    #          value = "EPSG:25832"),
                    selectInput("palmuEPSG", "Koordinatensystem: EPSG",
                                choices = c("EPSG:25831", "EPSG:25832", "EPSG:25833", "EPSG:31468"),
                                selected = "EPSG:25832"),
                    selectInput("palmGIS", "Mit welcher Software wurden die Daten gerastert?",
                                choices = c("QGIS", "ArcGIS"),
                                selected = "QGIS"),
                    textInput("palmuHeightAMSL", "Hoehe uber NN [m]:", placeholder = 30.81),
                    numericInput("palmgrid", "Horizontale Gitterweite", value = 10, min = 1, step = 1),
                    selectInput("palmTime", "Fragestellung der PALM-GUI",
                                choices = c("Thermischer Komfort" = "2018-07-21 21:00:00 +00",
                                            "Windkomfort" = "2018-07-21 12:00:00 +00")),
                    actionButton("palmglobal", "Import")
                   # ,
                   #
                   #  actionButton("palmglobal_summary", "Summary"),
                   #  br(), br(), br()
                   # ,
                   # htmlOutput("palmglSummary")



                  ),
                 conditionalPanel(condition="output.treeselect == 'Topography'",
                                  fileInput("palmtopography", "Wählen Sie Ihre Topographie Datei",
                                            multiple = FALSE, accept=c(".nc")),
                                  actionButton("palmtopo_upload", "Import")
                 ),
                 # conditionalPanel(condition="output.treeselect == 'Terrain Height'",
                 #                  plotOutput("palmplot_topo", height = "600px")
                 # ),
               #   conditionalPanel(condition="output.treeselect == 'Buildings'",
               #                    fileInput("palmbuildings2d", "Wählen Sie Ihre 2D-Gebäude Datei",
               #                              multiple = FALSE),
               #                    actionButton("palmbuilding_upload", "Import")
               # ),
               conditionalPanel(condition="output.treeselect == 'Building Height'",
                                fileInput("palmbuildings2d", "Wählen Sie Ihre Datei mit Gebäudehöhe",
                                          multiple = FALSE, accept=c(".nc")),
                                actionButton("palmbuilding_upload", "Import")
               ),
               conditionalPanel(condition="output.treeselect == 'Building ID'",
                                fileInput("palmbuildingID", "Wählen Sie Ihre Gebäude-ID-Datei",
                                          multiple = FALSE, accept=c(".nc")),
                                actionButton("palmbuildingID_upload", "Import")
                                # selectInput(
                                #   "breaks", "Breaks",
                                #   c("Sturges",
                                #     "Scott",
                                #     "Freedman-Diaconis",
                                #     "[Custom]" = "custom"))
               ),
               conditionalPanel(condition="output.treeselect == 'Building Type'",
                                selectInput("palmbuildingtype_select", label=NULL,
                                            choices=c("Dateiupload einer nc-Datei zum Gebäudetyp"=1,
                                                      "Belegung mit Standardwert"=2)),
                                conditionalPanel(condition="input.palmbuildingtype_select == 1",
                                                 fileInput("palmbuildingtype", "Wählen Sie Ihren Gebäude-Typ-Datei",
                                                           multiple=F,accept=c(".nc"))),
                                conditionalPanel(condition="input.palmbuildingtype_select == 2",
                                                 selectInput(
                                                   "buildingtype_select", "Building Type",
                                                   c("Wohngebäude, bis 1950" = 1,
                                                     "Wohngebäude, 1950 - 2000" = 2,
                                                     "Wohngebäude, ab 2000" = 3,
                                                     "Bürogebäude, bis 1950" = 4,
                                                     "Bürogebäude, 1950-2000" = 5,
                                                     "Bürogebäude, ab 2000" = 6)
                                                 )),
                                actionButton("palmbuildingtype_upload", "Import")
                                ),

               conditionalPanel(condition="output.treeselect == 'Vegetation Type'",
                                fileInput("palmvegetation", "Wählen Sie Ihre Vegetation Type Datei",
                                          multiple = FALSE, accept=c(".nc")),
                                actionButton("palmvegetationtype_upload", "Import")
                                ),
               # conditionalPanel(condition="output.treeselect == 'LeafAreaIndex'",
               #                  selectInput("leafareaindex_select", label=NULL,
               #                              choices=c("Dateiupload einer nc-Datei zum LeafAreaIndex"=1,
               #                                        "Belegung mit Standardwert"=2)),
               #conditionalPanel(condition="input.leafareaindex_select == 1",
               #                                   fileInput("leafareaindex", "Wählen Sie Ihre Datei mit dem LeafAreaIndex",
               #                                            multiple=F,accept=c(".nc"))),
               #                  actionButton("leafareaindex_upload", "Import")
               #                 ),
               conditionalPanel(condition="output.treeselect == 'Vegetation Height'",
                                selectInput("palmvegetationheight_select", label=NULL,
                                            choices=c("Dateiupload einer nc-Datei zur Vegetationshöhe"=1,
                                                      "Belegung mit Standardwert"=2)),
               conditionalPanel(condition="input.palmvegetationheight_select == 1",
                                                 fileInput("palmvegetationheight", "Wählen Sie Ihre Datei mit der Vegetationshöhe",
                                                           multiple=F,accept=c(".nc"))),
                                actionButton("palmvegetationheight_upload", "Import")
                                ),
               conditionalPanel(condition="output.treeselect == 'Water'",
                                fileInput("palmwater", "Wählen Sie Ihre Water_Type Datei",
                                          multiple = FALSE,accept=c(".nc")),
                                actionButton("palmwater_upload", "Import")
                                ),
               conditionalPanel(condition="output.treeselect == 'Pavement'",
                                fileInput("palmpavement", "Wählen Sie Ihre Pavement_Type Datei",
                                          multiple = FALSE,accept=c(".nc")),
                                actionButton("palmpavement_upload", "Import")
                                ),
               conditionalPanel(condition="output.treeselect == 'Settings'",
                                selectizeInput("sort_select", "Reihenfolge",
                                               c("Gebäude" = "B", "Wasser" = "W", "Strasse" = "P", "Vegetation" = "V"),
                                               multiple=T,
                                               selected=c("Gebäude" = "B", "Wasser" = "W", "Strasse" = "P", "Vegetation" = "V")),
                                actionButton("palm_sort", "Sortieren")

                                )
              )

            )
          ),
    tabPanel("Viewer/ Editor", #icon = "upload",
             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(
                h4("Fill-Funktion"),
                selectInput("palmtype", "Palm-Type", c("Vegetation", "Pavement")),
                selectInput("palmid", "PalmID:", ""),
                actionButton("palmtype_fill", "Fill", icon = icon("fill")),
                hr(),
                h4("Auswahl:"),
                verbatimTextOutput("hover_info")
                #  tabsetPanel(
                #  tabPanel("Flaechenauswahl",
                #   # sliderInput("bins",
                #   #             "Number of bins:",
                #   #             min = 1,
                #   #             max = 50,
                #   #             value = 30)
                #   h4("Select"),
                #   selectInput("palmtype", "Palmtype:",
                #               c("Building",
                #                 "Pavement",
                #                 "Vegetation",
                #                  "Water")),
                #    selectInput("palmid", "PalmID:",
                #               "")
                # ),
                # tabPanel("Fill-Funktion",
                #          h4("Select")
                # )
                #  )
               ),
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("plot1", #height = "600px",
                            # Equivalent to: click = clickOpts(id = "plot_click")
                            # click = "plot1_click",
                            hover = hoverOpts(
                              id = "plot1_hover"
                            )
                            ),
                            actionButton("redraw", "Redraw", icon = icon("sync"))
                            # ,
                            # brush = brushOpts(
                            #   id = "plot1_brush"
                            # )
                 #)
               )
             )),#, icon = "user-edit"),
    tabPanel("Export", # icon = "download")
             sidebarLayout(
               sidebarPanel(
                 h4("Download"),
                 textInput("exportfile", "Dateiname", value = "MYStaticDriver.nc"),
                 # actionButton("file_save", "Save", icon=icon("download")),
                 downloadButton("file_download", "Download")
               ),
               mainPanel(
                 dataTableOutput("palmSummary")
               )
             )
             ),
    tabPanel("Hilfe",
             h4("Hilfe"),
             a("Hilfe starten", target = "_blank", href = "UseUClim_BHB_StaticDriverTool.pdf")
             )
  )
  )
)
