.onLoad <- function(libname, pkgname){
  Building <- "Building"
  Pavement   <- "Pavement"
  Vegetation <- "Vegetation"
  Water      <- "Water"


  Buildtypes <- c("Wohn bis 1950", "Wohn 1950 bis 2000", "Wohn seit 2000",
                  "Buero bis 1950", "Buero 1950 bis 2000", "Buero seit 2000")
  Pavetypes  <- c("Nutzerdefiniert", "Unbekannter Asphalt", "Asphalt",
                  "Beton", "Pflaster", "Pflastersteine (Paving)", "Pflastersteine (Cobble)",
                  "Metal", "Holz", "Kies", "Feiner Kies", "Kieselstein", "Hackschnitzel",
                  "Tartan", "Kunstrasen", "Lehm", "Gebaeude (Dummy)")
  Vegtypes   <- c("Nutzerdefiniert", "Erdboden", "Feld (Getreide)", "Kurzes Gras", "Immergruene Nadelbaeume",
                  "Laubabwerfender Nadelbaum", "Immergruener Laubbaum", "Laubbaum", "Hohes Gras",
                  "Wueste", "Tundra", "Bewaessertes Feld", "Halbwueste", "Gletscher (funktioniert nicht)",
                  "Suempfe und Marsche", "Immergruene Straeucher", "Laubabwerfende Straeucher", "Mischwald",
                  "Unterbrochener Wald")
  Wattypes  <-  c("Nutzerdefiniert", "See", "Fluss", "Ozean", "Teich", "Brunnen")



  assign("Building", Building , envir = parent.env(environment()))
  assign("Pavement", Pavement , envir = parent.env(environment()))
  assign("Vegetation", Vegetation , envir = parent.env(environment()))
  assign("Water", Water , envir = parent.env(environment()))

  assign("Buildtypes", Buildtypes , envir = parent.env(environment()))
  assign("Pavetypes", Pavetypes , envir = parent.env(environment()))
  assign("Vegtypes", Vegtypes , envir = parent.env(environment()))
  assign("Wattypes", Wattypes , envir = parent.env(environment()))


  PIDS <- list()
  PIDS$pavement <- list()
  PIDS$pavement$predefined_type <- data.frame("ID" = 0:16,
                                              "Name" = c("user defined",
                                                         "unknown pavement",
                                                         "asphalt (asphalt concrete)",
                                                         "concrete (Portland concrete)",
                                                         "sett",
                                                         "paving stones",
                                                         "cobblestone",
                                                         "metal",
                                                         "wood",
                                                         "gravel",
                                                         "fine gravel",
                                                         "pebblestone",
                                                         "woodchips",
                                                         "tartan (sports)",
                                                         "artificial turf (sports)",
                                                         "clay (sports)",
                                                         "building (dummy)"),
                                              "Albedo type" = c(0,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33),
                                              stringsAsFactors = F
  )
  PIDS$pavement$parameters  <- data.frame("ID_pos" = 0:3,
                                          "Name" = c("roughness length for momentum (m)",
                                                     "roughness length for heat (m)",
                                                     "albedo type",
                                                     "emissivity (0-1)"),
                                          stringsAsFactors = F)


  PIDS$vegetation <- list()
  PIDS$vegetation$predefined_type <- data.frame("ID" = 0:18,
                                                "Name" = c("user defined",
                                                           "bare soil",
                                                           "crops, mixed farmin",
                                                           "short grass",
                                                           "evergreen needleleaf trees",
                                                           "deciduous needleleaf trees",
                                                           "evergreen broadleaf trees",
                                                           "deciduous broadleaf trees",
                                                           "tall grass",
                                                           "desert",
                                                           "tundra",
                                                           "irrigated crops",
                                                           "semidesert",
                                                           "not implemented yet",
                                                           "bogs and marshes",
                                                           "evergreen shrubs",
                                                           "deciduous shrubs",
                                                           "mixed forest / woodland",
                                                           "interrupted forest"),
                                                "Albedo type" = c(0,17,2,5,6,8,9,8,3,11,13,2,11,0,4,4,4,7,8),
                                                stringsAsFactors = F
  )
  PIDS$vegetation$parameters <- data.frame("ID_pos" = c(0:11),
                                           "Name" = c("minimum canopy resistance (s/m)",
                                                      "leaf area index",
                                                      "vegetation coverage (0-1)",
                                                      "canopy resistance coefficient (1/hPa)",
                                                      "roughness length for momentum (m)",
                                                      "roughness length for heat (m)",
                                                      "skin layer heat conductivity (stable conditions) (W/m2/K)",
                                                      "skin layer heat conductivity (unstable conditions) (W/m2/K)",
                                                      "fraction of incoming shortwave radiation transmitted directly to the soil",
                                                      "heat capacity of the surface / skin layer (J/m2/K)",
                                                      "albedoy type",
                                                      "emissivity"),
                                           stringsAsFactors = F)


  PIDS$water <- list()
  PIDS$water$predefined_type <- data.frame("ID" = 0:5,
                                           "Name" = c("user defined",
                                                      "lake",
                                                      "river",
                                                      "ocean",
                                                      "pond",
                                                      "fountain"),
                                           "Albedo type" = c(1,1,1,1,1,1),
                                           stringsAsFactors = F
  )
  PIDS$water$parameters <- data.frame( "ID_pos" = 0:6,
                                       "Name" = c("water temperature (fixed) (K)",
                                                  "roughness length for momentum (Charnock parameterization is used if not set) (m)",
                                                  "roughness length for heat (Charnock parameterization is used if not set) (m)",
                                                  "heat conductivity between skin layer and water (stable conditions) (W/m2/K) (should not be changed)",
                                                  "heat conductivity between skin layer and water (unstable conditions) (W/m2/K) (should not be changed)",
                                                  "albedo type",
                                                  "emissivity (0-1)"),
                                       stringsAsFactors = F)

  PIDS$soil <- list()
  PIDS$soil$predefined_type <- data.frame("ID" = 0:6,
                                          "Name" = c("user defined",
                                                     "coarse",
                                                     "medium",
                                                     "medium-fine",
                                                     "fine",
                                                     "very fine",
                                                     "organic"),
                                          stringsAsFactors = F)
  PIDS$soil$parameters <- data.frame( "ID_pos" = 0:7,
                                      "Name" = c("Van Genuchten parameter alpha",
                                                 "Van Genuchten parameter I",
                                                 "Van Genuchten parameter n",
                                                 "saturation hydraulic conductivity (m/s)",
                                                 "saturation soil moisture (m3/m3)",
                                                 "field capacity (m3/m3)",
                                                 "wilting point (m3/m3)",
                                                 "residual moisture (m3/m3)"),
                                      stringsAsFactors = F)

  PIDS$building <- list()
  PIDS$building$predefined_type <- data.frame("ID" = 0:6,
                                              "Name" = c("user defined",
                                                         "Residential <1950",
                                                         "Residential 1950 - 2000",
                                                         "Residential >2000",
                                                         "Office <1950",
                                                         "Office 1950 - 2000",
                                                         "Office >2000"
                                              ),
                                              "Albedo type" = c(0,rep(33,6)),
                                                                stringsAsFactors = F
  )

  PIDS$building$parameters <- data.frame( "ID_pos" = 0:44,
                                          "Name" = c("wall fraction (0-1)",
                                                     "window fractions (0-1)",
                                                     "green fraction on wall (0-1)",
                                                     "green fraction on roof (0-1)",
                                                     "leaf area index of green fraction (roof)",
                                                     "leaf area index of green fraction (wall)",
                                                     "heat capacity of wall layer 1",
                                                     "heat capacity of wall layer 2",
                                                     "heat capacity of wall layer 3",
                                                     "thermal conductivity of wall layer 1",
                                                     "thermal conductivity of wall layer 2",
                                                     "thermal conductivity of wall layer 3",
                                                     "indoor target summer tempreature (K)",
                                                     "indoor target winter temperatuer (K)",
                                                     "emissivity of wall fraction (0-1)",
                                                     "emissivity of green fraction (0-1)",
                                                     "emissivity of window fraction (0-1)",
                                                     "transmissivity of wind fraction (0-1)",
                                                     "roughness length for momentum (m)",
                                                     "roughness length for heat (m)",
                                                     "ground floor height (m)",
                                                     "ground floor wall fraction (0-1)",
                                                     "ground floor window fraction (0-1)",
                                                     "ground floor green fraction on wall (0-1)",
                                                     "ground floor green fraction on roof (0-1)",
                                                     "ground floor leaf area index of green fraction (wall)",
                                                     "ground floor hea capacity of wall layer 1",
                                                     "ground floor hea capacity of wall layer 2",
                                                     "ground floor hea capacity of wall layer 3",
                                                     "ground floor thermal conductivity of wall layer 1",
                                                     "ground floor thermal conductivity of wall layer 2",
                                                     "ground floor thermal conductivity of wall layer 3",
                                                     "ground floor emissivity of wall fraction (0-1)",
                                                     "ground floor emissivity of green fraction (0-1)",
                                                     "ground floor emissivity of window fraction (0-1)",
                                                     "ground floor transmissivity of window fraction (0-1)",
                                                     "ground floor roughness length for momentum (m)",
                                                     "ground floor roughness length for heat (m)",
                                                     "albedo_type of wall fraction",
                                                     "albedo_type of green fraction",
                                                     "albedo_type of windows fraction",
                                                     "wall thickness of layer 1 (m)",
                                                     "wall thickness of layer 2 (m)",
                                                     "wall thickness of layer 3 (m)",
                                                     "wall thickness of layer 4 (m)"
                                          ),
                                          stringsAsFactors = F
  )

  PIDS$albedo <- list()
  PIDS$albedo$predefined_type <- data.frame("ID" = 0:33,
                                            "Name" = c("ocean",
                                                       "mixed farmin, tall grassland",
                                                       "tall/medium grassland",
                                                       "evergreen grasslan",
                                                       "evergreen shrubland",
                                                       "short grassland/meadow/shrubland",
                                                       "evergeen needleleaf forest",
                                                       "mixed deciduous forest",
                                                       "deciduous forest",
                                                       "tropical evergreen broadleaved forest",
                                                       "medium/tall grassland//woodland",
                                                       "desert, sandy",
                                                       "desert, rocky",
                                                       "tundra",
                                                       "land ice",
                                                       "sea ice",
                                                       "snow",
                                                       "bare soil",
                                                       "asphalt/concrete mix",
                                                       "asphalt (asphalt concrete)",
                                                       "concrete (Portland concrete)",
                                                       "sett",
                                                       "paving stones",
                                                       "cobblestone",
                                                       "metal",
                                                       "wood",
                                                       "gravel",
                                                       "fine gravel",
                                                       "pebblestone",
                                                       "woodchips",
                                                       "tartan (sports)",
                                                       "artificial turf (sports)",
                                                       "clay (sports)",
                                                       "building (dummy)"),
                                            "Broadband_albedo" =c(0.00, 0.06, 0.19, 0.23, 0.23,
                                                                  0.25, 0.14, 0.17, 0.19, 0.14,
                                                                  0.18, 0.43, 0.32, 0.19, 0.77,
                                                                  0.77, 0.82, 0.08, 0.17, 0.17,
                                                                  0.30, 0.17, 0.17, 0.17, 0.17,
                                                                  0.17, 0.17, 0.17, 0.17, 0.17,
                                                                  0.17, 0.17, 0.17, 0.17),
                                            "Longwave_albedo" = c(0.00, 0.06, 0.28, 0.33, 0.33,
                                                                  0.34, 0.22, 0.27, 0.31, 0.22,
                                                                  0.28, 0.51, 0.40, 0.27, 0.65,
                                                                  0.65, 0.70, 0.08, 0.17, 0.17,
                                                                  0.30, 0.17, 0.17, 0.17, 0.17,
                                                                  0.17, 0.17, 0.17, 0.17, 0.17,
                                                                  0.17, 0.17, 0.17, 0.17),
                                            "Shortwave_albeo" = c(0.00, 0.06, 0.09, 0.11, 0.11,
                                                                  0.14, 0.06, 0.06, 0.06, 0.06,
                                                                  0.06, 0.35, 0.24, 0.10, 0.90,
                                                                  0.90, 0.95, 0.08, 0.17, 0.17,
                                                                  0.30, 0.17, 0.17, 0.17, 0.17,
                                                                  0.17, 0.17, 0.17, 0.17, 0.17,
                                                                  0.17, 0.17, 0.17, 0.17),
                                            stringsAsFactors = F
  )
  assign("PIDS", PIDS, envir = parent.env(environment()))
}
