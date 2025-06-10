# ==================================================================
# generate spatial objects MPA.r
# 
# Martin Pastoors
#
# 16/04/2025 Read NZO files
# ==================================================================

# Reset lists
# rm(list=ls())

library(tidyverse)     # Tidyverse ofcourse
library(sf)            # Support for simple features, a standardized way to encode spatial vector data.
library(patchwork)

# library(mregions)      # Marine Regions package

source("../FLYSHOOT/r/FLYSHOOT utils.R")

onedrive   <- "C:/DATA"
spatialdir <- "C:/DATA/RDATA"
path       <- file.path(onedrive, "gis") 

jffdir  <- "C:/Users/MartinPastoors/Martin Pastoors/NZO - General/WKC/adviezen/2025-xx Fact Finding beschermde gebieden"

load(file=paste(onedrive, "RDATA/world_lr_sf.RData", sep="/"))
load(file=paste(onedrive, "RDATA/world_mr_sf.RData", sep="/"))
load(file=paste(onedrive, "RDATA/eez_sf.RData", sep="/"))

nl_eez <- eez_sf %>% filter(ISO_TER1 == "NLD")
bb <- sf::st_bbox(nl_eez)

wserdir  <- "C:/Users/MartinPastoors/Martin Pastoors/NZO - General/WKC/adviezen/2025-xx Fact Finding beschermde gebieden/data/WSER"
wsergrid <- readRDS(file.path(wserdir,"grid.Rdata"))

# ================================================================================
# NL EEZ
# ================================================================================

folder <- "NL TerritorialSea_(TerritorialeZee)_NL_apr2024"
layer  <- 'NL_TerritorialSea_apr2024_TESARE(A)'

nl_ts <- 
  sf::read_sf(file.path(path, folder), layer=layer) %>% 
  sf::st_as_sf()

folder <- "NL Exclusive_Economic_Zone_(EEZ)_NL_apr2024"
layer  <- 'NL_ExclusiveEconomicZone_apr2024_EXEZNE(A)'

nl_eez <- 
  sf::read_sf(file.path(path, folder), layer=layer) %>% 
  sf::st_as_sf()

nl_eez_all <-
  st_union(nl_eez, nl_ts) %>% 
  dplyr::select() %>% 
  mutate(SITE_NAME = "EEZ", SITE_TYPE = "eez")

save(nl_eez_all, file=file.path(jffdir, "rdata/nl_eez_all.RData"))
# sf::st_write(nl_eez_all, file.path(jffdir, "shapes/nl_eez_all.shp"))

# nl_eez_all3857 <-
#   st_union(nl_eez, nl_ts) %>% 
#   dplyr::select() %>% 
#   mutate(SITE_NAME = "EEZ", SITE_TYPE = "eez") %>% 
#   st_transform(crs=3857)
# 
# sf::st_write(nl_eez_all3857, file.path(jffdir, "shapes/nl_eez_all3857.shp"))


# ================================================================================
# Wind
# ================================================================================

folder <- "Wind energy area NZ NWP 2024"
layer  <- 'aangewezen_windgebiedenPolygon'

wind <- 
  sf::read_sf(file.path(path, folder), layer=layer) %>% 
  mutate(opmerking = ifelse(is.na(opmerking), "", opmerking)) %>% 
  mutate(SITE_TYPE = "wind") %>% 
  mutate(SITE_NAME = paste(opmerking, windgebied))  %>% 
  mutate(SITE_REG  = "fishery restricted")

# ================================================================================
# N2000 gebieden
# ================================================================================

folder <- "n2000_gebied"
layer  <- 'ps_natura2000Polygon'

n2000 <- 
  sf::read_sf(file.path(path, folder), layer=layer) %>% 
  filter(text %in% c("Bruine Bank",
                     "Doggersbank",
                     "Friese Front",
                     "Klaverbank",
                     "Noordzeekustzone",
                     "Voordelta",
                     "Vlakte van de Raan")) %>%
  dplyr::select(SITE_NAME=text) %>% 
  mutate(SITE_TYPE = "N2000") %>% 
  
  sf::st_transform(crs=23031) %>%
  dplyr::mutate(area_km2 = as.integer(sf::st_area(.)/1000000)) %>% 
  dplyr::mutate(area_ha = as.integer(sf::st_area(.)/10000)) %>% 
  sf::st_transform(crs=4326) %>% 
  
  sf::st_as_sf()

n2000 %>% 
  ggplot() +
  theme_publication() +
  geom_sf(aes(fill=SITE_NAME)) +
  geom_sf(data=world_mr_sf, fill="lightgray") +
  geom_sf(data=eez_sf, fill=NA, linetype="dashed") +
  coord_sf(xlim=c(bb[1],bb[3]), ylim=c(bb[2],bb[4]))


# ================================================================================
# KRM gebieden
# ================================================================================

folder <- "krm_gebied"
layer  <- 'krm_gebied'

krm <- 
  sf::read_sf(file.path(path, folder), layer=layer) %>% 
  dplyr::select(SITE_NAME=Gebied) %>% 
  mutate(SITE_TYPE="KRM") %>% 
  
  sf::st_transform(crs=23031) %>%
  dplyr::mutate(area_km2 = as.integer(sf::st_area(.)/1000000)) %>% 
  dplyr::mutate(area_ha = as.integer(sf::st_area(.)/10000)) %>% 
  sf::st_transform(crs=4326) %>% 
  
  sf::st_as_sf()

krm %>% 
  ggplot() +
  theme_publication() +
  geom_sf(aes(fill=SITE_NAME)) +
  geom_sf(data=world_mr_sf, fill="lightgray") +
  geom_sf(data=eez_sf, fill=NA, linetype="dashed") +
  coord_sf(xlim=c(bb[1],bb[3]), ylim=c(bb[2],bb[4]))

ffn2000   <- n2000 %>% filter(SITE_NAME=="Friese Front") 
ffkrm     <- krm %>% filter(SITE_NAME=="Friese Front") 
ffn2000_only <- 
  sf::st_difference(ffn2000, ffkrm) %>% 
  st_cast('MULTIPOLYGON') %>%
  st_cast('POLYGON', warn=FALSE) %>%
  
  sf::st_transform(crs=23031) %>%
  dplyr::mutate(area_km2 = as.integer(sf::st_area(.)/1000000)) %>% 
  dplyr::mutate(area_ha = as.integer(sf::st_area(.)/10000)) %>% 
  sf::st_transform(crs=4326) %>% 
  filter(area_km2>10) %>% 
  dplyr::select(SITE_NAME, SITE_TYPE, area_km2, area_ha) 

ffn2000_krm <- 
  sf::st_intersection(ffn2000, ffkrm) %>% 
  sf::st_transform(crs=23031) %>%
  dplyr::mutate(area_km2 = as.integer(sf::st_area(.)/1000000)) %>% 
  dplyr::mutate(area_ha = as.integer(sf::st_area(.)/10000)) %>% 
  sf::st_transform(crs=4326) %>% 
  mutate(SITE_TYPE = "KRM_N2000") %>% 
  dplyr::select(SITE_NAME, SITE_TYPE, area_km2, area_ha) 

# Combined N2000/KRM dataset
n2000_krm <-
  bind_rows(
    krm %>% filter(SITE_NAME !="Friese Front") ,
    n2000 %>% filter(SITE_NAME !="Friese Front"),
    ffn2000_only,
    ffn2000_krm
  )

# n2000_krm %>% 
#   ggplot() +
#   theme_publication() +
#   geom_sf(aes(fill=SITE_TYPE), alpha=0.5) +
#   # geom_sf_text(aes(label=SITE_NAME)) +
#   geom_sf(data=world_mr_sf, fill="lightgray") +
#   geom_sf(data=eez_sf, fill=NA, linetype="dashed") +
#   coord_sf(xlim=c(bb[1],bb[3]), ylim=c(bb[2],bb[4])) +
#   facet_wrap(~SITE_NAME)


# ================================================================================
# NZO mogelijk aanvullende gebieden / visserij suggesties
# ================================================================================

folder <- "20241107_NZO-WGBeschermdeGebieden_IndicatievePolygonen"
layer  <- '07112024_NZO-WGBeschermdeGebieden_IndicatievePolygonen'

nzo_sf <- 
  sf::read_sf(file.path(path, folder), layer=layer) %>% 
  sf::st_transform(crs=4326) %>% 
  sf::st_cast(., "POLYGON") %>% 
  lowcase() %>% 
  
  sf::st_transform(crs=23031) %>%
  dplyr::mutate(area_km2 = as.integer(sf::st_area(.)/1000000)) %>% 
  dplyr::mutate(area_ha = as.integer(sf::st_area(.)/10000)) %>% 
  sf::st_transform(crs=4326) %>% 
  
  bind_cols(SITE_NAME=c("Doggerbank 478",
                        "Doggerbank 431",
                        "Doggerbank 271",
                        "Doggerbank 619",
                        "Bruine Bank 265",
                        "Friese Front 232",
                        "Friese Front 115")) %>% 
  bind_cols(SITE_TYPE="eez") %>% 
  dplyr::select(-id, -area)



t <-
  sf::st_intersection(nzo_sf, wsergrid) 

t %>%
  filter(SITE_NAME=="Bruine Bank 265") %>% 
  ggplot() +
  theme_publication() +
  geom_sf(aes(fill=SITE_TYPE)) +
  geom_sf_text(aes(label=gridID)) +
  # geom_sf_text(aes(label=SITE_NAME)) +
  geom_sf(data=world_mr_sf, fill="lightgray") +
  # geom_sf(data=eez_sf, fill=NA, linetype="dashed") +
  coord_sf(xlim=c(3,3.6), ylim=c(52.5,52.8))
  # coord_sf(xlim=c(bb[1],bb[3]), ylim=c(bb[2],bb[4]))


dbviss    <- nzo_sf %>% filter(SITE_NAME=="Doggerbank 619")
ffviss1   <- nzo_sf %>% filter(SITE_NAME=="Friese Front 115") 
ff232     <- nzo_sf %>% filter(SITE_NAME=="Friese Front 232")
ffviss2   <- sf::st_difference(ff232, ffviss1)

aanvullende_suggesties <-
  bind_rows(
    dbviss,
    ffviss1,
    ffviss2
  ) %>% 
  mutate(
    SITE_NAME = case_when(
      SITE_NAME == "Friese Front 115" ~ "FF viss1",
      SITE_NAME == "Friese Front 232" ~ "FF viss2",
      TRUE                            ~ SITE_NAME
    )
  ) %>% 
  
  sf::st_transform(crs=23031) %>%
  dplyr::mutate(area_km2 = as.integer(round(sf::st_area(.)/1000000), digits=0)) %>% 
  dplyr::mutate(area_ha = as.integer(round(sf::st_area(.)/10000), digits=0)) %>% 
  sf::st_transform(crs=4326) %>% 
  
  mutate(SITE_REG = "fishery restriction suggested") %>% 
  dplyr::select(SITE_NAME, SITE_TYPE, SITE_REG, area_km2, area_ha)

aanvullende_suggesties %>% 
  ggplot() +
  theme_publication() +
  geom_sf(aes(fill=SITE_TYPE)) +
  # geom_sf_text(aes(label=SITE_NAME)) +
  geom_sf(data=world_mr_sf, fill="lightgray") +
  geom_sf(data=eez_sf, fill=NA, linetype="dashed") +
  coord_sf(xlim=c(bb[1],bb[3]), ylim=c(bb[2],bb[4])) 

# nzo_sf %>% 
#   filter(grepl("Friese",SITE_NAME)) %>% 
#   ggplot() +
#   theme_publication() +
#   geom_sf(aes(fill=SITE_NAME)) +
#   geom_sf_text(aes(label=area_km2)) +
#   geom_sf(data=world_mr_sf, fill="lightgray") +
#   geom_sf(data=eez_sf, fill=NA, linetype="dashed") +
#   coord_sf(xlim=c(5,6), ylim=c(53,54)) +
#   facet_wrap(~SITE_NAME)

viss_voorstel <-
  readxl::read_excel("C:/Users/MartinPastoors/Martin Pastoors/NZO - General/WKC/adviezen/2025-xx Fact Finding beschermde gebieden/shapes/Visserijvoorstel Friese Front.xlsx") %>%
  sf::st_as_sf(coords = c("lon","lat")) %>%
  sf::st_set_crs(4326) %>%
  group_by(SITE_NAME, SITE_TYPE) %>%
  summarize(do_union=FALSE) %>%
  st_cast("POLYGON") %>% 
  sf::st_make_valid() %>% 
  ungroup() %>% 
  filter(SITE_NAME=="viss2") %>% 
  
  sf::st_transform(crs=23031) %>%
  dplyr::mutate(area_km2 = as.integer(round(sf::st_area(.)/1000000), digits=0)) %>%
  sf::st_transform(crs=4326) 
  


# ==============================================================================
# NL MPA's met visserijbeperkingen
# ==============================================================================

# Noordzee kustzone
folder <- "Visserij_toegangsbeperking_Natura_2000_gebied_NZ_kustzone"
layer  <- 'toegangbeperkingsbesluit_nzkustzonePolygon'

nzk<- 
  sf::read_sf(file.path(path, folder), layer=layer) %>% 
  lowcase() %>% 
  filter(grepl("1", zone)) %>% 
  dplyr::select(SITE_NAME=gebiedsnaa, nr=gebiedsnr, letter, zone, bron) %>% 
  mutate(SITE_TYPE = "N2000") %>% 
  mutate(SITE_REG  = "fishery restricted") %>% 
  sf::st_transform(crs=4326) 
  
nzk %>%
  # filter(!grepl("NZKZ", SITE_NAME)) %>% 
  ggplot() +
  theme_publication() +
  geom_sf(aes(fill=SITE_NAME), alpha=0.5) +
  geom_sf(data=world_mr_sf, fill="lightgray") +
  geom_sf(data=eez_sf, fill=NA, linetype="dashed") +
  coord_sf(xlim=c(4,7), ylim=c(52.5,54))

# Voordelta
folder <- "Visserij_toegangsbeperking_Voordelta_Rustgebieden"
layer  <- 'rustgebiedenPolygon'

vd<- 
  sf::read_sf(file.path(path, folder), layer=layer) %>% 
  lowcase() %>% 
  dplyr::select(SITE_NAME=gebiedsnaa, nr=gebiedsnr, letter, bron, omschrijving=omschrijvi) %>% 
  mutate(SITE_TYPE = "N2000") %>% 
  mutate(SITE_REG  = "fishery restricted") %>% 
  sf::st_transform(crs=4326) 


# Vlakte van de Raan
folder <- "Visserij_toegangsbeperking_Vlakte van de Raan"
layer  <- 'VlakteVanDeRaan'

vvdr<- 
  sf::read_sf(file.path(path, folder), layer=layer) %>% 
  lowcase() %>% 
  mutate(nr = as.character(row_number())) %>% 
  dplyr::select(SITE_NAME=gebied, nr, bron=grondvan, omschrijving=kenmerken) %>% 
  mutate(SITE_TYPE = "N2000") %>%
  mutate(SITE_REG  = "fishery restricted") %>% 
  sf::st_transform(crs=4326) 


# voorgesteld / Offshore
folder <- "Voorgestelde_visserijmaatregelen_MPAs"
layer  <- 'jr_cm_fm_cfp_nl_22_23_v02'

voorgesteld <- 
  sf::read_sf(file.path(path, folder), layer=layer) %>% 
  lowcase() %>%
  mutate(gebied = ifelse(gebied == "Doogersbank","Doggersbank",gebied)) %>% 
  tidyr::separate(labelconc, into=c("id","conc"), sep=":") %>% 
  filter(
    gebied %in% c("Klaverbank", "Doggersbank", "Doggersbank Zuid", "Friese Front","Borkumse Stenen", "Oestergronden")  
  ) %>% 
  mutate(
    SITE_TYPE = case_when(
      gebied %in% c("Friese Front") & nr == "4a"                                 ~ "KRM_N2000",
      gebied %in% c("Friese Front") & nr == "4b"                                 ~ "KRM",
      gebied %in% c("Oestergronden", "Borkumse Stenen")                          ~ "KRM",
      gebied %in% c("Doggersbank Zuid")                                          ~ "KRM",
      TRUE                                                                       ~ "N2000"
    )
  ) %>% 
  mutate(
    SITE_REG = case_when(
      gebied %in% c("Oestergronden")                          ~ "fishery restricted",
      TRUE                                                    ~ "fishery restriction proposed"
    )
  ) %>% 
  dplyr::select(SITE_NAME=gebied, SITE_TYPE, SITE_REG, nr, bron=remark) %>% 
  sf::st_transform(crs=4326) 


# visserijbeperkingen
visserijbeperkingen <-
  bind_rows(voorgesteld, nzk, vd, vvdr) %>% 
  mutate(nr = ifelse(is.na(nr), 99, nr)) %>% 
  
  sf::st_transform(crs=23031) %>%
  dplyr::mutate(area_km2 = as.integer(sf::st_area(.)/1000000)) %>% 
  dplyr::mutate(area_ha = as.integer(sf::st_area(.)/10000)) %>% 
  sf::st_transform(crs=4326) %>% 
  
  dplyr::select(SITE_NAME, SITE_TYPE, SITE_REG, nr, letter, zone, area_km2, area_ha, bron, omschrijving) %>% 
  sf::st_as_sf()



visserijbeperkingen %>% 
  ggplot() +
  theme_publication() +
  geom_sf(aes(fill=SITE_TYPE), alpha=0.5) +
  geom_sf_text(aes(label=nr), alpha=0.5) +
  geom_sf(data=world_mr_sf, fill="lightgray") +
  geom_sf(data=eez_sf, fill=NA, linetype="dashed") +
  coord_sf(xlim=c(bb[1],bb[3]), ylim=c(bb[2],bb[4])) 

# ==============================================================================
# Open parts of N2000/KRM areas
# ==============================================================================

n2000_krm_open <-
  sf::st_difference(n2000_krm, sf::st_union(visserijbeperkingen) ) %>% 
  sf::st_make_valid() %>% 
  sf::st_transform(crs=23031) %>%   
  dplyr::mutate(area_km2 = as.integer(sf::st_area(.)/1000000)) %>% 
  dplyr::mutate(area_ha = as.integer(sf::st_area(.)/10000)) %>% 
  sf::st_transform(crs=4326) %>% 
  mutate(SITE_REG = "open") %>% 
  filter(area_km2 > 0)

# ==============================================================================
# Open parts of EEZ (not taking into account windfarm areas)
# ==============================================================================
bijzondere_gebieden <-
  sf::st_union(
    sf::st_union(
      st_union(visserijbeperkingen), 
      st_union(n2000_krm)),st_union(aanvullende_suggesties))

nl_eez_open <-
  sf::st_difference(nl_eez_all, bijzondere_gebieden ) %>% 
  sf::st_make_valid() %>% 
  sf::st_transform(crs=23031) %>%   
  dplyr::mutate(area_km2 = as.integer(sf::st_area(.)/1000000)) %>% 
  dplyr::mutate(area_ha = as.integer(sf::st_area(.)/10000)) %>% 
  sf::st_transform(crs=4326) %>% 
  mutate(SITE_REG = "open") %>% 
  filter(area_km2 > 0)

# ==============================================================================
# Gecombineerd
# ==============================================================================

jff_comb <-
  bind_rows(
    n2000_krm_open,
    visserijbeperkingen,
    aanvullende_suggesties,
    nl_eez_open
  ) %>% 
  mutate(
    perc = area_km2 / sum(area_km2)
  )

jff_comb %>% 
  ggplot() +
  theme_publication() +
  geom_sf(aes(fill=SITE_REG), alpha=0.5) +
  geom_sf(data=world_mr_sf, fill="lightgray") +
  geom_sf(data=eez_sf, fill=NA, linetype="dashed") +
  coord_sf(xlim=c(bb[1],bb[3]), ylim=c(bb[2],bb[4])) 


library(graticule)
minor_grat <- graticule(
  lon = seq(bb[1], bb[3], 0.0625),
  lat = seq(bb[2], bb[4], 0.03125)
  ) %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(crs=4326) 

jff_comb %>% 
  ggplot() +
  theme_publication() +
  theme(legend.position="right", legend.direction="vertical") +
  geom_sf(aes(fill=SITE_REG), alpha=0.5) +
  geom_sf(data = minor_grat, color = "gray50", linewidth=0.1) +
  # geom_sf(data = wind, aes(fill=as.character(nummer)), colour="red") +
  geom_sf(data=world_mr_sf, fill="lightgray") +
  geom_sf(data=eez_sf, fill=NA, linetype="dashed") +
  coord_sf(xlim=c(bb[1],bb[3]), ylim=c(bb[2],bb[4])) 

p1 <-
  jff_comb %>% 
  ggplot() +
  theme_publication() +
  # theme(legend.position="right", legend.direction="vertical") +
  geom_sf(aes(fill=SITE_TYPE), alpha=0.5) +
  geom_sf(data = minor_grat, color = "gray50", linewidth=0.1) +
  # geom_sf(data = wind, fill=NA, colour="red", linewidth=0.1, linetype="dashed") +
  geom_sf(data=world_mr_sf, fill="lightgray") +
  geom_sf(data=eez_sf, fill=NA, linetype="dashed") +
  coord_sf(xlim=c(bb[1],bb[3]), ylim=c(bb[2],bb[4])) 

p2 <-
  jff_comb %>% 
  ggplot() +
  theme_publication() +
  # theme(legend.position="right", legend.direction="vertical") +
  geom_sf(aes(fill=SITE_REG), alpha=0.5) +
  geom_sf(data = minor_grat, color = "gray50", linewidth=0.1) +
  # geom_sf(data = wind, fill=NA, colour="red", linewidth=0.1, linetype="dashed") +
  geom_sf(data=world_mr_sf, fill="lightgray") +
  geom_sf(data=eez_sf, fill=NA, linetype="dashed") +
  coord_sf(xlim=c(bb[1],bb[3]), ylim=c(bb[2],bb[4])) 

p1 + p2

# jff_comb %>% 
#   group_by(SITE_REG) %>% 
#   summarise(perc = sum(perc, na.rm=TRUE)) %>% 
#   sf::st_drop_geometry() 

# save(jff_comb, file="rdata/jff_comb_v1.0.RData")
# sf::st_write(jff_comb, "shape/jff_comb_v1.0.shp")
# load(file="rdata/jff_comb_v1.0.RData")

save(jff_comb, file="rdata/jff_comb_v1.1.RData")
sf::st_write(jff_comb, "shape/jff_comb_v1.1.shp")
load(file="rdata/jff_comb_v1.1.RData")

jff_comb %>% 
  ggplot() +
  theme_publication() +
  geom_sf(aes(fill=SITE_REG), alpha=0.5) +
  geom_sf(data=world_mr_sf, fill="lightgray") +
  geom_sf(data=eez_sf, fill=NA, linetype="dashed") +
  geom_sf(data=viss_voorstel, fill=NA, colour="red", linewidth=1) +
  coord_sf(xlim=c(5,5.6), ylim=c(53.6,53.7)) 
  # coord_sf(xlim=c(4,6), ylim=c(53,54)) 



# ================================================================================
# Habitat maps
# ================================================================================

sf_use_s2(FALSE)

folder <- "shapes"
layer  <- 'euseamap nl_eez'

euseamap_nl <- 
  sf::read_sf(file.path(jffdir, folder), layer=layer) %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(crs=4326)

# sum(euseamap_nl$opp, na.rm=TRUE)
# euseamap_nl %>% dplyr::mutate(area_km2 = as.integer(sf::st_area(.)/1000000)) %>% sf::st_drop_geometry() %>% summarise(area_km2=sum(area_km2, na.rm=TRUE))

euseamap_nl %>% 
  ggplot() +
  theme_publication() +
  geom_sf(aes(fill=MSFD_BBHT))

save(euseamap_nl, file=file.path(jffdir,"rdata/euseamap_nl.RData"))


# ================================================================================
# TNO Geotif maps
# ================================================================================
library(stars)

datadir  <- "C:/Users/MartinPastoors/Martin Pastoors/NZO - General/WKC/adviezen/2025-xx Fact Finding beschermde gebieden/data/TNO"
filelist <- list.files(
  path=file.path(datadir),
  pattern="tif",
  recursive=TRUE,
  full.names = TRUE)

i = 0
for (fn in filelist) {
  i = i + 1
  if (i==1) {
    r <- read_stars(file.path(fn)) %>% sf::st_transform(crs=4326) %>% sf::st_as_sf() %>% setNames(c("value", "geometry")) %>% mutate(sample_area=fn)
  } else {
    r <- bind_rows(r, 
                   read_stars(file.path(fn)) %>% sf::st_transform(crs=4326) %>% sf::st_as_sf() %>% setNames(c("value", "geometry")) %>% mutate(sample_area=fn))
  }
} 
sabellaria <- r
remove(r)

ggplot() +
  theme_publication() +
  theme(legend.key.width=unit(2, "cm")) + 
  geom_sf(data=sabellaria, aes(fill=value), lwd=0) +
  geom_sf(data=nl_eez_all, fill=NA) +
  geom_sf(data=jff_comb, fill=NA, colour="red") +
  coord_sf(xlim=c(3.0,3.5), ylim=c(52.25,52.75)) +
  scale_fill_viridis(option="inferno", direction=-1)

save(sabellaria, file=file.path(jffdir,"rdata/sabellaria.RData"))


# ================================================================================
# Partiele herziening maps
# ================================================================================

folder <- "shapes/Shapefiles MRP-kaart ontwerp PH/Natuur"
layer  <- 'boundary only'

ph_natuur_boundary <- 
  sf::read_sf(file.path(jffdir, folder), layer=layer) %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(crs=4326)

folder <- "shapes/Shapefiles MRP-kaart ontwerp PH/Natuur"

layer1  <- 'KRM_20240822'
layer2  <- 'KRM-gebied'
layer3  <- 'Natuurbescherming'
layer4  <- 'Zones waar visserijactiviteiten (deels) worden beperkt of zullen worden beperkt'
ph_natuur <- 
  bind_rows(
    sf::read_sf(file.path(jffdir, folder), layer=layer1) %>% sf::st_as_sf() %>% sf::st_transform(crs=4326) %>% mutate(layer = layer1),
    sf::read_sf(file.path(jffdir, folder), layer=layer2) %>% sf::st_as_sf() %>% sf::st_transform(crs=4326) %>% mutate(layer = layer2),
    sf::read_sf(file.path(jffdir, folder), layer=layer3) %>% sf::st_as_sf() %>% sf::st_transform(crs=4326) %>% mutate(layer = layer3),
    sf::read_sf(file.path(jffdir, folder), layer=layer4) %>% sf::st_as_sf() %>% sf::st_transform(crs=4326) %>% mutate(layer = layer4),
  )

ggplot() +
  theme_publication() +
  geom_sf(data=ph_natuur, aes(fill=layer), alpha=0.5) +
  geom_sf(data=nl_eez_all, fill=NA) +
  facet_wrap(~layer)

# sum(euseamap_nl$opp, na.rm=TRUE)
# euseamap_nl %>% dplyr::mutate(area_km2 = as.integer(sf::st_area(.)/1000000)) %>% sf::st_drop_geometry() %>% summarise(area_km2=sum(area_km2, na.rm=TRUE))

# windenergie
folder <- "shapes/Shapefiles MRP-kaart ontwerp PH/Windenergie"
layer1  <- 'pgeo_zd_aangewezen_windgebied'
layer2  <- 'Windenergie'
layer3  <- 'Windenergiegebied 6_7'
layer4  <- 'Windenergiegebieden'

ph_windenergie <-
  bind_rows(
    sf::read_sf(file.path(jffdir, folder), layer=layer1) %>% sf::st_as_sf() %>% sf::st_transform(crs=4326) %>% mutate(layer = layer1),
    sf::read_sf(file.path(jffdir, folder), layer=layer2) %>% sf::st_as_sf() %>% sf::st_transform(crs=4326) %>% mutate(layer = layer2),
    sf::read_sf(file.path(jffdir, folder), layer=layer3) %>% sf::st_as_sf() %>% sf::st_transform(crs=4326) %>% mutate(layer = layer3),
    sf::read_sf(file.path(jffdir, folder), layer=layer4) %>% sf::st_as_sf() %>% sf::st_transform(crs=4326) %>% mutate(layer = layer4),
  )

#scheepvaart
folder <- "shapes/Shapefiles MRP-kaart ontwerp PH/Scheepvaart"
layer  <- 'Clearways aanpassing Ontwerp PH'

ph_scheepvaart <- 
  sf::read_sf(file.path(jffdir, folder), layer=layer) %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(crs=4326)

#zandwinning

folder <- "shapes/Shapefiles MRP-kaart ontwerp PH/Zandwinning"
layer  <- 'Reserveringszone zandwinning nautische 14 mijl'

ph_zandwinning <- 
  sf::read_sf(file.path(jffdir, folder), layer=layer) %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(crs=4326)

#olie en gas
folder <- "shapes/nlog_facilities_ed50utm31n"
layer  <- 'May-2025_NLOG_Facilities_ED50UTM31N'

nlog <- 
  sf::read_sf(file.path(jffdir, folder), layer=layer) %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(crs=4326) %>% 
  sf::st_intersection(nl_eez_all)

load(file=file.path(jffdir, "rdata/nl_eez_all.RData"))
load(file=file.path(jffdir, "rdata/jff_comb_v1.1.RData"))

ggplot() +
  theme_publication() +
  geom_sf(data=jff_comb %>% filter(SITE_NAME != "EEZ"), fill="green", alpha=0.5) +
  geom_sf(data=ph_windenergie, aes(fill=layer), alpha=0.5) +
  geom_sf(data=nl_eez_all, fill=NA) +
  geom_sf(data=ph_zandwinning, fill="yellow", alpha=0.5) +
  geom_sf(data=ph_scheepvaart, fill="darkgreen", colour="red", alpha=0.5) +
  geom_sf(data=nlog %>% filter(STATUS=="In gebruik"), colour="blue")

  

save(ph_natuur,      file=file.path(jffdir,"rdata/ph_natuur.RData"))
save(ph_scheepvaart, file=file.path(jffdir,"rdata/ph_scheepvaart.RData"))
save(ph_zandwinning, file=file.path(jffdir,"rdata/ph_zandwinning.RData"))
save(ph_windenergie, file=file.path(jffdir,"rdata/ph_windenergie.RData"))
save(nlog,           file=file.path(jffdir,"rdata/nlog.RData"))










