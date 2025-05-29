pacman::p_load(
  terra,tidyverse,data.table,lubridate,stars,mgcv,mgcViz,tictoc,
               patchwork,scico,arrow)

# prep data ===============================================================
# Koppen ------------------------------------------------------------------
# A: Tropical
# B: Arid
# C: Temperate
# D: Continental
# E: Polar

kop <- rast("../../data_general/Koppen_climate/Beck_KG_V1_present_0p083.tif")
# terra::resample(kop,knr,method='mode') %>% plot()
# list.files("../data_general/Koppen_climate/")
legend <- read_fwf(file = "../../data_general/Koppen_climate/legend.txt",
  skip = 3, n_max=30,
  # col_names = F,
  # col_names=c("id","class","class_long","rgb"),
  col_types = 'c')
names(legend) <- c("id","class","class_long","rgb")
legend <- legend %>% 
  mutate(kop_id = as.numeric(str_remove(id,":"))) %>% 
  rename(kop_class = class, kop_long = class_long, kop_rgb = rgb) %>% 
  select(-id)
dkop <- as.data.table(kop,xy=T) %>% 
  set_names('xc','yc','kop_id')
dkop <- merge(dkop,legend,by=c("kop_id"))

# coast lines
coast <- rnaturalearth::ne_coastline(returnclass = 'sf',scale = 'large')
# qld <- rnaturalearth::ne_countries(country="Australia",returnclass = 'sf')
qld <- ozmaps::ozmap_states %>% filter(NAME=="Queensland")

# oz tropical rainforests ===========================================
oz_lc <- terra::rast("../../data_general/ABARES_landcover/aus_for18_geotiff/aus_for18.tif")
activeCat(oz_lc) <- 4
levels(oz_lc)
categories(oz_lc)
# cats(oz_lc) %>% head
# plot(oz_lc)

roi <- ext(c(145,147,-20,-15))
r1 <- rast(#ncols=40, nrows=40, 
     xmin=142, xmax=147, ymin=-23, ymax=-13, 
          crs="+proj=longlat +datum=WGS84")
r2 <- project(r1,crs(oz_lc))
r3 <- crop(oz_lc,ext(r2))

r5 <- project(r3, "+proj=longlat +datum=WGS84")
activeCat(r5) <- 4
# plot(r5)

r6 <- aggregate(r5, fact=5, fun='modal')
activeCat(r6) <- 4
# plot(r6)

d6 <- as.data.table(r6,xy=T)
cities <- data.table(name=c("Cooktown","Cairns","Townsville"),
  x= c(145.2441, 145.744, 146.807), 
  y = c(-15.4785, -16.923, -19.257)) %>% 
sf::st_as_sf(., coords=c("x","y")) %>% 
  st_set_crs(4236)

pan1 <- d6 %>% 
  mutate(land_cover = case_when(
    FOR_TYPE=="Rainforest"~"Rainforest",
    FOR_TYPE=="Non forest"~"Non forest",
    TRUE ~ "Other forest type")) %>% 
  ggplot(data=., aes(x,y))+
  geom_sf(data=coast,inherit.aes = F,lwd=1,col="grey30")+
  geom_tile()+
  geom_tile(aes(x,y,fill=land_cover))+
  geom_sf(data= cities, 
    inherit.aes = F, 
    shape = 20,
    size=6,
    col='black') +
  geom_sf_label(data= cities, 
    aes(label=name), 
    inherit.aes = F, 
    nudge_x = 0.5,
    nudge_y = 0.15)+
  coord_sf(ylim=c(-20,-15), 
    xlim=c(145,147.8), 
    crs = st_crs(4326))+
  scale_x_continuous(breaks = c(145,146,147))+
  scale_fill_manual(values = c("grey","#9CE6A0AF","darkgreen"))+
  # scale_fill_manual(values=c(F = "grey", T = "darkgreen"))+
  labs(x=NULL,y=NULL,
    fill='Land Cover')+
  ggspatial::annotation_scale(
    # location='br'
    # location = c(0.9,0.5)
    )+
  theme(panel.grid = element_blank(),
    legend.position = c(0.99,0.99),
    legend.justification = c(0.99,0.99), 
    legend.background = element_rect(fill='transparent'))
# pan1
ggsave(pan1, 
  filename='figures/figure_map_tropics_panel1.png',
  width=8,
  height=13,
  units='cm',
  dpi=350)


# # Top panel =================================
(pan2 <- dkop[kop_long=="Tropical, rainforest"] %>%
  ggplot(data=.,aes(xc,yc))+
  geom_sf(inherit.aes = F,
    data=coast,
    col='grey30',
    lwd=0.25)+
  geom_tile(fill='darkgreen')+
  coord_sf(ylim=c(-23.5,23.5),
    xlim=c(-85,160),
    crs = st_crs(4326))+
  labs(x=NULL,
    y=NULL)+
  theme(panel.grid = element_blank()))
ggsave(pan2, 
  filename='figures/figure_map_tropics_panel2.png',
  width=20,
  height=5,
  units='cm',
  device = grDevices::png,
  dpi=350)

# climate panel ===============================
v_u <- rast("data/dp24_gee_exports/vpd_mean_1990-01-01_2021-12-31.tif")
v_min <- rast("data/dp24_gee_exports/vpd_min_1990-01-01_2021-12-31.tif")
v_max <- rast("data/dp24_gee_exports/vpd_max_1990-01-01_2021-12-31.tif")
v_sd <- rast("data/dp24_gee_exports/vpd_sd_1990-01-01_2021-12-31.tif")
v_p5 <- rast("data/dp24_gee_exports/vpd_p5_1990-01-01_2021-12-31.tif")
v_p95 <- rast("data/dp24_gee_exports/vpd_p95_1990-01-01_2021-12-31.tif")

p_u <- rast("data/dp24_gee_exports/GPM-IMERG-v6_precip-mean-annual_2000-06-01_2021-09-30.tif")
p_p5 <- rast("data/dp24_gee_exports/GPM-IMERG-v6_precip-5percentile_2000-06-01_2021-09-30.tif")
p_p95 <- rast("data/dp24_gee_exports/GPM-IMERG-v6_precip-95percentile_2000-06-01_2021-09-30.tif")


tmp <- rnaturalearth::ne_countries(returnclass = 'sf') %>% 
  mutate(continent = case_when(
    (continent=="Oceania" & name=="Australia") ~ "Australia",
    TRUE ~ continent
  )) %>% 
  select(continent) 

tmp2 <- terra::rasterize(vect(tmp),v_u,field='continent')
  # as.data.table(xy=T) %>% 
  # pull(layer) %>% table

coords <- dkop[kop_long=="Tropical, rainforest"] %>% 
  rename(x=xc,y=yc)
coords$continent <- terra::extract(tmp2,
     y =coords %>% select(x,y))$continent
coords$vpd_u <- terra::extract(v_u,
     y =coords %>% select(x,y))$vpd
coords$vpd_min <- terra::extract(v_min,
     y =coords %>% select(x,y))$vpd
coords$vpd_max <- terra::extract(v_max,
     y =coords %>% select(x,y))$vpd
coords$vpd_sd <- terra::extract(v_sd,
     y =coords %>% select(x,y))$vpd
coords$vpd_p5 <- terra::extract(v_p5,
     y =coords %>% select(x,y))$vpd
coords$vpd_p95 <- terra::extract(v_p95,
     y =coords %>% select(x,y))$vpd

coords$p_u <- terra::extract(p_u,
     y =coords %>% select(x,y))$precipitation
coords$p_p5 <- terra::extract(p_p5,
     y =coords %>% select(x,y))$precipitation_p5
coords$p_p95 <- terra::extract(p_p95,
     y =coords %>% select(x,y))$precipitation_p95
coords <- coords[is.na(continent)==F] %>% 
  mutate(cont1 = as.character(continent)) %>% 
  mutate(cont2 =       case_when(
    (cont1=="Europe") ~ "South America",
    (cont1=="North America") ~"Central America",
    TRUE~cont1))



library(cols4all)
cols4all::c4a_gui()
(pan3 <- coords %>%
  select(cont2, vpd_p5,vpd_u,vpd_p95) %>% 
  pivot_longer(-cont2) %>% 
  ggplot(data=.,aes(y=name, x=value,fill=cont2))+
  geom_boxplot(outlier.colour = NA)+
  scale_y_discrete(limits=c("vpd_p5","vpd_u","vpd_p95"),
    labels=c("5% percentile","mean", "95% percentile"))+
  scale_fill_discrete_c4a_cat(palette = 'okabe')+
  labs(x='VPD (kPa)',
       y=NULL,
    fill='Continent')+
  theme_linedraw()+
  theme(panel.grid = element_blank(),
    legend.position = c(0.99,0.01),
    legend.justification = c(0.99,0.01),
    legend.background = element_rect(fill='transparent')))
(pan4 <- coords %>%
  select(cont2, p_p5,p_u,p_p95) %>% 
  pivot_longer(-cont2) %>% 
  ggplot(data=.,aes(y=name, x=value,fill=cont2))+
  geom_boxplot(outlier.colour = NA)+
  scale_y_discrete(limits=c("p_p5","p_u","p_p95"),
    labels=c("5% percentile","mean", "95% percentile"))+
  scale_fill_discrete_c4a_cat(palette = 'okabe')+
  labs(x='Annual Precipitation (mm yr¯¹)',
       y=NULL,
    fill='Continent')+
  theme_linedraw()+
  theme(panel.grid = element_blank(),
    legend.position='none',
    # legend.position = c(0.99,0.01),
    legend.justification = c(0.99,0.01)))

pan3/pan4

ggsave(pan3/pan4, 
  filename='figures/figure_map_tropics_panel3.png',
  width=17,
  height=13,
  units='cm',
  device = grDevices::png,
  dpi=350)


# 
# 
# ggplot(dkop[kop_long=="Tropical, rainforest"], 
#   aes(xc,yc))+
#   geom_sf(data=qld,
#     inherit.aes = F)+
#     coord_sf(ylim=c(-20,-15), 
#     xlim=c(145,147), 
#     crs = st_crs(4326))+
#   geom_tile()+
#   labs(x=NULL,
#     y=NULL)+
#   theme(panel.grid = element_blank())



# tmp[sample(.N,1000)] %>% 
#   ggplot(data=.,aes(x,y,color=FOR_TYPE))+
#   geom_point()+
#   coord_sf()
# 
# 
# ibra <- sf::read_sf("../data_general/IBRA/IBRA7_subregions_states.shp")
# ibra <- ibra %>% filter(STA_CODE=="QLD")
# ibra <- ibra %>% st_transform(., st_crs(4326))
# ibra <- ibra %>% select(SUB_NAME_7)
# library(leaflet)
# ?leaflet
# class(ibra)
# leaflet(ibra) %>% setView(lng=146,lat=-17,zoom=4)
# leaflet(ibra)%>% setView(lng=146,lat=-17,zoom=4) %>% 
#   addTiles() %>% 
#   addPolygons()
# 
# ibra %>% select(SUB_NAME_7) %>% plot
# ibra %>% 
#   st_transform(., st_crs(4326)) %>% 
#   # st_as_sf() %>% 
#   st_crop(., xmin=145, xmax=147, ymin=-20,ymax=-15)
# ibra %>% select(REG_CODE_7) %>% plot()
# 
# ib2 <- terra::vect("../data_general/IBRA/ibra7_regions.shp")
# plot(ib2)
