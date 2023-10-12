# 1- import libraries & dataset -------------------------------------------

pacman::p_load(dplyr,
               tidyr,
               sf,
               ggplot2,
               scales,
               rnaturalearth,
               install = FALSE
)

haunted_places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-10/haunted_places.csv')

theme_set(theme_minimal(base_size = 15))
theme_update(
  plot.title.position = "plot",
  axis.line.x = element_line(linewidth = .2, colour = "grey50"),
  axis.line.y = element_line(linewidth = .2, colour = "grey50"),
  panel.grid = element_blank()
)


# 2- Explore dataset ------------------------------------------------------

glimpse(haunted_places)

haunted_places%>% 
  ggplot(aes(fct_reorder(state, n), n))+
  geom_col()+
  coord_flip()

haunted_places_count <- 
  haunted_places %>%
  count(state, sort = TRUE) %>% 
  rename(name = state)



# 3- import map data ------------------------------------------------------


x <- rnaturalearth::ne_states(country = "united states of america", 
                              returnclass = "sf") 



# 4- join datasets --------------------------------------------------------


haunted_places_sf <- left_join(x = haunted_places_count,y = x,
                               by = "name") %>% 
  select(name, n, geometry) %>% 
  st_as_sf() %>% 
  st_transform(5070)
# https://gis.stackexchange.com/questions/457636/what-is-the-right-epsg-crs-code-for-the-united-states

haunted_places_sf

sf::st_bbox(haunted_places_sf)



# 5- create the ggplot object ---------------------------------------------


p <- haunted_places_sf %>% 
  ggplot()+
  geom_sf(aes(fill = n,
              color = n),
  )+
  geom_sf_text(aes(label = name),
               size = 1,
               color = "#44355b", 
               #alpha = .8,
               family = "Barlow Condensed"
  )+
  geom_sf_text(aes(label = n),
               size = .9,
               vjust = 2.7,
               color = "#44355b", 
               #alpha = .8,
               family = "Barlow Condensed"
  )+
  theme_void()+
  scale_fill_gradient2(low = "#fee8c8",
                       mid = "#fdbb84",
                       high = "#e34a33",
                       name = ""
  )+
  scale_color_gradient2(low = "#fee8c8",
                        mid = "#fdbb84",
                        high = "#e34a33",
                        name = ""
  )+
  theme(legend.position="bottom",
        legend.text = element_text(size = 5,
                                   color = "#333333"),
        legend.key.height = unit(.3, "lines"),
        legend.key.width = unit(2, "lines"))


p 

# 6- rayshader  ---------------------------------------------------------

pacman::p_load(rayshader,
               rgl,
               glue,
               install = FALSE)



rgl::close3d() 

rayshader::plot_gg(
  p,
  width=6,
  height=5,
  scale=100,
  solid = FALSE,
  raytrace = FALSE,
  offset_edges = TRUE,
  #shadowdepth = 0.1,  
  background = "white",
  max_error = 0.01,
  windowsize=c(800,800),
  zoom = 0.7, 
  phi = 70,
  theta = 0)

render_camera(zoom=.7,theta=0,phi = 70)
render_snapshot()

# This took 2.5 hours to render.
# geom_text works perfectly on macOS.
# When running on Windows, I encountered an error 
# that prevented plot_gg() from displaying the correct font family

render_highquality(
  filename = "usa_haunted_manual_s2000.png",
  width = 2000, height = 2000,
  samples = 2000, 
  interactive = FALSE,
  lightdirection = c(290), 
  lightaltitude=45, 
  lightintensity = 600,
  lightcolor=c("#44355b"),
  scene_elements = rayrender::sphere(z=0,
                                     y=2300, 
                                     x=-1000, 
                                     radius=350,
                                     material=rayrender::diffuse(color = "white",
                                                                 noise=1/20,
                                                                 checkercolor = "#44355b",
                                                                 noiseintensity = 50
                                     )),
  min_variance = 0, 
  sample_method = "sobol_blue")
