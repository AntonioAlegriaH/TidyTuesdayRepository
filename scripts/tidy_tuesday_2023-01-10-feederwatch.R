# libraries
  pacman::p_load(
    readr,
    dplyr,
    tidyr,
    forcats,
    lubridate,
    stringr,
    pryr,
    sf,
    maps,
    scales,
    ggplot2,
    ggrepel,
    glue,
    ggfx,
    RColorBrewer,
    patchwork
  )



# Observation Data
  feederwatch <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv')
# Site Description  
  site_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_count_site_data_public_2021.csv')
# Species names
  df_species_name <- read_csv("FeederWatch_Data_Dictionary_Spc_name.csv") %>% 
    select(SPECIES_CODE, SCI_NAME, PRIMARY_COM_NAME) %>% 
    janitor::clean_names()
# Species primary name
  feederwatch_names <- left_join(feederwatch, df_species_name, by = "species_code" )
  
  
  theme_set(theme_minimal(base_size = 15))
  theme_update(
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "#f1f1f1",linewidth = .1),
    axis.line.y = element_line(color = "#f1f1f1",linewidth = .1),
    plot.title.position = "plot",
    plot.margin = margin(1,2,1,2,"cm"),
    plot.caption = element_text(size = 9)
  )

  
# Parse dates  (in case I need a date)
  
  feederwatch_parsed <- feederwatch_names %>% 
    mutate(date = make_date(year = Year,
                            month = Month, 
                            day = Day
    )
    ) %>% 
    select(-c(Year, Month, Day)
    )

##### Texas analysis

# Map Data
  species_count <- feederwatch_parsed %>% 
    group_by(loc_id,  latitude, longitude, subnational1_code, species_code, primary_com_name )  %>% 
    summarise(n_species = sum(how_many)
    ) %>% 
    drop_na() %>% 
    ungroup()
  
# Separate subnational1_code ()
  species_count[ , 8:9] <- 
    str_split_fixed(species_count$subnational1_code, 
                    pattern = "-",
                    n = 2) 
  
  
# Lumping and filtering data
# Count species higher than 30 counts & more than 10 individual observations
  species_count_tx <- species_count %>% 
    select(!subnational1_code) %>% 
    rename(country = "V1",
           state_or_providence = "V2") %>% 
    filter(state_or_providence == "TX") %>% 
    mutate(species_lump = fct_lump_min(primary_com_name, 
                                       min = 30,
                                       w = n_species)
    ) %>% 
    filter(species_lump != "Other",
           n_species > 10)
  
# How many species are going to be mapped ()
  unique(species_count_tx$species_lump)

  
# Bar plot data
# Count total of species observations (No filter)
  
  species_no_filter <- species_count %>% 
    filter(subnational1_code == "US-TX") %>% 
    count(primary_com_name, wt= n_species, sort = T) %>% 
    mutate(primary_com_name_lump = fct_lump_min(primary_com_name,
                                                min = 100,
                                                w = n)
    ) 
  
  
  
  
# prep variables for plots

  #base map of texas
   tx_map <- st_as_sf(map("county", regions = "texas", fill = TRUE)) 
  # st_as_sf()
  #color lenght & palette colors
   colourCount = length(unique(species_count_tx$species_lump))
   # getPalette = colorRampPalette(brewer.pal(12, "Paired")) 
   # getPalette = colorRampPalette(brewer.pal(8, "Set2")) 
   # getPalette = colorRampPalette(brewer.pal(11, "PuOr")) 
   getPalette = colorRampPalette(brewer.pal(12, "Paired")) 
   bg_plot <- "#1b263b"
   fill_map <- "#1b263b"

# Plots   
  # Map plot
   plot_map <- species_count_tx %>% 
     ggplot() +
     ggfx::with_shadow(
       geom_sf(data = tx_map,
               fill = fill_map,
               colour = "grey50"
       ) ,colour = "white" 
     )+
     geom_point(aes(longitude, latitude, size = n_species, color = species_lump), alpha = 0.7, show.legend = FALSE)+ 
     geom_text_repel(aes(longitude, latitude, label =  glue("{species_lump}:{n_species}"), color = species_lump), 
                     show.legend = FALSE, max.overlaps = 20, 
                     size = 1, alpha = .9,
                     segment.size = 0.2, segment.alpha	= 0.3,
                     box.padding = 0.5,
                     nudge_y = .5,
                     segment.curvature = -0.1,
                     segment.ncp = 1,
                     segment.angle = 10)+
     scale_color_manual(values = getPalette(colourCount))+
     labs(
       title = "Bird sightings in Texas",
       subtitle = "",
       caption = "Data: feederwatch.org |    @feederwatch
     Dataviz: antonioalegria.io | @elmedicobrujo",
     size = "Number\nof birds"
     ) +
     theme_void() +
     theme(
       plot.title.position = "plot",
       plot.title = element_text(size = 66, hjust = .5,
                                 family = "ITC Bookman Std", 
                                 face = "bold", color = "white"),
       #plot.subtitle = element_text(family = ft, hjust = 0.5, margin = margin(b = 60)),
       plot.caption = element_text(size = 8,
                                   family = "ITC Bookman Std",
                                   margin = margin(t = 1,)),
       plot.background = element_rect(fill = bg_plot, colour = NA),
       plot.margin = margin(t= 10, r = 50, b = 30,  l = 50),
       text = element_text(size = 15, 
                           colour = "#f1f1f1"),
       legend.position = "bottom"
     ) 


 # Bar Plot
   bar_plot <- species_no_filter %>% 
    ggplot(aes(reorder(primary_com_name_lump, n), n))+
    with_shadow(geom_col(aes(fill = primary_com_name_lump), 
                         alpha = .6,
                         show.legend = FALSE),
                colour = "white",
                x_offset = 3,y_offset = 2
    )+
    geom_text(data = filter(species_no_filter, n>99), 
              aes(label = comma(n)), 
              color = "#f1f1f1",
              size = 2,
              hjust = -.4)+
    coord_flip(clip = "off", expand = FALSE)+
    labs(title = "Top 30 Species",
         x = "",
         y = "")+
    theme(plot.title.position = "panel",
          plot.title = element_text(size = 14,
                                    family = "ITC Bookman Std",
                                    face = "plain",
                                    hjust = 1),
          axis.text = element_text(size = 6,
                                   family = "Roboto",
                                   face = "plain",
                                   color = "#f1f1f1"),
          text = element_text(color = "#f1f1f1"),
          plot.margin = margin(0,0,0,0,"cm"),)+
    scale_fill_manual(values = getPalette(colourCount))
  
  
  
# Assemble final dataviz:
  plot_final <-   
    plot_map+inset_element(bar_plot,
                           left=-0.16,
                           bottom = -0.06,
                           right = 0.09,
                           top = .96,#0.15
                           align_to = "plot"
    )
  
  ggsave(
    "tx_final_3.png",
    plot = plot_final,
    width = 1200,
    height = 1200,
    units = "px",
    scale = 3,
    bg = bg_plot,
    dpi = 300
  )  
  