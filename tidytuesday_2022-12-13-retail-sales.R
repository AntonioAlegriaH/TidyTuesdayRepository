

# Palette:
# https://coolors.co/b7094c-a01a58-892b64-723c70-5c4d7d-455e89-2e6f95-1780a1-0091ad
# Load data & packages
  pacman::p_load(dplyr,
                 tidyr,
                 lubridate,
                 ggplot2, 
                 scales, 
                 pryr, 
                 stringr,
                 ggridges,
                 geofacet,
                 gghighlight,
                 install = FALSE
  )
  
  
  theme_set(theme_minimal(base_size = 15))
  theme_update(panel.grid = element_blank(),
               axis.line.x = element_line(),
               axis.line.y = element_line(),
               plot.caption = element_text(size = 9))
  
  
  state_retail <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-13/state_retail.csv',  
                                  col_types = "cciciiccc") 
  coverage_codes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-13/coverage_codes.csv')
  
  

# Data wrangling
  state_retail_num <- state_retail %>% 
    mutate(change_yoy = as.numeric(change_yoy), 
           change_yoy_se = as.numeric(change_yoy_se),
           coverage_code = as.factor(coverage_code))
  
## Subseting Health and Personal Care
  usa_health_care <-  state_retail_num %>% 
    filter(state_abbr == "USA") %>% 
    mutate(date_parsed = make_date(year = year, 
                                   month = month, 
                                   day = 1)) %>% 
    select(-c(year, month))%>% 
    filter(subsector == "Health and Personal Care")
  
  

# Area plot  
  
# Years label  
  year_labels_a <- tibble(x = as_date(c("2019-02-01",
                                        "2020-02-01",
                                        "2021-02-01",
                                        "2022-02-01")),
                          y = c(1.55,1.55,1.55,1.55)
                          )
  
# plot
  
  usa_health_care %>% 
    ggplot(aes(date_parsed, 
               change_yoy)) +
    geom_hline(yintercept = 0, color = "grey90")+
    geom_text(data = year_labels_a, 
              aes(x = x,
                  y = y, label = year(x)),
              color = "grey50", fontface = "bold",
              size = 20, nudge_x = 100)+
    geom_area(aes(x = date_parsed, 
                  y = if_else(change_yoy>0, change_yoy, 0)),
              color = "#0091ad",
              fill = "#0091ad",
              alpha = .5)+
    geom_area(aes(x = date_parsed, 
                  y = if_else(change_yoy<0, change_yoy, 0)),
              color = "#b7094c", 
              fill = "#b7094c", 
              alpha = .5 )+
    annotate(geom = "point", 
             x = as.Date("2021-04-01"),
             y = max(usa_health_care$change_yoy),
             color = "orange",
             size = 7)+
    # annotate(geom = "text", 
    #          label = "Max percent \nchange: 24% ",
    #          x = as.Date("2021-02-01"),
    #          y = max(usa_health_total$change_yoy),
    #          size = 8)+
    annotate(geom = "point", 
             x = as.Date("2020-05-01"),
             y = min(usa_health_care$change_yoy),
             color = "orange",
             size = 7)+
    #  annotate(geom = "text", 
    #           label = "Min percent \nchange: -11% ",
    #           x = as.Date("2020-02-01"),
    #           y = min(usa_health_total$change_yoy),
    #           size = 8)+
    geom_point(aes(color = if_else(change_yoy>0, "#b7094c", "#0091ad")),
               show.legend = FALSE,
               alpha = .5)+
    scale_y_continuous(labels = percent_format(scale = 1,
                                               suffix = "%"))+
    labs(title = "",
         subtitle = "",
         x= "",
         y = "")+
    theme(plot.title.position = "plot",
          plot.title = element_text(size = 40),
          axis.text.y = element_text(size = 20),
          axis.text.x = element_blank(),
          panel.grid.major.x =element_line(color = "gray95"),
          axis.line.x = element_blank(),
          strip.text = element_text(face = "bold"),
          #text = element_text(family = "Poppins")
    )+
    coord_cartesian(expand = TRUE,
                    clip = "off")
  
  ggsave("plot-line_t.png",
         plot = last_plot(),
         scale = 4.5,
         height = 800,
         width = 1400,
         units = "px",
         dpi = 350,
         bg = "white"
  )
  

  
  
# geo facet
  
  
  
  year_labels_b <- tibble(x = as_date(c("2019-02-01",
                                        "2020-02-01",
                                        "2021-02-01",
                                        "2022-02-01")),
                          y = c(25,25,25,25)
  )
  
  
  
  
  ## Subseting Health and Personal Care
  usa_health_care_map <-  state_retail_num %>% 
    filter(state_abbr != "USA") %>% 
    mutate(date_parsed = make_date(year = year, 
                                   month = month, 
                                   day = 1)) %>% 
    select(-c(year, month))%>% 
    filter(subsector == "Health and Personal Care")
  
  
  
  # Assemble for visual test
  
  map_plot <- usa_health_care_map %>% 
    ggplot()+
    geom_hline(yintercept = 0, 
               color = "grey50")+
    geom_text(data = year_labels_b, 
              aes(x = x,
                  y = y, label = year(x)),
              color = "grey50", fontface = "bold",
              size = 4, nudge_x = 120)+
    geom_area(aes(x = date_parsed, 
                  y = if_else(change_yoy>0, change_yoy, 0)),
              color = "#0091ad",
              fill = "#0091ad",
              alpha = .5)+
    geom_area(aes(x = date_parsed, 
                  y = if_else(change_yoy<0, change_yoy, 0)),
              color = "#b7094c", 
              fill = "#b7094c", 
              alpha = .5 )+
    scale_y_continuous(labels = percent_format(scale = 1,
                                               suffix = "%"))+
    labs(title = "",
         subtitle = "",
         x= "",
         y = "")+
    theme(plot.title.position = "plot",
          plot.title = element_text(size= 50),
          axis.text.y = element_text(size = 10),
          axis.text.x = element_blank(),
          panel.grid.major.x =element_line(color = "gray95"),
          strip.text = element_text(face = "bold",
                                    size = 20,
                                    color = "grey30" ),
          panel.border = element_rect(fill = NA,
                                      color = "grey80"),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          text = element_text(family = "Poppins")
    )+
    coord_cartesian(expand = TRUE,
                    clip = "off")+
    facet_geo(vars(state_abbr)
    )
  
  ggsave("health_geofacet_10.png",
         plot = map_plot,
         width = 1600,
         height = 1200,
         units = "px",
         dpi = 350,
         bg = "white",
         scale = 4.5)
  
  
  
# tiles
  
  year_labels_tiles <- tibble(x = as_date(c("2019-02-01",
                                      "2020-02-01",
                                      "2021-02-01",
                                      "2022-02-01")),
                        y = c(1,1,1,1)
  )
  
  
  
# Tile usa complete  
  usa_health_care %>% 
    ggplot()+
    geom_tile(aes(date_parsed, subsector, fill = change_yoy),
              width=100)+
    geom_text(data = year_labels_tiles, 
              aes(x = x,
                  y = y, label = year(x)),
              color = "grey50", fontface = "bold",
              size = 40, nudge_x = 100)+
    scale_fill_gradient2(low = "#b7094c",
                         mid = "grey98",
                         high = "#0091ad",
                         label = percent_format(scale = 1),
                         limits = c(-50,50),
                         na.value = "transparent",
                         name = "Percent Change" 
                         
    ) +
    theme(plot.title.position = "plot",
          plot.title = element_text(size= 50),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.x =element_line(color = "gray55"),
          strip.text = element_text(face = "bold",
                                    size = 20),
          # panel.border = element_rect(fill = NA,
          #                             color = "grey50"),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          legend.position = "bottom",
          legend.key.width = unit(6, "cm"),
          legend.key.height = unit(.8, "cm"),
          legend.title = element_blank(),
          legend.text = element_text(color = "grey50",
                                     size = 30),
          panel.ontop = TRUE)
  
  ggsave("health_tile_test.png",
         plot = last_plot(),
         width = 1700,
         height = 1000,
         units = "px",
         dpi = 350,
         bg = "white",
         scale = 4.5)
  

# geofacet tiles
  
  usa_health_care_geofacet <-  state_retail_num %>% 
    filter(state_abbr != "USA") %>% 
    mutate(date_parsed = make_date(year = year, 
                                   month = month, 
                                   day = 1)) %>% 
    select(-c(year, month))%>% 
    filter(subsector == "Health and Personal Care")
  
  
  
  
  usa_health_care_geofacet %>% 
    ggplot()+
    geom_tile(aes(date_parsed, subsector, fill = change_yoy),
              width = 50,
              linetype = 1, 
              show.legend = FALSE)+
    geom_text(data = year_labels_a, 
              aes(x = x,
                  y = y, label = year(x)),
              color = "grey50", fontface = "bold",
              size = 3.5, nudge_x = 110
    )+
    coord_cartesian(clip = "off")+
    scale_fill_gradient2(low = "#b7094c",
                         mid = "grey98",
                         high = "#0091ad",
                         label = percent_format(scale = 1),
                         limits = c(-50,50),
                         na.value = "transparent",
                         name = "Percent Change" 
                         
    ) +
    theme(plot.title.position = "plot",
          plot.title = element_text(size= 50),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.x =element_line(color = "gray55"),
          strip.text = element_text(face = "bold",
                                    size = 20,
                                    color = "grey30"),
          # panel.border = element_rect(fill = NA,
          #                             color = "grey50"),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          panel.ontop = TRUE
    )+
    geofacet::facet_geo(vars(state_abbr))
  
  
  ggsave("health_geofacet_test.png",
         plot = last_plot(),
         width = 1600,
         height = 1200,
         units = "px",
         dpi = 350,
         bg = "white",
         scale = 4.5)
  