# 1- import libraries -----------------------------------------------------

pacman::p_load(dplyr,
               tidyr,
               ggplot2,
               forcats,
               ggfx,
               scales,
               install = FALSE
)

theme_set(theme_minimal(base_size = 15))
theme_update(
  plot.title.position = "panel",
  axis.line.x = element_line(linewidth = .2, colour = "grey50"),
  axis.line.y = element_line(linewidth = .2, colour = "grey50"),
  panel.grid = element_blank()
)

# https://coolors.co/eff7fb-277da1-4d908e-43aa8b-90be6d-f9c74f-f8961e-f3722c-f94144-fa5255

# 2- import data Global Human Day -----------------------------------------

# Avg time by category and country
all_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/all_countries.csv')

# 3- hours x day x country ---------------------------------------------

glimpse(all_countries)
unique(all_countries$country_iso3)


palette <- c(
  "#277DA1",  #
  "#4D908E",  #
  "#43AA8B",  #
  "#90BE6D",  #
  "#F9C74F",  #
  "#F8961E",  #
  "#F3722C",  #
  "#F94144"   #
)

annotation_plot_title <- "THE HUMAN CHRONOME PROJECT"

annotation_plot <- 
  "The daily activities of approximately 
8 billion people, constrained by the 24-hour day, 
form the basis of global human behavior. 
This project estimates how people worldwide 
allocate their time using a versatile categorization system, 
integrating data from diverse sources."


p <- all_countries %>% 
  filter(country_iso3 == "JPN",  #Just change this iso3 code for any other country
         Category != "Nonfood provision") %>% 
  reframe(
    hoursPerDayCombined_total = sum(hoursPerDayCombined),
    country_iso3,
    population,
    .by = Category
  ) %>% 
  distinct() %>% 
  ggplot(aes(hoursPerDayCombined_total, country_iso3, 
             fill = Category,
             color = Category),
  )+
  ggfx::with_bloom(
    geom_col(
      alpha = .6,
      position = "stack",
      width = 1
    ),strength = 1
  )+
  geom_text(aes(label = Category),
            position = position_stack(vjust = 0.5),
            angle = 90,
            family = "bold",
            size = 2.7
  )+
  geom_text(aes(label = hoursPerDayCombined_total),
            nudge_y = -.7,
            nudge_x = 5, alpha = 0)+
  geom_text(aes(label = glue::glue("{hoursPerDayCombined_total} hrs") ),
            position = position_stack(vjust = 0.5), 
            vjust = 17,
            size = 2,
  )+
  annotate(geom = "text",
           label = annotation_plot_title,
           hjust = 0,
           vjust = -22, 
           fontface = "bold",
           color = "#EFF7FB",
           x = 0,
           y = 1.3,
           size = 2.3, 
  )+
  annotate(geom = "text",
           label = annotation_plot,
           hjust = 0,
           vjust = -.6,
           color = "#EFF7FB",
           x = 0,
           y = 1.4,
           size = 2.3, 
  )+
  scale_x_continuous(breaks = c(0,8, 16, 24))+
  labs(title = "A DAY IN A LIFE IN: JAPAN",
       subtitle = "",
       x = "Hours Per Day Combined",
       y = "",
       caption = "Data: The global human day | doi.org/10.1073/pnas.2219564120 | #TidyTuesday
       Dataviz: antonioalegria.io | @elmedicobrujo " 
  )+
  scale_fill_manual(values = palette)+
  scale_color_manual(values = palette)+
  theme(title = element_text(size = 65, 
                             
  ),
  plot.title = element_text(margin = margin(0,0,30,0, "pt"),
                            hjust = .5,
                            family = "Gloucester MT Extra Condensed"),
  #plot.subtitle = element_text(margin = margin(0,0,60,0, "pt"),
  #                             size = 30),
  plot.caption = element_text(size = 7,
                              color = "#F5FAFC",
                              margin = margin(-15,0,0,0,"pt")
  ),
  
  axis.line.y = element_blank(),
  axis.line.x = element_line(),
  axis.text.y = element_blank(),
  axis.text.x = element_blank(),
  axis.title.x = element_text(size = 13,
                              hjust = 0,
                              family = "Gloucester MT Extra Condensed",
                              margin = margin(20,0,0,0, "pt")),
  legend.position = "none",
  text = element_text(colour = "#EFF7FB"),
  plot.margin = margin(3,4,5,2, "cm")
  )+
  coord_cartesian(clip = "off")


ggsave(filename = "hours_life_2.png",
       plot = p,
       width = 1200,
       height = 800,
       units = "px",
       scale = 4,
       dpi = 400,device = "png",
       bg = "#0C2631")
