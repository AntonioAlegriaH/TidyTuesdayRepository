# Libraries

pacman::p_load(dplyr,
               tidyr,
               ggplot2,
               scales,
               stringr,
               ggfx,
               install = FALSE
)

# Data:
bob_ross <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv')

# presets

theme_set(theme_minimal(base_size = 15))
theme_update(
  plot.title.position = "panel",
  axis.line.x = element_line(size = .2, colour = "grey90"),
  axis.line.y = element_line(size = .5, colour = "grey90"),
  panel.grid = element_blank()
  )


# Transform 
bob_ross_color_count <-       
  bob_ross %>% 
  group_by(season) %>% 
  transmute(across(Black_Gesso:Alizarin_Crimson, 
                   ~ case_when(. == "TRUE" ~ 1,
                               . == "FALSE" ~ 0))
  )


sum_colors_season <- bob_ross_color_count %>% 
  summarise(across(Black_Gesso:Alizarin_Crimson, sum)) %>% 
  pivot_longer(names_to = "color",
               cols = Black_Gesso:Alizarin_Crimson) %>% 
  mutate(colors_renamed = case_when(
    color == "Alizarin_Crimson" ~ "Alizarin Crimson",
    color == "Black_Gesso" ~ "Black Gesso",
    color == "Bright_Red" ~ "Bright Red",
    color == "Burnt_Umber" ~ "Burnt Umber",
    color == "Cadmium_Yellow" ~ "Cadmium Yellow",
    color == "Dark_Sienna" ~ "Dark Sienna",
    color == "Indian_Red" ~ "Indian Red",
    color == "Indian_Yellow" ~ "Indian Yellow",
    color == "Liquid_Black" ~ "Liquid Black",
    color == "Liquid_Clear" ~ "Liquid Clear",
    color == "Midnight_Black" ~ "Midnight Black",
    color == "Phthalo_Blue" ~ "Phthalo Blue",
    color == "Phthalo_Green" ~ "Phthalo Green",
    color == "Prussian_Blue" ~ "Prussian Blue",
    color == "Sap_Green" ~ "Sap Green",
    color == "Titanium_White" ~ "Titanium White",
    color == "Van_Dyke_Brown" ~ "Van Dyke Brown",
    color == "Yellow_Ochre" ~ "Yellow Ochre"))


# extract colors  
bob_ross_colors <- bob_ross %>% 
  select(colors, color_hex) %>% 
  str_split_fixed(pattern = ",", n = Inf) %>% 
  t %>% 
  as_tibble() 

# Palette
pal_colors <-  c("#4E1500", #Alizarin_Crimson
                 "#000000", #Black_Gesso
                 "#DB0000", #Bright_Red
                 "#8A3324", #Burnt_Umber
                 "#FFEC00", #Cadmium_Yellow
                 "#5F2E1F", #Dark_Sienna
                 "#CD5C5C", #Indian_Red
                 "#FFB800", #Indian_Yellow
                 "#000000", #Liquid_Black
                 "#FFFFFF", #Liquid_Clear
                 "#000000", #Midnight_Black
                 "#0C0040", #Phthalo_Blue
                 "#102E3C", #Phthalo_Green
                 "#021E44", #Prussian_Blue
                 "#0A3410", #Sap_Green
                 "#FFFFFF", #Titanium_White
                 "#221B15", #Van_Dyke_Brown
                 "#C79B00" #Yellow_Ochre
)

# plot 
p <- sum_colors_season %>% 
  ggplot(aes(season, value, 
             fill = colors_renamed))+
  ggfx::with_shadow(
    ggfx::with_bloom(
      ggstream::geom_stream(type = "proportional",
                            bw = 0.5,
                            color = "#444444",
                            alpha = .9),
      strength = 1
    )
  )+
  ggfx::with_shadow(
    ggstream::geom_stream_label(aes(label = colors_renamed),
                                type = "proportional",
                                color = "white",
                                extra_span = .0001,
                                size = 2, family = "Avenir Next Condensed"),
    colour = "black", 
    x_offset = 0, 
    y_offset = 0,
    sigma = 1
  )+
  scale_fill_manual(values = pal_colors)+
  scale_y_continuous(labels = percent_format())+
  labs(title = "18 colors used by Bob Ross",
       subtitle = "",
       caption = "Data:{BobRossColors} by @frankiethull
         Dataviz: antonioalegria.io | @elmedicobrujo",
       x = "Season",
       y = "")+
  theme(plot.title = element_text(size = 58,
                                  family = "RightBankFLF",
                                  color = "#333333"),
        panel.border = element_rect(colour = "grey80", 
                                    fill=NA, linewidth=1),
        panel.background = element_rect(fill = "#73956F"),
        plot.caption = element_text(color = "#444444"),
        text = element_text(family = "Avenir Next Condensed",
                            color = "grey50"),
        axis.text = element_text(color = "grey60"),
        legend.position = "none",
        plot.margin = margin(2,2,2,1, "cm"))


# green  
panel.background = element_rect(fill = "#519872") 

# blue/grey  
panel.background = element_rect(fill = "#6D8A96") 

# silver lake blue
panel.background = element_rect(fill = "#748CAB") 

# asparagus
panel.background = element_rect(fill = "#73956F") 




ggsave("2023-02-24-bob-ross-colors.png",
       plot = p,
       width = 1000,
       height = 1200,
       units = "px",
       bg = "#f1f1f1",
       dpi = 300,
       scale = 3
)
