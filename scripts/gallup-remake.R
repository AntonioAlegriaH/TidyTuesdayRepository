# Gallup Remake
# 1-import data -----------------------------------------------------------

pacman::p_load(dplyr,
               tidyr,
               ggplot2,
               scales,
               ggpol,
               tidytext,
               forcats,
               install = FALSE)

##Paste from a .pdf: Gallup International Association October(2024)
data_text <- "
Country Kamala_Harris Donald_Trump DK_NR
Global_Average 54 26 21
AUSTRIA 71 16 13
BELGIUM 66 19 16
BULGARIA 36 49 15
CROATIA 50 34 16
CZECH_REPUBLIC 43 37 20
DENMARK 85 4 10
ESTONIA 53 19 28
FINLAND 82 7 11
FRANCE 62 13 25
GEORGIA 22 43 35
GERMANY 73 13 14
GREECE 51 33 16
HUNGARY 30 49 21
INDONESIA 47 34 19
IRAN 43 19 38
IRAQ 38 40 22
IRELAND 71 14 15
ITALY 54 18 28
JAPAN 56 20 24
KAZAKHSTAN 17 44 39
SOUTH_KOREA 71 16 13
KOSOVO 55 27 17
LATVIA 46 21 33
LEBANON 57 36 6
LITHUANIA 70 14 15
MALAYSIA 60 33 7
MEXICO 72 16 12
MOLDOVA 28 36 36
NETHERLANDS 74 11 15
NORWAY 81 7 12
PAKISTAN 24 16 59
POLAND 56 25 20
PORTUGAL 70 19 11
ROMANIA 53 33 13
RUSSIAN_FEDERATION 12 43 45
SERBIA 29 59 12
SLOVAKIA_REPUBLIC 38 41 20
SLOVENIA 41 43 15
SPAIN 66 17 16
SWEDEN 81 9 10
SWITZERLAND 61 25 14
SYRIA 48 33 19
UNITED_KINGDOM 58 27 15
"

data <- read.table(text = data_text, header = TRUE)



# 2-plot ------------------------------------------------------------------

# Clean and reshape the data
data_clean <- data %>% 
  pivot_longer(cols = Kamala_Harris:DK_NR,
               names_to = "candidate",
               values_to = "percent_value") %>% 
  filter(candidate != "DK_NR") %>% 
  mutate(candidate = factor(candidate, levels = c("Kamala_Harris", "Donald_Trump"))) %>% 
  janitor::clean_names() %>% 
  mutate(country = stringr::str_to_title(country))



# Reorder countries by Kamala's approval in descending order
data_clean <- data_clean %>%
  group_by(country) %>%
  mutate(order_by_kamala = ifelse(candidate == "Kamala_Harris", percent_value, NA)) %>%
  fill(order_by_kamala, .direction = "downup") %>%
  ungroup() %>%
  mutate(country = fct_reorder(country, order_by_kamala, .desc = FALSE)) %>%
  select(-order_by_kamala) 


data_clean


library(ggpol)

bg_color <- "#f2ecec"
title_font <- "Palatino"

h1_color <- rgb(0,0,0,alpha =195, maxColorValue = 255)
h1_size <- 34

h2_color <- "#212529"
h2_color_alt <- rgb(0,0,0,alpha =180, maxColorValue = 255)
h2_size <- 18

h3_color <- rgb(108,117,125, alpha = 180, maxColorValue = 255)
h3_size <- 11

dem_color <- "#5280a8"
rep_color <- "#b05a60"


# define custom labels for the facets (labeller)
facet_labels <- c("Kamala_Harris" = "Kamala Harris", "Donald_Trump" = "Donald Trump")


ggplot(data_clean, 
       aes(country, percent_value* ifelse(candidate == "Kamala_Harris", -1, 1),
           fill = candidate)) + 
  geom_col(show.legend = FALSE,
           width = 1) +
  geom_text(aes(label = comma(percent_value, suffix = "%"),
                color = candidate,
                ),
            nudge_y = c(7,7),
            family = title_font,
            fontface= "bold",
            size = 3.5,
            alpha = .8
            )+
  scale_fill_manual(values = c("Kamala_Harris" = dem_color, "Donald_Trump" = rep_color)) +
  scale_color_manual(values = c("#f1f1f1", h2_color))+
  scale_y_continuous(
    breaks = c(-20, -40, -60, -80,0, 20, 40, 60, 80), # breakpoints
    labels = scales::label_percent(scale = 1), 
    sec.axis = dup_axis(name = NULL, labels = NULL)  # secondary axis for top ticks(first try)
  ) +
  facet_share(~candidate, dir = "h",scales = "free", 
              reverse_num = FALSE, 
              labeller = labeller(candidate = facet_labels)) + 
  coord_flip(clip = "off")+
  labs(y = NULL,
       title = "Europe Wants Kamala Harris",
       subtitle = "Response to the question: If you wereto vote in the\nAmerican election for president who would you vote for?",
       caption = "Source: Gallup International Association Survey(2024)\nRemake in R created by antonioalegria.io from the original data visualization."   )+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.title.y = element_text(size =30),
        axis.text.y = element_text(family = title_font,
                                   face = "bold",
                                   colour = rgb(0,0,0,alpha =180, maxColorValue = 255),
                                   size = h3_size+2,
                                   ),
        axis.text.x = element_text(vjust = 237, 
                                   family = title_font,
                                   face = "bold",
                                   size = h3_size,
                                   colour = h2_color_alt
                                   ),
        axis.ticks.x.bottom = element_blank(),
        axis.ticks.y = element_line(color = bg_color),
        axis.ticks.x.top = element_line(colour = h3_color),
        plot.margin = margin(30,30,20,-90,"pt"),
        plot.title.position = "panel",
        plot.title = element_text(family = title_font,
                                  face = "bold",
                                  color = h1_color,
                                  size = h1_size,
                                  ),
        plot.subtitle = element_text(margin = margin(0,0,60,0,"pt"),
                                     family = title_font,
                                     face = "bold",
                                     color = rgb(0,0,0,alpha =180, maxColorValue = 255),
                                     size = h2_size-1,
                                     ),
        plot.caption = element_text(size = h3_size,
                                    color = h3_color,
                                    family = title_font,
                                    face = "bold",
                                    hjust = 0,
                                    margin = margin(10,0,-10,0,"pt")),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(family = title_font,
                                  face = "bold",
                                  color = rgb(0,0,0,alpha =200, maxColorValue = 255),
                                  size = h3_size+2,
                                  margin = margin(-33,0,20,0,"pt")
                                  
                                  ),
        strip.background = element_blank()
         ) -> p


# datasets for geoms_text, instead of individual annotations

ann_text_1 <- data.frame(country = "Slovenia", 
                         percent_value = 41,
                         candidate = factor("Kamala_Harris",levels = c("Kamala_Harris","Donald_Trump")))
ann_text_2 <- data.frame(country = "Norway", 
                         percent_value = 7,
                         candidate = factor("Donald_Trump",levels = c("Kamala_Harris","Donald_Trump")))


  
  geom_text(data = ann_text_1,
              label = "Northen and\nCentral European
countries such as
Denmark, Finland, 
Norway and 
Sweden show the 
highest approval 
for Kamala Harris",
              nudge_x = -5,
              nudge_y = -40,
              size = 3.5,
              family = title_font,
              fontface = "bold",
              color = h2_color_alt, 
              alpha = .8,
              hjust = 0
              ) -> p_ann_1
  
  geom_text(data = ann_text_2,
              label = "Besides Serbia\nHungary and
Bulgaria have the
highest approval
for Trump among
EU members, with 
nearly half of 
respondents 
voicing their 
support for him",
              size = 3.5,
              nudge_x = -5,
              nudge_y = 60,
              family = title_font,
              fontface = "bold",
              color = h2_color_alt, 
              alpha = .8,
              hjust = 1
              ) -> p_ann_2


  
p + p_ann_1 + p_ann_2 -> p_final
     

ggsave(
  plot = p_final,
  filename = "challenge_1.png",
  width = 610,
  height = 920,
  units = "px",
  scale = 4,
  bg = bg_color
)
