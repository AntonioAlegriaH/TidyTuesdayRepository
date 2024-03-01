
# 1-libraries and data ----------------------------------------------------

pacman::p_load(dplyr,
               tidyr,
               ggplot2,
               scales,
               glue,
               ggfx,
               ggtext,
               install = FALSE)



births <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/births.csv')
deaths <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/deaths.csv')



theme_set(theme_minimal(base_size = 15))
theme_update(
  plot.title.position = "plot",
  axis.line.x = element_line(linewidth = .2, colour = "grey50"),
  axis.line.y = element_line(linewidth = .2, colour = "grey50"),
  panel.grid = element_blank()
)


# 2- quick summary --------------------------------------------------------


glimpse(births)
glimpse(deaths)

# nothing fancy

births_count <-births %>% 
  count(year_birth) %>% 
  pivot_longer(!n, names_to = "event", values_to = "year") %>% 
  select(year,
         event, 
         n)

deaths_count <- deaths %>% 
  count(year_death) %>% 
  summarise(year_death,
            n = n*-1) %>% 
  pivot_longer(!n, names_to = "event", values_to = "year") %>% 
  select(year,
         event,
         n)



df_final <- 
  full_join(births_count,
            deaths_count, by = NULL)

df_final %>% 
  count(event, wt = n)



# 3- dataviz -----------------------------------------------------------------

# hex codes + fonts
year_label_color = "#353535"
shadow_color_bold = "#333333"
shadow_color = "#666666"
shadow_color_light = "#999999"
line_color = "grey60"
line_color_R = "#317ec1"
death_color = "#e63946"
birth_color = "#1d3557"
bg_plot = "#F4F4F0"
year_font = "Barlow Condensed"
caption_font = "Roboto"
text_font = "Barlow"

# text and labels

label_r_birth <- "R version 1.0 \nwas released on \n29-Feb-2000."

label_birth <-"During leap years, births outnumber deaths by 121. \nNotable figures born on such days include \nPedro Sanchez, Spain's prime minister, and \nJessie T. Usher, who portrays A-Train in The Boys."
label_death <-"Conversely, leap years mark the passing of 62 \nhistorical figures. Among them is Sheldon Moldoff, \nco-creator of Poison Ivy, Mr. Freeze, Bat-Girl, and Batwoman."

# plot 

plot <- df_final %>% 
  ggplot(aes(year, n,
             fill = event))+
  geom_col(show.legend = FALSE,
           alpha = .9)+
  annotate(geom = "segment",
           x = (min(df_final$year)-100), 
           xend = (max(df_final$year)+100), 
           y = 0, 
           yend = 0,
           color = line_color
          )+
  with_shadow(
    annotate(geom = "text", #titles
             label = "Births",
             x = 750, y = 6, 
             size = 30,
             family = year_font,
             color = birth_color,
             alpha = .8,hjust = .58
             ),
    colour = shadow_color,
    x_offset = 1,
    y_offset = 4,
    sigma = 4
    )+
  with_shadow(
    annotate(geom = "text",
             label = "Deaths",
             x = 750, y = -4, 
             size = 30, angle = 360,
             family = year_font,
             color = death_color,
             alpha = .7, hjust = .5
             ),
    color = shadow_color,
    x_offset = 1,
    y_offset = 4,
    sigma = 2
    )+
  with_shadow(
    annotate(geom = "text",  #text info
             label = label_birth,
             x = 450 , y = 3.5,
             family = text_font,
             color = birth_color,
             alpha = .8,hjust = "left",
             fontface = "bold",
             size = 3
    ),
    colour = shadow_color_light,
    x_offset = 1,
    y_offset = 2,
    sigma = 4
    )+
  with_shadow(
    annotate(geom = "text",  #text info
             label = label_death,
             x = 1100 , y = -4.5,
             family = text_font,
             color = death_color,
             alpha = .8,hjust = "left",
             fontface = "bold",
             size = 3
             ),
    colour = shadow_color_light,
    x_offset = 1,
    y_offset = 2,
    sigma = 2
  )+
  annotate(            #R 1.0 release
    geom = "segment",
    x = 2000,
    xend = 2000,
    y = Inf,
    yend = 0,
    color = line_color_R
  )+
  annotate(
    geom = "text",
    x = 2009,
    y = 8,
    label = label_r_birth,
    family = year_font,
    fontface = "bold",
    hjust = "left",
    color = line_color_R,
    size = 2.5
  )+
  geom_text(data = df_final %>% 
              filter(event == "year_death"), 
              aes(label = glue("{n*-1} in\n{year}")),
            size = 1,nudge_y = -.5,
            color = shadow_color,
            family = year_font,
            fontface = "bold",
            check_overlap = TRUE,
            show.legend = FALSE)+
  geom_text(data = df_final %>% 
            filter(event == "year_birth"), 
              aes(label = glue("{n} in\n{year}")),
            size = 1,nudge_y = .5,
            check_overlap = TRUE,
            color = shadow_color,
            family = year_font,
            fontface = "bold",
            show.legend = FALSE)+
  labs(
    title = "Leap year:\nHistorical Births and Deaths",
    caption = "Data: Wikipedia February 29 | #TidyTuesday | Dataviz: antonioalegria.io | tw: @elmedicobrujo | @antonioalegria.bsky.social") +
  coord_cartesian(expand = FALSE,
                  clip = "off")+
  scale_x_continuous(breaks = unique(df_final$year))+
  scale_fill_manual(values = c(birth_color, death_color))+
  scale_color_manual(values = c(birth_color, death_color))+
  theme(axis.text.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y =  element_blank(),
        axis.line.x  = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_blank(),
        #plot.title = ggfx::with_shadow(
        #  element_text(hjust = .5,
        #               family = "Barlow",
        #               colour = shadow_color_bold,
        #               size = 14,
        #               margin = margin(0,0,1,0,"cm")
        #               ),
        #  colour = shadow_color_light,
        #  x_offset = 0, 
        #  y_offset = 1,
        #  sigma = 1
        #  ),
        plot.caption = element_text(family = year_font,
                                    size = 6,
                                    face = "bold",
                                    color = shadow_color_light,
                                    margin = margin(1,0,0,0, "cm")),
        plot.margin = margin(.5,1,1,.5,"cm"))


ggsave(
  plot = plot,
  filename = "death-births_2.png",
  width = 1200,
  height = 900,
  units = "px",
  scale = 2,
  bg = bg_plot,
  dpi = 300
  )


