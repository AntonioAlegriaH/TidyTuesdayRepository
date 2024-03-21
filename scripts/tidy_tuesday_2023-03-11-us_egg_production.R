
# 1-exploring egg ugh -----------------------------------------------------
pacman::p_load(dplyr,
               tidyr,
               lubridate,
               ggplot2,
               scales,
               patchwork,
               ggfx)


eggproduction  <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/egg-production.csv')
cagefreepercentages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/cage-free-percentages.csv')

theme_set(theme_minimal(base_size = 15))
#theme_update(
#  plot.title.position = "plot",
#  axis.line.x = element_line(size = .2, colour = "grey50"),
#  axis.line.y = element_line(size = .2, colour = "grey50")
#)
# 

#palette: https://coolors.co/palette/20bf55-0b4f6c-01baef-fbfbff-757575


# Def
# hatching eggs: Hatching eggs are fertilized eggs used for the reproduction of chicken flocks, either layer hens (both table and
# hatching) or chickens raised for meat (“broilers”). Hens
# who produce hatching eggs are called “hatching egg type
# layers” (hereafter “hatching layers”). 
# 
# Table eggs: are all eggs sold to be used as food ingredients. Shell eggs are purchased still in their shells, while
# processed eggs are broken out of their shells and sold in a
# variety of processed forms including liquid, frozen, or
# dried/powdered. Hens who produce table eggs meant for
# consumption are known as “table egg type layers” (hereafter “table layers”). 
# 
# 

# 2.1 How many table and hatching eggs are being produced -----------------



total_hatch <- eggproduction %>% 
  filter(prod_type == "hatching eggs") %>% 
  summarise(sum_total_eggs = sum(n_eggs),
            sum_total_hens = sum(n_hens)
            ) %>% 
  pivot_longer(cols= starts_with("sum"), 
               names_to = "product",
               values_to = "total_sum_hatch") 


  

total_table <- eggproduction %>% 
  filter(prod_type == "table eggs",
         prod_process == "all") %>% 
  summarise(sum_total_eggs = sum(n_eggs),
            sum_total_hens = sum(n_hens)) %>% 
  pivot_longer(cols = starts_with("sum"),
               names_to = "product",
               values_to = "total_sum_table_eggs") 
  


joined_products <- left_join(total_hatch,
                             total_table,
                             by = "product") %>% 
  mutate(location_hatch = 1,
         location_table = 10)
  


# 2.2 Generate plot -------------------------------------------------------

colores <- c("#0b4f6c","#01baef")

p1 <- joined_products %>% 
  ggplot()+
  ggfx::with_shadow(
  ggfx::with_bloom(
  geom_point(aes(location_hatch, location_hatch,
                 size = total_sum_hatch,
                 color = product),
             show.legend = FALSE),
  sigma = 10,strength = 2)
  )+
  geom_point(aes(location_table, location_hatch,
                 size = total_sum_table_eggs,
                 color = product),
             show.legend = FALSE)+
 # annotate(geom = "text", 
 #          x= 1, y = 1,
 #          label = "Hens")+
  annotate(geom = "text",
           x = 1, y = 1, vjust = -12,
           label = "Eggs for Hatching",
           family = "Poppins")+
  scale_x_continuous(breaks = c(-1:1),
                     limits = c(0,2))+
  scale_y_continuous(limits = c(0.5,1.5))+
  scale_size(range = c(17,100))+
  scale_color_manual(values = colores)+
  coord_cartesian(clip = "off")+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.line.x.top = element_blank())
  

p1

# 2. How is Table eggs divided ?--------------------------------


table_eggs_class <- eggproduction %>% 
  filter(prod_type == "table eggs") %>% 
  group_by(prod_process) %>% 
  summarise(sum_total_eggs = sum(n_eggs),
            sum_total_hens = sum(n_hens)
            )
  

  pivot_table_eggs <- table_eggs_class %>% 
    pivot_longer(cols = starts_with("sum"),
                 names_to = "product"
                 ) %>% 
    mutate(location_x = c(2,2,.6,.6,.6,.6),
           location_y = c(0.1,0.1,5.6,5.6,-7.6,-7.6))

  
  
p2 <-   pivot_table_eggs %>% 
    ggplot()+
  ggfx::with_shadow(
    ggfx::with_bloom(
    geom_point(aes(location_y, location_x,
               size = value,
               color = product),
               show.legend = FALSE),
    sigma = 10,strength = 2)
  )+
 #annotate(geom = "text",
 #         x = -29.9,
 #         y = 2.1,
 #         size = 13.5, 
 #         family = "Anton",
 #         label = "US \nEGG \nPRODUCTION",
 #         hjust = -0)+
# scale_size(range = c((55642132131*100)/433924900000,100))+
    scale_x_continuous(breaks = c(-1:1),
                       limits = c(-30,10))+
    scale_y_continuous(limits = c(0,3))+
  scale_color_manual(values = colores)+
  scale_size(range = c(4.5,100))+
  coord_cartesian(clip = "off")+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank())
p2    
    





p2+inset_element(p1,
                 left = .3,
                 bottom = .4,
                 right = 0,
                 top = 0)



ggsave("plot_assamble_9.png",
       plot = last_plot(),
       scale = 2,
       width = 1200,
       height = 1200,
       dpi = 400,
       units = "px",
       bg = "white"
)



# Calculate proportional size ratios
#(64281100000*100)/433924900000
#14.81388
#(55642132131*100)/433924900000
#12.82299
#(19618735423*100)/433924900000
#4.521228
#



#### pd: Final Lettering were done in figma :)