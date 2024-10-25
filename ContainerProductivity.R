#Testing A2CARES dataset for generating the container productivity figure

library(ggplot2)
library(ggtext)
library(tidyverse)
library(dplyr)
library(readxl)
library(esquisse)
library(GWalkR)
library(gapminder)
library(patchwork)
df1 <- read_excel("DataContainers.xlsx")
View(df1)

df1$Perc_Larvae <- df1$Perc_Larvae*100
df1$Perc_Puapae <- df1$Perc_Puapae*100

df1.1 <- df1 |>
  filter(Year==2022)

df1.2 <- df1 |>
  filter(Year==2023)


p1 <- ggplot(df1.1, aes(fill = Con_Type, y = Perc_Puapae, x= Season)) + 
      facet_wrap(~Community) +
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_viridis_d(labels = c('Barrels','Buckets', 'Wash Basin', 'Not Useful', 'Useful', 'Tires'), direction = -1) + 
      scale_y_continuous(labels = scales::percent) +
      theme_classic() +
      theme(legend.position = 'none') + 
      labs(fill = "Container Type",
         y = "",
         x = "2022") +
  coord_flip()
p1

p2 <- ggplot(df1.2, aes(fill = Con_Type, y = Perc_Puapae, x= Season)) + 
  facet_wrap(~Community) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_viridis_d(labels = c('Barrels','Buckets', 'Wash Basin', 'Not Useful', 'Useful', 'Tires'), direction = -1) + 
  scale_y_continuous(labels = scales::percent) +
  theme_classic() + 
  labs(fill = "Container Type",
         y = "Percentage of Pupae",
         x = "2023")  +
  coord_flip()

Fig1 <- p1/p2

ggsave("Fig1.svg",
       plot = Fig1,
       width = 10, 
       height = 6, 
       dpi = 300, 
       units = "in", 
       bg = "white")

p3 <- ggplot(df1.1, aes(fill = Con_Type, y = Perc_Larvae, x= Season)) + 
  facet_wrap(~Community) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_viridis_d(labels = c('Barrels','Buckets', 'Wash Basin', 'Not Useful', 'Useful', 'Tires'), direction = -1) + 
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(legend.position = 'none') + 
  labs(fill = "Container Type",
       y = "",
       x = "2022") +
  coord_flip()
p3

p4 <- ggplot(df1.2, aes(fill = Con_Type, y = Perc_Larvae, x= Season)) + 
  facet_wrap(~Community) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_viridis_d(labels = c('Barrels','Buckets', 'Wash Basin', 'Not Useful', 'Useful', 'Tires'), direction = -1) + 
  scale_y_continuous(labels = scales::percent) +
  theme_classic() + 
  theme(legend.position = 'none') +
  labs(fill = "Container Type",
       y = "Percentage of Larvae",
       x = "2023")  +
  coord_flip()

Fig1 <- p3/p4/p1/p2

Fig1

ggsave("Fig1.svg",
       plot = Fig1,
       width = 15, 
       height = 15, 
       dpi = 300, 
       units = "cm", 
       bg = "white")



#TESTING
p1 <- ggplot(df1, aes(fill = Con_Type, y = P_Pupae, x= Season)) + 
  facet_grid(Community~Year) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_viridis_d(labels = c('Barrels','Buckets', 'Wash Basin', 'Not Useful', 'Useful', 'Tires'), direction = -1) + 
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(strip.text.x = element_text(face = "bold", color = "black", hjust = 0.5, size = 13),
        strip.text.y = element_text(face = "bold", color = "black", hjust = 0.5, size = 13),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = 'none') + 
  labs(fill = "Container Type",
       y = "Percentage of container with pupae",
       x = "Season") +
  coord_flip()
p1

p2 <- ggplot(df1, aes(fill = Con_Type, y = Perc_Puapae, x= Season)) + 
  facet_grid(Community~Year) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_viridis_d(labels = c('Barrels','Buckets', 'Wash Basin', 'Not Useful', 'Useful', 'Tires'), direction = -1) + 
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(strip.text.x = element_text(face = "bold", color = "black", hjust = 0.5, size = 13),
        strip.text.y = element_text(face = "bold", color = "black", hjust = 0.5, size = 13),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = 'bottom') + 
  labs(fill = "Container Type",
       y = "Productivity of pupae per container",
       x = "Season") +
  coord_flip()

Fig1 <- p1/p2

Fig1

ggsave("Fig1.svg",
       plot = Fig1,
       width = 21, 
       height = 15, 
       dpi = 300, 
       units = "cm", 
       bg = "white")

df_summary <- df1 %>%
  group_by(Community, Con_Type) %>%
  summarise(sd_variable = sd(Perc_Pupae, na.rm = TRUE))

df_summary <- df1 %>%
  group_by(Community, Con_Type, Season) %>%
  summarise(mean_variable = mean(Perc_Puapae, na.rm = TRUE), .groups = 'drop')

print(df_summary)

