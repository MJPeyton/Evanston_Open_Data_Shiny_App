## Fun but not included

## Income vs Poverty

distance_table %>%
  ggplot(aes(Median_Income, Poverty_Rate)) +
  geom_point(alpha = .05) +
  geom_point(data=top, color="red", size = 2) +
  geom_point(data=evanston, color="#4F2984", size = 2) +
  geom_label_repel(data = top, aes(label=Place),hjust=0, vjust=0) +
  scale_y_log10()

## Population vs Median Income

distance_table %>%
  ggplot(aes(Median_Income, Total_population)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_gradient(low="pink", high="blue") +
  geom_point(data=top, color="red", size = 2) +
  geom_point(data=evanston, color="#4F2984", size = 2) +
  scale_y_log10() +
  coord_cartesian(xlim=c(0, 100000)) +
  theme(legend.position = "none")

# distance_table %>%
#   ggplot(aes(Median_Income, Total_population)) +
#   stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white") +
#   scale_fill_distiller(palette= "PuRd", direction=1) +
#   geom_point(data=top, color="red", size = 2) +
#   geom_point(data=evanston, color="#4F2984", size = 2) +
#   scale_y_log10()

## Poverty vs HS Education

distance_table %>%
  ggplot(aes(Per_HS, Poverty_Rate)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_gradient(low="pink", high="blue") +
  geom_point(data=top, color="red", size = 2) +
  geom_point(data=evanston, color="#4F2984", size = 2) +
  scale_y_log10() +
  coord_cartesian(xlim=c(0, .4)) +
  theme(legend.position = "none")

ggsave("poverty_raster.svg", device = "svg", width = 5, height = 3, units = "in", dpi = 300)