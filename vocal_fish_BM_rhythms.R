# Codes for: 
# "Rhythmic properties of Sciaena umbra calls across space and time in the Mediterranean Sea"
# Marta Picciulin, Marta Bolgan, Lara S. Burchardt
# accepted in: PLOS ONE

# author of this script: Lara S. Burchardt


############

# 00: load packages ----

if (!require(install.load)) {
  install.packages("install.load")
}

library(install.load)

install_load("tidyverse","psych","vegan","lsr", "plm", "corrplot", "lme4", "multilevelTools",
             "JWileymisc", "jtools", "Hmisc", "cowplot", "mgcv")

# 01: data ----

# rhythms were calculated with the RANTO app: https://github.com/LSBurchardt/R_app_rhythm/tree/master/RhythmAnalysis 
# for that raw ioi data need to be seperated into individual files per sequence, see RANTO manual for details

BM_geo_data$read_delim('rhythm_results_RANTO_BM.csv', delim = ";")

BM_geo_data$location <- factor(BM_geo_data$location, levels = c("Mallorca", "Venice","Trieste_09", "Trieste_21", "Crete"))

BM_raw_ioi <- read_delim("raw_data_ioi_per_seq_location.csv", delim = ";")

BM_raw_ioi$Location <- factor(BM_raw_ioi$Location, levels = c("Mallorca", "Venice", "Trieste", "Crete"))

# 02: plotting ----


# map plot of study region, open access data 

adriatic.coastline <- ggplot2::map_data('world')
land.colour   <- "grey75"
border.colour <- "grey10"
basemap= adriatic.coastline
xmin= 0
xmax= 27
ymin=35
ymax=46

map <- ggplot() +
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
  ylab("Latitute [°]")+
  xlab("Longitude [°]")

ggsave("map_adriatic_coastline.jpg", dpi = 300)

# IOI Beat across locations

box_ioibeat <- BM_geo_data %>% 
  group_by(location) %>% 
  ggplot(aes(x = location, y = ioi_beat))+
  geom_boxplot(aes(fill = location), alpha = 0.5)+
  #geom_point(aes(color = as.factor(year), alpha = 0.7, size = 2))+
  geom_jitter(aes(alpha = 0.7, size = 0.5), width = 0.1)+
  #scale_color_brewer(palette = "Set2")+
  theme_minimal()+
  labs(x = "Location",
       y = "IOI Beat [Hz]")+
  scale_x_discrete(labels = c( "Mallorca", "Venice", "Trieste","Crete"))+
  theme_minimal()+
  theme(axis.text = element_text(size = 24),
        axis.title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text.x = element_text(size = 28),
        legend.position = "none")+
  annotate("text", x="Venice", y= 0.200, label= "***", size = 10)


# ioi cv 
box_cv <- BM_geo_data %>% 
  group_by(location) %>% 
  ggplot(aes(x = location, y = unbiased_cv))+
  geom_boxplot(aes(fill = location), alpha = 0.5)+
  #geom_point(aes(color = as.factor(year), alpha = 0.7, size = 2))+
  geom_jitter(aes(alpha = 0.7, size = 0.5), width = 0.1)+
  #scale_color_brewer(palette = "Set2")+
  theme_minimal()+
  labs(x = "Location",
       y = "Coefficient of Variation")+
  scale_x_discrete(labels = c("Mallorca", "Venice", "Trieste","Crete"))+
  theme(legend.position = "none")+
  theme(axis.text = element_text(size = 24),
        axis.title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text.x = element_text(size = 28)) 

# ioi histogram 
hist_ioi <- BM_raw_ioi %>% 
  filter(year>2010) %>% 
  ggplot(aes(x= IOI))+
  geom_histogram(aes(y=stat((count)/sum(stat(count))*100)),
                 color = "white", fill = "darkblue", binwidth = 0.25)+
  facet_grid(~ Location)+
  xlab("IOI [sec]")+
  ylab(" Percentage [%]")+
  theme_minimal()+
  theme(axis.text = element_text(size = 24),
        axis.title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text.x = element_text(size = 28))

# plot grid
cowplot::plot_grid(hist_ioi, box_cv, box_ioibeat, ncol = 1, nrow = 3, rel_heights = c(0.3,0.3,0.3),
                   labels= c("A", "B", "C"), label_size = 24, align = "h")

ggsave("main_fig_fish_rhythm.jpg", dpi = 300, width = 10, height = 18)


# temperature vs ioi beat

BM_geo_data %>%
  ggplot(aes(x= temperature))+
  #ggplot(aes(x= temperature))+
  geom_jitter(aes(y= ioi_beat,  fill = location, color = location, ),alpha = 0.8,width = 0.4, size = 4)+
  #geom_smooth(method='loess', aes(y = ioi_beat))+
  #geom_smooth(aes(y = ioi_beat))+
  theme_minimal()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))+
  scale_fill_discrete(name = "Location", labels = c("Mallorca","Venice","Trieste", "Crete"))+
  scale_color_discrete(name = "Location", labels = c("Mallorca","Venice","Trieste", "Crete"))+
  xlab("Water Temperature [°C]")+
  ylab(" IOI Beat [Hz]")

ggsave("IOI_beat_Temperature_fish_figure5.jpg", dpi = 300, width = 10, height = 8)


# IOI Beat per Date (pooled over years)

# ioi beat per recording months

BM_geo_data %>%
  #mutate(date = as.Date(paste(month, `day `))) %>% 
  group_by(month) %>% 
  ggplot(aes(y= ioi_beat, x= month_day))+
  #geom_boxplot(aes(group = month_day))+
  geom_point(aes(fill = location, color = location), size = 5)+
  #facet_grid(~ month)+
  theme_minimal()+
  theme(axis.text = element_text(size = 24),
        axis.title = element_text(size = 26),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text.x = element_text(size = 28),
        axis.text.x=element_text(angle=90, size = 20))+
  scale_fill_discrete(name = "Location", labels = c("Mallorca","Venice","Trieste", "Crete"))+
  scale_color_discrete(name = "Location", labels = c("Mallorca","Venice","Trieste", "Crete"))+
  xlab("Date")+
  ylab(" IOI Beat [Hz]")#+

ggsave("IOI_beat_date_fish_figure3.jpg", dpi = 300, width = 12, height = 8)

#04: GAMS modelling ----

# needed package: "mgcv"


cor_data <- your_dataset %>% 
  select(month, day, acoustic_diversity, temperature, vess_den_trade, vess_den_fishing,
         vess_den_passenger, vess_den_other, vess_den_tot, vess_den_big, vess_den_pleasure )
cor_gam <-rcorr(as.matrix(cor_data))
# we don't include the highly correlated vessel densities: other, big, passenger and pleasure

gam_model_1 <- gam(ioi_beat ~ s(day) + acoustic_diversity + s(vess_den_trade, k = 9) +
                     s(vess_den_fishing, k = 7) + s(vess_den_tot) + s(temperature) + s(time_h, k = 4) +
                     month + s(day, by = month), data = your_dataset)

gam_model_2 <- gam(ioi_beat ~ s(day) + acoustic_diversity +s(vess_den_trade, k = 9) +
                     s(vess_den_fishing, k = 7) + s(vess_den_tot) + s(temperature) + 
                     month+ s(day, by = month), data = your_dataset)

gam_model_3 <- gam(ioi_beat ~ s(day) + acoustic_diversity + s(vess_den_trade, k = 9) +  s(vess_den_fishing, k = 7) + s(vess_den_tot) + s(temperature)  +
                     s(time_h, k = 4) + s(day, by = month),
                   data = your_dataset)

gam_model_4 <- gam(ioi_beat ~ s(day) + acoustic_diversity + s(vess_den_trade, k = 9) +  s(vess_den_fishing, k = 7) + s(vess_den_tot) + s(temperature)  +
                     s(day, by = month),
                   data = your_dataset)

gam_model_5 <- gam(ioi_beat ~ s(day) + acoustic_diversity + s(vess_den_trade, k = 9) +  s(vess_den_fishing, k = 7) + s(temperature)  +
                     s(day, by = month),
                   data = your_dataset)

# summary and aic: change input according to which model to summarize ("gam_model_x")
summary(gam_model_1)
aic_value <- AIC(gam_model_1)


# Predictions from the GAM model
predictions <- predict(gam_model_1, newdata = your_dataset)

ggplot(data= your_dataset, aes(x = ioi_beat, y = predictions))+
  geom_point()+
  theme_minimal()+
  ylim(c(0.15,0.7))+
  xlim(c(0.15,0.7))+
  ylab("Predicted Values for IOI Beat [Hz]")+
  xlab("Original Values for IOI Beat [Hz]")+
  theme(text = element_text(size=16))

ggsave("GAMs_predictions_ioi_beat.jpg", dpi = 300, height = 6, width = 6)  

ggplot(data= your_dataset, aes(y = ioi_beat, x = vess_den_fishing))+
  geom_point()+
  theme_minimal()+
  #ylim(c(0.15,0.7))+
  #xlim(c(0.15,0.7))+
  ylab("IOI Beat [Hz]")+
  xlab("Vessel Density Fishing [h/m²/month]")+
  theme(text = element_text(size=16))

ggsave("ioi_beat_vess_den_fishing.jpg", dpi = 300, height = 6, width = 6)

ggplot(data= your_dataset, aes(y = ioi_beat, x = vess_den_trade))+
  geom_point()+
  theme_minimal()+
  #ylim(c(0.15,0.7))+
  #xlim(c(0.15,0.7))+
  ylab("IOI Beat [Hz]")+
  xlab("Vessel Density Trade [h/m²/month]")+
  theme(text = element_text(size=16))

ggsave("ioi_beat_vess_den_trade.jpg", dpi = 300, height = 6, width = 6)

ggplot(data= your_dataset, aes(y = ioi_beat, x = acoustic_diversity))+
  geom_point()+
  theme_minimal()+
  #ylim(c(0.15,0.7))+
  #xlim(c(0.15,0.7))+
  ylab("IOI Beat [Hz]")+
  xlab("Acoustic Diversity [#n species]")+
  theme(text = element_text(size=16))


