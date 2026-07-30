
########################################
#main figurs of PNG manuscript
########################################


#Download packages
packages_needed = c("writexl","sf","tmap","ggspatial","ggrepel","malariaAtlas","ggpubr","tseries","forecast", "leaps", "dplyr","gsubfn", "xlsx", "openxlsx", "readxl", "tidyverse", "ggplot2", "cowplot", "tidyr", "reshape")
lapply(packages_needed, require, character.only = TRUE)

setwd("C:\\Users\\thawsu\\Swiss Tropical and Public Health Institute, Swiss TPH\\PNG Collaborations - PNG SNT papers\\Paper 1 - SNT stratification & eligibility")

#Shapefiles
shapefile_districtmatch <- read_excel("Codes_data\\Data\\Shapefiles\\png_adm2/shapefile_districtmatch.xlsx")
shapefile_provincematch <- read_excel("Codes_data\\Data\\Shapefiles\\PNG_adm1/shapefile_provincematch.xlsx")

PNG_shapefile_admin1 <- st_read(file.path("Codes_data\\Data\\Shapefiles\\PNG_adm1\\png_admbnda_adm1_20180419.shp"))
PNG_shapefile_admin2 <- st_read(file.path("Codes_data\\Data\\Shapefiles\\png_adm2\\png_admbnda_adm2_20180419.shp"))


#import data
PNG_Stratification <- read_excel("Codes_data\\Data\\PNG_Stratification_updated_07_2026.xlsx", 
                                 sheet = "District")

##########################################
#plot maps
#############################################
admin2_merged <-  PNG_shapefile_admin2 %>%
  left_join( shapefile_districtmatch, by = "ADM2_EN") 

admin2_merged <-   admin2_merged %>%
  left_join( PNG_Stratification, by = "district")  # Use the correct column names that match

######################
#stratification map
#####################
setwd("C:\\Users\\thawsu\\Swiss Tropical and Public Health Institute, Swiss TPH\\PNG Collaborations - PNG SNT papers\\Paper 1 - SNT stratification & eligibility\\Codes_data\\Results")

#Strata2025 map
custom_colors <- c("Pre-elimination" = "darkblue",
                   "Very low" = "darkgreen", 
                   "Low" = "lightgreen", 
                   "Moderate" = "orange", 
                   "High" = "darkred")

admin2_merged<-admin2_merged%>%
  mutate(Strata = factor(
    Strata,
    levels = c("Pre-elimination","Very low", "Low", "Moderate", "High"),
    ordered = TRUE
  ))

plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill = Strata), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "Strata") +
  labs(title = "Strata 2025",
       #subtitle = "Categorized Incidence Rates",
       caption = "Source: eNHIS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )


print(plot)
ggsave("Strata2025.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")

#elimination areas
custom_colors <- c("<1" = "#00441b", 
                   "1-<5" = "#33a02c", 
                   "5-<10" = "#b2df8a")



plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill = Elimination_cutoffs), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "Incidence") +
  labs(title = "Elimination areas")+
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )


print(plot)
ggsave("Elimination.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")

#moderate high
admin2_plot <- admin2_merged %>%
  filter(Strata %in% c( "Moderate", "High")) %>%
  mutate(
    Strata = factor(Strata,
                    levels = c("Moderate", "High"))
  )

custom_colors <- c(
  "Moderate" = "orange",
  "High" = "darkred"
)

plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill = Strata), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "Strata") +
  labs(title = "Strata 2025",
       #subtitle = "Categorized Incidence Rates",
       caption = "Source: eNHIS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

print(plot)
ggsave("Strata2025_moderate_high.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")


#moderate, high, low
admin2_plot <- admin2_merged %>%
  filter(Strata %in% c("Low", "Moderate", "High")) %>%
  mutate(
    Strata = factor(Strata,
                    levels = c("Low", "Moderate", "High"))
  )

custom_colors <- c(
  "Low" = "lightgreen",
  "Moderate" = "orange",
  "High" = "darkred"
)

plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill = Strata), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "Strata") +
  labs(title = "Strata 2025",
       #subtitle = "Categorized Incidence Rates",
       caption = "Source: eNHIS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggsave("Strata2025_moderate_high_low.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")

#high
admin2_plot <- admin2_merged %>%
  filter(Strata %in% c("High")) %>%
  mutate(
    Strata = factor(Strata,
                    levels = c( "High"))
  )

custom_colors <- c(
  "High" = "darkred"
)

plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill = Strata), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "Strata") +
  labs(title = "Strata 2025",
       #subtitle = "Categorized Incidence Rates",
       caption = "Source: eNHIS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

print(plot)
ggsave("Strata2025_high.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")
#######################
#Eligibility maps
#######################

#Spatial emanators
custom_colors <- c("Yes" = "violet")


plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill = Spatial_repellents), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "Spatial emanators") +
  labs(title = "Spatial emanators eligible districts"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )


print(plot)

ggsave("Spatialemanotrs.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")

#altitude
custom_colors <- c(">=2000" = "#fcae91", 
                   "1600-<2000" =  "#fb6a4a", 
                   "<1600" = "#cb181d")



plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill = altitude_cat), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "Altitude") +
  theme_minimal() +
  labs(title = "Altitude"
       #subtitle = "Categorized Incidence Rates",
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )


print(plot)

ggsave("Altitude.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")

#ITN usage
custom_colors <- c("<50" = "#a6cee3")

plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill = mean_ITN_usage_cat), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "ITN Usage") +
  labs(title = "ITN Usage"
       #subtitle = "Categorized Incidence Rates",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )


print(plot)
ggsave("ITNusage.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")



#areas <1600 + villages <1600m
custom_colors <- c( 
  "Villages <1600m" =  "#fb6a4a", 
  "<1600m" = "#cb181d")


plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill = areasb1600m), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "Altitude") +
  labs(title = "Altitude"
       #subtitle = "Categorized Incidence Rates",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )


print(plot)
ggsave("areasb1600m.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")

#ITN
custom_colors <- c( "Yes" = "purple",
                    "Targeted"="thistle")

plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill = STD_Nets), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "ITN") +
  labs(title = "ITN eligible districts") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )


print(plot)
ggsave("Pyrethroid_nets.png", plot = plot, width = 10, height = 8, dpi = 300)
ggsave("Pyrethroid_nets.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")

#ITN outbreak
custom_colors <- c( 
  "Yes" = "purple")

plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill =STD_Nets_outbreak), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "Pyrethroid_Nets") +
  labs(title = "ITN eligible areas - Outbreak") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )


print(plot)

ggsave("Nets_outbreak.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")

#IRS codeployment
custom_colors <- c( 
  "Yes" = "slateblue")

plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill =IRS_LLIN_Codeployment), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "IRS") +
  labs(title = "IRS eligible districts") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )


print(plot)
ggsave("codeployment.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")




#IRS burden reduction
scustom_colors <- c( 
  "Yes" = "darkblue")

plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill =IRS_burdenreduction), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "IRS") +
  labs(title = "IRS Burden Reduction") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )


print(plot)
ggsave("IRS_burdenreduction.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")

#IRS elimination
custom_colors <- c("<1" = "darkblue", 
                   "1-<5" = "darkblue", 
                   "5-<10" = "darkblue")


plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill =Elimination_cutoffs), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "IRS") +
  labs(title = "IRS in elimination areas") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )


print(plot)
ggsave("IRS_elimination.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")

#RACDT
custom_colors <- c( 
  "Yes" = "pink")

plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill =RACDT), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "RACDT") +
  labs(title = "RACDT eligible districts") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )


print(plot)

ggsave("RACDT.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")


#MDA
custom_colors <- c( 
  "Yes" = "darkcyan")

plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill =MDA), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "MDA") +
  labs(title = "MDA eligible districts") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

print(plot)
ggsave("MDA_RISKIMPORTATION.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")

#areas where HMM is already implemented
custom_colors <- c( 
  "yes" =  "#664400")



plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill =HMMimplementedareas), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "HMM") +
  labs(title = "HMM Implementation") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

print(plot)
ggsave("HMMimplementation.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")

#LSM
custom_colors <- c( 
  "Yes" =  "#e6ab02")



plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill =LSM), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "LSM") +
  labs(title = "LSM eligible districts") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

print(plot)
ggsave("LSM.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")


#HMM
custom_colors <- c( 
  "Yes" =  "#fdbf6f")


plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill =HMM), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "HMM") +
  labs(title = "HMM eligible districts") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

print(plot)
ggsave("HMM.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")

#IPTP
custom_colors <- c( 
  "Yes" =    "#d95f02")


plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill =IPTp), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "IPTp") +
  labs(title = "IPTp eligible districts") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

print(plot)
ggsave("IPTP.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")

#ACSM
custom_colors <- c( 
  "Yes" =     "#e7298a")


plot<-ggplot( admin2_merged ) +
  geom_sf(aes(fill =ACSM), color = "grey40", linewidth = 0.2) +
  geom_sf(data = PNG_shapefile_admin1, fill = NA, color = "black", linewidth = 0.8,inherit.aes = FALSE) +  # Regional borders (admin1)
  scale_fill_manual(values = custom_colors, na.translate = FALSE, name = "ACSM") +
  labs(title = "ACSM Eligible districts") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10, face = "bold"),   # Bold legend text
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

print(plot)
ggsave("ACSM.tiff", 
       plot = plot, 
       width = 10, 
       height = 8,
       units = "in",
       dpi = 600,
       compression = "lzw")
