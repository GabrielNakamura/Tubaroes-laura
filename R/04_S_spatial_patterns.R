
# PACOTES -----------------------------------------------------------------
library(ggplot2)
library(ggthemes)
library(sf)
library(magrittr)
library(dplyr)
library(patchwork)
library(biscale)
library(cowplot)
library(unikn)

# raster dos IV'S , EE'S,  Atributos and Spatial Data --------------------------------------

# metric_matrix <- read.table(file = here::here("data", "matrizclean.txt"), header = TRUE) # metricas brutas
# sf_metrics <- sf::st_as_sf(metric_matrix)
medidas <- readRDS(file = here::here("data", "dimensionality_06_06_23.rds")) # matriz com IV e EE
medidas <- as.data.frame(medidas)
medidas$Realm <- rownames(medidas)
realms_sf <- sf::st_read(here::here("data", "spatial_data", "MarineRealms.shp")) # shapefile with Realms
realms_sf <- realms_sf[-which(realms_sf$Realm == c(1, 2)), ]
realms_stats <- read.table(file = here::here("data", "reinos_stat.txt"), header = TRUE) # Realms features
realms_stats <- data.frame(Realm = realms_sf$Realm, realms_stats)
class(realms_stats$Realm) <- "character"

# editing spatial objects -------------------------------------------------

realms_sf <- merge(realms_sf, medidas, by.x = 'Realm', by.y = 'Realm', all = FALSE, sort = T) %>%
  st_transform(crs = "+proj=robin") # joining realms and metrics
class(realms_sf$Realm) <- "character"
colnames(realms_sf)[9] <- "log.richness" # just changing the name of richness variable

# IVs Eveness -------------------------------------------------------------
IV_all <-
data.frame(ses.pd = realms_sf$ses.pd,
           mpdphy = realms_sf$mpdphy,
           vpdphy = realms_sf$vpdphy,
           ses.fd = realms_sf$ses.fd,
           mpdfunc = realms_sf$mpdfunc,
           vpdfunc = realms_sf$vpdfunc,
           richness = realms_sf$log.richness)
Eveness_IVs <- apply(IV_all,
                     MARGIN = 1,
                     function(x) vegan::diversity(x, index = "shannon"))/(log(7)) # Equitability of IV values
Eveness_IVs <- data.frame(Realm = realms_sf$Realm, Eveness_IVs) # joining with Realms names

# assembling spatial data -------------------------------------------------

sf_dimensionality <-
  realms_sf %>%
  st_transform(crs = "+proj=robin") %>%
  left_join(Eveness_IVs) %>%
  left_join(realms_stats) # joining dimensionality metrics with Realms and reprojecting the maps to an equal are projection

# plotting realms Fig 1  ----------------------------------------------------------

# color pallete (Laura)
pal <- usecol(pal = pal_unikn_pair, n = 28)

# plotting marine realms
map_reinos <- ggplot() +
  geom_sf(data = sf_dimensionality, aes(geometry = geometry,
                                        fill = Realm),
          color = "black", size = 0.5) +
  scale_fill_manual(
    name = "",
    values = rev(pal)
  ) +
  geom_sf_text(data = sf_dimensionality,
               aes(label = Realm),
               colour = "black",
               show.legend = FALSE,
               inherit.aes = FALSE) +
  xlab("") +
  ylab("") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "mm"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times", color = "black", size = 10),
        axis.text = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle = element_blank()
  )



# plotting EE Fig 2 -------------------------------------------------------------

# EE values
map_EE <- ggplot() +
  geom_sf(data = sf_dimensionality, aes(geometry = geometry,
                                        fill = EE),
          color = "black", size = 0.5) +
  rcartocolor::scale_fill_carto_c(palette = "Teal",
                                  direction = 1,
                                  limits = c(0, .52),  ## max percent overall
                                  breaks = seq(0, .52, by = .1),
                                  labels = glue::glue("{seq(0, .52, by = 0.1)}")) +
  guides(fill = guide_colorbar(barheight = unit(30, units = "mm"),
                               barwidth = unit(3, units = "mm"),
                               direction = "vertical",
                               ticks.colour = "grey20",
                               label.position = "right",
                               title.hjust = 0.5)) +
  labs(subtitle = "", title = "") +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "mm"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times", color = "black", size = 10),
        axis.text = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(family = "Arial",
                                     color = "black",
                                     size = 11,
                                     hjust = 0.5,
                                     margin = margin(b = 6))
  )

# IVs evenness
map_EvenessIV <- ggplot() +
  geom_sf(data = sf_dimensionality, aes(geometry = geometry,
                                        fill = Eveness_IVs),
          color = "black", size = 0.5) +
  rcartocolor::scale_fill_carto_c(palette = "Teal",
                                  direction = 1,
                                  limits = c(0, .8),  ## max percent overall
                                  breaks = seq(0, .8, by = .1),
                                  labels = glue::glue("{seq(0, .8, by = 0.1)}")) +
  guides(fill = guide_colorbar(barheight = unit(2.3, units = "mm"),
                               barwidth = unit(50, units = "mm"),
                               direction = "horizontal",
                               ticks.colour = "grey20",
                               title.position = "top",
                               label.position = "bottom",
                               title.hjust = 0.5)) +
  labs(subtitle = "Eveness of IVs", title = "") +
  theme(legend.position = "bottom", panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "mm"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times", color = "black", size = 10),
        axis.text = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(family = "Arial",
                                     color = "black",
                                     size = 11,
                                     hjust = 0.5,
                                     margin = margin(b = 6))
  )

# plotting IV for each metric ----------------------------------------------------

# IV vpd func
map_vpdfunc <-
  ggplot() +
  geom_sf(data = sf_dimensionality, aes(geometry = geometry,
                                        fill = vpdfunc),
          color = "black", size = 0.5) +
  rcartocolor::scale_fill_carto_c(palette = "Teal",
                                  direction = 1,
                                  limits = c(0, .2),  ## max percent overall
                                  breaks = seq(0, .2, by = .03),
                                  labels = glue::glue("{seq(0, .2, by = 0.03)}")) +
  guides(fill = guide_colorbar(barheight = unit(20, units = "mm"),
                               barwidth = unit(3, units = "mm"),
                               direction = "vertical",
                               ticks.colour = "grey20",
                               title.position = "top",
                               label.position = "right",
                               title.hjust = 0.5)) +
  labs(subtitle = "IV Functional VPD", title = "") +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "mm"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times", color = "black", size = 8),
        axis.text = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(family = "Arial",
                                     color = "black",
                                     size = 11,
                                     hjust = 0.5,
                                     margin = margin(b = 6))
  )

# IV mpd func
map_mpdfunc <- ggplot() +
  geom_sf(data = sf_dimensionality, aes(geometry = geometry,
                                        fill = mpdfunc),
          color = "black", size = 0.05) +
  rcartocolor::scale_fill_carto_c(palette = "Teal",
                                  direction = 1,
                                  limits = c(0, .07),  ## max percent overall
                                  breaks = seq(0, 0.07, by = 0.02),
                                  labels = glue::glue("{seq(0, 0.07, by = 0.02)}")) +
  guides(fill = guide_colorbar(barheight = unit(20, units = "mm"),
                               barwidth = unit(3, units = "mm"),
                               direction = "vertical",
                               ticks.colour = "grey20",
                               title.position = "top",
                               label.position = "right",
                               title.hjust = 0.5)) +
  labs(subtitle = "IV Functional MPD", title = "") +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "mm"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times", color = "black", size = 8),
        axis.text = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(family = "Arial",
                                     color = "black",
                                     size = 11,
                                     hjust = 0.5,
                                     margin = margin(b = 6))
  )


# IV vpd phylo
map_vpdphy <- ggplot() +
  geom_sf(data = sf_dimensionality, aes(geometry = geometry,
                                        fill = vpdphy),
          color = "black", size = 0.5) +
  rcartocolor::scale_fill_carto_c(palette = "Teal",
                                  direction = 1,
                                  limits = c(0, .1),  ## max percent overall
                                  breaks = seq(0, .1, by = .02),
                                  labels = glue::glue("{seq(0, .1, by = 0.02)}")) +
  guides(fill = guide_colorbar(barheight = unit(20, units = "mm"),
                               barwidth = unit(3, units = "mm"),
                               direction = "vertical",
                               ticks.colour = "grey20",
                               title.position = "top",
                               label.position = "right",
                               title.hjust = 0.5)) +
  labs(subtitle = "IV Phylogenetic VPD", title = "") +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "mm"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times", color = "black", size = 8),
        axis.text = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(family = "Arial",
                                     color = "black",
                                     size = 11,
                                     hjust = 0.5,
                                     margin = margin(b = 6))
  )

# IV mpd phy
map_mpdphy <- ggplot() +
  geom_sf(data = sf_dimensionality, aes(geometry = geometry,
                                        fill = mpdphy),
          color = "black", size = 0.5) +
  rcartocolor::scale_fill_carto_c(palette = "Teal",
                                  direction = 1,
                                  limits = c(0, .05),  ## max percent overall
                                  breaks = seq(0, .05, by = .01),
                                  labels = glue::glue("{seq(0, .05, by = 0.01)}")) +
  guides(fill = guide_colorbar(barheight = unit(20, units = "mm"),
                               barwidth = unit(3, units = "mm"),
                               direction = "vertical",
                               ticks.colour = "grey20",
                               title.position = "top",
                               label.position = "right",
                               title.hjust = 0.5)) +
  labs(subtitle = "IV Phylogenetic MPD", title = "") +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "mm"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times", color = "black", size = 8),
        axis.text = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(family = "Arial",
                                     color = "black",
                                     size = 11,
                                     hjust = 0.5,
                                     margin = margin(b = 6))
  )

# IV sesfd
map_ses.fd <- ggplot() +
  geom_sf(data = sf_dimensionality, aes(geometry = geometry,
                                        fill = ses.fd),
          color = "black", size = 0.5) +
  rcartocolor::scale_fill_carto_c(palette = "Teal",
                                  direction = 1,
                                  limits = c(0, .5),  ## max percent overall
                                  breaks = seq(0, .5, by = .1),
                                  labels = glue::glue("{seq(0, .5, by = 0.1)}")) +
  guides(fill = guide_colorbar(barheight = unit(20, units = "mm"),
                               barwidth = unit(3, units = "mm"),
                               direction = "vertical",
                               ticks.colour = "grey20",
                               title.position = "top",
                               label.position = "right",
                               title.hjust = 0.5)) +
  labs(subtitle = "IV Functional Richness", title = "") +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "mm"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times", color = "black", size = 8),
        axis.text = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(family = "Arial",
                                     color = "black",
                                     size = 11,
                                     hjust = 0.5,
                                     margin = margin(b = 6))
  )

# IV sespd
map_ses.pd <- ggplot() +
  geom_sf(data = sf_dimensionality, aes(geometry = geometry,
                                        fill = ses.pd),
          color = "black", size = 0.5) +
  rcartocolor::scale_fill_carto_c(palette = "Teal",
                                  direction = 1,
                                  limits = c(0, 1),  ## max percent overall
                                  breaks = seq(0, 1, by = .2),
                                  labels = glue::glue("{seq(0, 1, by = 0.2)}")) +
  guides(fill = guide_colorbar(barheight = unit(20, units = "mm"),
                               barwidth = unit(3, units = "mm"),
                               direction = "vertical",
                               ticks.colour = "grey20",
                               title.position = "top",
                               label.position = "right",
                               title.hjust = 0.5)) +
  labs(subtitle = "IV Phylogenetic Richness", title = "") +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "mm"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times", color = "black", size = 8),
        axis.text = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(family = "Arial",
                                     color = "black",
                                     size = 11,
                                     hjust = 0.5,
                                     margin = margin(b = 6))
  )

# IV richness
map_richness <- ggplot() +
  geom_sf(data = sf_dimensionality, aes(geometry = geometry,
                                        fill = log.richness),
          color = "black", size = 0.5) +
  rcartocolor::scale_fill_carto_c(palette = "Teal",
                                  direction = 1,
                                  limits = c(0, .05),  ## max percent overall
                                  breaks = seq(0, .05, by = .01),
                                  labels = glue::glue("{seq(0, .05, by = 0.01)}")) +
  guides(fill = guide_colorbar(barheight = unit(20, units = "mm"),
                               barwidth = unit(3, units = "mm"),
                               direction = "vertical",
                               ticks.colour = "grey20",
                               title.position = "top",
                               label.position = "right",
                               title.hjust = 0.5)) +
  labs(subtitle = "IV Richness (log)", title = "") +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "mm"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times", color = "black", size = 8),
        axis.text = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(family = "Arial",
                                     color = "black",
                                     size = 11,
                                     hjust = 0.5,
                                     margin = margin(b = 6))
  )


list_map_IVs <- list(map_ses.pd, map_mpdphy, map_vpdphy, map_ses.fd, map_mpdfunc, map_vpdfunc, map_richness)

# joining maps with all IVs

map_IVs <-
cowplot::plot_grid(plotlist = list_map_IVs, ncol = 3)


# bivariate spatial plot --------------------------------------------------

# bivariate plots with EE and Evenness of IVs

sf_bivar_dimensionality <- bi_class(sf_dimensionality,
                                    x = EE,
                                    y = Eveness_IVs,
                                    style = "jenks",
                                    dim = 3) # creating data combining the two variables

bivar_map_dimensionality <-
  ggplot() +
  geom_sf(data = sf_bivar_dimensionality,
          mapping = aes(fill = bi_class),
          color = "black",
          size = 0.1,
          show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 3) +
  labs(
    title = "",
    subtitle = ""
  ) +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "mm"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times", color = "black", size = 10),
        axis.text = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(family = "Arial",
                                     color = "black",
                                     size = 11,
                                     hjust = 0.5,
                                     margin = margin(b = 6))
  )

legend <- bi_legend(pal = "DkCyan",
                    dim = 3,
                    xlab = "EE",
                    ylab = "Evennes IVs",
                    size = 8)

# joining map and legend in a single Figure
bivar_map_dimensionality_single <-
  ggdraw() +
  draw_plot(bivar_map_dimensionality, 0, 0, 1, 1) +
  draw_plot(legend, 0, .79, 0.25, 0.25)

# patchworking and saving maps -------------------------------------------------------


ggsave(filename = here::here("output", "Fig1_Realms_map.png"), plot = map_reinos, device = "png",
       width = 10, height = 5,
       dpi = 300)

ggsave(filename = here::here("output", "Fig2_EE_map.png"), plot = map_EE, device = "png",
       width = 10, height = 5,
       dpi = 300)


ggsave(filename = here::here("output", "Fig3_IVs_map.png"), plot = map_IVs, device = "png",
       width = 10, height = 5,
       dpi = 300)

ggsave(filename = here::here("output", "Fig4_bivar_map.png"), plot = bivar_map_dimensionality_single, device = "png",
       width = 10, height = 5,
       dpi = 300)

