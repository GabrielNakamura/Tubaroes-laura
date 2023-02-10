
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

metric_matrix <- read.table(file = here::here("data", "matrizclean.txt"), header = TRUE) # metricas brutas
sf_metrics <- sf::st_as_sf(metric_matrix)
medidas <- read.table(file = here::here("data", "dimensionality_all_realms.txt"), header = TRUE) # matriz com IV e EE
colnames(medidas)[1] <- "Realm"
realms_sf <- sf::st_read(here::here("data", "spatial_data", "MarineRealms.shp"))
realms_sf <- realms_sf[-which(realms_sf$Realm == c(1, 2)), ]
realms_stats <- read.table(file = here::here("data", "reinos_stat.txt"), header = TRUE) # caracteristicas dos reinos
realms_stats <- data.frame(Realm = realms_sf$Realm, realms_stats)
class(realms_stats$Realm) <- "character"

# editing spatial objects -------------------------------------------------

realms_sf <- merge(realms_sf, medidas, by.x = 'Realm', by.y = 'Realm', all = FALSE, sort = T) %>%
  st_transform(crs = "+proj=robin")
class(realms_sf$Realm) <- "character"

# IVs Eveness -------------------------------------------------------------
IV_all <-
data.frame(vpdmorf = realms_sf$vpdmorf,
           mpdmorf = realms_sf$vpdmorf,
           pd.obs.z = realms_sf$pd.obs.z,
           FDivmorf = realms_sf$FDivmorf,
           FEvemorf = realms_sf$FEvemorf,
           ses_fric = realms_sf$ses_fric,
           richness = realms_sf$ntaxa)
Eveness_IVs <- apply(IV_all,
                     MARGIN = 1,
                     function(x) vegan::diversity(x, index = "shannon"))/(log(7))
Eveness_IVs <- data.frame(Realm = realms_sf$Realm, Eveness_IVs)

# assembling spatial data -------------------------------------------------

sf_dimensionality <-
  realms_sf %>%
  st_transform(crs = "+proj=robin") %>%
  left_join(Eveness_IVs) %>%
  left_join(realms_stats)

# plotting realms Fig 1  ----------------------------------------------------------

# color palete (Laura)
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
                                  limits = c(0, .5),  ## max percent overall
                                  breaks = seq(0, .5, by = .1),
                                  labels = glue::glue("{seq(0, .5, by = 0.1)}")) +
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

# IVs eveness
map_EvenessIV <- ggplot() +
  geom_sf(data = sf_dimensionality, aes(geometry = geometry,
                                        fill = Eveness_IVs),
          color = "black", size = 0.5) +
  rcartocolor::scale_fill_carto_c(palette = "Teal",
                                  direction = 1,
                                  limits = c(0, .7),  ## max percent overall
                                  breaks = seq(0, .7, by = .1),
                                  labels = glue::glue("{seq(0, .7, by = 0.1)}")) +
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

# plotting IV evenness ----------------------------------------------------

# IV vpd
map_vpdmorf <-
  ggplot() +
  geom_sf(data = sf_dimensionality, aes(geometry = geometry,
                                        fill = vpdmorf),
          color = "black", size = 0.5) +
  rcartocolor::scale_fill_carto_c(palette = "Teal",
                                  direction = 1,
                                  limits = c(0, .09),  ## max percent overall
                                  breaks = seq(0, .09, by = .03),
                                  labels = glue::glue("{seq(0, .09, by = 0.03)}")) +
  guides(fill = guide_colorbar(barheight = unit(20, units = "mm"),
                               barwidth = unit(3, units = "mm"),
                               direction = "vertical",
                               ticks.colour = "grey20",
                               title.position = "top",
                               label.position = "right",
                               title.hjust = 0.5)) +
  labs(subtitle = "IV VPD", title = "") +
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

# IV pd
map_pd.obs.z <- ggplot() +
  geom_sf(data = sf_dimensionality, aes(geometry = geometry,
                                        fill = pd.obs.z),
          color = "black", size = 0.05) +
  rcartocolor::scale_fill_carto_c(palette = "Teal",
                                  direction = 1,
                                  limits = c(0, 1),  ## max percent overall
                                  breaks = seq(0, 1, by = 0.2),
                                  labels = glue::glue("{seq(0, 1, by = 0.2)}")) +
  guides(fill = guide_colorbar(barheight = unit(20, units = "mm"),
                               barwidth = unit(3, units = "mm"),
                               direction = "vertical",
                               ticks.colour = "grey20",
                               title.position = "top",
                               label.position = "right",
                               title.hjust = 0.5)) +
  labs(subtitle = "IV ses.PD", title = "") +
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


# IV FDivmorf
map_FDivmorf <- ggplot() +
  geom_sf(data = sf_dimensionality, aes(geometry = geometry,
                                        fill = FDivmorf),
          color = "black", size = 0.5) +
  rcartocolor::scale_fill_carto_c(palette = "Teal",
                                  direction = 1,
                                  limits = c(0, .01),  ## max percent overall
                                  breaks = seq(0, .01, by = .002),
                                  labels = glue::glue("{seq(0, .01, by = 0.002)}")) +
  guides(fill = guide_colorbar(barheight = unit(20, units = "mm"),
                               barwidth = unit(3, units = "mm"),
                               direction = "vertical",
                               ticks.colour = "grey20",
                               title.position = "top",
                               label.position = "right",
                               title.hjust = 0.5)) +
  labs(subtitle = "IV FDiv", title = "") +
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

# IV FEvemorf
map_ses.FEvemorf <- ggplot() +
  geom_sf(data = sf_dimensionality, aes(geometry = geometry,
                                        fill = FEvemorf),
          color = "black", size = 0.5) +
  rcartocolor::scale_fill_carto_c(palette = "Teal",
                                  direction = 1,
                                  limits = c(0, .04),  ## max percent overall
                                  breaks = seq(0, .04, by = .01),
                                  labels = glue::glue("{seq(0, .04, by = 0.01)}")) +
  guides(fill = guide_colorbar(barheight = unit(20, units = "mm"),
                               barwidth = unit(3, units = "mm"),
                               direction = "vertical",
                               ticks.colour = "grey20",
                               title.position = "top",
                               label.position = "right",
                               title.hjust = 0.5)) +
  labs(subtitle = "IV FEve", title = "") +
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

# IV fric
map_ses.ses_fric <- ggplot() +
  geom_sf(data = sf_dimensionality, aes(geometry = geometry,
                                        fill = ses_fric),
          color = "black", size = 0.5) +
  rcartocolor::scale_fill_carto_c(palette = "Teal",
                                  direction = 1,
                                  limits = c(0, .9),  ## max percent overall
                                  breaks = seq(0, .9, by = .1),
                                  labels = glue::glue("{seq(0, .9, by = 0.1)}")) +
  guides(fill = guide_colorbar(barheight = unit(20, units = "mm"),
                               barwidth = unit(3, units = "mm"),
                               direction = "vertical",
                               ticks.colour = "grey20",
                               title.position = "top",
                               label.position = "right",
                               title.hjust = 0.5)) +
  labs(subtitle = "IV ses.FD", title = "") +
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
map_ses.ntaxa <- ggplot() +
  geom_sf(data = sf_dimensionality, aes(geometry = geometry,
                                        fill = ntaxa),
          color = "black", size = 0.5) +
  rcartocolor::scale_fill_carto_c(palette = "Teal",
                                  direction = 1,
                                  limits = c(0, .2),  ## max percent overall
                                  breaks = seq(0, .2, by = .05),
                                  labels = glue::glue("{seq(0, .2, by = 0.05)}")) +
  guides(fill = guide_colorbar(barheight = unit(20, units = "mm"),
                               barwidth = unit(3, units = "mm"),
                               direction = "vertical",
                               ticks.colour = "grey20",
                               title.position = "top",
                               label.position = "right",
                               title.hjust = 0.5)) +
  labs(subtitle = "IV Richness", title = "") +
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

# IV MPD
map_mpdmorf <- ggplot() +
  geom_sf(data = sf_dimensionality, aes(geometry = geometry,
                                        fill = mpdmorf),
          color = "black", size = 0.5) +
  rcartocolor::scale_fill_carto_c(palette = "Teal",
                                  direction = 1,
                                  limits = c(0, .09),  ## max percent overall
                                  breaks = seq(0, .09, by = .03),
                                  labels = glue::glue("{seq(0, .09, by = 0.03)}")) +
  guides(fill = guide_colorbar(barheight = unit(20, units = "mm"),
                               barwidth = unit(3, units = "mm"),
                               direction = "vertical",
                               ticks.colour = "grey20",
                               title.position = "top",
                               label.position = "right",
                               title.hjust = 0.5)) +
  labs(subtitle = "IV MPD", title = "") +
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

list_map_IVs <- list(map_FDivmorf, map_mpdmorf, map_pd.obs.z, map_ses.FEvemorf, map_ses.ntaxa, map_ses.ses_fric, map_vpdmorf)

# joining maps
map_IVs <-
patchwork::wrap_plots(list_map_IVs) +
  patchwork::plot_annotation(tag_levels = "A")

map_IVs <-
cowplot::plot_grid(plotlist = list_map_IVs, ncol = 4)

all_IV_maps <-
(map_FDivmorf + map_mpdmorf + map_pd.obs.z + map_ses.FEvemorf)|(map_ses.ntaxa + map_ses.ses_fric + map_vpdmorf)


# bivariate spatial plot --------------------------------------------------

sf_bivar_dimensionality <- bi_class(sf_dimensionality,
                                    x = EE,
                                    y = Eveness_IVs,
                                    style = "jenks",
                                    dim = 3)

bivar_map_dimensionality <- ggplot() +
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

bivar_map_dimensionality_single <-
  ggdraw() +
  draw_plot(bivar_map_dimensionality, 0, 0, 1, 1) +
  draw_plot(legend, 0, .79, 0.25, 0.25)

# patchworking and saving maps -------------------------------------------------------



ggsave(filename = here::here("output", "bivar_map.png"), plot = bivar_map_dimensionality_single, device = "png",
       width = 10, height = 5,
       dpi = 300)

ggsave(filename = here::here("output", "EE_map.png"), plot = map_EE, device = "png",
       width = 10, height = 5,
       dpi = 300)

ggsave(filename = here::here("output", "Reinos_map.png"), plot = map_reinos, device = "png",
       width = 10, height = 5,
       dpi = 300)

ggsave(filename = here::here("output", "IVs_map.png"), plot = map_IVs, device = "png",
       width = 10, height = 5,
       dpi = 300)

save_plot(filename = here::here("output", "IVs_map.png"), plot = map_IVs,
          base_height = 4, base_width = 12)
