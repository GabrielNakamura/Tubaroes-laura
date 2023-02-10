library(MuMIn)
library(QuantPsyc)

#ler planilha reinos_stat

realms_stats <- read.table(file = here::here("data", "reinos_stat.txt"), header = TRUE) # caracteristicas dos reinos

reg.reino <- (lm(dim ~ log.area + median.bs.rg + median.div.rg + log.riqreino + x + centlong + centlat + taxaend,data= realms_stats))
summary(reg.reino)
lm.beta(reg.reino) # coeficientes estandardizados

options(na.action = "na.fail")
multi1 <-dredge(reg.reino, beta = "partial.sd", rank = "AICc")
multi1
subset(multi1, delta <= 2)
model.avg(multi1, subset = delta <= 2)

plot_model_lm <-
ggplot(data = realms_stats) +
  geom_point(aes(x = x, y = dim)) +
  geom_smooth(aes(x = x, y = dim), method = "lm") +
  xlab("Environmental EE") +
  ylab("Biodiversity EE")

ggsave(filename = here::here("output", "plot_model_lm.png"), plot = plot_model_lm, device = "png",
       width = 8, height = 5,
       dpi = 300)

