# library
library(ggplot2)

dimensions <- rep(colnames(matrix_M), each = dim(matrix_M)[1])
locations <- rep(1:length(rownames(matrix_M))[-c(17, 20)], times = ncol(matrix_M))
metrics_long <- do.call(c, matrix_M)
long_matrixM <- data.frame(dimensions = dimensions,
                           Realms = as.character(locations),
                           metrics = metrics_long)




IV_EE_plot <-
  ggplot(data = long_matrixM, aes(fill = Realms, y = metrics, x = dimensions)) +
  geom_bar(position="dodge", stat="identity") +
  labs(y = "EE and IVs metric", x = "Dimensions of diversity")


ggsave(filename = here::here("output", "IVs_EEs.pdf"), plot = IV_EE_plot,
       width = 8, height = 7,
       dpi = 500)
