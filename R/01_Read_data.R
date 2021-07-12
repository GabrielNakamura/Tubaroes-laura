
# Read data ---------------------------------------------------------------

metric_matrix <- read.table(file = here::here("data", "reinoslimpa.txt"), header = TRUE)


# separating metrics  -----------------------------------------------------

div_metrics <- metric_matrix[, -c(1, 2)]
