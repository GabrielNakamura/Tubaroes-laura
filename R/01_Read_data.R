
# Read data ---------------------------------------------------------------

metric_matrix <- read.table(file = here::here("data", "reinoslimpa.txt"), header = TRUE)
matrix_M <- read.table(file = here::here("dimensionality_all_realms.txt"))

# separating metrics  -----------------------------------------------------

div_metrics <- metric_matrix[, -c(1, 2)]

