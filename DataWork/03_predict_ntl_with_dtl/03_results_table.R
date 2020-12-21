# CNN Results Table

# https://stackoverflow.com/questions/33081702/accuracy-precision-and-recall-for-multi-class-model

# Load Data --------------------------------------------------------------------
results_df <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", "1sPsrCGrppYuwdKHu97u8bgjDkVfhGada"))

# Prep Data --------------------------------------------------------------------

r <- ml_test(results_df$predY,
             results_df$testY)
r$precision
r$recall
r$accuracy


