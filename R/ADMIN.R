library(here)

cat("Step 1: Cleaning raw data...\n")
source(here("R", "read&clean.R"))

cat("Step 2: Processing demographic summaries & plots...\n")
source(here("R", "Demographics.R"))

cat("Step 3: Executing primary statistical tests...\n")
source(here("R", "Testing.R"))

cat("Step 4: Running exploratory analysis...\n")
source(here("R", "explorativ.R"))

cat("Step 5: Visualizing primary distributions...\n")
source(here("R", "Visualization.R"))

cat("Step 6: Writing formatted results to Excel...\n")
source(here("R", "generate_xlsx.R"))

cat("Pipeline run successfully completed!\n")