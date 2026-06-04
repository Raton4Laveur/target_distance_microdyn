library(here)

cat("Step 1: Datenbereinigung...\n")
source(here("R", "read&clean.R"))

cat("Step 2: Demografie...\n")
source(here("R", "Demographics.R"))

cat("Step 3: Visualisierung der Daten...\n")
source(here("R", "Visualization.R"))

cat("Step 4: Statistische Tests...\n")
source(here("R", "Testing.R"))

cat("Step 5: Explorative Analyse...\n")
source(here("R", "explorativ.R"))

cat("Step 6: Resultate in Excel...\n")
source(here("R", "generate_xlsx.R"))

cat("Pipeline erfolgreich abgeschlossen!\n")