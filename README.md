# Der Einfluss der Zielentfernung auf die Wissensanwendung in MicroDYN-Systemen

## Fragestellung

> Beeinflusst die Distanz zwischen Ausgangs- und Zielwert erfolgreiche Wissensanwendung in MicroDYN-Systemen?

---

## Setup

Dieses Projekt benutzt [`renv`](https://rstudio.github.io/renv/) für dependency management.

**Um die Dependencies anzeigen:**
```r
renv::dependencies()
```

**Um die R-Umgebung wiederherzustellen:**
```r
renv::restore()
```

---

## Execution Order

> Zu Beginn bitte das Projekt öffnen: `ExPra_Aufgabenschwierigkeit.Rproj`, dann die Scripts in dieser Reihenfolge ausführen:

| Step | Script |
|------|--------|
| 1 | `read&clean.R` |
| 2 | `Demographics.R` |
| 3 | `Visualizations.R` |
| 4 | `Testing.R` |
| 5 | `explorativ.R` |
| 6 | `generate_xlsx.R` |

> Alternativ kann man einfach `ADMIN.R` laufen lassen.

---

## Data

Die Rohdaten sind im `.rds` Format gespeichert.

> ⚠️ **Die Rohdaten (`expra_eg_b.rds`) sind nicht öffentlich erhältlich** aufgrund von Datenschutzrichtlinien.

Um den Code zu testen, bitte **`mock_data.csv`** benutzen.