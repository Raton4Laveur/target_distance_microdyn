# The Effect of Target Distance on Knowledge Application in MicroDYN Systems

## Research Question

> Does the distance between starting value and goal value influence the successful knowledge application in MicroDYN systems?

---

## Setup

This project uses [`renv`](https://rstudio.github.io/renv/) for dependency management.

**List dependencies:**
```r
renv::dependencies()
```

**Recreate the environment:**
```r
renv::restore()
```

---

## Execution Order

> Start by opening `ExPra_Aufgabenschwierigkeit.Rproj`, then run the scripts in this order:

| Step | Script |
|------|--------|
| 1 | `read&clean.R` |
| 2 | `Demographics.R` |
| 3 | `Visualizations.R` |
| 4 | `Testing.R` |

---

## Data

Raw data is stored in `.rds` format.

> ⚠️ **The raw data file (`expra_eg_b.rds`) is not publicly available** due to privacy/institutional restrictions.

To test this code, use the provided **`mock_data.csv`** instead.