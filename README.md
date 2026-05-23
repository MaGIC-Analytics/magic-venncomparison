# magic-venncomparison

![GitHub last commit](https://img.shields.io/github/last-commit/MaGIC-Analytics/magic-venncomparison)
[![run with docker](https://img.shields.io/badge/run%20with-docker-0db7ed?labelColor=000000&logo=docker)](https://www.docker.com/)
[![made with Shiny](https://img.shields.io/badge/R-Shiny-blue)](https://shiny.rstudio.com/)

The MaGIC **Set Comparison Tool** renders Venn diagrams and UpSet plots from either
generic lists or differential-expression (DE) tables.

## Input modes

- **Generic lists** — upload one wide table (CSV/TSV) where each column is a set and the
  rows are its members (genes, proteins, GO terms, anything). Empty cells / NAs are ignored;
  no filtering is applied. Column headers become the set labels (editable inline).
- **DE tables** — upload one or more DE results tables, one per comparison, each with an
  editable label. Map the gene ID / log2FC / pvalue / padj columns, then filter by
  significance (padj or pvalue), |log2FC|, and direction (up / down / both).

## Outputs

- **Plot** — Venn diagram (2–5 sets) or UpSet plot (any number of sets), with per-set colors,
  Venn fill/font/label options, and UpSet intersection/sort/size controls. Download as
  PNG / PDF / SVG / TIFF / JPEG / EPS.
- **Intersection Table** — which items fall in which combination of sets. Download as CSV.

## Running the App
This Shiny app is built into a Docker container for easy deployment. Build the image yourself
(and customize the port if needed):
```
docker build -t venncomparison .
docker run -d --rm -p 8080:8080 venncomparison
```
It is then hosted at localhost:8080.
