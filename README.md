# Mapping roadkill risk in Taiwan using MaxEnt

A demo R workflow (including code and data) to map relative roadkill risk on Taiwan's highway network using MaxEnt (presence-only species distribution modeling).
- Walkthrough: [HackMD page](https://hackmd.io/@sychamore/HyypHLLFxe)
- Live map: [interactive roadkill risk map](https://stephchia.github.io/roadkill-sdm/roadkill_risk_herpet_vect.html) (screenshot below)

<img width="600" alt="image" src="https://github.com/user-attachments/assets/dc3c0970-bb03-4319-ab1a-5d2127737427" />

## Data sources
- Occurence data: [TRON Roadkill data on GBIF](https://www.gbif.org/dataset/db09684b-0fd1-431e-b5fa-4c1532fbdb14)  (years 2011-2017, ~46,000 records)
- Modeling target: [Taiwan highway network](https://www.thb.gov.tw/News_Content_thbOpenData.aspx?n=13&s=490)
- Environmental predictors:
    - Land cover: [WorldCover 10 m 2021 v200](https://zenodo.org/records/7254221)
    - Vegetation: [Atlas of Natural Vegetation in Taiwan](https://scidm.nchc.org.tw/dataset/best_wish9930) (one file not committed due to size)

## Quick start
- Requirements: R; packages `dplyr`, `raster`, `terra`, `sf`, `mapview`, `rJava`, `dismo`
- Input data placed under `data_input/`
  - `highway/` (shapefile of highway network)
  - `Taiwan_diss` (shapefile of Taiwan land)
  - `landcover/` (TIFs of WorldCover land cover)
  - `vegetation/` (shapefile of vegetation type. Obtain full data from the link above.)
  - `roadkill.csv` (TRON/GBIF subset of roadkill records)
- Run these scripts:
  - scripts/01_prepare_data.R
  - scripts/02_fit_model.R
  - scripts/03_visualization.R
- Outputs: intermediates in `data/`; fitted model and HTML maps in `output/`.

