# Evaluation of simulated ET from rsofun

This repository implements the analysis of Grossi et al. (in prep.).

---

## Table of Contents
- [Repository Structure](#repository-structure)
  - [analysis](#analysis)
  - [R](#r)
  - [fig](#fig)
  - [data-raw](#data-raw)
  - [processed](#processed_global_data)

---

## Repository Structure

### analysis/

Contains the scripts to create the figures.  
These are divided by spatial extent:  
- **Fluxnet-scale simulation**  
- **Camels-scale simulation**  
- **Global-scale simulation**  

Scripts ending with `*_plot.R` can be run immediately.  

**TODO**: add global P model output  

---

### R/

Contains the new **SoFunCalVal** scripts, including the **CAMELS yearly evaluation**.  

---

### fig/

Output figures are saved here.  

---

### data-raw/

Contains the scripts used to generate the data inside the `data/` folder.  
These scripts won’t work directly, since the data are only available on the workstation.  

### processed_global_data

Three global data have been used to compare the P model performance:

- ERA5
- FLUXCOM
- PML

### FLUXCOM download

Fluxcom can be acquired via FTP from Max Planck

https://www.fluxcom.org/EF-Download/

The instruction are available on their page.

The link on the aforementioned page don't work, here you may find it (to check)

https://www.bgc-jena.mpg.de/en/bgi/software

From there, I didn't process anything, I used the RS+METEO ensembles data product.

### ERA5 DOWNLOAD

For ERA5, I just donwload the "Surface latent heat flux" timeserie at monthly scale from 1982 to 2011 in nc format from the official website

https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-monthly-means?tab=download

import cdsapi

dataset = "reanalysis-era5-single-levels-monthly-means"
request = {
    "product_type": ["monthly_averaged_reanalysis"],
    "variable": ["surface_latent_heat_flux"],
    "year": [
        "1982", "1983", "1984",
        "1985", "1986", "1987",
        "1988", "1989", "1990",
        "1991", "1992", "1993",
        "1994", "1995", "1996",
        "1997", "1998", "1999",
        "2000", "2001", "2002",
        "2003", "2004", "2005",
        "2006", "2007", "2008",
        "2009", "2010", "2011"
    ],
    "month": [
        "01", "02", "03",
        "04", "05", "06",
        "07", "08", "09",
        "10", "11", "12"
    ],
    "time": ["00:00"],
    "data_format": "netcdf",
    "download_format": "zip"
}

client = cdsapi.Client()
client.retrieve(dataset, request).download()


### PML donwload

PML is downloaded from https://data.csiro.au/collection/csiro:17375v2 following the article https://www.nature.com/articles/srep19124


