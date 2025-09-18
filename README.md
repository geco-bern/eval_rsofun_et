# Rsofun Phydro Branch Evaluation

This repository serves to create the figures for my manuscript.  

---

## Table of Contents
- [Repository Structure](#repository-structure)
  - [analysis/](#analysis)
  - [R/](#r)
  - [fig/](#fig)
  - [data-raw/](#data-raw)

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
