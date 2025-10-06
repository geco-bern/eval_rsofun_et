The following folder ... lead the creation of the images used in my manuscript

The first three script are used to calibrate the model parameter for all three setup, you don't have to run it since the result of the analysis are stored in the data folder

The script 03_fluxnet_plot.R will create the figures used on my manuscript.

After completing the first three script, the simulation is run at camels scae-resolution.

The first script called 04_create_camels_driver.R will generate the driver data used to run the simulation.
The parameter used for the simualation are the same of the PM-S0 setup.

To run the driver data creation, I donwloaded the data the entire caravan dataset and select only the camels_us. The whole data set is 15 GB.

To prepare the driver data, some envriomental variable needs to be added (VPD and PPFD). Those variable are found in the workstation.

The script 05_camels_plot.R will create the images used in the manuscript.

To do so, I modify the SoFUnCalVal package to work also with camels and added here in the R folder.

The script to genereate the global gridded data and run is still missing.

