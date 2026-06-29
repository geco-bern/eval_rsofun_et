import cdsapi

dataset = "reanalysis-era5-single-levels-monthly-means"
request = {
    "product_type": ["monthly_averaged_reanalysis"],
    "variable": ["surface_latent_heat_flux"],
    "year": [str(y) for y in range(1982, 2012)],
    "month": [f"{m:02d}" for m in range(1, 13)],
    "time": ["00:00"],
    "data_format": "netcdf",
    "download_format": "zip"
}

client = cdsapi.Client()
client.retrieve(dataset, request).download("era5_slhf_1982_2011.zip")
