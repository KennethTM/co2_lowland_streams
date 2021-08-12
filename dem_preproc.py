#DEM preproc using richdem

import richdem as rd

dem_path = "data/dhym_10m_crop.tif"

dem = rd.LoadGDAL(dem_path)

rd.BreachDepressions(dem, in_place=True)

rd.ResolveFlats(dem, in_place=True)

rd.SaveGDAL(dem_path.split(".")[0]+"_breach.tif", dem)
