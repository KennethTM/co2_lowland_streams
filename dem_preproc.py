#dem preproc using richdem

import richdem as rd

dem_path = "data/dhym_25m.tif"

dem = rd.LoadGDAL(dem_path)

dem_breach = rd.BreachDepressions(dem, in_place=False)

dem_breach_fill = rd.FillDepressions(dem_breach, epsilon=False, in_place=False)

dem_breach_fill_flats = rd.ResolveFlats(dem_breach_fill, in_place=False)

rd.SaveGDAL(dem_path.split(".")[0]+"_breach.tif", dem_breach_fill_flats)