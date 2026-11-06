``` numberSource
library(tidyverse) # A staple of modern data processing in R
library(tidync) # For easily dealing with NetCDF data
library(data.table)
library(rerddap) # For easily downloading subsets of data
library(lubridate)
library(reticulate)
library(doParallel) # For parallel processing
```

First, I define the Benguela region and time extent of interest:

``` numberSource
lats <- c(-37.5, -20)
lons <- c(15, 20)
time <- c("2021-01-01", "2021-12-31")
```

Then I find some data.

## 1 VIIRS chlorophyll-*a* data

``` numberSource
which_chl <- ed_search(query = "Chlorophyll-a", which = "griddap")
```

I select the VIIRS chl-*a* data. These data start in 2012 and it has a spatial resolution of ~4km lat/lon. The data, “VIIRSN, Suomi-NPP, Level-3 SMI, NASA, Global, 4km, Chlorophyll a, OCI Algorithm, R2018, 2012-present, Daily,” were retrieved from [here](https://upwell.pfeg.noaa.gov/erddap/info/erdVH2018chla1day/index.html).

``` numberSource
browse("erdVH2018chla1day")
```

And now I download it for the region and time period specified earlier:

``` numberSource
chl <- griddap(datasetx = "erdVH2018chla1day", 
               url = "https://upwell.pfeg.noaa.gov/erddap/", 
               time = c(time[1], time[2]),
               latitude = lats,
               longitude = lons,
               fields = "all")$data %>% 
  mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) |> 
  as.data.table()
```

## 2 ERA5 reanalysis wind data

I use the “ERA5 hourly data on single levels from 1940 to present” data, which come in at an hourly resolution for the whole world, starting in 1940. The spatial resolution is an unimpressive 0.25° × 0.25° lat/lon, far coarser than the chl-*a* data.

The ERA5 reanalysis wind data (*u* and *v* components) were downloaded from [Copernicus](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=overview) using a python script, which can be generated on the website using the “Show API request” option after selecting the variables and spatio-temporal ranges of interest:

``` numberSource
import cdsapi

c = cdsapi.Client()

c.retrieve(
    'reanalysis-era5-single-levels',
    {
        'product_type': 'reanalysis',
        'variable': [
            '10m_u_component_of_wind', '10m_v_component_of_wind',
        ],
        'year': '2021',
        'month': [
            '01', '02', '03',
            '04', '05', '06',
            '07', '08', '09',
            '10', '11', '12',
        ],
        'day': [
            '01', '02', '03',
            '04', '05', '06',
            '07', '08', '09',
            '10', '11', '12',
            '13', '14', '15',
            '16', '17', '18',
            '19', '20', '21',
            '22', '23', '24',
            '25', '26', '27',
            '28', '29', '30',
            '31',
        ],
        'time': [
            '00:00', '01:00', '02:00',
            '03:00', '04:00', '05:00',
            '06:00', '07:00', '08:00',
            '09:00', '10:00', '11:00',
            '12:00', '13:00', '14:00',
            '15:00', '16:00', '17:00',
            '18:00', '19:00', '20:00',
            '21:00', '22:00', '23:00',
        ],
        'area': [
            -20, 15, -37.5,
            20,
        ],
        'format': 'netcdf',
    },
    'download.nc')
```

The end product of the python download is a 52.3 Mb netCDF file, which I will now load and process to produce the daily temperature values to match the daily resolution of the VIIRS chl-*a* data:

``` numberSource
# time units: hours since 1900-01-01 00:00:00.0
origin <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")

ncFile <- "/Volumes/OceanData/ERA5/ERA5_2021_Benguela.nc"
# ncFile <- "~/Downloads/ERA5_2021_Benguela.nc"

era5 <- tidync(ncFile) |>
hyper_tibble() %>%
mutate(time = floor_date(time * 3600 + origin, "day")) |>
reframe(u10 = mean(u10),
v10 = mean(v10),
.by = c(time, longitude, latitude)) |> 
as.data.table()
```

Note that I coerce the data to a **date.table** object since the regridding step (Option 1) uses

## 3 Regridding

Let’s check out their respective spatial resolutions:

``` numberSource
head(unique(chl$longitude))
sort(unique(era5$longitude))

head(unique(chl$latitude))
head(sort(unique(era5$latitude)))
```

There is a huge difference. It is seldom a good idea to go from a low resolution like 25 km to a higher resolution like 4 km, but if you insist, you can do it with bi-linear interpolation. Here we will degrade the 4 km product to match the 25 km resolution of the wind data.

### 3.1 Option 1

Define a new output grid. This will be the coarsest resolution one from ERA5:

``` numberSource
lon.out <- unique(era5$longitude)
lat.out <- sort(unique(era5$latitude))
```

Using the **metR** package and its `Interpolate()` function, interpolate the data to the new coarser resolution grid. I show two approaches: i) a method shown in the function’s help file that uses **data.table**, and ii) a **dplyr** (tidyverse) method using the new `reframe()` function. What `reframe()` does when used within a **dplyr** data pipe is reveal the column names, which can then be given to the function of interest in the usual way; it then returns a dataframe or tibble of arbitrary length. `reframe()` also accommodates the grouping structure within the function itself through the use of the `.by =` argument (i.e. no need for an *a priori* `group_by()`), making the syntax not dissimilar to that of **data.table**’s. A few years ago the **data.table** approach would have been faster, but it seems the new versions of **dplyr** have undergone some significant speed improvements. See the results of the [`system.time()`](https://rdrr.io/r/base/system.time.html) function:

``` numberSource
library(metR)

# using the data.table method
system.time(
  interp_chl <- chl[, Interpolate(chla ~ longitude + latitude, lon.out, lat.out), by = time]
)

head(interp_chl)

# using dplyr
system.time(
  interp_chl <- chl |> 
    reframe(Interpolate(chla ~ longitude + latitude,
                        x.out = lon.out, y.out = lat.out), .by = time) |> 
    as_tibble()
) 

interp_chl
```

Note that daily chl-*a* data are very gappy and hence there are many `NA`s in the interpolated dataset.

Let’s verify that the output grids are now the same:

``` numberSource
head(unique(interp_chl$longitude))
head(unique(era5$longitude))

head(unique(interp_chl$latitude))
head(sort(unique(era5$latitude)))
```

These approaches seem to work… ‘work’ as in they don’t fail. I have not tested the output data to see if the results are believable; for example, how does the `Interpolate()` function handle missing values? I am unsure.

### 3.2 Option 2

Another commonly used function for interpolation lives in the **akima** package. The advantage of this package is that it can do spline interpolation in addition to linear interpolation (`Interpolate()` only does linear interpolation). The disadvantage is that is really does not like `NA`s. This is how it would work:

``` numberSource
# Assuming 'chl' is your data.frame and 'time', 'longitude', 'latitude' and 'chla' are your columns
library(akima)

chl |> 
  reframe(interp_chl = interp(longitude, latitude, chla,
                              xo = lon.out, yo = lat.out))
```

### 3.3 Option 3

The next regridding option is done entirely within a spatial data framework. Traditionally we used the **raster** package, but this has been phased out in favour of [**sf**](https://r-spatial.github.io/sf/) (Simple Features for R), [**stars**](https://r-spatial.github.io/stars/) (Spatiotemporal Arrays: Raster and Vector Data Cubes), and [**terra**](https://rspatial.org/) (Spatial Data Analysis). Here I shall use **sf** and **stars**. Refer to [Spatial Data Science](https://r-spatial.org/book/) for information about these spatial methods; specifically, see [Chapter 7, Introduction to **sf** and **stars**](https://r-spatial.org/book/07-Introsf.html).

R spatial packages are experiencing a rapid evolution and the learning curve might be steep. I think, however, that it’s well worth one’s time as a host of spatial mapping options become available, bringing R closer in functionality to GIS. I am still learning all the various features myself and I am exploring options for integrating the spatial functionality into our marine heatwave workflows.

Make **stars** objects from the gridded data. Let’s start with the chl-*a* data first:

``` numberSource
library(stars)
library(sf)

# EPSG:4326
# WGS 84 -- WGS84 - World Geodetic System 1984, used in GPS
chl_st <- chl |>
  st_as_stars(dims = c("longitude", "latitude", "time"),
              raster = "chlorophyll") |>
  sf::st_set_crs(4326) |> 
  st_warp(crs = st_crs(4326))
```

Here are a few interrogation methods to see info about the data’s spatial extent:

``` numberSource
print(chl_st)
dim(chl_st)
st_dimensions(chl_st)
st_bbox(chl_st)
```

We can also plot the **stars** data directly; here I plot the data on the 34th day in the time series:

``` numberSource
# visualise a time step (day 34):
plot(chl_st[, , , 34])
```

Next we also need to get the ERA5 data into a **stars** format, and we print out some spatial info and make a basic map:

``` numberSource
era5_st <- era5 |>
  st_as_stars(dims = c("longitude", "latitude", "time"),
              raster = c("u10", "v10")) |>
  sf::st_set_crs(4326) |> 
  st_warp(crs = st_crs(4326))
print(era5_st)
st_bbox(era5_st)

plot(era5_st["u10", , , 34])
```

All of this was to bring us to a point where we can do the actual regridding. This is done with the same `st_warp()` function. Previously it was used to make the data conform to a specific coordinate reference system (CRS) but here I use it to perform the regridding:

``` numberSource
chl_st_regrid <- st_warp(src = chl_st, dest = era5_st)
```

Let us see if it worked as advertised:

``` numberSource
print(chl_st_regrid)
st_bbox(chl_st_regrid)

plot(chl_st_regrid[, , , 34])
```

Convert the stars object back to a tibble if necessary:

``` numberSource
chl_st_regrid_df <- as_tibble(chl_st_regrid, xy = TRUE)
chl_st_regrid_df
```

That’s it, folks!

If you have any suggestions about how to do regridding or make the spatial functions more user-friendly in a marine heatwave analysis workflow, please let me know.

## Reuse

[CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/)

## Citation

BibTeX citation:

``` quarto-appendix-bibtex
@online{smit,
  author = {Smit, A. J. and Smit, AJ},
  title = {Regridding Gridded Data},
  date = {},
  url = {https://tangledbank.netlify.app/vignettes/regridding.html},
  langid = {en}
}
```

For attribution, please cite this work as:

Smit AJ, Smit A Regridding gridded data. [https://tangledbank.netlify.app/vignettes/regridding.html.](https://tangledbank.netlify.app/vignettes/regridding.html)
