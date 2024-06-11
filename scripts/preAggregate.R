## What do you call an alligator that's already done it's work? A
## preaggre-gator!🐊🤣
library(tidyverse)
library(countrycode)
library(terra)
library(doParallel)
library(readxl)
library(here)

here::i_am("global.R")

registerDoParallel()

createSusceptibleLayer <- function(country, rasterAggregationFactor = 0) {
  stopifnot(rasterAggregationFactor > 1 && is.integer(rasterAggregationFactor))
  stopifnot(length(country) == 1 && is.character(country))

  ## Converts country name to ISO Alpha
  inputISO <- countrycode(country,
                          origin = "country.name",
                          destination = "iso3c")
  url <- paste0("https://data.worldpop.org/GIS/Population",
                "Global_2000_2020_1km_UNadj/2020/",
                inputISO,
                "/",
                tolower(inputISO),
                "_ppp_2020_1km_Aggregated_UNadj.tif")

 rasterPath <- here("tif", basename(url))

  if (!file.exists(rasterPath))
    download.file(url, rasterPath, mode = "wb")

  countryRaster <- rast(rasterPath)
  countryRaster[is.na(countryRaster)] <- 0

  countryRasterPath <- here("preaggregated_data",
                            country,
                            sprintf("%s.grd", country))

  ## Protection while running, because errors are annoying.
  stopifnot(length(countryRasterPath) == 1)
  if (!file.exists(countryRasterPath))
    writeRaster(countryRaster, countryRasterPath)

  return(aggregate(countryRaster,
                   fact = c(rasterAggregationFactor, rasterAggregationFactor),
                   fun = sum,
                   cores = 6,
                   na.rm = TRUE))
}

foreach(row = {
  read_excel("misc/population.xlsx", 1) |>
    filter(shortList == TRUE) |>
    select(Country) |>
    expand_grid(aggregationFactor = seq(2, 100)) %>% # magrittr pipe
    split(., seq(nrow(.)))
}) %do% {
  directory <- here("preaggregated_data", row$Country)
  if(!dir.exists(directory))
    dir.create(directory, recursive = TRUE)
  aggregatedRaster <- createSusceptibleLayer(row$Country, row$aggregationFactor)
  aggregatedRasterPath <- here(directory, sprintf("%s_%s.grd",
                                                  row$Country,
                                                  row$aggregationFactor))

  ## Protection while running, because errors are annoying.
  stopifnot(length(aggregatedRasterPath) == 1)
  if(file.exists(aggregatedRasterPath)) file.remove(aggregatedRasterPath)

  writeRaster(aggregatedRaster, aggregatedRasterPath)
}
