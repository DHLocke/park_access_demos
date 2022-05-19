# this is a stand alone script intended to run half of a large task as a job
# while the active R session runs the other half

library(tidyverse)
library(sf)
library(tictoc)

(ws_cty_int_lookup <- read_csv('input_data/ws_cty_int_lookup_batch_size_850_2022-04-27.csv'))



(combined_layer <- '/Users/dlocke/SESYNC/sesync_greenspace/park_access/park_access_demos/input_data/combined_blocks_2020_no_water_single_part/combined_blocks_2020_no_water_single_part.gpkg')

# make a string for querying down the combined blocks geopackage
query_start <- 'SELECT * FROM "combined_blocks_2020_no_water_single_part" WHERE '
# query_start <- 'SELECT mult_id, GISJOIN, st, stcode, cty_code, a_blkkm2, a_snglkm2 FROM "combined_blocks_2020_no_water_single_part" WHERE ' # didn't work not sure why..


tic(); (ws <- st_read('input_data/Walk Sheds-selected/10MWS Parks within Urban (CONUS)- TPL (ParkServe) - 2020 - crs102039.shp', as_tibble = TRUE) |> 
          rowid_to_column(var = 'sesync_id') |> 
          mutate(sesync_id = sesync_id -1)); toc() # ID must start at zero to match FID

unique(ws_cty_int_lookup$batch)
172 / 4     # = 43
44 + 43     # = 87
44 + (43)*2 # = 130
tic(); for(i in unique(ws_cty_int_lookup$batch)[131:172]){ # loop through 4th of 4 meta-batches
  
  tic() # iteration clock in
  # cat('working on sesync_id:', i, '\n')
  cat('\n working on batch:', i, '\n')
  
  # generate list of walkshed ids (sesync_ids)
  ws_cty_int_lookup |> 
    filter(batch == i) |> 
    pull(sesync_id) -> i_ws_sesync_ids
  
  # generate query to filter on import the relevant blocks
  ws_cty_int_lookup |> 
    # filter(sesync_id == i) |> # legacy of iterating on sesync_id
    filter(batch == i) |>
    distinct(stcode, cty_code) |> # MAJOR TIME SAVER!!!
    mutate(query = paste0('(stcode = "', stcode, '" AND cty_code = "', cty_code, '") OR '
                          , collapse = '' # not sure why this is needed, but makes it work as intended!
    ),
    query =  ifelse(str_sub(query, nchar(query) - 4, nchar(query)) == ") OR ", # do you end in ") OR "?
                    str_sub(query, 1, nchar(query) - 4),                       # then trim off last 4 chars
                    i_query)                                                   # otherwise chill, you good
    ) |> 
    slice(1) |> # each row repeats when multiple counties.. just take the first row 
    pull(query) -> i_query
  
  # read in only the polygons of ith state-county combo
  tic()
  i_layer <- st_read(
    combined_layer
    , query = paste0(query_start, i_query)
    , as_tibble = TRUE) |>
    st_transform(crs = st_crs(ws)) |> 
    st_make_valid()
  cat('read/query duration: ')
  toc(); beepr::beep()
  
  tic() # start timing the intersection (the workhorse)
  st_intersection(  i_layer                                 # selected blocks
                    , ws |>                                   # ith ws polygon
                      select(sesync_id) |>                    # drop everything but sesync_id
                      # filter(sesync_id == i)                # legacy of iterating on sesync_id
                      filter(sesync_id %in% i_ws_sesync_ids)  # select ws in the ith batch
  ) %>%
    mutate(int_area_km2 = as.double(st_area(.) / 1e+6)) %>% # get intersecting area
    filter(int_area_km2 > 1e-6) %>%                         # intersection has to be greater than 1 m^2
    st_drop_geometry() %>%                                  # drop weight
    group_by(GISJOIN, sesync_id) %>%                        # block and walkshed ids
    mutate(  sum_land_area_km2 = max(a_blkkm2)
             , prop_in = int_area_km2 / sum_land_area_km2) |>
    select(sesync_id, GISJOIN, prop_in) |>                  # use col these names on import!
    write_csv(  file = paste0(getwd(),
                              #'/input_data/walkshed_block_intersections/walkshed_block_intersections.csv'
                              '/input_data/walkshed_block_intersections/walkshed_block_intersections_batch_4_131_to_172.csv'
    )
    , append = TRUE
    , progress = TRUE)
  cat('intersect duration: ')
  toc()   # clock out of the intersection
  
  cat('iteration duration: ')
  toc()    # clock out of the i-th iteration
};       # end loop and clock out

print('loop duration: '); toc()
