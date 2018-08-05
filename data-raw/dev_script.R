library(tidyverse)
library(magrittr)

# reverse engineer this shit:
# https://github.com/agehrke/mobile-dmi-byvejr/blob/master/app/scripts/DmiFacade.ts

###

dmi_forecast_places <- function(lon_min = -180, lon_max = 180, lat_min = -90, lat_max = 90, level = 0){

  url <- glue::glue("http://www.dmi.dk/Data4DmiDk/getData?type=forecast&",
                    "lonMin={lon_min}&lonMax={lon_max}&latMin={lat_min}&latMax={lat_max}&level={level}")


  json <- jsonlite::fromJSON(url)

  return(json)

}

dmi_forecast <- function(id){

  url <- glue::glue("http://www.dmi.dk/Data4DmiDk/getData?country=&by_hour=true&id={id}")

  json <- jsonlite::fromJSON(url)

  return(json)

}

dmi_search_place <- function(query){

  url <- glue::glue("http://www.dmi.dk/Data4DmiDk/getData?type=forecast&term={query}")

  out <- jsonlite::fromJSON(url)

  return(out)

}

dmi_parse_forecast <- function(forecast){

  my_days <- paste0("day", 1:3)

  my_time <- lubridate::with_tz(Sys.time(), "CET")
  my_date <- lubridate::as_date(my_time)

  out <- purrr::map_df(my_days, function(my_day){

    day_data <- forecast$weather_data[[my_day]]

    day_data

  })

  time_1 <- out$time_text[1] %>%
    stringr::str_replace_all("kl. ", "") %>%
    stringr::str_c(my_date, " ", .) %>%
    lubridate::ymd_hm(tz = "CET")

  out$timestamp <- time_1 + (3600 * 0:(nrow(out)-1))

  out$precip <- out$precip %>%
    stringr::str_replace_all(",", ".") %>%
    as.double()

  return(out)

}

dmi_get_distance <- function(lon1, lat1, lon2, lat2){

  deg2rad <- function(deg) {
    deg * (pi / 180)
  }

  R <- 6371 # Radius of the earth in km
  dLat = deg2rad(lat2 - lat1) # deg2rad below
  dLon = deg2rad(lon2 - lon1)

  a <-
    sin(dLat / 2) * sin(dLat / 2) +
    cos(deg2rad(lat1)) * cos(deg2rad(lat2)) *
    sin(dLon / 2) * sin(dLon / 2)

  c = 2 * atan2(sqrt(a), sqrt(1 - a))

  d = R * c # Distance in km


  return(d)

}

dmi_find_nearest_station <- function(lon, lat){

  my_dist <- purrr::map_dbl(1:nrow(places), function(i){
    dmi_get_distance(lon, lat, places$longitude[i], places$latitude[i])
  })

  df <- places

  df$distance <- my_dist

  out <- df %>%
    dplyr::filter(distance == min(distance))

  return(out)
}

dmi_get_all_places <- function(){

  lon_min = -180
  lon_max = 180
  lat_min = -90
  lat_max = 90
  level = 11

  lons <- seq(from = lon_min, to = lon_max, by = 20)
  lats <- seq(from = lat_min, to = lat_max, length.out = length(lons))

  df_lon <- tibble::tibble(lon_min = lons[-length(lons)], lon_max = lons[-1]) %>%
    dplyr::mutate(id = 1:n())

  df_lat <- tibble::tibble(lat_min = lats[-length(lats)], lat_max = lats[-1]) %>%
    dplyr::mutate(id = 1:n())

  grid <- expand.grid(df_lon$id, df_lat$id)

  search_grid <- purrr::map2_dfr(grid$Var1, grid$Var2, ~{
    x <- df_lon %>% dplyr::filter(id == .x) %>% dplyr::select(-id)
    y <- df_lat %>% dplyr::filter(id == .y) %>% dplyr::select(-id)
    out <- dplyr::bind_cols(x, y)
    return(out)
  })

  pb <- progress::progress_bar$new(total = nrow(search_grid))
  places <- purrr::map_df(1:nrow(search_grid), function(i){
    pb$tick()
    dmi_forecast_places(search_grid$lon_min[i], search_grid$lon_max[i],
                        search_grid$lat_min[i], search_grid$lat_max[i],
                        level)
  })

  places <- dplyr::distinct(places) %>%
    dplyr::select(id, name, country, longitude, latitude)

  return(places)
}


###

test <- dmi_get_all_places()
forecast <- dmi_forecast(45008660)
dmi_search_place("skanderborg")
dmi_parse_forecast(forecast)
dmi_normalize_city_id <- function(id) {

  id <- as.integer(id)

  if (id > 45000000) {
    return(id - 45000000)
  } else {
    return (id)
  }
}





### Images
dmi_forecast_image <- function(id, type){

  id <- dmi_normalize_city_id(id)

  if (id < 9999) {
    if (type == 'dag1_2') {
      url <- glue::glue('http://servlet.dmi.dk/byvejr/servlet/byvejr_dag1?by={id}&mode=long&eps=true')
    } else {
      url <- glue::glue('http://servlet.dmi.dk/byvejr/servlet/byvejr?by={id}&tabel=dag3_9')
    }
  } else {
    url <- glue::glue("http://servlet.dmi.dk/byvejr/servlet/world_image?city={id}&mode={type}")
  }


  out <- httr::GET(url)

  httr::content(out)



  temp <- tempfile(fileext = ".png")
  download.file(url, temp, mode = 'wb')
  jj <- png::readPNG(temp)

  jj <- readJPEG("y.jpg", native=TRUE)
  plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
  rasterImage(jj,0,0,1,1)

}

id <- "45008660"
type = 'dag1_2'

dmi_normalize_city_id <- function(id) {

  id <- as.integer(id)

  if (id > 45000000) {
    return(id - 45000000)
  } else {
    return (id)
  }
}

dmi_normalize_city_id(id)
