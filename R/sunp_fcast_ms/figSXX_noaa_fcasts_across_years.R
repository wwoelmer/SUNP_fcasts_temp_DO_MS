library(tidyverse)
site <- 'sunp'
s3 <- arrow::s3_bucket('drivers/noaa/gefs-v12-reprocess/stage2/parquet/0', endpoint_override = "s3.flare-forecast.org", anonymous = TRUE)
buoy_dates <- c(seq.Date(as.Date('2021-08-04'), as.Date('2021-10-17'), by = 'day'),
                seq.Date(as.Date('2022-08-04'), as.Date('2022-10-17'), by = 'day'))
buoy_dates <- as.character(buoy_dates)


# Download the forecast from yesterday and subset to the sites we want
noaa_future <- arrow::open_dataset(s3, partitioning = c("start_date","site_id"), hive_style = FALSE) |>
  dplyr::filter(horizon <= 24,
                start_date %in% buoy_dates,
                site_id %in% "sunp") |> 
  collect()

noaa_future <- noaa_future %>% 
  mutate(year = year(datetime))

noaa_future |> 
  filter(variable == "air_temperature") |> 
  ggplot(aes(x = datetime, y = prediction, group = parameter)) + geom_line()

hist <- ggplot(noaa_future, aes(x = prediction, group = as.factor(year), fill = as.factor(year))) +
  geom_histogram() +
  labs(fill = 'Year') +  
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~variable, scales = 'free')

bplot <- ggplot(noaa_future, aes(x = as.factor(year), y = prediction, group = as.factor(year), fill = as.factor(year))) +
  geom_boxplot() +
  labs(fill = 'Year') +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~variable, scales = 'free')

ggarrange(hist, bplot, common.legend = TRUE)

###############################################################################
## further process noaa variables
use_ler_vars = FALSE

noaa_processed <- noaa_future %>% 
  select(horizon, parameter, reference_datetime, datetime, variable, prediction) %>% 
  pivot_wider(names_from = variable, values_from = prediction) %>% 
  dplyr::arrange(parameter, datetime) |>
  dplyr::mutate(WindSpeed = sqrt(eastward_wind^2 + northward_wind^2)) |>
  dplyr::rename(AirTemp = air_temperature,
                ShortWave = surface_downwelling_shortwave_flux_in_air,
                LongWave = surface_downwelling_longwave_flux_in_air,
                RelHum = relative_humidity,
                Rain = precipitation_flux,
                ensemble = parameter,
                time = datetime) |>
  dplyr::mutate(AirTemp = AirTemp - 273.15,
                RelHum = RelHum * 100,
                RelHum = ifelse(RelHum > 100, 100, RelHum)) |>
  dplyr::mutate_at(dplyr::vars(all_of(c("AirTemp", "ShortWave","LongWave","RelHum","WindSpeed"))),
                   list(~round(., 2))) |>
  dplyr::mutate(Rain = round(Rain, 5),
                time = format(time, format="%Y-%m-%d %H:%M", tz = "UTC")) |>
  dplyr::select(ensemble, time, AirTemp,ShortWave, LongWave, RelHum, WindSpeed,Rain) |>
  dplyr::group_by(ensemble) |>
  dplyr::slice(-dplyr::n()) |>
  dplyr::ungroup() %>% 
  pivot_longer(AirTemp:Rain, names_to = 'variable', values_to = 'prediction') %>% 
  mutate(year = year(time))

hist <- ggplot(noaa_processed, aes(x = prediction, group = as.factor(year), fill = as.factor(year))) +
  geom_histogram(position = 'identity', alpha = 0.8) +
  labs(fill = 'Year') +  
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~variable, scales = 'free')
hist

hist <- ggplot(noaa_processed, aes(x = prediction, group = as.factor(year), fill = as.factor(year))) +
  geom_density(alpha = 0.8) +
  labs(fill = 'Year') +  
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~variable, scales = 'free')
hist

bplot <- ggplot(noaa_processed, aes(x = as.factor(year), y = prediction, group = as.factor(year), fill = as.factor(year))) +
  geom_boxplot() +
  labs(fill = 'Year') +
  scale_fill_manual(values = c('#17BEBB', '#9E2B25')) +
  facet_wrap(~variable, scales = 'free')
bplot

ggarrange(hist, bplot, common.legend = TRUE)


