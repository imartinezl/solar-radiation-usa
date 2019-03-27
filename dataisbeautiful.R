
library(dplyr)
library(zeallot)

metadata <- read.csv('TMY3_StationsMeta.csv')

site_metadata_header <- c("site_id","station_name","station_state","site_time_zone","site_latitude","site_longitude","site_elevation")
site_metadata <- read.csv('725090TYA.CSV', nrows = 1, header = F, col.names = site_metadata_header)

data_header <- c("date","local_time",
                 "ETR[Wh/m2]","ETRN[Wh/m2]",
                 "GHI[Wh/m2]","GHI source","GHI unc",
                 "DNI[Wh/m2]","DNI source","DNI unc",
                 "DHI[Wh/m2]","DHI source","DHI unc",
                 "GH illum[lux]","GH illum source","GHI illum unc",
                 "DN illum[lux]","DN illum source","DN illum unc",
                 "DH illum[lux]","DH illum source","DH illum unc",
                 "Zenit lum[cd/m2]","Zenit lum source","Zenit lum unc",
                 "TotCld[tenth]","TotCld source","TotCld unc",
                 "OpqCld[tenth]","OpqCld source","OpqCld unc",
                 "Dry bulb[C]","Dry bulb source","Dry bulb unc",
                 "Dew point bulb[C]","Dew point source","Dew point unc",
                 "RHum[-]","RHum source","RHum unc",
                 "Pressure[mbar]","Pressure source","Pressure unc",
                 "Wdir[degree]","Wdir source","Wdir unc",
                 "Wspd[m/s]","Wspd source","Wspd unc",
                 "Hvis[m]","Hvis source","Hvis unc",
                 "CeilHgt[m]","CeilHgt source","CeilHgt unc",
                 "Pwat[cm]","Pwat source","Pwat unc",
                 "AOD[-]","AOD source","AOD unc",
                 "Alb[-]","Alb source","Alb unc",
                 "Lprecip depth[mm]","Lprecip quantity[hr]","Lprecip depth source","Lprecip depth unc",
                 "PresWth[-]","PresWth source","PresWth unc")
data <- read.csv('725090TYA.CSV', skip = 1, header = T, col.names = data_header) %>% 
  dplyr::mutate(date_local = as.POSIXct(paste0(date, ' ', local_time), format="%d/%m/%Y %H:%M"),
                date_global = date_local + lubridate::hours(site_metadata$site_time_zone),
                local_year = date_local %>% lubridate::year(),
                local_month = date_local %>% lubridate::month(),
                local_hour = date_local %>% lubridate::hour(),
                local_minute = date_local %>% lubridate::minute(),
                local_time = local_hour*60 + local_minute,
                global_year = date_local %>% lubridate::year(),
                global_month = date_local %>% lubridate::month(),
                global_hour = date_global %>% lubridate::hour(),
                global_minute = date_global %>% lubridate::minute(),
                global_time = global_hour*60 + global_minute)

ggplot2::ggplot(data)+
  ggplot2::geom_point(ggplot2::aes(x=date_local, y=ETR.Wh.m2.), na.rm=T)+
  ggplot2::scale_x_datetime()

data %>% 
  # dplyr::filter(local_month == 1) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_path(ggplot2::aes(x=local_time, y=ETR.Wh.m2., 
                                  group = local_year, color=local_year), na.rm=T)+
  ggplot2::facet_grid(rows=vars(local_month), scales = "free")


                 