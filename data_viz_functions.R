
# Data Importation --------------------------------------------------------

read.site.metadata <- function(file){
  site_metadata_header <- c("site_id","station_name","station_state","site_time_zone","site_latitude","site_longitude","site_elevation")
  site_metadata <- read.csv(file, nrows = 1, header = F, col.names = site_metadata_header)
}
read.data <- function(file, site_metadata){
  data_header <- c("date","time",
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
  data <- read.csv(file, skip = 1, header = T, col.names = data_header) %>% 
    dplyr::mutate(date_id = dplyr::group_indices(., date),
                  date_local = lubridate::mdy_hm(paste(date, time)) - lubridate::minutes(1),
                  date_global = date_local + lubridate::hours(site_metadata$site_time_zone),
                  year_local = date_local %>% lubridate::year(),
                  month_local = date_local %>% lubridate::month(),
                  day_local = date_local %>% lubridate::day(),
                  hour_local = date_local %>% lubridate::hour(),
                  minute_local = date_local %>% lubridate::minute(),
                  time_local = as.numeric(date_local),
                  year_global = date_global %>% lubridate::year(),
                  month_global = date_global %>% lubridate::month(),
                  hour_global = date_global %>% lubridate::hour(),
                  minute_global = date_global %>% lubridate::minute(),
                  time_global = as.numeric(date_global),
                  date = as.Date(date, format="%m/%d/%Y")) %>% 
    dplyr::group_by(date_id) %>% 
    dplyr::mutate(time_local_rel = time_local-min(time_local),
                  time_global_rel = time_global-min(time_global)) %>% 
    dplyr::ungroup()
}

file <- "./alltmy3a/725090TYA.CSV"
site_metadata <- read.site.metadata(file)
data <- read.data(file,site_metadata)

# Exploration Plots -------------------------------------------------------

plot.availability <- function(data){
  data %>% 
    dplyr::select(date) %>% 
    unique() %>% 
    dplyr::group_by(date) %>% 
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(month = date %>% lubridate::month(),
                  year = date %>% lubridate::year(),
                  date_group = lubridate::dmy(paste(1,month,year))
    ) %>% 
    ggplot2::ggplot()+
    ggbeeswarm::geom_beeswarm(ggplot2::aes(x=date_group,y="H",color=factor(month)), 
                              cex=0.5, groupOnX=F, alpha=0.5, shape=19, na.rm=T)
}
# plot.availability(data)

# Interesting Plots -------------------------------------------------------

plot.oneDayPerMonth <- function(data, variable){
  # Variable plot in first day of month, per year
  if(!(variable %in% names(data))){
    stop("hey, variable does not exist")
  }
  selection <- data %>% 
    dplyr::group_by(year_local, month_local) %>% 
    dplyr::count(day_local) %>% 
    dplyr::top_n(-1,day_local) %>% 
    dplyr::select(-n) %>% 
    dplyr::ungroup()
  
  data %>% 
    merge(selection, sort = T) %>%
    dplyr::arrange(date_local) %>%
    dplyr::mutate(timestamp = date %>% as.numeric) %>% 
    ggplot2::ggplot()+
    ggplot2::geom_path(ggplot2::aes(x=time_local_rel, y=!!ensym(variable), group=date, color=month_local) )+
    ggplot2::scale_color_gradient(low="yellow",high="red") +
    ggplot2::facet_grid(rows=vars(year_local))
}
# variable <- "ETR.Wh.m2."
# plot.oneDayPerMonth(data,variable)

plot.oneYear <- function(data, variable, variable_name=NULL){
  if(!(variable %in% names(data))){
    stop("hey, variable does not exist")
  }
  if(is.null(variable_name)){
    variable_name <- variable
  }
  # ETR independent of year
  format_date <- function(x) {
    as.Date(x, origin=lubridate::origin)%>% format(format="%B")
  }
  data %>% 
    dplyr::mutate(timestamp = date %>% as.numeric,
                  day_of_year = timestamp %% 365 - 1,
                  day_of_year_date = (day_of_year) %>% as.Date(origin=lubridate::origin),
                  date_local_rel = time_local_rel %>% as.POSIXct(origin=lubridate::origin) ) %>% 
    ggplot2::ggplot()+
    # ggplot2::geom_path(data = . %>% dplyr::filter(day_local==1),
    # ggplot2::aes(x=date_local_rel, y=ETR.Wh.m2., group=date), color="black", linetype="dotted",alpha=1)+
    ggplot2::geom_path(ggplot2::aes(x=date_local_rel, y=!!ensym(variable), group=date, color=day_of_year), alpha=0.4)+
    ggplot2::scale_color_gradientn(name="", labels = format_date, breaks=seq(1,365,by = 30),
                                   colors = c( rev(heat.colors(3)), heat.colors(3) ) ) +
    ggplot2::scale_x_datetime(labels = scales::date_format("%H:00"), date_breaks = "2 hours",
                              limits = c(0,24*3600)  %>% as.POSIXct(origin=lubridate::origin) )+
    ggplot2::scale_y_continuous(name=variable_name, breaks = scales::pretty_breaks(n=5))+
    hrbrthemes::theme_ipsum_rc()+
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   legend.key.width = grid::unit(1.5, "cm"),
                   legend.text = ggplot2::element_text(size = ggplot2::rel(1), angle = 50, hjust = 1, vjust=1),
                   legend.position = "bottom"
                   )+
    ggplot2::coord_polar(theta="x")
}
# variable <- "ETR.Wh.m2."
# variable_name <- bquote(ETR~Wh/m^2)
# plot.oneYear(data, variable, variable_name)

# Visualize start, stop and maximum points ------------------------------------------

point.up <- function(x, value){
  (x != value) & (cumsum(x != value) == 1)
}
plot.specificDay <- function(data, date, variable, variable_name=NULL){
  if(!(variable %in% names(data))){
    stop("hey, variable does not exist")
  }
  if(is.null(variable_name)){
    variable_name <- variable
  }

  year <- date %>% lubridate::ymd() %>% lubridate::year()
  month <- date %>% lubridate::ymd() %>% lubridate::month()
  day <- date %>% lubridate::ymd() %>% lubridate::day()
  # if(data %>% dplyr::filter(day_local==day, month_local==month, year_local==year ) %>% nrow() == 0){
  #   stop("Sorry, there is no information about that date")
  # }
  data %>% 
    dplyr::filter(day_local==day, month_local==month, year_local==year ) %>% 
    dplyr::mutate_(x = variable) %>% 
    dplyr::mutate(start = point.up(x,0),
                  stop = rev(point.up(rev(x),0)),
                  maximum = x == max(x),
                  x_max = max(x),
                  date_local_max = date_local[which(maximum)],
                  date_local_start = date_local[which(start)],
                  date_local_stop = date_local[which(stop)] ) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=date_local, y=x), color="#111111", size=1, na.rm=T )+
    ggplot2::geom_segment(ggplot2::aes(x=date_local_max, xend=date_local_max, y=0, yend=max(x)),
                          color="red", linetype="dashed", na.rm=T )+
    ggplot2::geom_label(ggplot2::aes(x=date_local_max, y=max(x), label="Maximum ETR"), color="red", na.rm=T )+
    ggplot2::geom_segment(ggplot2::aes(x=date_local_start, xend=date_local_start, y=0, yend=max(x)),
                          color="orange", linetype="dashed", na.rm=T )+
    ggplot2::geom_label(ggplot2::aes(x=date_local_start, y=max(x), label="Dawn"), color="orange", na.rm=T )+
    ggplot2::geom_segment(ggplot2::aes(x=date_local_stop, xend=date_local_stop, y=0, yend=max(x)),
                          color="blue", linetype="dashed",na.rm=T )+
    ggplot2::geom_label(ggplot2::aes(x=date_local_stop, y=max(x), label="Dusk"), color="blue", na.rm=T )+
    ggplot2::scale_x_datetime(labels = scales::date_format("%H:00"), date_breaks = "2 hours",
                              limits = c(0,24*3600)  %>% as.POSIXct(origin=date) )+
    ggplot2::scale_y_continuous(name = variable_name)+
    ggplot2::scale_color_gradientn(colors = rev(heat.colors(10)))+
    hrbrthemes::theme_ipsum_rc()+
    ggplot2::theme(axis.title.x = ggplot2::element_blank())+
    ggplot2::coord_polar(theta="x")
}
# date <- "1976-01-02"
# variable <- "ETR.Wh.m2."
# variable_name <- bquote(ETR~Wh/m^2)
# plot.specificDay(data,date,variable, variable_name)

plot.maximum <- function(data, variable, variable_name ){
  if(!(variable %in% names(data))){
    stop("hey, variable does not exist")
  }
  if(is.null(variable_name)){
    variable_name <- variable
  }
  data %>% 
    dplyr::group_by(day_local, month_local) %>% 
    dplyr::mutate_(x = variable) %>% 
    dplyr::mutate(start = point.up(x,0),
                  stop = rev(point.up(rev(x),0)),
                  maximum = x == max(x)) %>% 
    dplyr::summarise(x_max = max(x),
                     date_local_max = hour_local[which(maximum)] %>% head(1),
                     date_local_start = hour_local[which(start)],
                     date_local_stop = hour_local[which(stop)]) %>% 
    dplyr::mutate(date_chr = paste(month_local,day_local),
                  date = date_chr %>% as.POSIXct(origin=lubridate::origin, format="%m %d")) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=date, y=x_max))+
    ggplot2::scale_x_datetime(labels = scales::date_format("%B"), date_breaks = "1 month")+
    ggplot2::scale_y_continuous(name = variable_name)+
    hrbrthemes::theme_ipsum_rc()+
    ggplot2::theme(axis.title.x = ggplot2::element_blank())+
    ggplot2::coord_polar(theta="x")
}
# plot.maximum(data,variable, variable_name)
