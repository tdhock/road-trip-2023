library(data.table)
### Read csv file, and lookup lat/long coords if not already
### known/cached, then return joined data.
read_lat_lon <- function(f.csv, address.col){
  f.data <- fread(f.csv)
  no.suffix <- sub("[.][^.]+", "", f.csv)
  lat_lon.csv <- paste0(no.suffix, "_lat_lon.csv")
  lat.lon.old <- if(file.exists(lat_lon.csv)){
    unique(fread(lat_lon.csv))
  }else{
    data.table(address=character(), lat=numeric(), long=numeric())
  }
  on.vec <- c(address=address.col)
  join.dt <- lat.lon.old[f.data, on=on.vec]
  lookup.dt <- join.dt[is.na(lat)]
  lat.lon.new <- tidygeocoder::geo(
    address=lookup.dt[["address"]], 
    method="arcgis")
  lat.lon.all <- rbind(lat.lon.old, lat.lon.new)
  fwrite(lat.lon.all, lat_lon.csv)
  lat.lon.all[f.data, on=on.vec]
}
gas <- read_lat_lon("gas.tsv", "city")
total.USD <- sum(gas$USD)
int.pattern <- list("[0-9]+", as.integer)
date.pattern <- nc::alternatives_with_shared_groups(
  month=int.pattern,
  day=int.pattern,
  year=int.pattern,
  list(month, "/", day, "/", year),
  list(year, "-", month, "-", day))
gas[, date.std := nc::capture_first_vec(
  date, 
  date.pattern
)[, sprintf("%d-%02d-%02d", ifelse(year<2000,2000+year,year), month, day)] ]
gas[, datetime := suppressWarnings(strptime(paste(date.std, time), "%Y-%m-%d %H:%M"))]
STATE.pattern <- list(
  ".*",
  ", ",
  STATE=".*")
gas.more <- nc::capture_first_df(gas, address=STATE.pattern)
(gas.ord <- gas.more[order(datetime), .(datetime, address, USD, lat, long)])
gas.ord[, .(datetime, address, USD, lat)]
library(ggplot2)

USpolygons <- animint2::map_data("state")
center <- function(x)(min(x)+max(x))/2
name.dt <- data.table(USpolygons)[, .(
  long=center(long), lat=center(lat)
), by=region]
border.color <- "grey50"
ggplot()+
  theme_bw()+
  geom_path(aes(
    long, lat, group=group),
    color=border.color,
    data=USpolygons)+
  geom_text(aes(
    long, lat, label=region),
    data=name.dt,
    color=border.color)+
  geom_point(aes(
    long, lat),
    data=gas.ord)+
  geom_path(aes(
    long, lat),
    data=gas.ord)+
  coord_equal()

## from https://stackoverflow.com/questions/60192799/map-data-for-canadian-provinces-in-r
state_prov <- rnaturalearth::ne_states(c("united states of america", "canada"))
if(FALSE){
  states <- raster::getData(country="USA", level=1)
  provinces <- raster::getData(country="Canada", level=1)
}

state_prov_dt <- data.table(
  raster::geom(state_prov)
)[x < 0 & x > -150 & y > 25 & y < 50]
ggplot()+
  theme_bw()+
  geom_path(aes(
    x, y, group=cump),
    color=border.color,
    data=state_prov_dt)+
  geom_point(aes(
    long, lat),
    data=gas.ord)+
  geom_path(aes(
    long, lat),
    data=gas.ord)+
  coord_equal()

overnights <- read_lat_lon("overnights.tsv", "where")
month2int <- c(
  May=5,
  June=6,
  July=7,
  Aug=8)
overnights.day <- nc::capture_first_df(
  overnights, 
  nights=list(
    month=".*?", function(x)month2int[x],
    " ", 
    day=int.pattern)
)[, 
  first.night := sprintf("2023-%02d-%02d 23:00", month, day)
][, 
  datetime := suppressWarnings(strptime(first.night, "%Y-%m-%d %H:%M"))
][]
names(overnights.day)
names(gas.ord)

stops.dt <- rbind(
  data.table(gas.ord[, .(datetime, lat, long, address, stop="gas")]),
  data.table(overnights.day[, .(
    datetime, lat, long, address, stop=ifelse(chez=="", "camping", "indoors"))])
)[order(datetime)]
gg <- ggplot()+
  theme_bw()+
  geom_polygon(aes(
    x, y, group=cump),
    color=border.color,
    fill="grey",
    data=state_prov_dt)+
  geom_point(aes(
    long, lat, color=stop, fill=stop, size=stop),
    shape=21,
    data=stops.dt)+
  geom_path(aes(
    long, lat),
    data=stops.dt)+
  scale_color_manual(values=c(
    camping=NA,
    indoors=NA,
    gas="red"))+
  scale_size_manual(values=c(
    camping=3,
    indoors=3,
    gas=1))+
  scale_fill_manual(values=c(
    camping="black",
    indoors="blue",
    gas=NA))+
  ## geom_text(aes(
  ##   long, lat, label=where),
  ##   data=stops.dt)+
  theme(
    legend.position=c(0.9, 0.3),
    axis.line=element_blank(), axis.text=element_blank(), 
    axis.ticks=element_blank(), axis.title=element_blank())+
  coord_equal()+
  ggtitle(paste0("Road trip May-Aug 2023, total gas cost = $", total.USD))
png("figure-map-of-stops-on-road-trip-2023.png", width=10, height=4, units="in", res=100)
print(gg)
dev.off()

province.key <- rbind(
  data.table(STATE="ON", state.name="Ontario"),
  data.table(STATE="QC", state.name="Québec"))
state.key <- data.table(
  STATE=state.abb, state.name)
(both.key <- rbind(
  data.table(country="Canada", province.key), 
  data.table(country="USA", state.key)
)[, region := tolower(state.name)][])
