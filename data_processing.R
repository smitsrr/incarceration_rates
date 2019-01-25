library(readr); library(dplyr); library(ggplot2); library(rgdal)
library(data.table); library(RColorBrewer) # for brewer.pal(...)
library(gganimate)

df_raw <- read_csv("https://raw.githubusercontent.com/vera-institute/incarceration_trends/master/incarceration_trends.csv")

df_raw %>% str()

# add a row id (for later joining)
df <- df_raw %>% 
  mutate(row_id = row_number())


totals<- df %>%
  mutate(fips = substr(yfips, 5,nchar(yfips))) %>%
  select(year, fips, state, total_pop, 
         total_pop_15to64, total_jail_pop) %>%
  mutate(per_capita = total_jail_pop / total_pop_15to64)





#get the shape files from Tiger
setwd("./tiger_files")
US.states <- readOGR(dsn=".",layer="cb_2016_us_state_5m")
exclude_states<- c("02", "15", "78", "60", "66", "69", "72")
US.states<- US.states[!US.states$GEOID %in% exclude_states,]
us_states<- fortify(US.states)
us.counties <- readOGR(dsn=".",layer="cb_2016_us_county_5m")
us.counties<- us.counties[!us.counties$STATEFP %in% exclude_states,]
county.data <- us.counties@data
county.data <- cbind(id=rownames(county.data),county.data)
county.data <- data.table(county.data)
county.data[,FIPS:=paste0(STATEFP,COUNTYFP)] # this is the state + county FIPS code
# setkey(county.data,FIPS)  

## New way!
totals<-data.table(totals)
setkey(totals, fips)

county.data.2<- left_join(totals, county.data, by=c("fips" = "FIPS"))
county.data.3<- left_join(county.data.2, map.df)


p<-ggplot(map.df) +
  geom_polygon(aes(x = long, y = lat, group = group, fill=per_capita, frame = year)) +
  scale_fill_gradientn("",colours=brewer.pal(9,"YlGnBu")) +
  coord_quickmap()+
  coord_map("polyconic" ) +
  theme_void()+
  geom_polygon(data = us_states, aes(x=long, y=lat, group = group), color = "black", fill = NA)
# save gif
gg_animate(p, "total_per_capita.gif", title_frame =T, 
           ani.width=1600, ani.height=820, dpi=800, interval = .4)


#add the interesting data from above
totals_yr<- data.table(filter(totals, year == 2000))
setkey(totals_yr, fips)

# join the ___ data with the county data.
county.data <- county.data[totals_yr]  # should be joined on fips
setkey(county.data, id)

# make the map layer
map.df<-data.table(fortify(us.counties))
setkey(map.df,id)


# add in interesting data
map.df<- map.df[county.data]  # should be joined on ID

ggplot(map.df, aes(x=long, y=lat, group = group)) +
  geom_polygon(aes(fill = per_capita)) +
  scale_fill_gradientn("",colours=brewer.pal(9,"YlGnBu")) +
  coord_quickmap()+
  coord_map("polyconic" ) +
  theme_void()+
  geom_polygon(data = us_states, aes(x=long, y=lat, group = group), color = "black", fill = NA)


## It would be cool to facet by ethnicity and also produce an over-time annimated gif. 

## New tactic for mapping

map_data<- fortify(us.counties)
map_data<- left_join(map_data, county.data)


## maybe I should take this chance to gganimate incarceration trends over time? 


## per capita by Ethnicity
## per capita by Gender
## 