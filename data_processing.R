library(readr); library(dplyr)
library(ggplot2); library(rgdal)
library(data.table)
library(RColorBrewer) # for brewer.pal(...)
library(gganimate)    # ggplot animations 
library(gifski)       # gganimate dependency
library(transformr)   # gganimate dependency for a polygon

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
county.data.small<-filter(county.data.3, year %in% c('1990', '1995', '2000'))

p<-ggplot(county.data.small) +
  geom_polygon(aes(x = long, y = lat, group = group, fill=per_capita)) +
  scale_fill_gradientn("",colours=brewer.pal(9,"YlGnBu")) +
  coord_quickmap()+
  coord_map("polyconic" ) +
  theme_void()+
  geom_polygon(data = us_states, aes(x=long, y=lat, group = group), color = "black", fill = NA) +
  # gganimate code
  transition_states(year, 
                    transition_length = 2,
                    state_length = 1) 
p
## GRR ! Getting a weird 'subscript out of bounds' error





## maybe I should take this chance to gganimate incarceration trends over time? 


## per capita by Ethnicity
## per capita by Gender
## 