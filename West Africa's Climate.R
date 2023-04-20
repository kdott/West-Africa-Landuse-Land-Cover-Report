#Applied Human Geography

#Seminar 2
#14-04-2023

#dr hab. Łukasz Pawlik, UŚ

#Theme: Land cover changes in Western Africa between 1975 and 2013
install.packages ("terra")
library(terra)

#we now download zip file from USGS web site

wa1975 <- rast("C:/Users/kdot/Desktop/Seminar2/west_africa_land-use_land-cover_1975_2km/swa_1975lulc_2km.tif")
wa2000 <- rast("C:/Users/kdot/Desktop/Seminar2/west_africa_land-use_land-cover_2000_2km/swa_2000lulc_2km.tif")
wa2013 <- rast("C:/Users/kdot/Desktop/Seminar2/west_africa_land-use_land-cover_2013_2km/swa_2013lulc_2km.tif")

#we can now plot the data
plot(wa1975)
plot(wa2000)
plot(wa2013)

#we can also plot it using this package
library(tidyterra)

ggplot()+
  geom_spatraster(data = wa1975)

#raster stack
rast_stack <- c(wa1975, wa2000, wa2013)

plot(rast_stack)

#values
wa1975_values <- values(wa1975, na.rm = TRUE)
wa1975_values

summary(wa1975_values)
unique(wa1975_values)

library(dplyr)

#1975
wa1975_values_df <- wa1975_values %>%
  data.frame() %>%
  group_by(class_name) %>%
  summarise(n1975 = n())
  
wa1975_values_df

#we now change the names of land cover classes

#here are numerical classes and land cover classes

LC_classes <- data.frame(class_name = c(0:16, 21:25, 27:29, 31, 32, 78, 98, 99),
                         LC_class = c("no data", "forest", "savanna", "wetland-floodplain",
                                      "steppe", "oasis", "plantation", "mangrove",
                                      "agriculture", "water bodies", "sandy area", "rocky land",
                                      "bare soil", "settlements", "irrigated agriculture",
                                      "gallery forest and riparian forest", "shrub and tree savanna",
                                      "degraded forest", "semi desert", "thicket",
                                      "agriculture in bottomlands and flood recessional",
                                      "woodland", "cropland and fallow with oil palms",
                                      "swamp forest", "sahelian short grass savanna",
                                      "herbaceous savanna", "shrubland", "open mine",
                                      "cloud shadow", "cloud"))

#we now can combine both data frames

wa1975_df1 <- left_join(wa1975_values_df, LC_classes, by = "class_name")

ggplot()+
  geom_col(data=wa1975_df1, aes(x=LC_class, y=n1975))+
  theme(axis.text.x = element_text(angle=90),
        axis.title.x = element_blank())+
  ggtitle("Land cover classes. West Africa - 1975")

#now we can repeat it for the 2000 and 2013 data

#values for the year 2000
wa2000_values <- values(wa2000, na.rm = TRUE)
wa2000_values

wa2000_values_df <- wa2000_values %>%
  data.frame() %>%
  group_by(class_name) %>%
  summarise(n2000 = n())

wa2000_df1 <- left_join(wa2000_values_df, LC_classes, by = "class_name")

ggplot()+
  geom_col(data=wa2000_df1, aes(x=LC_class, y=n2000))+
  theme(axis.text.x = element_text(angle=90),
        axis.title.x = element_blank())+
  ggtitle("Land cover classes. West Africa - 2000")

#values for the year 2013
wa2013_values <- values(wa2013, na.rm = TRUE)
wa2013_values

wa2013_values_df <- wa2013_values %>%
  data.frame() %>%
  group_by(class_name) %>%
  summarise(n2013 = n())

wa2013_df1 <- left_join(wa2013_values_df, LC_classes, by = "class_name")

ggplot()+
  geom_col(data=wa2013_df1, aes(x=LC_class, y=n2013))+
  theme(axis.text.x = element_text(angle=90),
        axis.title.x = element_blank())+
  ggtitle("Land cover classes. West Africa - 2013")

#now we try to combine all plots

#first we combine data

all_years <- LC_classes %>%
  inner_join(wa1975_df1, by = "class_name") %>%
  inner_join(wa2000_df1, by ="class_name") %>%
  inner_join(wa2013_df1, by ="class_name")
all_years

#we now reduce the data frame, some information is repeated

all_years1 <- all_years %>%
  select(LC_class.x, n1975, n2000, n2013) %>%
  dplyr::rename(LC_class = LC_class.x)
all_years1

#we now have changes for 23 classea in three time intervals
#please remember that this data is a number of pixels each 4 sq km

#we will transform the data from wide to long data frame

install.packages("tidyr")
library(tidyr)

all_years2 <- pivot_longer(all_years1, names_to = "year",
                           cols = c("n1975", "n2000", "n2013"))
all_years2

ggplot()+
  geom_col(all_years2, mapping = aes(x=LC_class, y=value,
                                     fill=LC_class))+
  facet_wrap(~year)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=90))

#we can now remove legend or column names under X axis

ggplot()+
  geom_col(all_years2, mapping = aes(x=LC_class, y=value,
                                     fill=LC_class))+
  facet_wrap(~year)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

ggplot()+
  geom_col(all_years2, mapping = aes(x=LC_class, y=value,
                                     fill=LC_class))+
  facet_wrap(~year)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=90, vjust = 0.5),
        legend.position = "none")

#we now want to see the land cover changes only for Nigeria

# install.packages("remotes")
install.packages("remotes")
remotes::install_gitlab("dickoa/rgeoboundaries")

library(rgeoboundaries)
nigeria <- gb_adm0("Nigeria")
plot(nigeria$geometry)

crs(nigeria)#what is the coordinate system?

crs(wa1975)

#we need to reproject the vector data for Nigeria

library(sf)
nigeria1 <- st_transform(nigeria, crs = st_crs(wa1975))

#now we can mask the raster data

wa1975_nigeria <- mask(wa1975, nigeria1)#too large

plot(wa1975_nigeria, type = "classes")   

#zoom in the area
wa1975_nigeria <- crop(wa1975_nigeria, nigeria1)

plot(wa1975_nigeria, type = "classes")  

wa2000_nigeria <- mask(wa2000, nigeria1)
levels(wa2000_nigeria)

#zoom in the area
wa2000_nigeria <- crop(wa2000_nigeria, nigeria1)
plot(wa2000_nigeria, type = "classes") 

wa2013_nigeria <- mask(wa2013, nigeria1)
levels(wa2013_nigeria)

#zoom in the area
wa2013_nigeria <- crop(wa2013_nigeria, nigeria1)
plot(wa2013_nigeria, type = "classes") 

wa2000_nigeria <- crop(wa2000_nigeria, nigeria1)
wa2013_nigeria <- mask(wa2013, nigeria1)
wa2013_nigeria <- crop(wa2013_nigeria, nigeria1)

#we can show these maps in one figure
par(mfrow = c(1,3))
plot(wa1975_nigeria, type = "classes", main = "Nigeria 1975")
plot(wa2000_nigeria, type = "classes", main = "Nigeria 2000")
plot(wa2013_nigeria, type = "classes", main = "Nigeria 2013")

dev.off()#remove graphical parameters of the previous plot

#we will extract values only for Nigeria
wa1975_values_nigeria <- values(wa1975_nigeria)

wa1975_values_df_nigeria <- wa1975_values_nigeria %>%
  data.frame() %>%
  group_by(class_name) %>%
  summarise(n1975 = n())

wa1975_values_df_nigeria

#we combine numerical and text categories
wa1975_df1_nigeria <- left_join(wa1975_values_df_nigeria, LC_classes, by = "class_name")

#this is a plot with modified color scale
plot(wa1975_nigeria, type = "classes", main = "Nigeria 1975",
     levels = wa1975_df1_nigeria$LC_class, col = rainbow(24))

#we can also plot it using ggplot

ggplot()+
  geom_spatraster(data=wa1975_nigeria)+
  scale_fill_discrete(labels = c(wa1975_df1$LC_class))
