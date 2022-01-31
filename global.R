library(dplyr)

df_loc <- read.csv("data/info_location.csv",encoding="UTF-8")
df_loc$Latitude <- jitter(df_loc$Latitude)
df_loc$Longitude <- jitter(df_loc$Longitude)

cleantable <- df_loc %>%
  select(
    Location = Location,
    LocationEN = Locations_Translate,
    Frequence = Frequence,
    Latitude = Latitude,
    Longitude = Longitude
  )

df_title <- read.csv("data/silver_allInfo.csv",encoding="UTF-8")

dat <- data.frame(
  Print = df_title$link_html, 
  Title = df_title$Title,                                             
  #Tags = df_title$Tags,
  Marked = df_title$Title_marked,
  #Link = df_title$link,
  stringsAsFactors = FALSE)




