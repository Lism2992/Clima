
library(openxlsx)

library(writexl)


setwd("C:/Users/Lism_/Desktop/ExpSTATIS")




datos = jsonlite::fromJSON("https://opendata.aemet.es/opendata/sh/23536c96")



write_xlsx(datos,"C:\\Users\\Lism_\\Desktop\\ExpSTATIS\\csa1.xlsx")



str(datos)

library(jsonlite)
library(RCurl)  

URL <- "https://opendata.aemet.es/opendata/sh/38d59cb1"

data <- fromJSON(getURL(URL, encoding = "latin1"))

write_xlsx(data,"C:\\Users\\Lism_\\Desktop\\ExpSTATIS\\cfb8.xlsx")

str(data)
