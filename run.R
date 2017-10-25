library("leaflet")
library("viridis")
library('raster')

df <-read.csv("data.csv",stringsAsFactors = F)

# Fix the date
df$Date <- as.Date(df$Date,"%m/%d/%y")

# Just used for counting
df$tmp <- paste(df$lat,df$long)
# Appends dataframe with number of crashes at location
df$sum <- sapply(df$tmp,function(x){
	return(nrow(df[which(df$tmp==x),]))
})

# Break into categories
df.fatalities <- df[which(df$Level=="Fatalities"),]
df.incapacitated <- df[which(df$Level=="Incap. Injury (A)"),]

# Standard df is everything except fatalities and incapacitated
df <- df[-which(df$Level %in% c("Fatalities","Incap. Injury (A)")),]

# SUMS
df$sum <- sapply(df$tmp,function(x){
	return(nrow(df[which(df$tmp==x),]))
})
df.fatalities$sum <- sapply(df.fatalities$tmp,function(x){
	return(nrow(df.fatalities[which(df.fatalities$tmp==x),]))
})
df.incapacitated$sum <- sapply(df.incapacitated$tmp,function(x){
	return(nrow(df.incapacitated[which(df.incapacitated$tmp==x),]))
})

# Three dataframes, this could be done better
leaflet() %>% addTiles() %>%
	# incidents
	addCircleMarkers(
		radius = df$sum+3,
		color = "#0000FF",
		stroke = FALSE, fillOpacity = 0.2,
		popup = paste0("<strong>",df$sum," Incidents</strong>"),
		group = '<strong style="color:#0000FF">Non-fatal nor Incapacitated</strong>',
		data = df
	) %>%
	# incapacitated
	addCircleMarkers(
		radius = df.incapacitated$sum+6,
		color = "#000000",
		stroke = FALSE, fillOpacity = 0.3,
		popup = paste0("<strong>",df.incapacitated$sum," Incapacitated</strong>"),
		group = '<strong style="color:#000000">Incapacitated</strong>',
		data = df.incapacitated
	) %>%
	# fatalities
	addCircleMarkers(
		radius = df.fatalities$sum+6,
		color = "#FF0000",
		stroke = FALSE, fillOpacity = 0.6,
		popup = paste0("<strong>",df.fatalities$sum," Fatalities</strong>"),
		group = '<strong style="color:#FF0000">Fatality</strong>',
		data = df.fatalities
	) %>% addProviderTiles(providers$CartoDB.Positron) %>%
	addLayersControl(
		overlayGroups = c('<strong style="color:#FF0000">Fatality</strong>','<strong style="color:#000000">Incapacitated</strong>','<strong style="color:#0000FF">Non-fatal nor Incapacitated</strong>'),
		options = layersControlOptions(collapsed = FALSE)) %>%
	fitBounds(-93.2985514,44.9501832,-93.2548996,44.9687897)
