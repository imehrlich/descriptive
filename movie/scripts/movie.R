### Movie Data ###

# Load Movie Functions
source('movie/scripts/movieFunctions.R', echo=TRUE)

# Load Movie Data
movie <- read.csv("movie/data/movie.csv", header = TRUE)



### GENERAL PLOTS ###
plotGenre()
plotLanguage()
plotMPAA()
plotRatings()
plotRuntime()
plotTopActor()
plotTopDirector()
plotYear()



### SERIES AND REMAKES ###
series <- movie[!is.na(movie$series),] %>% 
  arrange(series, year) %>% # Order Data by Series, and then by Year
  seriesOrder()             # Add Column for Order in Series

remakes <- movie[!is.na(movie$remakes),]%>%
  arrange(remakes, year) %>%  # Order Data by Remake, and then by Year
  remakesOrder()              # Add Column for Remake Order

## Plot Series

# plotSeriesRating() # Plot All Series

names <- c("Pirates of the Caribbean", "Ocean's", "Despicable Me", "Mulan", "Cars", "Finding Nemo")
seriesNames <- getSeries(names)
plotSeriesRating(seriesNames, "movie/figures/seriesRatingSelect.png")

seriesLongest <- series[series$series %in% names(sort(table(series$series), decreasing = TRUE)[1:5]),] 
plotSeriesRating(seriesLongest, "movie/figures/seriesRatingLongest.png")


## Plot Remakes

# plotRemakesRating() # Plot All Remakes

names <- c("Infernal Affairs", "Le Diner de Cons", "The Intouchables")
remakesNames <- getRemakes(names)
plotRemakesRating(remakesNames, "movie/figures/remakesRatingSelect.png") 



### INDEX ###
actor <- "Dan Aykroyd";  findActor(actor)[,1:13]
director <- "F. Gary Gray"; findDirector(director)[,1:13]
genre <- "sport"; findGenre(genre)[,1:13]
lang <- "Spanish"; findLanguage(lang)[,1:13]
title <- "Die Another Day";  findMovie(title)[,1:13]
remakes <- "Bedtime Story"; findRemakes(remakes)[,1:13]
series <- "Men in Black"; findSeries(series)[,1:13]
year <- 1962;  findYear(year)[,1:13]
n <- 10; lastN(n)[,1:13]
