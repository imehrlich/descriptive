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
actor <- "Jon Voight";  findActor(actor)[,1:13]
director <- "Denis Villeneuve"; findDirector(director)[,1:13]
genre <- "history"; findGenre(genre)[,1:13]
lang <- "Spanish"; findLanguage(lang)[,1:13]
title <- "Shattered Glass";  findMovie(title)[,1:13]
remakes <- "The Italian Job"; findRemakes(remakes)[,1:13]
series <- "James Bond"; findSeries(series)[,1:13]
year <- 2021;  findYear(year)[,1:13]
n <- 10; lastN(n)[,1:13]
