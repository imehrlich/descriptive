# Packages
library(tidyverse)
library(gridExtra)

# Indexing Functions
findActor <- function(actor, data = movie){
  df <- data[data$firstCredit == actor | data$secondCredit == actor,]
  return(df)
}

findDirector <- function(director, data = movie){
  df <- data[data$director == director,]
  return(df)
}

findGenre <- function(genre, data = movie){
  df <- data[data[,genre] == 1,]
  return(df)
}

findLanguage <- function(lang, data = movie){
  data <- data[complete.cases(data$language),]
  df <- data[data$language == lang,]
  return(df)
}

findMovie <- function(title, data = movie){
  df <- data[data$title == title,]
  return(df)
}

findRemakes <- function(remakes, data = movie, order = TRUE){
  data <- data[complete.cases(data$remakes),]
  df <- data[data$remakes == remakes,]
  
  if(order == TRUE){
    df <- arrange(df, df$year)
  }
  return(df)
}

findSeries <- function(series, data = movie, order = TRUE){
  data <- data[complete.cases(data$series),]
  df <- data[data$series == series,]
  
  if(order == TRUE){
    df <- arrange(df, df$year)
  }
  return(df)
}

findYear <- function(year, data = movie){
  df <- data[data$year == year,]
  return(df)
}

lastN <- function(n, data = movie){
  last <- nrow(data)
  first <- last - n + 1
  df <- data[first:last,]
  return(df)
}

# Plotting Functions
plotGenre <- function(data = movie, output = "movie/figures/movieGenre.png"){
  
  freq <- colSums(data[,14:32])
  names <- names(freq)
  names <- paste(toupper(substring(names, 1, 1)),
                 substring(names, 2), sep = "")
  
  genres <- data.frame(genre = names, 
                       count = as.vector(freq))
  
  
  plot <- ggplot(genres, aes(x = genre, y = count)) +
    geom_bar(fill = "lightblue", color = "black", stat = "identity") +
    labs(title = "Movie Genres", x = "Genre", y = "Number of Movies") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(output, scale = 0.4, height = 50, width = 75, unit = "cm")
  
  return(plot)
}

plotLanguage <- function(data = movie, output = "movie/figures/movieLanguage.png"){
  
  data <- data[complete.cases(data$language),]
  
  languages <- sort(table(data$language, useNA = "ifany"), decreasing = TRUE)
  subset <- data.frame(languages)
  
  plot <- ggplot(subset, aes(x = Var1, y = Freq)) +
    geom_bar(fill = "lightblue", color = "black", stat = "identity") +
    labs(title = "Movie Languages", x = "Language", y = "Number of Movies") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(output, height = 16, width = 24, unit = "cm")
  
  return(plot)
}

plotMPAA <- function(data = movie, output = "movie/figures/movieMPAA.png"){
  
  plot <- ggplot(data, aes(x = mpaa)) +
    geom_bar(fill = "lightblue", color = "black") +
    labs(title = "Movie Audience Ratings", x = "Rating", y = "Number of Movies") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(output, height = 16, width = 24, unit = "cm")
  
  return(plot)
}

plotRatings <- function(data = movie, output = "movie/figures/movieRating.png"){
  imdb <-  ggplot(data, aes(x = imdbRating)) +
    geom_histogram(fill = "lightblue", color = "black"
                   , bins = 20, na.rm = TRUE) +
    labs(title = "Movie Ratings",
         x = "IMDB Rating", y = "Number of Movies") +
    scale_x_continuous(limits = c(0,1)) + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  rt <-  ggplot(data, aes(x = rtRating)) +
    geom_histogram(fill = "lightblue", color = "black"
                   , bins = 20, na.rm = TRUE) +
    labs(x = "Rotten Tomatoes Rating", y = "Number of Movies") +
    scale_x_continuous(limits = c(0,1)) + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  meta <-  ggplot(data, aes(x = metaRating)) +
    geom_histogram(fill = "lightblue", color = "black"
                   , bins = 20, na.rm = TRUE) +
    labs(x = "Metacritic Rating", y = "Number of Movies") +
    scale_x_continuous(limits = c(0,1)) + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  grid.arrange(imdb, rt, meta, nrow=3)
  
  plots <- arrangeGrob(imdb, rt, meta, nrow=3)
  ggsave(file=output, plots, height = 24, width = 24, units = "cm")
  
}

plotRuntime <- function(data = movie, output = "movie/figures/movieRuntime.png"){
  
  plot <- ggplot(data, aes(x = runtime)) +
    geom_histogram(fill = "lightblue", color = "black", bins = 20) +
    labs(title = "Movie Runtime", x = "Runtime", y = "Number of Movies") +
    scale_x_continuous(breaks = pretty(data$runtime, n = 10)) + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(output, height = 16, width = 24, unit = "cm")
  
  return(plot)
}

plotTopActor <- function(data = movie, top = 10, output = "movie/figures/movieActors.png"){
  
  actors <- c(as.character(data$firstCredit), as.character(data$secondCredit))
  
  table <- sort(table(actors), decreasing = TRUE)[1:top]
  subset <- data.frame(table)
  
  levels(subset$actors) <- gsub(" ", "\n", levels(subset$actors))
  
  plot <- ggplot(subset, aes(x = actors, y = Freq)) + 
    geom_bar(stat = "identity", fill = "lightblue", color = "black") +
    labs(title = "Top Movie Actors", x = "Actors",
         y = "Number of Movies") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(breaks = pretty(0:max(subset$Freq),
                                       n = max(subset$Freq) %/% 2))
  
  ggsave(output, height = 16, width = 24, unit = "cm")
  
  return(plot)
}

plotTopDirector <- function(data = movie, top = 10, output = "movie/figures/movieDirectors.png"){
  
  table <- sort(table(data$director), decreasing = TRUE)[1:top]
  subset <- data.frame(table)
  
  levels(subset$Var1) <- gsub(" ", "\n", levels(subset$Var1))
  
  plot <- ggplot(subset, aes(x = Var1, y = Freq)) + 
    geom_bar(stat = "identity", fill = "lightblue", color = "black") +
    labs(title = "Top Movie Directors", x = "Director",
         y = "Number of Movies Directed") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(breaks = pretty(0:max(subset$Freq),
                                       n = max(subset$Freq) %/% 2))
  
  ggsave(output, height = 16, width = 24, unit = "cm")
  
  return(plot)
}

plotYear <- function(data = movie, output = "movie/figures/movieYear.png"){
  
  plot <- ggplot(data, aes(x = year)) +
    geom_bar(fill = "lightblue", color = "black") +
    labs(title = "Movie Release Year", x = "Year", y = "Number of Movies") +
    scale_x_continuous(breaks = pretty(data$year, n = 10)) + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(output, height = 16, width = 24, unit = "cm")
  
  return(plot)
}

# Series Functions
seriesOrder <- function(data = series){
  data$order <- 1
  for(i in 2:nrow(data)){
    if(data$series[i] == data$series[i-1]){
      data$order[i] <- data$order[i-1] + 1
    }
  }
  return(data)
}

remakesOrder <- function(data = remakes){
  data$order <- 1
  for(i in 2:nrow(data)){
    if(data$remakes[i] == data$remakes[i-1]){
      data$order[i] <- data$order[i-1] + 1
    }
  }
  return(data)
}

getBreaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

getSeries <- function(seriesNames, data = series){
  seriesFrame <- data[data$series %in% seriesNames,]
  return(seriesFrame)
}

getRemakes <- function(remakesNames, data = remakes){
  remakesFrame <- data[data$remakes %in% remakesNames,]
  return(remakesFrame)
}

getLegend<-function(plot){
  table <- ggplot_gtable(ggplot_build(plot))
  legend <- which(sapply(table$grobs, function(x) x$name) == "guide-box")
  legend <- table$grobs[[legend]]
  return(legend)
}

# Series Plots
plotSeriesRating <- function(data = series, output = "movie/figures/seriesRating.png"){
  
  imdb <- ggplot(data, aes(x = order, y = imdbRating)) +
    geom_line(aes(color = series, linetype = series), size = 1, linetype = "solid") +
    geom_point(aes(color = series)) +
    labs(title = "Movie Rating by Order in Series", subtitle = "IMDB",
         x = "", y = "") +
    scale_x_continuous(limits = c(1, max(data$order)),
                       breaks = getBreaks()) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none")
  
  rt <- ggplot(data, aes(x = order, y = rtRating)) +
    geom_line(aes(color = series, linetype = series), size = 1, linetype = "solid") +
    geom_point(aes(color = series)) +
    labs(subtitle = "Rotten Tomatoes",
         x = "", y = "Rating") +
    scale_x_continuous(limits = c(1, max(data$order)),
                       breaks = getBreaks()) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal() +
    theme(legend.position = "none")
  
  meta <- ggplot(data, aes(x = order, y = metaRating)) +
    geom_line(aes(color = series, linetype = series), size = 1, linetype = "solid") +
    geom_point(aes(color = series)) +
    labs(subtitle = "Metacritic",
         x = "Number in Series (Watched)", y = "") +
    scale_x_continuous(limits = c(1, max(data$order)),
                       breaks = getBreaks()) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal() +
    theme(legend.title = element_blank(), legend.position = "bottom")
  
  legend <- getLegend(meta)
  meta <- meta + theme(legend.position="none")
  
  grid.arrange(imdb, rt, meta, legend, nrow=4, heights = c(1, 1, 1, 0.25))
  
  plots <- arrangeGrob(imdb, rt, meta, legend, nrow=4, heights = c(1, 1, 1, 0.25))
  ggsave(file=output, plots, height = 24, width = 24, units = "cm")
}

plotRemakesRating <- function(data = remakes, output = "movie/figures/remakesRating.png"){
  
  imdb <- ggplot(data, aes(x = order, y = imdbRating)) +
    geom_line(aes(color = remakes, linetype = remakes), size = 1, linetype = "solid") +
    geom_point(aes(color = remakes)) +
    labs(title = "Movie Rating by Remake Order", subtitle = "IMDB",
         x = "", y = "") +
    scale_x_continuous(limits = c(1, max(data$order)),
                       breaks = getBreaks()) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none")
  
  rt <- ggplot(data, aes(x = order, y = rtRating)) +
    geom_line(aes(color = remakes, linetype = remakes), size = 1, linetype = "solid") +
    geom_point(aes(color = remakes)) +
    labs(subtitle = "Rotten Tomatoes",
         x = "", y = "Rating") +
    scale_x_continuous(limits = c(1, max(data$order)),
                       breaks = getBreaks()) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal() +
    theme(legend.position = "none")
  
  meta <- ggplot(data, aes(x = order, y = metaRating)) +
    geom_line(aes(color = remakes, linetype = remakes), size = 1, linetype = "solid") +
    geom_point(aes(color = remakes)) +
    labs(subtitle = "Metacritic",
         x = "Remake Number (Watched)", y = "") +
    scale_x_continuous(limits = c(1, max(data$order)),
                       breaks = getBreaks()) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal() +
    theme(legend.title = element_blank(), legend.position = "bottom")
  
  legend <- getLegend(meta)
  meta <- meta + theme(legend.position="none")
  
  grid.arrange(imdb, rt, meta, legend, nrow=4, heights = c(1, 1, 1, 0.25))
  
  plots <- arrangeGrob(imdb, rt, meta, legend, nrow=4, heights = c(1, 1, 1, 0.25))
  ggsave(file=output, plots, height = 24, width = 24, units = "cm")
}