#
# Code for movie recommendation app
# Daniel Bortolussi
# Made with code from:
# Feng Liang: https://campuswire.com/c/G3D46BBBA/feed/1463
# https://github.com/pspachtholz/BookRecommender

library(recommenderlab)
library(stringr)
library(tidyverse)
library(proxy)
library(Matrix)

ratings = read.csv('ratings.dat', 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

movies = readLines('movies.dat')
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)

movies$Title = iconv(movies$Title, "latin1", "UTF-8")

movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))


rating.counts = ratings %>% group_by(MovieID) %>% summarise(count = n())
rating.counts = rating.counts[order(-rating.counts$count),]
top.movies = movies %>% filter(MovieID %in% as.numeric(unlist(rating.counts[1:120, 'MovieID'])))

most_popular = function(genre){
  movies.in.genre = movies[str_detect(movies$Genres, genre),'MovieID']
  ratings.in.genre = filter(ratings, MovieID %in% c(movies.in.genre) )
  rating.counts = ratings.in.genre %>% group_by(MovieID) %>% summarise(count = n())
  rating.counts = rating.counts[order(-rating.counts$count)[1:10],]
  top.movies = movies %>% merge(rating.counts, by="MovieID")
  return(top.movies[order(-top.movies$count),])
}


# Precompute genre table

g.vec = c( 
  ("Animation"),
  ("Children's"),
  ("Comedy"),
  ("Adventure"),
  ("Fantasy"),
  ("Romance"),
  ("Drama"),      
  ("Action"),
  ("Crime"),
  ("Thriller"),
  ("Horror"),
  ("Sci-Fi"),
  ("Documentary"),
  ("War"),
  ("Musical"),
  ("Mystery"),
  ("Film-Noir"),
  ("Western"))

genre.table = NULL

for(g in g.vec){
  genre.table = bind_rows(genre.table, most_popular(g))
}


get_most_popular = function(genre){
  return(genre.table[str_detect(genre.table$Genres, genre),])
}

na.avg = function(w, v){
  if(sum(w)==0){return(NA)}
  ind = !is.na(w) & !is.na(v)
  if(sum(ind)>0){
    w = w[ind]
    v = v[ind]
    return(w %*% v / sum(w))
  }
  return(NA)
}

i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)

train = Rmat[1:1000,]

train.data = train@data
train.data[train.data==0] = NA
trainmeans = rowMeans(train.data, na.rm=TRUE)
train.data = train.data - trainmeans

ubcfpred = function(test.data){

  if(sum(!is.na(test.data))==0){return(rep(NA, length(test.data)))}

  testmean = mean(test.data, na.rm=TRUE)
  test.data = test.data - testmean
  mat1 = t(as.matrix(test.data))
  mat2 = as.matrix(train.data)

  similarities = proxy::simil(t(as.matrix(test.data)), as.matrix(train.data), method='cosine')
  similarities = 0.5*(similarities+1)

  top.20 = order(-similarities)[1:20]
  weights = similarities[top.20]
  nearest = train.data[top.20,]

  weighted.avg = rep(0, length(test.data))

  for(i in 1:length(test.data)){
    weighted.avg[i] = na.avg(weights, as.numeric(nearest[,i]))
  }

  weighted.avg = weighted.avg + testmean

  weighted.avg[!is.na(as.numeric(test.data))] = NA

  return(weighted.avg)
}


get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}


withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      hidden(
        img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    hidden(
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}


withBusyIndicatorServer <- function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                       time = 0.5))
    value
  }, error = function(err) { errorFunc(err, buttonId) })
}


errorFunc <- function(err, buttonId) {
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}

appCSS <- "
.btn-loading-container {
  margin-left: 10px;
  font-size: 1.2em;
}
.btn-done-indicator {
  color: green;
}
.btn-err {
  margin-top: 10px;
  color: red;
}"