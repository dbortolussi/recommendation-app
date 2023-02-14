#
# Code for movie recommendation app
# Daniel Bortolussi
# Made with code from:
# Feng Liang: https://campuswire.com/c/G3D46BBBA/feed/1463
# https://github.com/pspachtholz/BookRecommender

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(devtools)
library(data.table)
library(ShinyRatingInput)

source("functions.R")

ui <- shinyUI(dashboardPage(skin = "blue",
  
  dashboardHeader(title='Movie Recommendation'),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("By Genre", tabName="genre", icon=icon("dashboard")),
      menuItem("By CF", tabName="cf", icon = icon("th"))
    )
  ),
  
  dashboardBody(includeCSS("movies.css"),
    
    tabItems(
    tabItem(tabName="genre",
      fluidPage(
        
          selectInput(inputId='genreinput', label='Choose Genre', choices=g.vec
          ),
      
        h1("Recommendation based on chosen genre"),
        tableOutput("genreresult")
      )
    
      ),
    
    tabItem(tabName="cf",
      
        h2("Collaborative Filtering"),
        fluidRow(
          box(title="Rate these movies", width=12, status="info", collapsible=TRUE,style='overflow-x: scroll;height:80rem;overflow-y: scroll;',
              div(class="rateitems", uiOutput('userratings'))
              )
        ),
        fluidRow(
          useShinyjs(),
          box(title="Recommended Movies", width = 12, status = "info",
              br(),
              withBusyIndicatorUI(actionButton("btn", "Click Here", class='btn-warning')),
              br(),
              tableOutput("cfresult")
            
          )
        )
      
    )
    )
  )
))


server <- function(input, output, session) {
  
  output$genreresult = renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- most_popular(input$genreinput)
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "primary", solidHeader = TRUE, title = paste0("Movie ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = recom_result$image_url[(i - 1) * num_movies + j], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(recom_result$Title[(i - 1) * num_movies + j])
            )
            
        )        
      })))
    })
    
  })
  
    output$userratings <- renderUI({
      num_rows <- 20
      num_movies <- 6
      
      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          list(box(width = 2,
                   div(style = "text-align:center", img(src = top.movies$image_url[(i - 1) * num_movies + j], height = 150)),
                   div(style = "text-align:center", strong(top.movies$Title[(i - 1) * num_movies + j])),
                   div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
        })))
      })
    })
    

    df <- eventReactive(input$btn, {
      withBusyIndicatorServer("btn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        #jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        #runjs(jsCode)
        
        # get the user's rating data
        value_list <- reactiveValuesToList(input)
        user_ratings <- get_user_ratings(value_list)
        
        user_ratings$MovieID = top.movies$MovieID[user_ratings$MovieID]
        
        
        user_features = rep(NA, ncol(train))

        user_features[which(colnames(train) %in% paste0('m',user_ratings$MovieID))] = user_ratings$Rating
        
        user_results = ubcfpred(user_features)
        top_ind = colnames(Rmat)[order(-user_results)[1:10]]
        top_ind = as.numeric(top_ind %>% map(function(x) substring(x, 2)))
        user_predicted_ids = movies$MovieID[top_ind]
        recom_results <- data.table(Rank = 1:10, 
                                    MovieID = user_predicted_ids, 
                                    Title = movies$Title[top_ind], 
                                    Predicted_rating =  user_results[top_ind])
        
      }) # still busy
      
    }) # clicked on button
    
    
    # display the recommendations
    output$cfresult <- renderUI({
      num_rows <- 2
      num_movies <- 5
      recom_result <- df()
      e = is.null(recom_result)
      if(e){
        selected_movies = movies[1:10,]
      } else{
        selected_movies = merge(movies, recom_result, by="MovieID") 
        selected_movies = selected_movies[order(selected_movies$Rank),]
      }
      

      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          box(width = 2, status = "primary", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
              
              div(style = "text-align:center", 
                  a(img(src = selected_movies$image_url[(i - 1) * num_movies + j], height = 150))
              ),
              div(style="text-align:center; font-size: 100%", 
                  strong(selected_movies$Title.x[(i - 1) * num_movies + j])
              )
              
          )        
        })))
      })
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
