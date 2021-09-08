#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' Select Door
#' 
#' @description
#' Contest chooses 1 door.
#' 
#' @details
#' Contest will only be able to choose 1 door no matter how many options
#' are available.
#' 
#' @param
#' None
#' 
#' @return
#' The function will return one door that is chosen by the contestant.
#'  
#' @examples
#' select_door()
#' 
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Open Goat Door
#' 
#' @description
#' Host will open a door that will have a goat behind it.
#' 
#' @details
#' If contestant selected car, randomly select one of two goats
#'
#' @param 
#' None
#' 
#' @return 
#' Host will open a door with a goat behind it, even if the 
#' contestant chose the door with the car behind it.
#' 
#' @examples
#' open_goat_door()
#' 
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Change Door
#' 
#' @description
#' Contestant has an option to change their door choice.
#' 
#' @details
#' Since the host exposed which of the 2 doors with a goat by opening
#' one of the doors, the contestant now has an option to change their
#' initial pick.
#' 
#' @param 
#' None
#' 
#' @return 
#' Contest will automatically either switch or stay with their initial pick.
#' 
#' @examples
#' change_door()
#' 
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine Winner
#' 
#' @description
#' Once the contestant sticks with their final choice the computer
#' will expose which door has the car behind it and if the contestant
#' is a winner or loser.
#' 
#' @details
#' The final step of the game.
#' 
#' @param 
#' None
#' 
#' @return 
#' This is the final step to find out if the contestant is a winner.
#' 
#' @examples
#' determine_winner()
#' 
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Play Game
#' 
#' @description
#' All the functions are put together to run a full game once.
#' 
#' @details
#' Test to see if switching or staying with initial pick will produce
#' a WIN or a LOSS.
#' 
#' @param 
#' None
#' 
#' @return 
#' A WIN or a LOSS,
#' 
#' @examples
#' play_game()
#' 
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Run a loop.
#' 
#' @description
#' Running a loop several times can show which option is better.
#' 
#' @details
#' This gives results of odds of winning if contestant chooses
#' to switch or stay.
#' 
#' @param 
#' None
#' 
#' @return 
#' Multiple loops run at once.
#' 
#' @examples
#' play_n_games()
#' 
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
