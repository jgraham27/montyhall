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
#'   Select a door.
#'
#' @description
#'   `select_door()` randomly selects one of the three doors
#'   in the Monty Hall game.
#'
#' @details
#'   The function represents the contestant's initial door
#'   selection. One door is randomly selected from doors
#'   1, 2, and 3.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a single numeric value
#'   between 1 and 3 representing the selected door.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Open a goat door.
#'
#' @description
#'   `open_goat_door()` selects a door containing a goat
#'   for the host to open after the contestant makes an
#'   initial selection.
#'
#' @details
#'   If the contestant initially selects the car, the host
#'   randomly opens one of the two doors containing goats.
#'   If the contestant initially selects a goat, the host
#'   opens the only remaining goat door that was not selected.
#'
#' @param game A length 3 character vector representing the
#'   locations of the two goats and one car.
#' @param a.pick A numeric value between 1 and 3 representing
#'   the contestant's initial door selection.
#'
#' @return The function returns a single numeric value between
#'   1 and 3 representing the door opened by the host.
#'
#' @examples
#'   game <- create_game()
#'   pick <- select_door()
#'   open_goat_door(game, pick)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  
  if( game[ a.pick ] == "car" )
  {
    goat.doors <- doors[ game != "car" ]
    opened.door <- sample( goat.doors, size=1 )
  }
  
  if( game[ a.pick ] == "goat" )
  {
    opened.door <- doors[ game != "car" & doors != a.pick ]
  }
  
  return( opened.door )
}



#' @title
#'   Stay with or switch the selected door.
#'
#' @description
#'   `change_door()` determines the contestant's final door
#'   selection after the host opens a goat door.
#'
#' @details
#'   If `stay` is TRUE, the contestant keeps the original
#'   door selection. If `stay` is FALSE, the contestant
#'   switches to the only unopened door that was not part
#'   of the original selection.
#'
#' @param stay A logical value indicating whether the contestant
#'   stays with the original door. The default is TRUE.
#' @param opened.door A numeric value between 1 and 3 representing
#'   the goat door opened by the host.
#' @param a.pick A numeric value between 1 and 3 representing
#'   the contestant's original door selection.
#'
#' @return The function returns a single numeric value between
#'   1 and 3 representing the contestant's final door selection.
#'
#' @examples
#'   change_door(stay=TRUE, opened.door=2, a.pick=1)
#'   change_door(stay=FALSE, opened.door=2, a.pick=1)
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
#'   Determine the winner of the game.
#'
#' @description
#'   `determine_winner()` determines whether the contestant's
#'   final door selection contains the car or a goat.
#'
#' @details
#'   The function checks the contestant's final door selection
#'   against the game setup. If the selected door contains the
#'   car, the contestant wins. If the selected door contains
#'   a goat, the contestant loses.
#'
#' @param final.pick A numeric value between 1 and 3 representing
#'   the contestant's final door selection.
#' @param game A length 3 character vector representing the
#'   locations of the two goats and one car.
#'
#' @return The function returns a character value of either
#'   "WIN" or "LOSE".
#'
#' @examples
#'   game <- c("goat", "car", "goat")
#'   determine_winner(final.pick=2, game=game)
#'   determine_winner(final.pick=1, game=game)
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
#'   Play one Monty Hall game.
#'
#' @description
#'   `play_game()` runs one complete Monty Hall game and
#'   compares the outcomes of staying with the original
#'   door and switching to the remaining unopened door.
#'
#' @details
#'   The function creates a new game, randomly selects an
#'   initial door, opens a goat door, and then evaluates
#'   both the stay and switch strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a data frame with two rows.
#'   The first row gives the outcome of the stay strategy
#'   and the second row gives the outcome of the switch strategy.
#'
#' @examples
#'   play_game()
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
#'   Play multiple Monty Hall games.
#'
#' @description
#'   `play_n_games()` runs the Monty Hall game multiple times
#'   and records the outcomes for both the stay and switch
#'   strategies.
#'
#' @details
#'   The function repeatedly calls `play_game()` for the number
#'   of games specified by `n`. It combines the results into
#'   one data frame and prints the proportion of wins and losses
#'   for each strategy.
#'
#' @param n A numeric value indicating the number of Monty Hall
#'   games to simulate. The default is 100.
#'
#' @return The function returns a data frame containing the
#'   stay and switch outcomes for all simulated games.
#'
#' @examples
#'   play_n_games(n=10)
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()
  loop.count <- 1
  
  for( i in 1:n )
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )
  
  table( results.df ) %>% 
    prop.table( margin=1 ) %>%
    round( 2 ) %>% 
    print()
  
  return( results.df )
}
