##### Part One -----
## Read in input
dat = read.table("input.tsv", header = FALSE)
## Define score function
score = function(input) {
    ## Validate input
    if ( !is.character(input) | length(input) != 2 ) {
        stop("Input should be a character vector of length two", call. = FALSE)
    }
    ## Recode input
    their_move = c(A = "rock", B = "paper", C = "scissors")[input[1]]
    my_move = c(X = "rock", Y = "paper", Z = "scissors")[input[2]]
    ## Calculate score
    shape_score = c(rock = 1, paper = 2, scissors = 3)[my_move]
    outcome = ifelse(
        my_move == their_move, "Tie",
        ifelse(
            (my_move == "rock" & their_move == "scissors")
            | (my_move == "scissors" & their_move == "paper")
            | (my_move == "paper" & their_move == "rock"),
            "Win", "Lose"
        )
    )
    outcome_score = c(Win = 6, Lose = 0, Tie = 3)[outcome]
    ## Return score
    return(shape_score + outcome_score)
}
## Calculate the score for each round and sum them
sum(apply(dat, 1, score))

##### Part Two -----
## Define new score function
score2 = function(input) {
    ## Validate input
    if ( !is.character(input) | length(input) != 2 ) {
        stop("Input should be a character vector of length two", call. = FALSE)
    }
    ## Recode input
    their_move = c(A = "rock", B = "paper", C = "scissors")[input[1]]
    outcome = c(X = "lose", Y = "draw",  Z = "win")[input[2]]
    ## Calculate score
    outcome_score = c(win = 6, lose = 0, draw = 3)[outcome]
    my_move = ifelse(
        outcome == "draw", their_move,
        ifelse(
            outcome == "win",
            switch(names(their_move), A = "paper", B = "scissors", C = "rock"),
            switch(names(their_move), A = "scissors", B = "rock", C = "paper")
        )
    )
    shape_score = c(rock = 1, paper = 2, scissors = 3)[my_move]
    ## Return score
    return(shape_score + outcome_score)
}
## Calculate the score for each round and sum them
sum(apply(dat, 1, score2))
