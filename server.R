library(shiny)
library(shinydashboard)


# MCP Validation

valid_serve_direction <- function(x){

	x <- gsub("c|V", "", x)

substr(x, 1, 1) %in% c(0, 4:6)
}


valid_ending <- function(x){

	x <- gsub("c|V|\\+", "", x)
	fault <-  grepl("n|w|d|x|g|e|!", substr(x, 2, 2))

	if(fault){
		if(nchar(x) > 2)			
			1
		else
			0
	}
	else{
		if(grepl("\\@|\\*|\\#", substr(x, nchar(x), nchar(x))))
			0
		else
			2
	}
}

valid_rally <- function(x){
	
	if(grepl("f|b|r|s|v|z|o|p|u|y|l|m|h|i|j|k|t|q", x)){
		
		x <- strsplit(gsub("f|b|r|s|v|z|o|p|u|y|l|m|h|i|j|k|t|q", "%", x), "%")[[1]]
		
		x <- x[-1] # Ignore serve
						
		if(length(x) != 0 && grepl("\\@|\\*|\\#", x[length(x)]))
			x <- x[-length(x)]
			
		if(any(!grepl("0|[7-9]$", x)))
			1
		else
			0
	}
	else
		0
}

validity_message <- function(x){
	
	if(x != ""){
		if(nchar(x) == 1){
			if(!valid_serve_direction(x)){
				"Pointer: Points begin with description of serve direction."
			}
			else
				NULL
		}
		else{
			if(!grepl("f|b|r|s|v|z|o|p|u|y|l|m|h|i|j|k|t|q", x)){
				# Not a rally yet
				if(valid_ending(x) == 1)
					"Pointer: Service faults should end with the fault type, nothing else."
				else if(valid_ending(x) == 2)
					"Pointer: All points, other than service faults, need an ending."
				else
					NULL
			}
			else{
				if(valid_rally(x) == 1)
					"Pointer: Shot descriptions have the order: type, (position), direction, and depth. Position is optional."
				else
					NULL
			}
			
		}
	}
	else{
		NULL
	}
}


player_name <- function(name){
	first <- substr(name, 1, 1)
	last <- strsplit(name, split = " ")[[1]]
paste(first, substr(last[length(last)], 1, 1), sep = "")
}

match_score <- function(data){
	
	point_deciding <- ifelse(data$second != "", data$second, data$first)

	minus_lets_time <- gsub("[cV]", "", data$second)

	double_fault <- minus_lets_time != "" & ( (nchar(minus_lets_time) == 2 & grepl("[nwdxge]",substr(minus_lets_time, 2, 2))) |
		(nchar(minus_lets_time) == 3 & grepl("\\+[nwdxge]",substr(minus_lets_time, 2, 3))) )

	if(!grepl("[\\*\\@\\#]", point_deciding[length(point_deciding)]) & !double_fault[length(point_deciding)])
		point_deciding <- point_deciding[-length(point_deciding)]
	
	if(length(point_deciding) == 0)
		return(NA)
	else{
			
		server_ended_point <- sapply(point_deciding, function(x){
			length(grep("[fbrsvzhijklmuyoptq]", strsplit(x, split = "")[[1]])) %% 2 == 0
		})

		hadrally <- grepl("[fbrsvzhijklmuyoptq]", point_deciding)
		server_ended_point[double_fault[1:length(point_deciding)]] <- TRUE
		ended_winner <- grepl("*", point_deciding, fixed = TRUE)
		serve_ending <- grepl("[\\*\\#]", point_deciding)

		serve_won <- (server_ended_point & ended_winner) | (!server_ended_point & !ended_winner) | (!hadrally & serve_ending)
	
	return(serve_won)
	}
}


game_score <- function(serve_won){
		

	if(all(is.na(serve_won))){
					
			games_won <- data.frame(
			player_serving = 1,
			server_won = NA,
			game = 1,
			player1_games = 0,
			player2_games = 0,
			player1_sets = 0,
			player2_sets = 0,
			set_end = FALSE
		)
		
		 current_point <- data.frame(
				serving = 1,
				serve_won = 0,
				return_won = 0
	 		)
	}
	else{	
	game_end <- player1_points <- player2_points <- player1_games <- player2_games <- player1_sets <- player2_sets <- set_end <- tiebreak <- rep(0, length(serve_won))
	
	point_end1 <- point_end2 <- 0
	player_serving <- rep(1, length(serve_won))
	
	player_last <- 1
	in_tiebreak <- FALSE
	
	# Assign end of game
	for(i in 1:length(game_end)){
		
		player_serving[i] <- player_last
		
		# Update serve position and serve/return points if in tiebreak
		if(any(tiebreak == 1)){
			
			max_tiebreak <- max(which(tiebreak == 1))
			in_tiebreak <- !any(game_end[(max_tiebreak+1):length(game_end)] == 1)
			
			if(in_tiebreak){
				
				if((i - max_tiebreak) == 1){ # First Point
					server <- ifelse(player_serving[max_tiebreak] == 1, 2, 1)
					}
				else{
					if(player_serving[max_tiebreak] == 1)
						server <- c(2, rep(c(1, 1, 2, 2), length = (i - max_tiebreak) - 1))
					else
						server <- c(1, rep(c(2, 2, 1, 1), length = (i - max_tiebreak) - 1))
				}
				
				player_serving[i] <- server[length(server)]
				player_last <- player_serving[i]
			}
		}
		
		player1_points <- cumsum((player_serving == 1 & serve_won) | (player_serving != 1 & !serve_won))
		player2_points <- cumsum((player_serving == 2 & serve_won) | (player_serving != 2 & !serve_won))
		
		game_end[i] <- as.numeric(ifelse(in_tiebreak,((player1_points[i] >= (7 + point_end1) & 
			(player1_points[i] - point_end1) > (1 + player2_points[i] - point_end2)) |
		(player2_points[i] >= (7 + point_end2) & 
		(player2_points[i] - point_end2) > (1 + player1_points[i] - point_end1))) , ((player1_points[i] >= (4 + point_end1) & 
			(player1_points[i] - point_end1) > (1 + player2_points[i] - point_end2)) |
		(player2_points[i] >= (4 + point_end2) & 
		(player2_points[i] - point_end2) > (1 + player1_points[i] - point_end1)))))
		
		if(game_end[i] == 1){
			
			point_end1 <- player1_points[i]
			point_end2 <- player2_points[i]
			
			# Assign game winner
				if((player_last == 1 & serve_won[i]) | (player_last != 1 & !serve_won[i])){
					player1_games[i]	 <- 1
				}
				else{
					player2_games[i] <- 1
				}
			
			# Check set winner
			index <- ifelse(any(set_end == 1), max(which(set_end == 1)), 1)
			games1 <- sum(player1_games[(index + 1):length(player1_games)])
			games2 <- sum(player2_games[(index + 1):length(player2_games)])
			
			# All possible ways of ending set
			if(!in_tiebreak & games1 >= 6 & (games1 - games2) >= 2){
				set_end[i] <- 1
				player1_sets[i] <- 1
			}
			else if(!in_tiebreak & games2 >= 6 & (games2 - games1) >= 2){
				set_end[i] <- 1
				player2_sets[i] <- 1
			}
			else if(!in_tiebreak & games1 == 6 & games2 == 6){
				tiebreak[i] <- 1
			}
			else if(in_tiebreak){
				set_end[i] <- 1
				player1_sets[i] <- ifelse(player_last == 1, 1, 0)
				player2_sets[i] <- ifelse(player_last == 2, 1, 0)
			}
			else{
				set_end[i]	 <- 0
			}
			
			# Switch server at the end of the game
			if(!in_tiebreak){
				player_last <- ifelse(player_last == 1, 2, 1)
			}
			else{
				player_last <- player_serving[max_tiebreak] # Same as player on game before tiebreak
			}		
   		}
   	} # End of for loop
   	
   	if(any(game_end == 1)){
   		
   		if(max(which(game_end == 1)) < length(game_end)){
			games_won <- data.frame(
				player_serving = player_serving[game_end == 1],
				server_won = serve_won[game_end == 1],
				game = cumsum(game_end[game_end == 1]),
				player1_games = player1_games[game_end == 1],
				player2_games = player2_games[game_end == 1],
				player1_sets = player1_sets[game_end == 1],
				player2_sets = player2_sets[game_end == 1],
				set_end = set_end[game_end == 1]
			)
		
			 current_point <- data.frame(
					serving = player_serving[(max(which(game_end == 1)) + 1):length(serve_won)],
					serve_won = cumsum(serve_won[(max(which(game_end == 1)) + 1):length(serve_won)]),
					return_won = cumsum(!serve_won[(max(which(game_end == 1)) + 1):length(serve_won)])
				)
			}
		else{
			games_won <- data.frame(
				player_serving = player_serving[game_end == 1],
				server_won = serve_won[game_end == 1],
				game = cumsum(game_end[game_end == 1]),
				player1_games = player1_games[game_end == 1],
				player2_games = player2_games[game_end == 1],
				player1_sets = player1_sets[game_end == 1],
				player2_sets = player2_sets[game_end == 1],
				set_end = set_end[game_end == 1]
			)
		
		 current_point <- data.frame(
				serving = ifelse(player_serving[length(game_end)] == 1, 2, 1),
				serve_won = 0,
				return_won = 0
	 		)
	 	  }
		}
 	else{

		games_won <- data.frame(
			player_serving = 1,
			server_won = NA,
			game = 1,
			player1_games = 0,
			player2_games = 0,
			player1_sets = 0,
			player2_sets = 0,
			set_end = FALSE
		)
		
		 current_point <- data.frame(
				serving = rep(1, length(serve_won)),
				serve_won = cumsum(serve_won),
				return_won = cumsum(!serve_won)
	 		)
	  }
   }
list(score = games_won, current_point = current_point)
}
	
assign_games <- function(obj){
	if(!any(obj == 1)){
		rep(1, length(obj))
	}
	else{
		ends <- which(obj == 1)
		seq_index <- c(ends[1], diff(ends))
		seq_index <- c(seq_index, length(obj) - sum(seq_index))
	rep(1:length(seq_index), seq_index)
 }
}	

match_results <- function(serve_won){
		

	if(all(is.na(serve_won))){
					
		games_won <- data.frame(
			player_serving = 1,
			server_won = NA,
			game_won = 1,
			player1_games = 0,
			player2_games = 0,
			player1_sets = 0,
			player2_sets = 0,
			set_end = FALSE,
			game = 1
		)
		
	}
	else{	

	game_end <- player1_points <- player2_points <- player1_games <- player2_games <- player1_sets <- player2_sets <- set_end <- tiebreak <- rep(0, length(serve_won))
	
	point_end1 <- point_end2 <- 0
	player_serving <- rep(1, length(serve_won))
	
	player_last <- 1
	in_tiebreak <- FALSE
	
	# Assign end of game
	for(i in 1:length(game_end)){
		
		player_serving[i] <- player_last
		
		# Update serve position and serve/return points if in tiebreak
		if(any(tiebreak == 1)){
			
			max_tiebreak <- max(which(tiebreak == 1))
			in_tiebreak <- !any(game_end[(max_tiebreak+1):length(game_end)] == 1)
			
			if(in_tiebreak){
				
				if((i - max_tiebreak) == 1){ # First Point
					server <- ifelse(player_serving[max_tiebreak] == 1, 2, 1)
					}
				else{
					if(player_serving[max_tiebreak] == 1)
						server <- c(2, rep(c(1, 1, 2, 2), length = (i - max_tiebreak) - 1))
					else
						server <- c(1, rep(c(2, 2, 1, 1), length = (i - max_tiebreak) - 1))
				}
				
				player_serving[i] <- server[length(server)]
				player_last <- player_serving[i]
			}
		}
		
		player1_points <- cumsum((player_serving == 1 & serve_won) | (player_serving != 1 & !serve_won))
		player2_points <- cumsum((player_serving == 2 & serve_won) | (player_serving != 2 & !serve_won))
		
		game_end[i] <- as.numeric(ifelse(in_tiebreak,((player1_points[i] >= (7 + point_end1) & 
			(player1_points[i] - point_end1) > (1 + player2_points[i] - point_end2)) |
		(player2_points[i] >= (7 + point_end2) & 
		(player2_points[i] - point_end2) > (1 + player1_points[i] - point_end1))) , ((player1_points[i] >= (4 + point_end1) & 
			(player1_points[i] - point_end1) > (1 + player2_points[i] - point_end2)) |
		(player2_points[i] >= (4 + point_end2) & 
		(player2_points[i] - point_end2) > (1 + player1_points[i] - point_end1)))))
		
		if(game_end[i] == 1){
			
			point_end1 <- player1_points[i]
			point_end2 <- player2_points[i]
			
			# Assign game winner
				if((player_last == 1 & serve_won[i]) | (player_last != 1 & !serve_won[i])){
					player1_games[i]	 <- 1
				}
				else{
					player2_games[i] <- 1
				}
			
			# Check set winner
			index <- ifelse(any(set_end == 1), max(which(set_end == 1)), 1)
			games1 <- sum(player1_games[(index + 1):length(player1_games)])
			games2 <- sum(player2_games[(index + 1):length(player2_games)])
			
			# All possible ways of ending set
			if(!in_tiebreak & games1 >= 6 & (games1 - games2) >= 2){
				set_end[i] <- 1
				player1_sets[i] <- 1
			}
			else if(!in_tiebreak & games2 >= 6 & (games2 - games1) >= 2){
				set_end[i] <- 1
				player2_sets[i] <- 1
			}
			else if(!in_tiebreak & games1 == 6 & games2 == 6){
				tiebreak[i] <- 1
			}
			else if(in_tiebreak){
				set_end[i] <- 1
				player1_sets[i] <- ifelse(player_last == 1, 1, 0)
				player2_sets[i] <- ifelse(player_last == 2, 1, 0)
			}
			else{
				set_end[i]	 <- 0
			}
			
			# Switch server at the end of the game
			if(!in_tiebreak){
				player_last <- ifelse(player_last == 1, 2, 1)
			}
			else{
				player_last <- player_serving[max_tiebreak] # Same as player on game before tiebreak
			}		
   		}
   	} # End of for loop
   	
   	games_won <- data.frame(
				player_serving = player_serving,
				server_won = serve_won,
				game_won = game_end,
				player1_games = player1_games,
				player2_games = player2_games,
				player1_sets = player1_sets,
				player2_sets = player2_sets,
				set_end = set_end
			)

   	games_won$game <- assign_games(game_end)
   }

   games_split <- split(games_won, f = games_won$game)
   
   points_won <- lapply(games_split, function(x){

   		serve_points <- cumsum(x$server_won)
   		return_points <- cumsum(!x$server_won)
   		server_won <- serve_points[length(serve_points)] > return_points[length(serve_points)]

   		for(i in 1:nrow(x)){
	   		if(x$player1_games[i] != 6 & x$player2_games[i] != 6){ # Not tiebreak

			if(return_points[i] >= 3 & serve_points[i] >= 3){
				if(return_points[i] == serve_points[i]){
					return_points[i] <- "40"
					serve_points[i] <- "40"
				}
				else if(return_points[i] > serve_points[i]){
					return_points[i] <- "Ad"
					serve_points[i] <- "40"			
				}
				else{
					return_points[i] <- "40"
					serve_points[i] <- "Ad"
				}

			}
			else{
		    serve_points[i] <- ifelse(serve_points[i] == 0, "0",
				ifelse(serve_points[i] == 1, "15",
					ifelse(serve_points[i] == 2, "30", "40")))
							
			return_points[i] <- ifelse(return_points[i] == 0, "0",
				ifelse(return_points[i] == 1, "15",
					ifelse(return_points[i] == 2, "30", "40")))	
			}	
		  }
		}

		if(any(x$game_won == 1)){
			if(server_won)
				serve_points[length(serve_points)] <- "GM"
			else
				return_points[length(serve_points)] <- "GM"
		}

			serve_points[length(serve_points)]

		if(x$player_serving[1] == 1){
			player1_points <- serve_points
			player2_points <- return_points
		}
		else{
			player1_points <- return_points
			player2_points <- serve_points		
		}

		data.frame(
				player1_points = player1_points,
				player2_points = player2_points
			)
   	})
 

 games_won$player1_points <- unlist(lapply(points_won, function(x) x$player1_points))
 games_won$player2_points <- unlist(lapply(points_won, function(x) x$player2_points))

games_won	
}
		
	
match_summary <- function(obj){
	
	game_start <- 0
	
	if(any(obj$score$set_end == 1)) game_start <- max(which(obj$score$set_end == 1))

	if(game_start == nrow(obj$score)){
		result <- data.frame(
			serve_points = obj$current_point$serve_won[nrow(obj$current_point)],
			return_points = obj$current_point$return_won[nrow(obj$current_point)],
			player1_games = 0,
			player2_games = 0,
			player1_sets = sum(obj$score$player1_sets),
			player2_sets = sum(obj$score$player2_sets)
		)
	}
	else{
		game_start <- game_start + 1

		result <- data.frame(
			serve_points = obj$current_point$serve_won[nrow(obj$current_point)],
			return_points = obj$current_point$return_won[nrow(obj$current_point)],
			player1_games = sum(obj$score$player1_games[game_start:nrow(obj$score)]),
			player2_games = sum(obj$score$player2_games[game_start:nrow(obj$score)]),
			player1_sets = sum(obj$score$player1_sets),
			player2_sets = sum(obj$score$player2_sets)
		)
	}

	if(result$player1_games != 6 & result$player2_games != 6){ # Not tiebreak

		if(result$return_points >= 3 & result$serve_points >= 3){
			if(result$return_points == result$serve_points){
				result$return_points <- "40"
				result$serve_points <- "40"
			}
			else if(result$return_points > result$serve_points){
				result$return_points <- "Ad"
				result$serve_points <- "40"			
			}
			else{
				result$return_points <- "40"
				result$serve_points <- "Ad"
			}

		}
		else{
	    result$serve_points <- ifelse(result$serve_points == 0, "0",
			ifelse(result$serve_points == 1, "15",
				ifelse(result$serve_points == 2, "30", "40")))
						
		result$return_points <- ifelse(result$return_points == 0, "0",
			ifelse(result$return_points == 1, "15",
				ifelse(result$return_points == 2, "30", "40")))	
		}	
	}

	if(obj$current_point$serving[length(obj$current_point$serving)] == 1){
		result$player1_points <- result$serve_points
		result$player2_points <- result$return_points
	}
	else{
		result$player1_points <- result$return_points
		result$player2_points <- result$serve_points		
	}

result	
}


f <- function(stroke, next_point = TRUE, reset = FALSE, data){
	
	second_serve_pattern <- "[nwdxge]"
	second_serve_approach_pattern <- "\\+[nwdxge]"
	minus_lets_time <- gsub("[cV]", "", data$first)

	if(reset){		
		data$first[nrow(data)] <- ""
		data$second[nrow(data)] <- ""
	}
	else if(next_point){
		data <- rbind(data, data.frame(first = "", second = "", stringsAsFactors = FALSE))
	}
	else{
		# Add to current point; check for serve pattern
		if(!grepl(second_serve_pattern, substr(minus_lets_time[nrow(data)], 2, 2)) & 
			!grepl(second_serve_approach_pattern, substr(minus_lets_time[nrow(data)], 2, 3)))
			data$first[nrow(data)] <- paste(data$first[nrow(data)], stroke, sep = "")
		else		
			data$second[nrow(data)] <- paste(data$second[nrow(data)], stroke, sep = "")	
	}

return(data)
}

translate <- function(seq){
	# Stroke endings
	seq <- gsub("([fbrsvzopuylmhijktq])", "/\\1", seq)
	seq <- sapply(strsplit(seq, split = "/")[[1]], description)

	if(grepl("Winner", seq[1]))
		seq[1] <- sub("Winner", "Ace", seq[1])
	if(grepl("Forced", seq[1]))
		seq[1] <- sub("Forced Error", "Unreturnable", seq[1])

paste(seq, collapse = "/", sep = "")
}

description <- function(seq){

	seq <- strsplit(seq, split = "")[[1]]

	wide <- grep("4", seq, fixed = TRUE)
	body <- grep("5", seq, fixed = TRUE)
	t <- grep("6", seq, fixed = TRUE)
	unknown <- grep("0", seq, fixed = TRUE)
	approach <- grep("+", seq, fixed = TRUE)
	net <- grep("-", seq, fixed = TRUE)
	baseline <- grep("=", seq, fixed = TRUE)
	clipped <- grep(";", seq, fixed = TRUE)
	within <- grep("7", seq, fixed = TRUE)
	behind <- grep("8", seq, fixed = TRUE)
	on_baseline <- grep("9", seq, fixed = TRUE)
	let <- grep("c", seq, fixed = TRUE)
	time <- grep("V", seq, fixed = TRUE)
	net_serve <- grep("n", seq, fixed = TRUE)
	deep <- grep("d", seq, fixed = TRUE)
	wide_fault <- grep("w", seq, fixed = TRUE)
	wide_deep <- grep("x", seq, fixed = TRUE)
	foot_fault <- grep("g", seq, fixed = TRUE)
	unknown_fault <- grep("e", seq, fixed = TRUE)
	shank <- grep("!", seq, fixed = TRUE)
	f_ground <- grep("f", seq, fixed = TRUE)
	b_ground <- grep("b", seq, fixed = TRUE)
	f_slice <- grep("r", seq, fixed = TRUE)
	b_slice <- grep("s", seq, fixed = TRUE)
	f_volley <- grep("v", seq, fixed = TRUE)
	b_volley <- grep("z", seq, fixed = TRUE)
	f_hvolley <- grep("h", seq, fixed = TRUE)
	b_hvolley <- grep("i", seq, fixed = TRUE)
	f_svolley <- grep("j", seq, fixed = TRUE)
	b_svolley <- grep("k", seq, fixed = TRUE) 
	f_lob <- grep("l", seq, fixed = TRUE)
	b_lob <- grep("m", seq, fixed = TRUE) 	
	f_drop <- grep("u", seq, fixed = TRUE)
	b_drop <- grep("y", seq, fixed = TRUE)
	f_over <- grep("o", seq, fixed = TRUE)
	b_over <- grep("p", seq, fixed = TRUE)
	trick <- grep("t", seq, fixed = TRUE)
	unknown_shot <- grep("q", seq, fixed = TRUE)
	left <- grep("3", seq, fixed = TRUE)
	right <- grep("1", seq, fixed = TRUE)
	middle <- grep("2", seq, fixed = TRUE)

	winner <- grep("*", seq, fixed = TRUE) # Need to check for Ace
	forced <- grep("#", seq, fixed = TRUE)
	unforced <- grep("@", seq, fixed = TRUE)

	# Add conditionals
	if(length(wide) != 0)
		seq[wide] <- "Wide Serve"

	if(length(body) != 0)
		seq[body] <- "Body Serve"

	if(length(t) != 0)
		seq[t] <- "Serve Down T"

	if(length(unknown) != 0)
		seq[unknown] <- "Unknown Serve"

	if(length(net) != 0)
		seq[net] <- "At Net"

	if(length(approach) != 0)
		seq[approach] <- "Approach"

	if(length(on_baseline) != 0)
		seq[on_baseline] <- "At Baseline"

	if(length(clipped) != 0)
		seq[clipped] <- "Clipped Net"

	if(length(within) != 0)
		seq[within] <- "In Service Box"

	if(length(behind) != 0)
		seq[behind] <- "Behind Service Line"

	if(length(baseline) != 0)
		seq[baseline] <- "By Baseline"

	if(length(let) != 0)
		seq[let] <- "Let"

	if(length(time) != 0)
		seq[time] <- "Time Violation"

	if(length(net_serve) != 0)
		seq[net_serve] <- "Net"

	if(length(deep) != 0)
		seq[deep] <- "Deep"

	if(length(wide_fault) != 0)
		seq[wide_fault] <- "Wide"

	if(length(wide_deep) != 0)
		seq[wide_deep] <- "Wide Deep"

	if(length(foot_fault) != 0)
		seq[foot_fault] <- "Foot Fault"

	if(length(unknown_fault) != 0)
		seq[unknown_fault] <- "Unknown"

	if(length(shank) != 0)
		seq[shank] <- "Shank"

	if(length(f_ground) != 0)
		seq[f_ground] <- "Forehand"

	if(length(b_ground) != 0)
		seq[b_ground] <- "Backhand"

	if(length(f_slice) != 0)
		seq[f_slice] <- "Forehand Slice"

	if(length(b_slice) != 0)
		seq[b_slice] <- "Backhand Slice"

	if(length(f_volley) != 0)
		seq[f_volley] <- "Forehand Volley"

	if(length(b_volley) != 0)
		seq[b_volley] <- "Backhand Volley"

	if(length(f_hvolley) != 0)
		seq[f_hvolley] <- "Forehand Half-Volley"

	if(length(f_lob) != 0)
		seq[f_lob] <- "Forehand Lob"

	if(length(b_lob) != 0)
		seq[b_lob] <- "Backhand Lob"

	if(length(b_hvolley) != 0)
		seq[b_hvolley] <- "Backhand Half-Volley"

	if(length(f_svolley) != 0)
		seq[f_svolley] <- "Forehand Swinging Volley"

	if(length(b_svolley) != 0)
		seq[b_svolley] <- "Backhand Swinging Volley"

	if(length(f_drop) != 0)
		seq[f_drop] <- "Forehand Drop Shot"

	if(length(b_drop) != 0)
		seq[b_drop] <- "Backhand Drop Shot"

	if(length(f_over) != 0)
		seq[f_over] <- "Forehand Overhead"

	if(length(b_over) != 0)
		seq[b_over] <- "Backhand Overhead"

	if(length(trick) != 0)
		seq[trick] <- "Trick Shot"

	if(length(unknown_shot) != 0)
		seq[unknown_shot] <- "Unknown Shot"

	if(length(left) != 0)
		seq[left] <- "To Player's Left"

	if(length(right) != 0)
		seq[right] <- "To Player's Right"

	if(length(middle) != 0)
		seq[middle] <- "To the Middle"

	if(length(winner) != 0)
		seq[winner] <- "Winner"

	if(length(forced) != 0)
		seq[forced] <- "Forced Error"

	if(length(unforced) != 0)
		seq[unforced] <- "Unforced Error"

paste(seq, collapse = "-", sep = "")
}

shinyServer( function(input, output) {

	updates <- reactiveValues(data = data.frame(first = "", second = "", stringsAsFactors = FALSE))
	
	updateData <- reactive({
		 	 the_data <- updates$data
						updates$data <- f("", next_point = TRUE, data = the_data)		
	})

	resetData <- reactive({
		 	 the_data <- updates$data
						updates$data <- f("", reset = TRUE, data = the_data)		
	})

	resetLastData <- reactive({
			the_data <- updates$data
			the_data <- the_data[!(the_data$first == "" & the_data$second == ""),]
		if(nrow(the_data) <= 1)
			updates$data <- data.frame(first = "", second = "", stringsAsFactors = FALSE)
		else
			updates$data <- the_data[-nrow(the_data),]
	})

	output$pointer <- renderText({
		temp <- updates$data[nrow(updates$data),,drop = FALSE]
		current_point <- ifelse(temp$second == "", temp$first, temp$second)
	validity_message(current_point)
	})
	
	
	mcp_format <- function(file) {
		
  	  	data <- updateData()
  	  	data <- data[!(data$first == "" & data$second == ""),]
  	  	results <- match_results(match_score(data))
  	  	
  	  	labels <- c(
  	  		"Player 1",
  	  		"Player 2",
  	  		"Pl 1 hand",
  	  		"Pl 2 hand",
  	  		"Gender",
  	  		"Date",
  	  		"Tournament",
  	  		"Round",
  	  		"Time",
  	  		"Court",
  	  		"Surface",
  	  		"Umpire",
  	  		"Best of",
  	  		"Final TB?",
  	  		"Charted by"
  	  	)
		
		M <- matrix("", nrow = nrow(data) + 17, ncol = 2 + ncol(results))
		M[1:15,1] <- labels
		M[1:15,2] <-			c(
			input$player1,
			input$player2,
			input$player1_hand,
			input$player2_hand,
			input$tour,
			input$date,
			input$tournament,
			input$round,
			input$time,
			input$court,
			input$surface,
			input$umpire,
			input$bestof5,
			input$finaltb,
			input$charter
		)
		
		M[17,] <- c("1st", "2nd", names(results))
		M[18:nrow(M), 1:2] <- as.matrix(data)
		M[18:nrow(M), 3:ncol(M)] <- as.matrix(results)
		
  	 write.table(M, file, row.names = FALSE, col.names = FALSE, sep = ",")
   	 }
	

  	output$downloadData <- downloadHandler(
  	  filename = function() { paste(input$filename, '.csv', sep='') },
  	  content = mcp_format
  	  )

	 observeEvent(input$new, {
						updateData() })

 	observeEvent(input$reset, {
		resetData() })

 	observeEvent(input$resetLast, {
		resetLastData() })

 	output$score_table <- renderTable({
 		output <- match_summary(game_score(match_score(updates$data)))
 		scores <- data.frame(
 			Player = c(player_name(input$player1), player_name(input$player2)),
 			Points = c(output$player1_points, output$player2_points),
 			Games = c(output$player1_games, output$player2_games),
 			Sets = c(output$player1_sets, output$player2_sets)
 			)
 	scores
 	}, digits = 0, align = "llccc", include.rownames = FALSE)

	output$point_score <- renderValueBox({
		output <- match_summary(game_score(match_score(updates$data)))
    valueBox(
      paste(output$player1_points, output$player2_points, sep = "-"), "Point Score", icon = icon("record"), color = "olive"
    )
  })

	output$game_score <- renderValueBox({
		output <- match_summary(game_score(match_score(updates$data)))
    valueBox(
      paste(output$player1_games, output$player2_games, sep = "-"), "Game Score", icon = icon("record"), color = "olive"
    )
  })

	output$set_score <- renderValueBox({
		output <- match_summary(game_score(match_score(updates$data)))
    valueBox(
       paste(output$player1_sets, output$player2_sets, sep = "-"), "Set Score", icon = icon("record"), color = "olive"
    )
  })


 output$first_score <- renderValueBox({
  	temp <- updates$data[nrow(updates$data),,drop = FALSE]
  	if(temp$second != ""){
  		serve <- "Second Serve"
  		point <- p(translate(temp$second), br(), paste("Code:", temp$second))
  	}
  	else{
  	  	serve <- "First Serve"
  		point <- p(translate(temp$first), br(), paste("Code:",temp$first))	
  	}
    valueBox(
      p(serve), point, color = "green"
    )
  })


  	observeEvent(input$unknown_direction, {
		 	 the_data <- updates$data
						updates$data <- f("0", next_point = FALSE, data = the_data)
						})

	observeEvent(input$outwide, {
		 	 the_data <- updates$data
				updates$data <- f("4", next_point = FALSE, data = the_data) })

	observeEvent(input$approach, {
		 	 the_data <- updates$data
				updates$data <- f("+", next_point = FALSE, data = the_data) })

	observeEvent(input$serve_volley, {
		 	 the_data <- updates$data
				updates$data <- f("+", next_point = FALSE, data = the_data) })

	observeEvent(input$net, {
		 	 the_data <- updates$data
				updates$data <- f("-", next_point = FALSE, data = the_data) })

	observeEvent(input$baseline_shot, {
		 	 the_data <- updates$data
				updates$data <- f("=", next_point = FALSE, data = the_data) })

	observeEvent(input$clipped, {
		 	 the_data <- updates$data
				updates$data <- f(";", next_point = FALSE, data = the_data) })

	observeEvent(input$within, {
		 	 the_data <- updates$data
				updates$data <- f("7", next_point = FALSE, data = the_data) })

	observeEvent(input$behind, {
		 	 the_data <- updates$data
				updates$data <- f("8", next_point = FALSE, data = the_data) })

	observeEvent(input$baseline, {
		 	 the_data <- updates$data
				updates$data <- f("9", next_point = FALSE, data = the_data) })

	observeEvent(input$unknown_depth, {
		 	 the_data <- updates$data
				updates$data <- f("0", next_point = FALSE, data = the_data) })

	observeEvent(input$body, {
		 	 the_data <- updates$data
			updates$data <- f("5", next_point = FALSE, data = the_data) })
	
	observeEvent(input$downthenet, {
		 	 the_data <- updates$data
			updates$data <- f("6", next_point = FALSE, data = the_data) })
			
	observeEvent(input$let, {
		 	 the_data <- updates$data
		updates$data <- f("c", next_point = FALSE, data = the_data) })
  
    observeEvent(input$time, {
		 	 the_data <- updates$data
		updates$data <- f("V", next_point = FALSE, data = the_data) })
  
	observeEvent(input$net_serve, {
		 	 the_data <- updates$data
			updates$data <- f("n", next_point = FALSE, data = the_data) })
 	
 	observeEvent(input$deep, {
 			the_data <- updates$data
		 	 updates$data <- f("d", next_point = FALSE, data = the_data) })
	
	observeEvent(input$wide, {
		 	 the_data <- updates$data
						updates$data <- f("w", next_point = FALSE, data = the_data) })

	observeEvent(input$wide_deep, {
		 	 the_data <- updates$data
			updates$data <- f("x", next_point = FALSE, data = the_data) })


	observeEvent(input$foot_fault, {
		 	 the_data <- updates$data
			updates$data <- f("g", next_point = FALSE, data = the_data) })


	observeEvent(input$unknown_fault, {
		 	 the_data <- updates$data
		updates$data <- f("e", next_point = FALSE, data = the_data) })

	observeEvent(input$net_error, {
		 	 the_data <- updates$data
			updates$data <- f("n", next_point = FALSE, data = the_data) })
 	
 	observeEvent(input$deep_error, {
 			the_data <- updates$data
		 	 updates$data <- f("d", next_point = FALSE, data = the_data) })
	
	observeEvent(input$wide_error, {
		 	 the_data <- updates$data
						updates$data <- f("w", next_point = FALSE, data = the_data) })

	observeEvent(input$wide_deep_error, {
		 	 the_data <- updates$data
			updates$data <- f("x", next_point = FALSE, data = the_data) })


	observeEvent(input$shank, {
		 	 the_data <- updates$data
			updates$data <- f("!", next_point = FALSE, data = the_data) })


	observeEvent(input$unknown_error, {
		 	 the_data <- updates$data
		updates$data <- f("e", next_point = FALSE, data = the_data) })

	observeEvent(input$seq1, {
		 	 the_data <- updates$data
		updates$data <- f("f17", next_point = FALSE, data = the_data) })

	observeEvent(input$seq2, {
		 	 the_data <- updates$data
		updates$data <- f("f18", next_point = FALSE, data = the_data) })

	observeEvent(input$seq3, {
		 	 the_data <- updates$data
		updates$data <- f("f19", next_point = FALSE, data = the_data) })

	observeEvent(input$seq4, {
		 	 the_data <- updates$data
		updates$data <- f("b17", next_point = FALSE, data = the_data) })

	observeEvent(input$seq5, {
		 	 the_data <- updates$data
		updates$data <- f("b18", next_point = FALSE, data = the_data) })

	observeEvent(input$seq6, {
		 	 the_data <- updates$data
		updates$data <- f("b19", next_point = FALSE, data = the_data) })

	observeEvent(input$seq7, {
		 	 the_data <- updates$data
		updates$data <- f("f27", next_point = FALSE, data = the_data) })

	observeEvent(input$seq8, {
		 	 the_data <- updates$data
		updates$data <- f("f28", next_point = FALSE, data = the_data) })

	observeEvent(input$seq9, {
		 	 the_data <- updates$data
		updates$data <- f("f29", next_point = FALSE, data = the_data) })

	observeEvent(input$seq10, {
		 	 the_data <- updates$data
		updates$data <- f("b27", next_point = FALSE, data = the_data) })

	observeEvent(input$seq11, {
		 	 the_data <- updates$data
		updates$data <- f("b28", next_point = FALSE, data = the_data) })

	observeEvent(input$seq12, {
		 	 the_data <- updates$data
		updates$data <- f("b29", next_point = FALSE, data = the_data) })

	observeEvent(input$seq13, {
		 	 the_data <- updates$data
		updates$data <- f("f37", next_point = FALSE, data = the_data) })

	observeEvent(input$seq14, {
		 	 the_data <- updates$data
		updates$data <- f("f38", next_point = FALSE, data = the_data) })

	observeEvent(input$seq15, {
		 	 the_data <- updates$data
		updates$data <- f("f39", next_point = FALSE, data = the_data) })

	observeEvent(input$seq16, {
		 	 the_data <- updates$data
		updates$data <- f("b37", next_point = FALSE, data = the_data) })

	observeEvent(input$seq17, {
		 	 the_data <- updates$data
		updates$data <- f("b38", next_point = FALSE, data = the_data) })

	observeEvent(input$seq18, {
		 	 the_data <- updates$data
		updates$data <- f("b39", next_point = FALSE, data = the_data) })

		
	observeEvent(input$ground_forehand, {
			the_data <- updates$data
		 	 updates$data <- f("f", next_point = FALSE, data = the_data) })
	
	observeEvent(input$ground_backhand, {
		 	 the_data <- updates$data
		 	 updates$data <- f("b", next_point = FALSE, data = the_data) })

	 observeEvent(input$slice_forehand, {
		 	 the_data <- updates$data
			updates$data <- f("r", next_point = FALSE, data = the_data) })

	 observeEvent(input$slice_backhand, {
		 	 the_data <- updates$data
		 	 updates$data <- f("s", next_point = FALSE, data = the_data) })

	  observeEvent(input$volley_forehand, {
		 	 the_data <- updates$data
		updates$data <- f("v", next_point = FALSE, data = the_data) })

	 observeEvent(input$volley_backhand, {
		 	 the_data <- updates$data
						updates$data <- f("z", next_point = FALSE, data = the_data) })

	 observeEvent(input$halfvolley_forehand, {
		 	 the_data <- updates$data
		updates$data <- f("h", next_point = FALSE, data = the_data) })

 	observeEvent(input$lob_forehand, {
		 	 the_data <- updates$data
		updates$data <- f("l", next_point = FALSE, data = the_data) })

 	observeEvent(input$lob_backhand, {
		 	 the_data <- updates$data
		updates$data <- f("m", next_point = FALSE, data = the_data) })

	 observeEvent(input$halfvolley_backhand, {
		 	 the_data <- updates$data
						updates$data <- f("i", next_point = FALSE, data = the_data) })

	 observeEvent(input$swingvolley_forehand, {
		 	 the_data <- updates$data
						updates$data <- f("j", next_point = FALSE, data = the_data) })

	 observeEvent(input$swingvolley_backhand, {
		 	 the_data <- updates$data
						updates$data <- f("k", next_point = FALSE, data = the_data) })

	 observeEvent(input$drop_forehand, {
		 	 the_data <- updates$data
						updates$data <- f("u", next_point = FALSE, data = the_data) })

	 observeEvent(input$drop_backhand, {
		 	 the_data <- updates$data
						updates$data <- f("y", next_point = FALSE, data = the_data) })

	 observeEvent(input$overhead_forehand, {
		 	 the_data <- updates$data
						updates$data <- f("o", next_point = FALSE, data = the_data) })

	 observeEvent(input$overhead_backhand, {
		 	 the_data <- updates$data
						updates$data <- f("p", next_point = FALSE, data = the_data) })

	 observeEvent(input$trick, {
		 	 the_data <- updates$data
						updates$data <- f("t", next_point = FALSE, data = the_data) })

	 observeEvent(input$unknown_shot, {
		 	 the_data <- updates$data
						updates$data <- f("q", next_point = FALSE, data = the_data) })

	 observeEvent(input$left, {
		 	 the_data <- updates$data
						updates$data <- f("3", next_point = FALSE, data = the_data) })

	 observeEvent(input$right, {
		 	 the_data <- updates$data
						updates$data <- f("1", next_point = FALSE, data = the_data) })

	 observeEvent(input$middle, {
		 	 the_data <- updates$data
						updates$data <- f("2", next_point = FALSE, data = the_data) })

	 observeEvent(input$unknown_stroke_direction, {
		 	 the_data <- updates$data
						updates$data <- f("0", next_point = FALSE, data = the_data) })


 observeEvent(input$ace, {
		 	 the_data <- updates$data
						updates$data <- f("*", next_point = FALSE, data = the_data) })

observeEvent(input$winner, {
		 	 the_data <- updates$data
						updates$data <- f("*", next_point = FALSE, data = the_data) })

 observeEvent(input$unreturnable, {
		 	 the_data <- updates$data
						updates$data <- f("#", next_point = FALSE, data = the_data) })


 observeEvent(input$forced, {
		 	 the_data <- updates$data
						updates$data <- f("#", next_point = FALSE, data = the_data) })


 observeEvent(input$unforced, {
		 	 the_data <- updates$data
						updates$data <- f("@", next_point = FALSE, data = the_data) })

 })
