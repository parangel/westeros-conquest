resample <- function(x, ...) {
	return(x[sample.int(length(x), ...)])
}

distribute <- function(n, k) {
	return(rmultinom(1, n, rep(1, k))[, 1])
}

distribute_territories <- function() {
	x <- sample(rep(1:n_armies, length.out = n_territories))

	return(x)
}

distribute_troops <- function(m) {
	troops <- numeric(n_territories)

	for (k in seq_len(n_armies)) {
		ik <- which(control == k)
		nk <- length(ik)
		if (nk > 0) {
			troops[ik] <- 1 + distribute(m - nk, nk)
		}
	}

	return(troops)
}

choose_army <- function() {
	new_armies <- setdiff(armies, army)

	new_army <- resample(new_armies, 1, prob = turn_weights[new_armies] ^ 2)
	new_weights <- turn_weights + 1
	new_weights[new_army] <- 0

	return(list(army = new_army, turn_weights = new_weights))
}

deploy_troops <- function() {
	ik <- which(control == army)
	nk <- length(i)
	n_troops <- max(n %/% 3, 3)

	control_houses <- sapply(houses, \(house) all(control[house] == army))
	n_troops <- n_troops + sum(house_bonus[control_houses])

	new <- numeric(n_territories)
	new[ik] <- distribute(n_troops, nk)

	return(new)
}

choose_battlefield <- function() {

	possible_atk <- which(control == army & troops > 1)

	for (atk in resample(possible_atk)) {
		possible_def <- connections[[atk]]
		possible_def <- possible_def[control[possible_def] != army]

		if (length(possible_def) > 0) {
			def <- resample(possible_def, 1)
			return(c(atk, def))
		}
	}

	return(NULL)
}

roll_dice <- function() {
	atk <- min(troops_battle[1] - 1, 3)
	def <- min(troops_battle[2], 2)

	n_dice <- min(atk, def)
	datk <- sort(sample(6, atk, TRUE), decreasing = TRUE)[1:n_dice]
	ddef <- sort(sample(6, def, TRUE), decreasing = TRUE)[1:n_dice]
	dice_diff <- sum(datk > ddef)

	return(c(
		troops_battle[1] - (n_dice - dice_diff),
		troops_battle[2] - dice_diff,
		atk))
}

battle <- function() {
	troops_battle <- troops[battlefield]
	battle_over <- FALSE
	atk_won <- FALSE

	while (!battle_over) {
		troops_battle <- roll_dice(troops_battle)

		if (troops_battle[1] == 1) {
			battle_over <- TRUE

		} else if (troops_battle[2] == 0) {
			battle_over <- TRUE
			atk_won <- TRUE
			troops_battle[2] <- min(troops_battle[1] %/% 2, troops_battle[3])
			troops_battle[1] <- troops_battle[1]  - troops_battle[2]
		}
	}

	return(list(attacker_won = atk_won, troops = troops_battle))
}
