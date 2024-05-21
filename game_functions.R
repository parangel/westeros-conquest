resample <- function(x, ...) {
	return(x[sample.int(length(x), ...)])
}

distribute <- function(n, k, w = NULL) {

	if (is.null(w)) {
		weights <- rep(1, k)
	} else {
		weights <- w
	}

	return(rmultinom(1, n, weights)[, 1])
}

distribute_territories <- function() {
	x <- sample(rep(1:n_armies, length.out = n_territories))

	return(x)
}

deploy_troops <- function(m = 0) {
	troops <- numeric(n_territories)

	for (k in seq_len(n_armies)) {
		ik <- which(control == k)
		nk <- length(ik)
		if (nk > 0) {
			troops[ik] <- 1 + distribute(max(0, m - nk), nk)
		}
	}

	return(troops)
}

choose_army <- function() {
	new_weights <- turn_weights + runif(n_armies)
	new_armies <- setdiff(armies, army)

	new_army <- new_armies[which.max(new_weights[new_armies])]
	new_weights[new_army] <- 0

	return(list(army = new_army, weights = new_weights))
}

reinforce_troops <- function() {
	army_territory <- which(control == army)
	army_size <- length(army_territory)
	n_troops <- max(army_size %/% 3, 3)

	control_regions <- sapply(regions, \(region) all(control[region] == army))
	n_troops <- n_troops + sum(regions_bonus[control_regions])

	border_territory <- sapply(army_territory, \(i)
		sum(troops[connections[[i]][control[connections[[i]]] != army]])
	)

	new <- numeric(n_territories)
	new[army_territory] <- distribute(n_troops, army_size, border_territory)

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

roll_dice <- function(x) {
	atk <- min(x[1] - 1, 3)
	def <- min(x[2], 2)

	n_dice <- min(atk, def)
	datk <- sort(sample(6, atk, TRUE), decreasing = TRUE)[1:n_dice]
	ddef <- sort(sample(6, def, TRUE), decreasing = TRUE)[1:n_dice]
	dice_diff <- sum(datk > ddef)

	return(c(
		x[1] - (n_dice - dice_diff),
		x[2] - dice_diff,
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
			troops_battle[2] <- max(troops_battle[1] %/% 2, troops_battle[3])
			troops_battle[1] <- troops_battle[1]  - troops_battle[2]
		}
	}

	return(list(attacker_won = atk_won, troops = troops_battle[1:2]))
}

plot_map <- function(
		map_lim     = NULL,
		plot_col    = NULL,
		plot_border = "#ffffff",
		plot_num    = FALSE,
		plot_conn   = FALSE
	) {

	plot_blank(map, map_lim)

	if (plot_conn) {
		for (i in seq_along(map)) {
			for (j in connections[[i]]) {
				lines(
					territories_coords$x[c(i, j)],
					territories_coords$y[c(i, j)],
					col = "#c7c7c7"
				)
			}
		}
	}

	if (is.null(plot_col) | length(plot_col) < n_armies) {
		plot_polygons(map, "#c7c7c7", plot_border)
	} else {
		for (k in unique(control)) {
			plot_polygons(map[control == k], army_col[k], plot_border)
		}
	}

	if (plot_num) {
		text(
			territories_coords$x, territories_coords$y,
			labels = troops, cex = .8
		)
	}
}

plot_territory <- function(
		territory,
		plot_col = "#c7c7c7",
		plot_border = "#ffffff"
	) {
	plot_polygons(map[[territory]], plot_col, plot_border)
}
