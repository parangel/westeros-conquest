# ----

load("data.RData")
source("helper.R")
source("connections.R")
source("game_functions.R")

regions       <- houses
regions_bonus <- c(1, 13, 11, 12, 7, 11, 9, 22, 13, 11)

n_territories <- length(map)
n_armies      <- 10
armies        <- 1:n_armies
armies_name   <- c(
	"Army of Wildings",
	"House Stark",
	"House Arryn",
	"House Tully",
	"House Greyjoy",
	"House Lannister",
	"House Targaryen",
	"House Tyrell",
	"House Baratheon",
	"House Martell"
)
army_col      <- c(
	"#d5d9e3", "#708090", "#87ceeb", "#4b61d1", "#786b26",
	"#e60026", "#660000", "#228b22", "#fada5e", "#e86100"
)

set.seed(1715979728)
turn_weights <- rep(n_armies, n_armies)
control      <- distribute_territories()
troops       <- deploy_troops(71)
army         <- NA


# ----

dir.create("images/game", showWarnings = FALSE)
png("images/game/%06d.png", width = 480, height = 1038)
par(mai = rep(0, 4))
plot_map(army_col, FALSE, FALSE)

new_turn     <- TRUE
while (new_turn) {
	turn <- choose_army()
	army <- turn$army
	turn_weights <- turn$weights

	troops <- troops + reinforce_troops()
	same_army <- TRUE

	while (same_army) {
		battlefield <- choose_battlefield()

		if (is.null(battlefield)) {
			same_army <- FALSE

			if (!army %in% armies) {
				cat("Army defeated.\n")
				browser()
			} else if (all(control != army)) {
				armies <- setdiff(armies, army)
				cat("Army defeated.\n")
				browser()
			} else if (length(armies) == 1) {
				cat("Army won.\n")
				browser()
				new_turn <- FALSE
			}

		} else {
			if (control[battlefield[1]] == control[battlefield[2]]) {
				browser()
			}

			battle_result <- battle()
			troops[battlefield] <- battle_result$troops

			if (battle_result$attacker_won) {
				defeated <- control[battlefield[2]]
				# turn_weights[defeated] <- turn_weights[defeated] + 1
				control[battlefield[2]] <- army

				plot_map(army_col, FALSE, FALSE)

				if (!any(control == defeated)) {
					# cat(sprintf("%s was defeated.\n", armies_name[defeated]))
					armies <- setdiff(armies, defeated)

					if (length(armies) == 1) {
						# cat(sprintf("%s won.\n", armies_name[army]))
						new_turn <- FALSE
						same_army <- FALSE
					}
				}
			}
		}
	}
}

dev.off()
