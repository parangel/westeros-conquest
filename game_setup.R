load("data.RData")
source("helper.R")
source("connections.R")
source("game_functions.R")

regions       <- houses
regions_bonus <- c(1, 13, 11, 12, 7, 11, 9, 22, 13, 11)
n_territories <- length(map)

path          <- "images/game/"
map_type      <- "westeros"
sleep_time    <- .05

n_armies      <- 10
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
	"#4c4c4c", "#b7bfc7", "#87ceeb", "#4b61d1", "#786b26",
	"#e60026", "#660000", "#228b22", "#fada5e", "#e88f00"
)

{
	set.seed(847892)

	army         <- NA
	armies       <- 1:n_armies
	turn_weights <- runif(n_armies)
	control      <- distribute_territories()
	troops       <- deploy_troops(71)

	source("game.R")
}

{
	set.seed(543829)
	army         <- NA
	armies       <- c(2, 10)
	turn_weights <- runif(n_armies)
	control      <- rep(1, n_territories)
	control[c(1, 176)] <- armies
	troops       <- deploy_troops()

	source("game.R")

}

{
	army         <- NA
	armies       <- 2:n_armies
	turn_weights <- runif(n_armies)
	control      <- rep(1, n_territories)
	control[c(12, 34, 48, 69, 81, 95, 121, 147, 169)] <- armies
	troops       <- deploy_troops(5)

	source("game.R")
}
