load("data.RData")
source("helper.R")
source("game_functions.R")

map                <- map[60:70]
territories        <- territories[60:70]
territories_coords <- territories_coords[60:70, ]
connections        <- list(c(2, 3, 5), c(4, 6, 7), c(4, 8, 9), c(6), c(9, 10),
	c(7, 8), c(8, 11), c(10), c(), c(11), c())

for (i in 1:length(connections)) {
	for (j in connections[[i]]) {
		connections[[j]] <- sort(union(i, connections[[j]]))
	}
}

territories_coords[2,  ] <- c( 70, 618)
territories_coords[5,  ] <- c(158, 589)
territories_coords[10, ] <- c(107, 565)

regions <- list(c(2, 7, 8), c(4, 6), c(5, 9))
regions_bonus <- c(3, 2, 2)

{
	pdf("images/Iron Islands.pdf")
	par(mai = rep(0, 4))

	plot_blank(map)

	for (i in seq_along(map)) {
		for (j in connections[[i]]) {
			lines(
				territories_coords$x[c(i, j)],
				territories_coords$y[c(i, j)],
				col = "#c7c7c7"
			)
		}
	}

	plot_polygons(map, "#c7c7c7", "#ffffff")

	text(
		territories_coords$x, territories_coords$y + 2.5,
		labels = seq_along(map), cex = .8
	)
	text(
		territories_coords$x, territories_coords$y,
		labels = territories, cex = .8
	)

	dev.off()
}
