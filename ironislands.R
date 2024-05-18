# Iron Islands ----

source("helper.R")
load("data.RData")

map         <- map[60:70]
regions     <- regions[60:70]
regions_xy  <- regions_xy[60:70, ]
connections <- list(c(2, 3, 5), c(4, 6, 7), c(4, 8, 9), c(6), c(9, 10),
	c(7, 8), c(8, 11), c(10), c(), c(11), c())

for (i in 1:length(connections)) {
	for (j in connections[[i]]) {
		connections[[j]] <- sort(union(i, connections[[j]]))
	}
}

regions_xy[2,  ] <- c( 70, 618)
regions_xy[5,  ] <- c(158, 589)
regions_xy[10, ] <- c(107, 565)

{
	pdf("images/Iron Islands.pdf")
	par(mai = rep(0, 4))
	plot_blank(map)

	for (i in seq_along(map)) {
		for (j in connections[[i]]) {
			lines(regions_xy[c(i, j), 1], regions_xy[c(i, j), 2],
				col = "#c7c7c7")
		}
	}

	plot_polygons(map, "white", "#c7c7c7")

	for (i in seq_along(map)) {
		text(regions_xy[i, 1], regions_xy[i, 2], labels = i)
	}

	dev.off()
}
