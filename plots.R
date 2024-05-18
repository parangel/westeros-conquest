load("data.RData")
source("helper.R")
source("connections.R")

col_houses <- c("#f7f7ff", "#708090", "#87ceeb", "#4b61d1", "#9a861c",
	"#e60026", "#660000", "#228b22", "#fada5e", "#e86100")

{
	pdf("images/Westeros.pdf", 7, 14)
	par(mar = c(0, 0, 1, 0))

	plot_blank(map, "Westeros")
	plot_polygons(map, "#c6c6c6", "#ffffff")

	dev.off()

	pdf("images/map.pdf", 7, 14)
	par(mar = c(0, 0, 1, 0))

	for (h in seq_along(houses)) {
		for (i in houses[[h]]) {
			plot_blank(map, regions[i])
			plot_polygons(map, "#ffffff", "#c7c7c7")
			plot_polygons(map[[i]], col_houses[h], "#c7c7c7")
		}
	}

	dev.off()
}

{
	pdf("images/Houses.pdf", 7, 14)
	par(mai = rep(0, 4))

	plot_blank(map)

	for (h in seq_along(houses)) {
		plot_polygons(map[houses[[h]]], col_houses[h], "#636363")
	}

	for (h in seq_along(houses)) {
		k <- houses[[h]]
		plot_blank(map[k])

		for (i in k) {
			path <- map[[i]]
			plot_polygons(path, NA, "#c7c7c7")
			text(mean(rangex(path)), mean(rangey(path)), labels = i)
		}
	}

	dev.off()
}

{
	pdf("images/Connections.pdf", 20, 20)
	par(mai = rep(0, 4))

	n <- length(map)

	for (i in 1:n) {

		k <- connections[[i]]
		plot_blank(map[c(i, k)])

		plot_polygons(map[-c(i, k)], "#cacbcf", "white")
		plot_polygons(map[k],        "#c6ddbd", "white")
		plot_polygons(map[[i]],      "#208439", "white")

	}

	dev.off()
}
