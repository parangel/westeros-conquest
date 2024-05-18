rangex <- function(data) {
	if (is.list(data)) {
		aux <- sapply(data, rangex)
		return(c(min(aux[1, ]), max(aux[2, ])))
	} else {
		return(range(data[, 1]))
	}
}


rangey <- function(data) {
	if (is.list(data)) {
		aux <- sapply(data, rangey)
		return(c(min(aux[1, ]), max(aux[2, ])))
	} else {
		return(range(data[, 2]))
	}
}


plot_blank <- function(region, main = NULL) {
	plot(0, 0, xlim = rangex(region), ylim = rangey(region),
		main = main, type = "n", asp = 1, axes = FALSE)
}


plot_polygons <- function(region, col = "grey", border = "black") {
	if (!is.list(region)) {
		polygon(region, col = col, border = border)
	} else {
		for (p in region) {
			plot_polygons(p, col, border)
		}
	}
}
