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

plot_westeros <- function(region, main = NULL) {
	plot(0, 0, xlim = c(0.40, 572.19), ylim = c(20.415, 1395.334),
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


sys_sleep <- function(val) {
	t0 <- microbenchmark::get_nanotime()
	val_ms <- val * 10**9

	repeat {
		t1 <- microbenchmark::get_nanotime()
		diff_time <- t1 - t0
		if (diff_time > val) break
	}
}


insidepolygon <- function(p, v) {
	v <- rbind(v, v[1, ])
	xp <- p[1]
	yp <- p[2]
	inside <- FALSE

	for (i in 1:(nrow(v) - 1)) {
		j <- i + 1

		if (v[i, 2] < v[j, 2]) {
			x <- v[c(i, j), 1]
			y <- v[c(i, j), 2]
		} else {
			x <- v[c(j, i), 1]
			y <- v[c(j, i), 2]
		}

		if (yp > y[1] & yp < y[2]) {
			xi <- x[1] + (x[2] - x[1]) / (y[2] - y[1]) * (yp - y[1])
			if (xp < xi) {
				inside <- !inside
			}
		}
	}

	return(inside)
}
