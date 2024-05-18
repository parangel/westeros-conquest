source("helper.R")
ipath <- "data/map.csv"

data <- as.matrix(read.csv(ipath))
data <- lapply(unique(data[, 1]), \(x) data[data[, 1] == x, 2:3])
n    <- length(data)

ysort <- order(sapply(data, \(x) max(x[, 2])), decreasing = TRUE)
data  <- data[ysort]

data <- c(list(data[c(46, 47, 50)]), data[-c(46, 47, 50)])
data <- c(list(data[c(49, 50)]), data[-c(49, 50)])
data <- c(list(data[c(197, 208)]), data[-c(197, 208)])
data <- data[-234]
data <- c(list(data[c(236, 251, 257)]), data[-c(236, 251, 257)])
data <- c(list(data[c(209, 253)]), data[-c(209, 253)])
data <- data[-278]

i_cities <- c(7, 8, 10, 12, 14, 16, 17, 22, 27, 28, 29, 31, 35, 36, 38, 41, 42,
	43, 44, 45, 49, 50, 52, 56, 58, 62, 63, 64, 67, 68, 72, 75, 80, 81, 84, 86,
	88, 94, 96, 100, 103, 110, 111, 114, 120, 121, 122, 124, 125, 128, 134, 139,
	140, 142, 143, 144, 150, 151, 153, 155, 157, 164, 165, 169, 170, 172, 174,
	176, 179, 185, 191, 192, 196, 199, 200, 203, 204, 209, 213, 214, 217, 222,
	223, 225, 227, 229, 233, 236, 237, 240, 241, 242, 246, 247, 248, 251, 252,
	253, 256, 257, 261, 263, 266, 267, 270, 274, 275, 276, 277, 278, 282, 285,
	287, 288, 291, 292, 294, 295, 296, 297)

cities <- data[i_cities]
data   <- data[-i_cities]


houses <- list(
	wall        = c(6, 9),
	north       = c(7, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
		23, 24, 25, 26, 27, 28),
	vale        = c(4, 5, 29, 30, 31, 32, 33, 34, 36, 38, 39, 40, 47, 53, 54,
		57, 59),
	riverlands  = c(35, 37, 51, 56, 58, 63, 65, 66, 68, 69, 71, 73, 74, 76, 78,
		80, 83, 87, 94),
	ironislands = c(41, 42, 43, 44, 45, 46, 48, 49, 50, 52, 55, 60),
	westerlands = c(61, 62, 64, 70, 72, 79, 81, 84, 90, 92, 93, 97, 100, 101,
		104, 107, 109, 117),
	crownlands  = c(67, 75, 77, 82, 85, 86, 88, 89, 91, 95, 98, 99, 103, 105,
		108),
	reach       = c(96, 102, 106, 110, 111, 113, 116, 119, 122, 123, 124, 125,
		126, 129, 130, 131, 133, 134, 135, 137, 140, 141, 142, 143, 148, 149,
		153, 154, 156, 158, 160, 161, 170, 173, 177),
	stormlands  = c(1, 2, 3, 112, 114, 115, 118, 120, 121, 127, 128, 132, 136,
		138, 139, 144, 145, 146, 147, 150, 151),
	dorne       = c(152, 155, 157, 159, 162, 163, 164, 165, 166, 167, 168, 169,
		171, 172, 174, 175, 176)
)

data[[49]] <- rbind(data[[49]], data[[52]])
houses$ironislands <- setdiff(houses$ironislands, 52)

map <- list()
for (house in houses) {
	yorder <- order(sapply(data[house], rangey)[2, ], decreasing = TRUE)
	map <- c(map, data[house[yorder]])
}

aux2 <- sapply(houses, length)
aux2 <- cumsum(aux2)
aux1 <- c(1, aux2[-10] + 1)

new_houses <- lapply(1:10, \(x) aux1[x]:aux2[x])
names(new_houses) <- names(houses)
houses <- new_houses

n <- length(map)
regions_xy <- matrix(nrow = n, ncol = 2)
for (i in 1:n) {
	path <- map[[i]]
	regions_xy[i, ] <- c(mean(rangex(path)), mean(rangey(path)))
}

regions <- readLines("data/regions.txt")

save(map, regions, regions_xy, houses, file = "data.RData")
