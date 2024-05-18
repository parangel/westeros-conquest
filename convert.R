ipath <- "data/map.tex"
opath <- "data/map.csv"

lines <- readLines(ipath)
lines <- lines[startsWith(lines, "\\moveto") | startsWith(lines, "\\lineto")]

data <- gregexec("[(]([0-9]+.?[0-9]*),([0-9]+.?[0-9]*)[)]", lines)
data <- regmatches(lines, data)
data <- t(sapply(data, \(x) as.numeric(x[2:3])))
colnames(data) <- c("x", "y")

start <- which(startsWith(lines, "\\moveto"))
end   <- c(start[-1] - 1, nrow(data))

data <- lapply(1:length(start), \(i) data[start[i]:end[i], ])
data <- data[!duplicated(data)]

data <- sapply(seq_along(data), \(i) cbind(i, data[[i]]))
data <- do.call(rbind, data)

write.csv(data, opath, row.names = FALSE)
