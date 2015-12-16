### Retrieve SW scripts and parse characters

library(data.table)
library(stringi)
library(igraph)
library(jsonlite)

### (1) Define movie script locations ####

sw4link <- "http://www.imsdb.com/scripts/Star-Wars-A-New-Hope.html"
sw5link <- "http://www.imsdb.com/scripts/Star-Wars-The-Empire-Strikes-Back.html"
sw6link <- "http://www.imsdb.com/scripts/Star-Wars-Return-of-the-Jedi.html"
sw1link <- "http://www.imsdb.com/scripts/Star-Wars-The-Phantom-Menace.html"
sw2link <- "http://www.imsdb.com/scripts/Star-Wars-Attack-of-the-Clones.html"
sw3link <- "http://www.imsdb.com/scripts/Star-Wars-Revenge-of-the-Sith.html"

### (2) Extract scenes and characters ####
### (2a) SW4

sw4.raw <- readLines(sw4link)

sw4.preStart <- (1:length(sw4.raw))[stri_detect_fixed(sw4.raw, "<pre>")]
sw4.preEnd   <- (1:length(sw4.raw))[stri_detect_fixed(sw4.raw, "</pre>")]

sw4.raw <- sw4.raw[sw4.preStart:sw4.preEnd]
sw4.int <- sw4.raw[(1:length(sw4.raw))[stri_detect_regex(sw4.raw, "<b>")]]
sw4.int <- sw4.int[c(-1:-5, (-1*(length(sw4.int) - 3)):(-1*length(sw4.int)))]
sw4.int <- stri_replace_all_regex(sw4.int, "<b>", "")
sw4.int <- stri_trim_both(sw4.int, pattern = "\\P{Wspace}")
sw4.int <- sw4.int[!stri_detect_regex(sw4.int, "</b>")]
sw4.int <- stri_replace_all_regex(sw4.int, "'S VOICE", "")

sw4.sceneIdx <- stri_detect_regex(sw4.int, "(INT. |EXT. )")
sw4.sceneNo <- rep(NA, times = length(sw4.sceneIdx))
sn <- 0
for (i in 1:length(sw4.sceneIdx)) {
  this.idx <- sw4.sceneIdx[i]
  if (this.idx) sn <- sn + 1
  sw4.sceneNo[i] <- sn
}

sw4.dat <- data.table(desc    = sw4.int,
                      isScene = sw4.sceneIdx,
                      sceneNo = sw4.sceneNo)

sw4.dat <- sw4.dat[isScene == FALSE, .(desc, sceneNo)]

rm(sw4.raw, sw4.preStart, sw4.preEnd, sw4.int, sw4.sceneIdx, sw4.sceneNo, i, sn,
   this.idx, sw4link)


### (2b) SW5

sw5.raw <- readLines(sw5link)

sw5.preStart <- (1:length(sw5.raw))[stri_detect_fixed(sw5.raw, "<pre>")]
sw5.preEnd   <- (1:length(sw5.raw))[stri_detect_fixed(sw5.raw, "</pre>")]

sw5.raw <- sw5.raw[sw5.preStart:sw5.preEnd]
sw5.int <- sw5.raw[(1:length(sw5.raw))[stri_detect_regex(sw5.raw, "<b>")]]
sw5.int <- sw5.int[c(-1:-7, (-1*(length(sw5.int) - 3)):(-1*length(sw5.int)))]
sw5.int <- stri_replace_all_regex(sw5.int, "<b>", "")
sw5.int <- stri_trim_both(sw5.int, pattern = "\\P{Wspace}")
sw5.int <- sw5.int[!stri_detect_regex(sw5.int, "</b>")]


sw5.sceneIdx <- stri_detect_regex(sw5.int, "(INT. |EXT. )")
sw5.sceneNo <- rep(NA, times = length(sw5.sceneIdx))
sn <- 0
for (i in 1:length(sw5.sceneIdx)) {
  this.idx <- sw5.sceneIdx[i]
  if (this.idx) sn <- sn + 1
  sw5.sceneNo[i] <- sn
}

sw5.dat <- data.table(desc    = sw5.int,
                      isScene = sw5.sceneIdx,
                      sceneNo = sw5.sceneNo)

sw5.dat <- sw5.dat[isScene == FALSE, .(desc, sceneNo)]

rm(sw5.raw, sw5.preStart, sw5.preEnd, sw5.int, sw5.sceneIdx, sw5.sceneNo, i, sn,
   this.idx, sw5link)

### (3) Compile exports ####

createLink <- function(ch) {
  if (length(ch) > 1) {
    res <- as.data.table(t(combn(ch, 2)))
    setnames(res, c("source", "target"))
    return(res)
  }
}

createGraph <- function(dat, film) {
  dat.l <- split(dat[, desc], dat[, sceneNo])
  dat.l <- lapply(dat.l, unique)

  dat.l <- rbindlist(lapply(dat.l, createLink))
  dat.l <- dat.l[, .(value = .N), by = c("source", "target")]
  dat.l <- dat.l[source != "" & target != ""]
  g <- graph_from_edgelist(as.matrix(dat.l[, .(source, target)]))
  edge_attr(g, "weight") <- dat.l[, value]
  vertex_attr(g, "film") <- film
  g <- as.undirected(g, mode = "collapse")
  return(g)
}


compileLinksNodes <- function(g) {
  res.l <- as.data.table(as_edgelist(g, names = FALSE))
  setnames(res.l, c("source", "target"))
  tmp <- data.frame(d1 = edge_attr(g, "weight_1"),
                    d2 = edge_attr(g, "weight_2"))
  tmp[is.na(tmp)] <- 0
  tmp <- tmp[,1] + tmp[,2]
  res.l[, value := tmp]
  res.l[, source := source - 1]
  res.l[, target := target - 1]

  res.n <- data.table(name = vertex_attr(g, "name"))
  res.n[, JSID := 0:(nrow(res.n) - 1)]
  tmp <- data.frame(d1 = vertex_attr(g, "film_1"),
                    d2 = vertex_attr(g, "film_2"))
  tmp <- apply(tmp, 1, function(x) {
    if (is.na(x[1])) {
      res <- x[2]
    } else if (is.na(x[2])) {
      res <- x[1]
    } else {
      res <- "beide"
    }
  })
  res.n[, film := tmp]
  res <- list(nodes = res.n,
              links = res.l)
  return(res)
}

sw4.g <- createGraph(sw4.dat, "hope")
sw5.g <- createGraph(sw5.dat, "empire")

sw45.g <- sw4.g + sw5.g

sw45.export <- compileLinksNodes(sw45.g)

sw4.export <- compileLinksNodes(sw4.g)
sw5.export <- compileLinksNodes(sw5.g)



### (4) Produce JSONs ####

writeLines(toJSON(sw4.export, pretty = TRUE), con = file("sw4.json"))
writeLines(toJSON(sw5.export, pretty = TRUE), con = file("sw5.json"))
writeLines(toJSON(sw45.export, pretty = TRUE), con = file("sw45.json"))
