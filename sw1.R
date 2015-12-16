### Retrieve SW scripts and parse characters

library(data.table)
library(stringi)
library(igraph)

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


compileLinksNodes <- function(dat) {
  dat.l <- split(dat[, desc], dat[, sceneNo])
  dat.l <- lapply(dat.l, unique)

  dat.l <- rbindlist(lapply(dat.l, createLink))
  dat.l <- dat.l[, .(value = .N), by = c("source", "target")]
  dat.l <- dat.l[source != "" & target != ""]
  tmp <- graph_from_edgelist(as.matrix(dat.l[, .(source, target)]))
  edge_attr(tmp, "weight") <- dat.l[, value]
  tmp <- as.undirected(tmp, mode = "collapse")
  res.l <- as.data.table(as_edgelist(tmp, names = FALSE))
  setnames(res.l, c("source", "target"))
  res.l[, value := edge_attr(tmp, "weight")]
  res.l[, source := source - 1]
  res.l[, target := target - 1]

  res.n <- data.table(name = vertex_attr(tmp, "name"))
  res.n[, JSID := 0:(nrow(res.n) - 1)]
  res <- list(nodes = res.n,
              links = res.l)
  return(res)
}

sw4.export <- compileLinksNodes(sw4.dat)
sw5.export <- compileLinksNodes(sw5.dat)