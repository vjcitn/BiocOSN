#' get the table of depth-5 bucket contents of 12 Oct 2025
#' @importFrom data.table fread
#' @export
gettab = function() {
 bioc_osn = data.table::fread(system.file("txt", "bioc_osn_5.txt.gz", package="BiocOSN"), fill=6)
 names(bioc_osn) = c("size", "date", "time", "path", "extra1", "extra2")
 bioc_osn
}

#' process table for hierarchy exploration
#' @param tab output of `gettab`
#' @export
process_tab = function(tab) {
 pa = tab$path
 spa = strsplit(pa, "\\/")
 toplev = sapply(spa, "[", 1)
 sizes = table(toplev)
 nms = names(sizes)
 lis = split(tab, toplev)
 names(lis) = nms
 ans = list(names=nms, sizes=sizes, split=lis)
 class(ans) = c("osninfo", "list")
 ans
}

#' printer for osninfo
#' @param x output of `process_tab`
#' @param \dots not used
#' @export
print.osninfo = function(x, ...) {
 cat("osninfo object:\n")
 cat(sprintf("  %d topics\n", length(x$split)))
}

#' blow up some info for a selected component of OSN info
#' @import dplyr
#' @importFrom tidyr separate
#' @param osni instance of "osninfo", as created with process_tab
#' @param node one of the components of osni$split
#' @examples
#' x = gettab()
#' y = process_tab(x)
#' probe(y, "AnnotationHub") |> head(20)
#' @export
probe = function(osni, node) {
 stopifnot(inherits(osni, "osninfo"))
 stopifnot(node %in% names(osni$split))
 np = osni$split[[node]]
 np |> separate(path, c(NA, "subnode"), fill="right", sep="/") |> group_by(subnode) |> summarize(sz=sum(size)) |>
    mutate(sz2 = format_bytes(sz))
}
 
format_bytes <- function(bytes) {
  sapply(bytes, function(b) {
    # Temporarily create an object of that size in memory and format it
    utils:::format.object_size(b, units = "auto", standard = "SI")
  })
}
