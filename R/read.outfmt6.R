
#' Read blast outfmt6 file
#'
#' @param fname input file name
#' @export
read.outfmt6 <- function(fname, col.names = c("qseqid", "sseqid", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore")) {
	read.table(fname	, col.names = col.names)
}

