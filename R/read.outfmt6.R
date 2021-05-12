
#' Read blast outfmt6 file
#'
#' @param fname input file name
#' @export
read.outfmt6 <- function(fname, col.names = c("qseqid", "sseqid", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore"), col.names.pre = c(), col.names.post = c()) {
	read.table(fname, sep = "\t", col.names = c(col.names.pre, col.names, col.names.post))
}
