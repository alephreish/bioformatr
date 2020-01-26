#' Read hmmer tblout file
#'
#' @param fname file name
#' @import readr
#' @export
read.hmmer.tblout <- function(fname) {
	col_width <- fwf_cols(
		target.name = 21,
		target.accession = 11,
		query.name  = 21,
		query.accession = 11,
		full.sequence.E.value = 10,
		full.sequence.score = 7,
		full.sequence.bias  = 6,
		best.1.domain.E.value = 10,
		best.1.domain.score   = 7,
		best.1.domain.bias    = 6,
		domain.number.estimation.exp = 6,
		domain.number.estimation.reg = 4,
		domain.number.estimation.clu = 4,
		domain.number.estimation.ov  = 4,
		domain.number.estimation.env = 4,
		domain.number.estimation.dom = 4,
		domain.number.estimation.rep = 4,
		domain.number.estimation.inc = 4,
		description.of.target = NA
	)
	col_types <- cols(
		target.name = "c",
		target.accession = "c",
		query.name = "c",
		query.accession = "c",
		full.sequence.E.value = "n",
		full.sequence.score   = "n",
		full.sequence.bias    = "n",
		best.1.domain.E.value = "n",
		best.1.domain.score   = "n",
		best.1.domain.bias    = "n",
		domain.number.estimation.exp = "n",
		domain.number.estimation.reg = "i",
		domain.number.estimation.clu = "i",
		domain.number.estimation.ov  = "i",
		domain.number.estimation.env = "i",
		domain.number.estimation.dom = "i",
		domain.number.estimation.rep = "i",
		domain.number.estimation.inc = "i",
		description.of.target = "c"
	)
	read_fwf(fname, col_width, col_types, comment = "#")
}
