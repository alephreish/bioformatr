#' Read hmmer tblout file
#'
#' @param fname file name
#' @import tidyr
#' @import dplyr
#' @export
read.hmmer.tblout <- function(fname) {

	col.names <- c(
		"target.name",
		"target.accession",
		"query.name",
		"query.accession",
		"full.sequence.E.value",
		"full.sequence.score",
		"full.sequence.bias",
		"best.1.domain.E.value",
		"best.1.domain.score",
		"best.1.domain.bias",
		"domain.number.estimation.exp",
		"domain.number.estimation.reg",
		"domain.number.estimation.clu",
		"domain.number.estimation.ov",
		"domain.number.estimation.env",
		"domain.number.estimation.dom",
		"domain.number.estimation.rep",
		"domain.number.estimation.inc",
		"description.of.target"
	)

	numeric.cols <- which(col.names == "full.sequence.E.value")        : which(col.names == "domain.number.estimation.exp")
	integer.cols <- which(col.names == "domain.number.estimation.reg") : which(col.names == "domain.number.estimation.inc")

	readLines(fname) %>%
		data.frame(line = .) %>%
		filter(!grepl("^#", line)) %>%
		separate(line, into = col.names, sep = " +", extra = "merge", convert = F) %>%
		mutate_at(numeric.cols, as.numeric) %>%
		mutate_at(integer.cols, as.integer)
}
