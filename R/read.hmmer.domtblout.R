#' Read hmmer domtblout file
#'
#' @param fname file name
#' @import tidyr
#' @import dplyr
#' @export
read.hmmer.domtblout <- function(fname) {

	col.names <- c(
		"target.name",
		"target.accession",
		"target.len",
		"query.name",
		"query.accession",
		"query.len",
		"full.sequence.E.value",
		"full.sequence.score",
		"full.sequence.bias",
		"this.domain.num",
		"this.domain.of",
		"this.domain.c.E.value",
		"this.domain.i.E.value",
		"this.domain.score",
		"this.domain.bias",
		"hmm.coord.from",
		"hmm.coord.to",
		"ali.coord.from",
		"ali.coord.to",
		"env.coord.from",
		"env.coord.to",
		"accuracy",
		"target.description"
	)

	len.cols <- which(grepl("\\.len", col.names))
	numeric.cols <- which(grepl("(value|score|bias|accuracy)", col.names))
	integer.cols <- which(grepl("\\.(from|to|num|of|len)$", col.names))

	readLines(fname) %>%
		data.frame(line = .) %>%
		filter(!grepl("^#", line)) %>%
		separate(line, into = col.names, sep = " +", extra = "merge", convert = F) %>%
		mutate_at(numeric.cols, as.numeric) %>%
		mutate_at(integer.cols, as.integer)
}
