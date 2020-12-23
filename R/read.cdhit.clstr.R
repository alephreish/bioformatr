#' Read cdhit cluster file
#'
#' @param fname input file name
#' @import dplyr
#' @import tidyr
#' @export
read.cdhit.clstr <- function(fname) {
	data.fields <- c("E.Value", "Aln", "Identity")
	read.table(fname, sep = "\t", comment.char = "", quote = "", fill = T, stringsAsFactors = F, col.names = c("Col1", "Col2")) %>%
		separate(Col1, into = c("Seq.Num", "Cluster"), sep = " ", fill = "right") %>%
		fill(Cluster) %>%
		filter(!grepl(">", Seq.Num)) %>%
		separate(Col2, into = c("Seq.Len", "Col2"), sep = "aa, >") %>%
		extract(Col2, into = c("Seq.Name", "Is.Representative", "Col2"), regex = "(.*?)[.]{3} ([*]|at) ?(.*)") %>%
		mutate(Is.Representative = Is.Representative == "*", Col2 = ifelse(Is.Representative, "100%", Col2)) %>%
		separate_rows(Col2, sep = ",") %>%
		separate(Col2, into = data.fields, sep = "/", fill = "left", convert = T) %>%
		mutate(Identity = sub("%", "", Identity) %>% as.numeric) %>%
		group_by(Seq.Name) %>%
		mutate(level.rank = paste0(".", 1:n() - 1), level.rank = ifelse(level.rank == ".0", "", level.rank)) %>%
		pivot_wider(names_from = level.rank, values_from = data.fields, names_sep = "") %>%
		ungroup
}
