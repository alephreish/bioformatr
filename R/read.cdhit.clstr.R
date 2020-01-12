
#' Read cdhit cluster file
#'
#' @param fname input file name
#' @import dplyr
#' @import tidyr
#' @export
read.cdhit.clstr <- function(fname) {
	read.table(fname, sep = "\t", comment.char = "", quote = "", fill = T, stringsAsFactors = F, col.names = c("Seq.Num", "Seq.Name")) %>%
		separate(Seq.Num, into = c("Seq.Num", "Cluster"), sep = " ", fill = "right") %>%
		fill(Cluster) %>%
		filter(!grepl(">", Seq.Num)) %>%
		separate(Seq.Name, into = c("Seq.Len", "Seq.Name"), sep = "aa, >") %>%
		extract(Seq.Name, into = c("Seq.Name", "Is.Representative", "Identity"), regex = "(.*?)[.][.][.] ([*]|at) ?([\\d.]+)?%?$") %>%
		mutate(Is.Representative = Is.Representative == "*") %>%
		mutate(Seq.Num = as.numeric(Seq.Num), Seq.Len = as.numeric(Seq.Len), Identity = as.numeric(Identity) %>% replace_na(100)) %>%
		group_by(Cluster) %>%
		mutate(Representative = Seq.Name[which(Is.Representative)]) %>%
		ungroup
}
