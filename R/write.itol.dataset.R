
#' Create itol dataset file
#'
#' @param TYPE dataset type
#' @param DATA dataframe with the data
#' @param FILE output file name
#' @param ... fields to add to the metadata block
#' @import dplyr
#' @import tidyr
#' @export
write.itol.dataset <- function(TYPE, DATA, FILE, ...) {
	cat(
		paste("DATASET", TYPE, sep = "_"),
		"SEPARATOR COMMA",
		list(...) %>% lapply(paste, collapse = ",") %>% do.call(rbind, .) %>% as.data.frame %>% mutate(cat = paste(rownames(.), V1, sep = ",")) %>% pull(cat),
		"DATA",
		DATA %>% unite(cat, sep = ",") %>% pull(cat),
		file = FILE, sep = "\n"
	)
}
