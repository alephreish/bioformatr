
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
	metadata <- list(...)
	METADATA <- ""
	if (length(metadata) > 0) {
		METADATA <- lapply(metadata, paste, collapse = ",") %>%
			do.call(rbind, .) %>%
			as.data.frame %>%
			mutate(cat = paste(rownames(.), .[[1]], sep = ",")) %>%
			pull(cat)
	}
	cat(
		TYPE,
		"SEPARATOR COMMA",
		METADATA,
		"DATA",
		DATA %>% unite(cat, sep = ",") %>% pull(cat),
		file = FILE, sep = "\n"
	)
}
