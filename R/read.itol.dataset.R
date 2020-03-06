
#' Read itol dataset file
#'
#' @param TYPE dataset type
#' @param DATA dataframe with the data
#' @param FILE output file name
#' @param ... fields to add to the metadata block
#' @import dplyr
#' @import tidyr
#' @export
read.itol.dataset <- function(FILE) {
	all.lines <- readLines(FILE) %>% `[`(. != "")
	ll <- 1:length(all.lines)

	typ.num <- which(grepl("^[A-Z]", all.lines)) %>% first
	sep.num <- which(grepl("^SEPARATOR", all.lines)) %>% first
	dat.num <- which(all.lines == "DATA") %>% first
	if (is.na(typ.num)) stop("Dataset type not defined")
	if (is.na(sep.num)) stop("Separator not defined")
	if (is.na(dat.num)) stop("Data not defined")

	TYPE <- all.lines[typ.num] %>% substr(9, 100)
	SEPARATOR <- all.lines[sep.num] %>% substr(11, 100)
	seps <- list(TAB = "\t", COMMA = ",")
	SEP <- seps[[SEPARATOR]]
	if (is.null(SEP)) stop("Unrecognized separator")

	METADATA <- all.lines[ll < dat.num & ll != typ.num & ll != sep.num] %>%
		data.frame %>%
		setNames("name") %>%
		mutate(to = strsplit(as.character(name), SEP)) %>%
		unnest(to) %>%
		group_by(name) %>%
		mutate(row = row_number()) %>%
		spread(row, to) %>%
		ungroup %>%
		select(-name) %>%
		split(.[1]) %>%
		lapply(function(x) {
			select(x, -1) %>% unlist %>% na.omit %>% c %>% unname
		})

	if (is.null(METADATA$FIELD_LABELS)) stop("FIELD_LABELS not defined")

	DATA <- all.lines[ll > dat.num] %>%
		data.frame %>%
		setNames("name") %>%
		separate(name, into = c("name", METADATA$FIELD_LABELS), sep = SEP)

	list(METADATA = METADATA, DATA = DATA)

}
