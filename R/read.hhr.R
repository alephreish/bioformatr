
#' Read hhr file (HH-suite output format)
#'
#' @param fname input file name
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @export
read.hhr <- function(fname) {
	all.lines <- readLines(fname)

	param.start <- 1
	data.start  <- which(grepl("^ *No Hit", all.lines)) %>% first %>% `+`(1)

	align.start <- which(grepl("^No 1", all.lines)) %>% first

	param.end <- data.start - 2
	data.end  <- align.start - 1
	align.end <- length(all.lines)

	data <- data.frame(line = all.lines[align.start:align.end], stringsAsFactors = F) %>%
		filter(line != "") %>%
		extract(line, into = c("name", "value"), regex = "([^ ]+) ?(.+)?", remove = F) %>%
		mutate(No = ifelse(name == "No", value, NA) %>% as.integer) %>%
		mutate(Hit.ID = ifelse(substr(name, 1, 1) == ">", substr(name, 2, nchar(.)), NA)) %>%
		mutate(Hit.Description = ifelse(substr(name, 1, 1) == ">", value, NA)) %>%
		mutate(Match = ifelse(grepl("=", name), line, NA)) %>%
		mutate(name = ifelse(grepl("Q Consensus", lag(line)) & grepl("T Consensus", lead(line)), "M", name)) %>%
		mutate(value = ifelse(name == "M", line, value)) %>%
		fill(No) %>%
		group_by(No) %>%
		summarize(
			Hit.ID       = na.omit(Hit.ID) %>% first,
			Hit.Description = na.omit(Hit.Description) %>% first,
			Match        = na.omit(Match) %>% first,
			Q.ss_pred    = value[name == "Q" & grepl("^ss_pred ", value)]         %>% substr(., 16, nchar(.)) %>% paste(collapse = "") %>% gsub(" +", "", .),
			Q.query      = value[name == "Q" & grepl("^Consensus ", lead(value))] %>% substr(., 16, nchar(.)) %>% paste(collapse = " "),
			Q.consensus  = value[name == "Q" & grepl("^Consensus ", value)]       %>% substr(., 16, nchar(.)) %>% paste(collapse = " "),
			T.consensus  = value[name == "T" & grepl("^Consensus ", value)]       %>% substr(., 16, nchar(.)) %>% paste(collapse = " "),
			T.hit        = value[name == "T" & grepl("^Consensus ", lag(value))]  %>% substr(., 16, nchar(.)) %>% paste(collapse = " "),
			T.ss_dssp    = value[name == "T" & grepl("^ss_dssp ", value)]         %>% substr(., 16, nchar(.)) %>% paste(collapse = " ") %>% gsub(" +", "", .),
			T.ss_pred    = value[name == "T" & grepl("^ss_pred ", value)]         %>% substr(., 16, nchar(.)) %>% paste(collapse = "")  %>% gsub(" ", "", .),
		) %>%
		ungroup %>%
		extract(Q.consensus, into = c("Q.Start", "Q.End", "Q.Length"), regex = "^ *(\\d+) .+ (\\d+) +[(](\\d+)[)]$", remove = F, convert = T) %>%
		extract(T.consensus, into = c("T.Start", "T.End", "T.Length"), regex = "^ *(\\d+) .+ (\\d+) +[(](\\d+)[)]$", remove = F, convert = T) %>%
		mutate(
			Q.consensus  = gsub("[0-9() ]+", "", Q.consensus),
			Q.query      = gsub("[0-9() ]+", "", Q.query),
			T.consensus  = gsub("[0-9() ]+", "", T.consensus),
			T.hit        = gsub("[0-9() ]+", "", T.hit),
		) %>%
		#extract(Hit.Description, into = "Hit.Organism",    regex = "[{]([^}]+)[}]",  remove = F) %>%
		#extract(Hit.Description, into = "Hit.Description", regex = "([^;]+)",        remove = F) %>%
		#extract(Hit.Description, into = "Hit.Keywords",    regex = "[^;]+; ([^;]+)", remove = F) %>%
		mutate(Match = str_split(Match, " +")) %>%
		unnest %>%
		separate(Match, into = c("key", "value"), "=") %>%
		mutate(value = sub("%", "", value) %>% as.numeric) %>%
		spread(key, value) %>%
		rename(E.value = `E-value`) %>%
		mutate(Aligned_cols = as.integer(Aligned_cols))

	attr(data, "metadata") <- data.frame(value = all.lines[param.start:param.end]) %>%
		mutate(
			key = substr(value, 1, 14) %>% trimws,
			value = substr(value, 14, 10000) %>% trimws
		)

	return(data)
}

