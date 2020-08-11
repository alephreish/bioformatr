#' Read hmmer output file
#'
#' @param fname file name
#' @import tidyr
#' @import dplyr
#' @import stringr
#' @export
read.hmmer <- function(fname) {
	all.lines <- readLines(fname)

	metadata <- list()

	query.start <- grepl("^Query:", all.lines) %>% which %>% first
	meta.lines <- all.lines[1:(query.start-1)]
	domains <- data.frame(lines = all.lines[query.start:length(all.lines)]) %>%
		extract(lines, into = "Query", regex = "^Query: +(.+?) +\\[.=\\d+\\]", remove = F) %>%
		fill(Query) %>%
		group_split(Query) %>%
		lapply(function(query.lines) {

			Query <- query.lines$Query[1]

			table.start   <- grepl("^Scores for complete sequence", query.lines$lines) %>% which
			domain.start  <- grepl("^Domain annotation for each", query.lines$lines) %>% which
			summary.start <- grepl("^Internal pipeline statistics summary", query.lines$lines) %>% which

			metadata[[Query]] <<- query.lines[c(1:(table.start-1), summary.start:nrow(query.lines)),] %>%
				separate(lines, into = c("key", "value"), sep = ": +", fill = "left", extra = "merge") %>%
				filter(!is.na(key)) %>%
				select(1:2)

			col.names <- c("full.E.value", "full.score", "full.bias", "best.E.value", "best.score", "best.bias", "dom.exp", "dom.N", "Sequence")
			table.data <- query.lines[(table.start+1):(domain.start-1),] %>%
				pull(lines) %>%
				textConnection %>%
				read.table(sep = "", fill = T, quote = "", header = F, row.names = NULL) %>%
				select(1:9) %>%
				setNames(col.names) %>%
				mutate(full.inclusion = ifelse(full.score == "inclusion", "?", NA)) %>%
				fill(full.inclusion) %>%
				mutate(full.inclusion = replace_na(full.inclusion, "!")) %>%
				filter(!grepl("---|E-value", full.E.value), Sequence != "") %>%
				mutate_at(1:8, as.numeric)

			query.lines[(domain.start+1):(summary.start-1),] %>%
				extract(lines, into = "Sequence", regex = "^>> ([^ ]+)", remove = F) %>%
				fill(Sequence) %>%
				filter(!is.na(Sequence)) %>%
				group_split(Sequence) %>%
				lapply(function(sequence.lines) {
					Sequence <- sequence.lines$Sequence[1]
					align.start <- grepl("Alignments for each domain", sequence.lines$lines) %>% which
					col.names <- c("domain", "inclusion", "score", "bias", "c.Evalue", "i.Evalue", "hmmfrom", "hmm.to", "hmm.ends", "alifrom", "ali.to", "ali.ends", "envfrom", "env.to", "env.ends", "acc")
					alignments <- extract(sequence.lines, lines, into = "domain", regex = " == domain ([^ ]+)", remove = F, convert = T) %>%
						fill(domain) %>%
						filter(!is.na(domain), !grepl("== domain", lines)) %>%
						group_split(domain) %>%
						lapply(function(domain.lines) {
							spaces <- filter(domain.lines, grepl(" *(.+?) +(\\d)+ +(.+?) +(\\d+)", lines)) %>% `[`(1,1) %>%
								str_locate_all(" +") %>% `[[`(1)
							align <- pull(domain.lines, lines) %>%
								textConnection %>%
								read.fwf(widths = c(spaces[2,1], spaces[3,2] - spaces[2,1], spaces[4,2] - spaces[3,1] - 1, 10), col.names = c("Name", "Start", "Seq", "End")) %>%
								mutate(Name = trimws(Name), End = trimws(End)) %>%
								filter(!is.na(Seq)) %>%
								mutate(Seq.Start = ifelse(lag(Name) == !!Sequence, lag(Start), NA)) %>%
								fill(Seq.Start, .direction = "up") %>%
								group_split(Seq.Start)
							Seqs <- lapply(align, pull, "Seq") %>%
								{suppressMessages(bind_cols(.))} %>%
								do.call(paste0, .) %>%
								rev
							data.frame(
								CS     = ifelse(length(Seqs) == 5, sub(" CS$", "", Seqs[5]), ""),
								M.Seq  = sub("[ 0-9]+$", "", Seqs[4]),
								Match  = Seqs[3],
								T.Seq  = sub("[ 0-9]+$", "", Seqs[2]),
								PP     = sub(" PP$", "", Seqs[1]),
								domain = first(domain.lines$domain)
							)
						}) %>% bind_rows

					sequence.lines$lines[4:(align.start-1)] %>%
						textConnection %>%
						read.table(sep = "", col.names = col.names) %>%
						mutate(Sequence = sequence.lines$Sequence[1], Description = str_match(sequence.lines$lines[1], ">> ([^ ]+) (.+)")[,3]) %>%
						left_join(alignments, by = "domain")
				}) %>% bind_rows %>%
				left_join(table.data, by = "Sequence")
		}) %>% bind_rows

	attr(domains, "metadata") <- metadata %>% bind_rows(.id = "Query") %>% as.data.frame
	attr(domains, "Program") <- gsub("^[# ]+| ::.+", "", all.lines[1])
	attr(domains, "Version") <- gsub("^[# ]+|;.+",   "", all.lines[2])

	return(domains)
}

