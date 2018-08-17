uniqueSequences <- function(files, nworks) {
	cat('Filtering repeated sequences...\n')
	tstart = Sys.time()
	fseqs <- {}
	nvector <- rep(0,nworks)
	for(i in 1:nworks) {
		seqs <- read.fasta(files[i], as.string = T)
		nvector[i] <- length(seqs)
		fseqs <- c(fseqs, unique(seqs))
	}
	n1 <- sum(nvector)
	fseqs <- unique(fseqs)
	n2 <- length(fseqs)
	uni <- rep(T, length(fseqs))
	for (i in 11:(length(uni)-11)) {
		if (!uni[i]) next
		atom <- fseqs[[i]]
		atom <- substr(atom, floor(nchar(atom)/2-10), ceiling(nchar(atom)/2+10))
		for (j in (i-10):(i+10)) {
			if (i==j || !uni[j]) next
			rgx <- regexpr(text = fseqs[[j]], pattern = atom)
			if (rgx > 10 && rgx < nchar(fseqs[[j]])-10){
				if (nchar(fseqs[[i]]) > nchar(fseqs[[j]]))
					uni[j] <- F
				else
					uni[i] <- F
			}
		}
	}
	fseqs <- fseqs[uni]
	sname <- rep("", length(fseqs))
	for(i in 1:length(fseqs))
		sname[i] <- attr(fseqs[[i]], 'name')
	n3 <- length(fseqs)
	files <- tempfile()
	write.fasta(file.out = files, sequences = fseqs, names = sname)
	cat('   - Deleted sequences: ', n1, ' -> ', n2, ' -> ', n3, '\n')
	cat('   - Elapsed time: ', Sys.time() - tstart, ' sec\n')
	return(files)
}
