#' @importFrom utils unzip
execBlast  <- function(seq_file, filter_files, blast_evalue, nthreads) {
	if(length(filter_files) == 0)
		return(filter_files)
	cat('Executing blast...\n')
	tstart = Sys.time()
	cmd <- paste0('formatdb -i ', seq_file, ' -p F > /dev/null')
	system(cmd)
	for(i in 1:length(filter_files)) {
		ffile <- filter_files[i]
		uncompressedFile = {}
		if(substr(ffile, nchar(ffile)-2, nchar(ffile)) == 'zip') {
			uncompressedFile = unzip(ffile)
			ffile = uncompressedFile$Name[1]
		}
		filter_files[i] <- tempfile()
		cmd <- paste0('blastall -p blastn -i ', ffile,
			      ' -F F -d ', seq_file,
			      ' -S 1 -e ', blast_evalue,
			      ' -a ', nthreads,
			      ' -m 8 -o ', filter_files[i])
		system(cmd)
		for(uf in uncompressedFile)
			file.remove(uf)
	}
	cat('   - Elapsed time: ', (Sys.time() - tstart) / 60, ' min\n')
	return(filter_files)
}
