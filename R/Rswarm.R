## The main wrapper around swarm
# frqfile <- list.files("_bind_primerseq_demultiplex_run2/cpmp", pattern="hapFreq", full.names=T)
# prefix <- sub("_hapFreq\\.fa", "", basename(frqfile))
# outdir <- file.path(dirname(frqfile), "fb3")
# outfile <- file.path(outdir, paste(prefix, ".swarms", sep=""))
# structfile <- file.path(outdir, paste(prefix, ".struct", sep=""))
# logfile <- file.path(outdir, paste(prefix, ".log.txt", sep=""))
# statsfile <- file.path(outdir, paste(prefix, ".stats.txt", sep=""))
# repfile <- file.path(outdir, paste(prefix, ".representatives.fasta", sep=""))
# syscall <- paste("-f -b 3 -w", repfile, "-l", logfile, "-s", statsfile, "-i", structfile, "-o", outfile, frqfile, sep=" ")
# swarm(syscall)
swarm <- function(sequences, ..., outfile,
                   force=FALSE, strict=TRUE)
{
    args <- list(...)
    args <- args[setdiff(names(args), c("1", "2", "12"))]
    seqIn <- !is.null(args[["c"]]) && args[["c"]]
    seqArg <- ""
    if(strict)
    {
        seqArg <- switch(type,
                         single={
                             if(!is.character(sequences) || (!seqIn && !all(file.exists(sequences))))
                                 stop("Argument 'sequences' has to be a character vector of filenames ",
                                      "to align against the swarm index or a character of read ",
                                      "sequences if the additional argument c==TRUE.")
                             paste(shQuote(sequences), collapse=",")
                         },
                         paired={
                         if(!is.list(sequences) || length(sequences)!=2)
                             stop("Argument 'sequences' must be a list of length 2.")
                         tmp <- NULL
                         for(i in 1:2)
                         {
                             if(!is.character(sequences[[i]]) || (!seqIn && !all(file.exists(sequences[[i]]))))
                                 stop("Argument 'sequences[[", i, "]]' has to be a character vector of filenames ",
                                      "to align against the swarm index or a character of read ",
                                      "sequences if the additional argument c==TRUE.")
                             tmp <- paste(tmp,  "-", i, " ", paste(shQuote(sequences[[i]]), collapse=","), " ", sep="")
                         }
                         tmp
                     },
                     crossbow={
                         if(!is.character(sequences) || (!seqIn && !all(file.exists(sequences))))
                                 stop("Argument 'sequences' has to be a character vector of filenames ",
                                      "to align against the swarm index or a character of read ",
                                      "sequences if the additional argument c==TRUE.")
                         paste("-12 ", paste(shQuote(sequences), collapse=","))
           })
    
        if(!is.character(index) || !file.exists(dirname(index)))
            stop("Argument 'index' has to be a character scalar giving the path to the index directory.")
    }
    outfile <- if(!missing(outfile))
    {
        if(strict && (!is.character(outfile) || length(outfile)!=1))
            stop("Argument 'outfile' must be a character scalar giving the output ",
                 "file name to store the swarm alignments in.")
        if(strict && (file.exists(outfile) && !force))
            stop("File '", outfile, "' exists. Use 'force=TRUE' to overwrite.")
        sprintf(" %s", shQuote(outfile))
    } else ""
   
    
    args <- sprintf("%s %s %s %s", .createFlags(args), shQuote(index), seqArg, outfile)
    return(invisible(.swarmBin("swarm", args)))
}

## Little helpers that return a description of the intended usage for swarm and swarm-build
swarm_usage <- function()
    print(swarm("dummy", "dummy", force=TRUE, usage=TRUE, strict=FALSE))

swarm_version <- function(){
    print(.swarmBin(bin="swarm", args="--version"))
}



## A helper function to create a scalar of command line arguments from a named list.
## Logical list entries are being interpreted as flags, all other entries are being
## collapsed into the form '<entryName>=<entryValue>'. Vectors of non-logical entry
## values will be collapsed into a single comma-separated scalar.
.createFlags <- function(flagList)
{
    if(!length(flagList))
        return("")
    if(is.null(names(flagList)) || any(names(flagList)==""))
        stop("Unable to create command line arguments from input.")
    logFlags <- sapply(flagList, is.logical)
    flags <- NULL
    if(any(logFlags))
    {
        fnames <- names(flagList)[logFlags][sapply(flagList[logFlags], function(x) x[1])]
        flags <- paste(sapply(fnames, function(x) ifelse(nchar(x)==1, sprintf("-%s", x), sprintf("--%s", x))),
                       collapse=" ")
    }
    fnames <- sapply(names(flagList)[!logFlags], function(x) ifelse(nchar(x)==1, sprintf("-%s", x),
                                                                    sprintf("--%s", x)))
    flags <- paste(flags, paste(fnames, sapply(flagList[!logFlags], paste, collapse=","),
                                collapse=" ", sep=" "), collapse=" ")
    return(gsub("^ *| *$", "", flags))
}


## A helper function to call one of the two swarm binaries with additional arguments.
.swarmBin <- function(args="")
{
    if(is.null(args) || args=="")
        stop("The swarm binaries need to be called with additional arguments")
    args <- gsub("^ *| *$", "", args)
    call <- paste(shQuote(file.path(system.file(package="Rswarm"), "swarm")), args)
    #return(call)
    output <- system(call, intern=TRUE)
    return(output)
}

## The direct binary call function
.execute <- function(callstr, ...){
  call <- file.path(shQuote(system.file(package="Rswarm")), callstr)
  return(system(call, ...))
}

