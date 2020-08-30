.downloadRsrcode <- function(path="Rswarm"){
  require(devtools)
  require(git2r)
  dir.create(path, recursive=TRUE)
  repo <- clone("https://github.com/lerch-a/Rswarm.git", path)
}


.downloadBinaries <- function(path="Rswarm", tag="3.0.0", platform=Sys.info()[["sysname"]]){

  require(devtools)
  
  # create directory for binary file
  dir.create(file.path(path, 'inst'), showWarnings = FALSE)
  
  # create temporary directory
  tmpdir <- file.path(path, 'tmp')
  dir.create(tmpdir, showWarnings = FALSE)

  url <- sprintf("https://github.com/torognes/swarm/releases/download/v%s/swarm-%s", tag, tag)

  if (grepl('windows', platform, ignore.case = TRUE)) {
    # windows binaries
    swarm_bin_url <- paste0(url, "-win-x86_64.zip")
    unwrap <- unzip
    exe_name <- "bin/swarm.exe"
    
  } else if (grepl('linux', platform, ignore.case = TRUE)) {
    # linux binaries
    swarm_bin_url <- paste0(url, "-linux-x86_64.tar.gz")
    unwrap <- untar
    exe_name <- "bin/swarm"
    
  } else {
    # macos binaries
    swarm_bin_url <- paste0(url, "-macos-x86_64.tar.gz")
    unwrap <- untar
    exe_name <- "bin/swarm"
  }
  
  # download swarm 
  swarm_bundle <- file.path(tmpdir, basename(swarm_bin_url))
  download.file(url = swarm_bin_url, destfile = swarm_bundle, mode = 'wb')
  # unpack
  unwrap(swarm_bundle, exdir = tmpdir)
  unlink(swarm_bundle)
  # move binary to inst/swarm(.exe)
  swarm_bin <- list.files(tmpdir, pattern = 'swarm', full.names = TRUE)[1]
  invisible(file.rename(file.path(swarm_bin, exe_name), file.path(path, "inst", basename(exe_name))))
  
  # remove tmpdir
  unlink(tmpdir, recursive = TRUE)
  # remove src to prevent compilation
  unlink(file.path(path, "src"), recursive = TRUE)

}

.buildBinaries <- function(path="Rswarm"){
  devtools::build(pkg = path, path = ".", binary = T, vignettes = F, manual = T)
}

#.downloadRsrcode()
.downloadBinaries()
.buildBinaries()