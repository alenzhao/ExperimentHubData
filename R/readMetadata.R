### =========================================================================
### readMetadata()
### -------------------------------------------------------------------------
###


readMetadata <- function(file, ...)
{
    meta <- read.csv(file, ...)

    ## required fields 
    fields <- c("Title", "Description", "BiocVersion", "Genome", 
                "SourceType", "SourceUrl", "SourceVersion", "Species", 
                "TaxonomyId", "Coordinate_1_based", "DataProvider", 
                "Maintainer", "RDataClass", "Tags")
    if (any(missing <- !names(meta) %in% fields))
        stop(paste0("missing fields in metadata.Rda: ", 
                    paste(names(meta)[missing], collapse=", ")))
    if (any(invalid <- !fields %in% names(meta)))
        stop(paste0("invalid fields in metadata.Rda: ", 
                    paste(fields[invalid], collapse=", ")))
    ## add date
    meta$RDataDateAdded <- rep(Sys.time(), nrows(meta))

    ## 

  ## rows / data objects

if(is.null(row.names)) {
rn <- tab[["Label"]]
if(anyDuplicated(rn)) rn <- NULL
if(is.null(rn)) rn <- tab[["Labels"]]
if(anyDuplicated(rn)) rn <- NULL
if(is.null(rn)) rn <- tab[["FileName"]]
if(!is.null(rn)) rn <- removeExt(rn)
if(anyDuplicated(rn)) rn <- NULL
if(!is.null(rn)) row.names(tab) <- rn
} else {
row.names <- as.character(row.names)
if(row.names %in% names(tab)) row.names(tab) <- tab[[row.names]]
}

{
	tab
}


