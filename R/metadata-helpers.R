### =========================================================================
### readMetadataFromCsv(), makeExperimentHubMetadata()
### -------------------------------------------------------------------------
###

## TODO: enforce data type
## TODO: sapply(as.character(row), strsplit, ",", ",", fixed=TRUE)
readMetadataFromCsv <- function(pathToPackage) 
{
     meta <- read.csv(file.path(pathToPackage, "inst/extdata/metadata.csv"),
                      stringsAsFactors=FALSE)

     fields <- c("Title", "Description", "BiocVersion", "Genome", 
                 "SourceType", "SourceUrl", "SourceVersion", "Species", 
                 "TaxonomyId", "Coordinate_1_based", "DataProvider", 
                 "Maintainer", "RDataClass", "Tags", "DispatchClass",
                 "ResourceName")
     missing <- !fields %in% names(meta)
     if (any(missing))
         stop(paste0("missing fields in metadata.csv: ", 
                     paste(names(meta)[missing], collapse=", ")))
     invalid <- !fields %in% names(meta)
     if (any(invalid))
         stop(paste0("invalid fields in metadata.csv: ", 
                     paste(fields[invalid], collapse=", ")))

    meta$RDataDateAdded <- rep(Sys.time(), nrow(meta))
    package <- basename(pathToPackage)
    path <- paste0("http://s3.amazonaws.com/experimenthub/", package) 
    meta$RDataPath <- paste0(path,"/",meta$ResourceName)
    meta$PreparerClass <- package 
    meta
}

makeExperimentHubMetadata <- function(pathToPackage) 
{
    meta <- readMetadataFromCsv(pathToPackage)
    apply(meta, 1, 
        function(xx) {
            ## Expect BiocVersion and Tags to be comma separated
            xx["BiocVersion"] <- 
                strsplit(as.character(xx["BiocVersion"]), ",", fixed=TRUE)
            xx["Tags"] <- 
                strsplit(as.character(xx["Tags"]), ",", fixed=TRUE)
            with(xx, 
                 ExperimentHubMetadata(Title=Title, Description=Description, 
                                       BiocVersion=BiocVersion, Genome=Genome, 
                                       SourceType=SourceType, 
                                       SourceUrl=SourceUrl,
                                       SourceVersion=SourceVersion, 
                                       Species=Species, TaxonomyId=TaxonomyId,
                                       Coordinate_1_based=Coordinate_1_based, 
                                       DataProvider=DataProvider,
                                       Maintainer=Maintainer, 
                                       RDataClass=RDataClass, Tags=Tags, 
                                       RDataDateAdded=RDataDateAdded, 
                                       RDataPath=RDataPath, 
                                       DispatchClass=DispatchClass,
                                       PreparerClass=PreparerClass)) 
        }
    )
}
