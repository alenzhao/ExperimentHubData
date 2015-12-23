### =========================================================================
### utility functions
### -------------------------------------------------------------------------
###

readMetadata <- function(pathToPackage) 
{
     meta <- read.csv(file.path(pathToPackage, "inst/extdata/metadata.csv"))

     fields <- c("Title", "Description", "BiocVersion", "Genome", 
                 "SourceType", "SourceUrl", "SourceVersion", "Species", 
                 "TaxonomyId", "Coordinate_1_based", "DataProvider", 
                 "Maintainer", "RDataClass", "Tags")
     missing <- !names(meta) %in% fields
     if (any(missing))
         stop(paste0("missing fields in metadata.csv: ", 
                     paste(names(meta)[missing], collapse=", ")))
     invalid <- !fields %in% names(meta)
     if (any(invalid))
         stop(paste0("invalid fields in metadata.csv: ", 
                     paste(fields[invalid], collapse=", ")))

    meta$RDataDateAdded <- rep(Sys.time(), nrow(meta))
    meta$RDataPath <- rep("http://s3.amazonaws.com/experimenthub/", nrow(meta)) 
    meta
}

makeMetadataFromCsv <- function(pathToPackage) 
{
    meta <- readMetadata(pathToPackage)
    apply(meta, 1, 
        function(xx) {
            args <- sapply(xx, function(elt) 
                strsplit(as.character(elt), ",", fixed=TRUE))
            with(args, 
                ExperimentHubMetadata(Title=Title, Description=Description, 
                                      BiocVersion=BiocVersion, Genome=Genome, 
                                      SourceType=SourceType, SourceUrl=SourceUrl,
                                      SourceVersion=SourceVersion, 
                                      Species=Species, TaxonomyId=TaxonomyId,
                                      Coordinate_1_based=Coordinate_1_based, 
                                      DataProvider=DataProvider,
                                      Maintainer=Maintainer, 
                                      RDataClass=RDataClass, Tags=Tags, 
                                      RDataDateAdded=RDataDateAdded, 
                                      RDataPath=RDataPath)) 
        }
    )
}


## NOTE: 'HubRoot' is the local prefix; 'pathToData' is used both
##        locally (to find the file) and remotely (to store the file).
## NOTE: This function replaces AnnotationHubData::updateResources().
##       An alternative is to make updateResources() more flexible ...
addResources <- function(pathToPackage, metadataOnly=TRUE, 
                         insert=FALSE, justRunUnitTest=FALSE, ...)
{

    if (insert) {
        if(is.null(url <- getOption("EXPERIMENT_HUB_SERVER_POST_URL")))
            stop(paste0("When 'insert=TRUE' option ",
                        "EXPERIMENT_HUB_SERVER_POST_URL must be set ",
                        "in .Rprofile"))
    }

    ## generate metadata
    message("generating metadata ...") 
    metadata <- makeMetadataFromCsv(pathToPackage)

    ## push data files to S3 
    if(!metadataOnly) {
        message("pushing data files to S3 ...")
        pushResources(metadata, ExperimentHubRoot, 
                      bucket = getOption("EXPERIMENT_HUB_BUCKET_NAME", 
                                         "experimenthub"))
    }

    ## insert metadata in db
    if(insert) {
        message("inserting metadata in db ...") 
        pushMetadata(metadata, url)
    }

    message("complete!") 
    metadata
}
