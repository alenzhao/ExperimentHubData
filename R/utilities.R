### =========================================================================
### utility functions
### -------------------------------------------------------------------------
###

writeMetadataDF <- function(Title, Description, BiocVersion, Genome, 
                            SourceType, SourceUrl, SourceVersion, Species,
                            TaxonomyId, RDataPath, Coordinate_1_based, 
                            DataProvider, Maintainer, RDataClass, Tags) 
{

    meta <- DataFrame(
        Title = CharacterList(Title),
        Description = CharacterList(Description),
        BiocVersion = CharacterList(BiocVersion),
        Genome = CharacterList(Genome),
        SourceType = CharacterList(SourceType),
        SourceUrl = CharacterList(SourceUrl),
        SourceVersion = CharacterList(SourceVersion),
        Species = CharacterList(Species),
        TaxonomyId = IntegerList(TaxonomyId),
        Coordinate_1_based = LogicalList(Coordinate_1_based),
        DataProvider = CharacterList(DataProvider),
        Maintainer = CharacterList(Maintainer),
        RDataClass = CharacterList(RDataClass),
        Tags = CharacterList(Tags))

    saveRDS(meta, file = "metadata.Rda") 
}

importMetadataDF <- function(pathToPackage) 
{
     meta <- readRDS(file.path(pathToPackage, "inst/extdata/metadata.Rda"))
     ## FIXME: data.frame also ok?
     if (!is(meta, "DataFrame"))
         stop("metadata.Rda must contain a single 'DataFrame' object")

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

    DataFrame(meta, RDataDateAdded=CharacterList(Sys.time()))
}

## NOTE: 'HubRoot' is the local prefix; 'pathToData' is used both
##        locally (to find the file) and remotely (to store the file).
## NOTE: This function replaces AnnotationHubData::updateResources().
##       An alternative is to make updateResources() more flexible ...
addResources <- function(ExperimentHubRoot, pathToPackage, pathToData, 
                         metadataOnly=TRUE, insert=FALSE,
                         justRunUnitTest=FALSE, ...) 
{

    if (insert) {
        if(is.null(url <- getOption("EXPERIMENT_HUB_SERVER_POST_URL")))
            stop(paste0("When 'insert=TRUE' option ",
                        "EXPERIMENT_HUB_SERVER_POST_URL must be set ",
                        "in .Rprofile"))
    }

    ## FIXME: better way to handle DF with multiple rows? 
    DF <- importMetadataDF(pathToPackage)
    metadata <- lapply(seq_len(nrow(DF)), 
        function(i) {
            row <- lapply(DF[i,], unlist)
            do.call(ExperimentHubMetadata, 
                    c(list(ExperimentHubRoot, RDataPath=pathToData), row))
        })

    ## push data to S3 
    ## FIXME: what is the role of 'ANNOTATION_HUB_BUCKET_NAME';
    ##        should we implement 'EXPERIMENT_HUB_BUCKET_NAME'?
    if(!metadataOnly)
        pushResources(metadata, ExperimentHubRoot, 
                      bucket = getOption("ANNOTATION_HUB_BUCKET_NAME", 
                                         "experimenthub"))

    ## insert metadata in db
    if(insert)
        pushMetadata(metadata, url)
 
    metadata 
}
