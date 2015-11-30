### =========================================================================
### ExperimentHubMetadata objects
### -------------------------------------------------------------------------
###

setClass("ExperimentHubMetadata",
    contains="HubMetadata",
    representation(
        ExperimentHubRoot="character"
    ),
    prototype = prototype(
        ExperimentHubRoot=NA_character_
    )
)

## -----------------------------------------------------------------------------
## constructor
## 

ExperimentHubMetadata <-
    function(ExperimentHubRoot=NA_character_, SourceUrl, SourceType, 
        SourceVersion, SourceLastModifiedDate=as.POSIXct(NA_character_), 
        SourceMd5=NA_character_, SourceSize=NA_real_,
        DataProvider, Title, Description,
        Species, TaxonomyId=NA_integer_, Genome, Tags,
        RDataClass, RDataDateAdded, RDataPath,
        Maintainer, BiocVersion=biocVersion(), Coordinate_1_based=TRUE,
        Notes=NA_character_,
        Location_Prefix='http://s3.amazonaws.com/experimenthub/')
{
    ## FIXME: move these checks to a general validity method
    ##        on HubMetadata that can be reused?
    if (is.na(TaxonomyId)) {
        if (!is.na(Species) &&
            requireNamespace("AnnotationHubData", quietly=TRUE))
            TaxonomyId <- GenomeInfoDb:::.taxonomyId(Species)
    }
    if(!(isSingleInteger(TaxonomyId) || is.na(TaxonomyId)))
        stop(wmsg(paste0("ExperimentHubMetdata objects can contain",
                         " only one taxonomy ID or NA")))

    if(any(is.na(SourceUrl)))
        stop(wmsg(paste0("ExperimentHubMetdata SourceUrl slot cannot",
                         " contain NAs")))

    if (missing(RDataPath)) { 
        ## Add two characters: one for substr starting AT clipChars
        ## and one for extra slash
        clipChars <- nchar(Location_Prefix) + 2 
        RDataPath <- substr(SourceUrl, clipChars, nchar(SourceUrl))
    }

    RDataDateAdded <-
        as.POSIXct(strsplit(
            as.character(RDataDateAdded), " ")[[1]][1], tz="GMT")

    mustBeSingleStringNoCommasOrNA <- 
        c(SourceType, Location_Prefix, RDataClass)
    lapply(mustBeSingleStringNoCommasOrNA, 
        AnnotationHubData:::.checkThatSingleStringAndNoCommas) 
    lapply(c(Genome, Species), 
        AnnotationHubData:::.checkThatSingleStringOrNA)
    AnnotationHubData:::.checkThatSingleStringOrNAAndNoCommas(SourceVersion)

    new("ExperimentHubMetadata",
        ExperimentHubRoot=ExperimentHubRoot,
        HubRoot=ExperimentHubRoot,
        BiocVersion=package_version(BiocVersion),
        Coordinate_1_based=Coordinate_1_based,
        DataProvider=DataProvider,
        Description=Description,
        Genome=Genome,
        Maintainer=Maintainer,
        Notes=Notes,
        RDataClass=RDataClass,
        RDataDateAdded=as.POSIXct(RDataDateAdded),
        RDataPath=RDataPath,
        SourceUrl=SourceUrl,
        SourceVersion=SourceVersion,
        SourceType=SourceType,
        Species=Species,
        Tags=Tags,
        TaxonomyId=TaxonomyId,
        Title=Title,
        Location_Prefix=Location_Prefix,
        ## FIXME: how to determine 
        SourceSize=NA_real_,
        SourceMd5=NA_character_, 
        SourceLastModifiedDate=as.POSIXct(NA_character_), #from url in AHD
        ## NOTE: not relevant 
        Recipe=NA_character_,
        DispatchClass=NA_character_
    )
}

## ------------------------------------------------------------------------------
## getters and setters
## 

## TODO

## -----------------------------------------------------------------------------
## validity 
## 

## TODO

## -----------------------------------------------------------------------------
## methods 
## 

setMethod("runRecipes", "ExperimentHubMetadata",
    function(metadata, hubroot, 
             bucket=getOption("ANNOTATION_HUB_BUCKET_NAME", "experimenthub"), 
             ...)
    {
        ## Note: allow for download from ftp site or ?

        ## upload to S3
        fileToUpload <- file.path(metadata(metadata)$HubRoot,
                                  metadata(metadata)$RDataPath)
        remotePath <- sub("^/", "", metadata(metadata)$RDataPath)
        res <- upload_to_S3(fileToUpload, remotePath, bucket, ...)
        ## TODO - if download is successful, delete local file?
    }
)
