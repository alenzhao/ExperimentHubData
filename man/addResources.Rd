\name{addResources}
\alias{addResources}


\title{addResources}

\description{
  Add new resources to ExperimentHub 
}

\usage{
addResources(ExperimentHubRoot, pathToPackage, pathToData,
             metadataOnly = TRUE, insert = FALSE,
             justRunUnitTest = FALSE, ...)
}

\arguments{
  \item{ExperimentHubRoot}{
    Prefix of the local path to data objects. This prefix is not included
    in the path when uploading files to S3.
  }
  \item{pathToPackage}{
    Local path to data package.
  }
  \item{pathToData}{
    Local path to data objects. This path must exist in the S3 bucket
    where the data will be uploaded.
  }
  \item{metadataOnly}{
    A \code{logical} to specify the processing of metadata only or both
    metadata and data files.

    When FALSE, metadata are generated and data files are downloaded,
    processed and pushed to their final location in S3 buckets. 
    \code{metadata = TRUE} produces only metadata and is useful for 
    testing.
  }
  \item{insert}{
    A \code{logical} to control if metadata are inserted in the ExperimentHub
    db. By default this option is FALSE which is a useful state in which
    to test a new recipe and confirm the metadata fields are correct.
 
    When \code{insert = TRUE}, the "EXPERIMENT_HUB_SERVER_POST_URL" global 
    option must be set to the http location of the ExperimentHubServer in
    the global environment or .Rprofile. Additionally, AWS command line tools
    must be installed on the local machine to push files to S3 buckets.
    See https://aws.amazon.com/cli/ for installation instructions.
  }
  \item{justRunUnitTest}{
    A \code{logical}. When TRUE, a small number of records (usually 5) are
    processed instead of all.
  }
  \item{\dots}{
    Arguments passed to \code{pushResources} and \code{pushMetadata}. See 
    the man pages for a description of arguments.
  }
}

\details{
   \itemize{
     \item addResources: 

           \code{addResources} is responsible for creating and inserting
           metadata records as well as pushing data files to AWS S3 buckets.
 
           By manipulating the \code{metadataOnly}, \code{insert} and
           \code{justRunUnitTest} arguments one can test metadata
           generation with a small number of records with or without 
           pushing data files to S3.


      \item global options: 

            TODO:
  }
}

\value{
  A list of \code{ExperimentHubMetadata} objects.
}

\seealso{
  \itemize{
    \item \link{ExperimentHubMetadata}
    \item \link[AnnotationHubData]{AnnotationHubMetadata}
  }
}

\examples{
## TODO
}

\keyword{methods}
