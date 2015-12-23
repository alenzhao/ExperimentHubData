\name{makeMetadataFromCsv}

\alias{makeMetadataFromCsv}
\alias{readMetadata}

\title{Metadata File Helpers}

\description{
  Read metadata file for an ExperimentHub resource into R 
}

\usage{
  makeMetadataFromCsv(pathToPackage)
  readMetadata(pathToPackage)
}

\arguments{
  \item{pathToPackage}{
    character string giving the full path to metadata.csv
  }
}

\details{
  The metadata file is a csv file containing information about the 
  \code{ExperimentHub} resource. Rows correspond to individual resources
  (i.e., data objects) and columns to the metadata fields.

  Required columns:
  \itemize{
    \item Title: \code{character}. Name of the resource.

    \item Description: \code{character}. Brief description of the resource,
          similar to the 'Description' field in a package DESCRIPTION file.

    \item BiocVersion: \code{character} vector. All Bioconductor versions
          the resource should be made available for.

    \item Genome: \code{character}. Genome.

    \item SourceType: \code{character}. Format of original data, e.g., FASTA,
          BAM, BigWig, etc.

    \item SourceUrl: \code{character}

    \item SourceVersion: \code{character}

    \item Species: \code{character}

    \item TaxonomyId: \code{character(1)}

    \item Coordinate_1_based: \code{logical}

    \item DataProvider: \code{character}

    \item Maintainer: \code{character}

    \item RDataClass: \code{character}

    \item Tags: \code{character}
  }
}

\value{
    A data.frame for \code{readMetadata} and a list of
    \code{ExperimentHubMetadata} objects for \code{makeMetadataFromCsv}.
}

\seealso{
}

\keyword{methods}
