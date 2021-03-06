% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/id_convert.R
\name{id_convert}
\alias{id_convert}
\title{Convert Metabolite Identifiers}
\usage{
id_convert(ids, to = "FOBI", fobi = fobitools::fobi)
}
\arguments{
\item{ids}{A vector with metabolite IDs to convert. Input ID types can be FOBI, raw metabolite names (used in FOBI), HMDB, KEGG, PubChemCID, InChIKey, InChICode, ChemSpider, and a combination of them.}

\item{to}{Target ID type. If possible, metabolites will be converted to this ID type. Options are "FOBI" (default), "metaboliteNames", "HMDB", "KEGG", "PubChemCID", "InChIKey", "InChICode", and "ChemSpider".}

\item{fobi}{FOBI table obtained with `parse_fobi()`. If this value is set to NULL, the last version of FOBI will be downloaded from GitHub.}
}
\value{
A tibble with input IDs and converted IDs.
}
\description{
This function can convert metabolite identifiers to other available IDs in FOBI. Input vector can be a combination of different IDs.
}
\examples{

ids <- c(fobitools::idmap$HMDB[1:10], 
         fobitools::idmap$KEGG[11:23], 
         fobitools::idmap$InChIKey[100:150])
fobitools::id_convert(ids, to = "FOBI")

}
\references{
Pol Castellano-Escuder, Raúl González-Domínguez, David S Wishart, Cristina Andrés-Lacueva, Alex Sánchez-Pla, FOBI: an ontology to represent food intake data and associate it with metabolomic data, Database, Volume 2020, 2020, baaa033, https://doi.org/10.1093/databa/baaa033.
}
\author{
Pol Castellano-Escuder
}
