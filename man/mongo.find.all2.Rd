\name{mongo.find.all2}
\alias{mongo.find.all2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Finds records in a collection and returns one R list or data.frame object.
}
\description{
Finds records in a collection that match a given query and returns a list or an R data frame object. This is a modified version of the official \link{mongo.find.all}. In contrast to the original function a list can be returned instead of a \link{data.frame} and mongo.oid are not destroyed when converting to a data.frame if mongo.oid2character is TRUE.
}
\usage{
mongo.find.all2(mongo, ns, query, fields=mongo.bson.empty(),   data.frame=FALSE, mongo.oid2character = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mongo}{(\link{mongo}) a mongo connection object.}
  \item{ns}{(string) namespace of the collection from which to find records.}
  \item{query}{(\link{mongo.bson}) The criteria with which to match the records to be found.}
  \item{fields}{(\link{mongo.bson}) The desired fields which are to be returned from the matching record. The default of mongo.bson.empty() will cause all fields of   the matching record to be returned; however, specific fields may be specified to cut down on network traffic and memory overhead.}
   \item{data.frame}{(boolean) If TRUE the result will be an \link{data.frame} object.}
  \item{mongo.oid2character}{(boolean) If TRUE the mongo.oid returned in the _id field will be converted to a character vector. Neccesary for correct output if \link{data.frame} is TRUE.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{An R list or an R \link{data.frame} object.}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jan Stanstrup, \email{stanstrup@gmail.com}
}
\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
