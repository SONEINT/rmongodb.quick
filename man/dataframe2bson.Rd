\name{dataframe2bson}
\alias{dataframe2bson}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts \link{data.frame} to a bson object.
}
\description{
Converts \link{data.frame} to a \link{mongo.bson} object. The bson object can be written to the database with  \link{mongo.insert.batch}.
}
\usage{
dataframe2bson(dataframe)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{A data.frame object.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{A \link{mongo.bson} object.}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jan Stanstrup, \email{stanstrup@gmail.com}
}
\note{

}

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
