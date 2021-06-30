\name{getTpPredictionFirstHalf}
\alias{getTpPredictionFirstHalf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{getTpPredictionFirstHalf
%%  ~~function to do ... ~~
}
\description{Generates the transition probabilties of a discrete time Markov chain using a set of pre-trained multinomial logistic regression models
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getTpPredictionFirstHalf(AWAY_EFG, AWAY_TOVr, AWAY_OREBr, AWAY_DREBr, AWAY_FTF, AWAY_oEFG, AWAY_oTOVr, AWAY_oFTF, HOME_EFG, HOME_TOVr, HOME_OREBr, HOME_DREBr, HOME_FTF, HOME_oEFG, HOME_oTOVr, HOME_oFTF)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{AWAY_EFG}{A conitinuous time Markov-chain generator matrix.
%%     ~~Describe \code{x} here~~
}
  \item{AWAY_TOVr}{Number of times to simulate the game.
%%     ~~Describe \code{x} here~~
}
  \item{AWAY_OREBr}{Amount of game clock deciseconds to run each game simulation for.
%%     ~~Describe \code{x} here~~
}
  \item{AWAY_DREBr}{A conitinuous time Markov-chain generator matrix.
%%     ~~Describe \code{x} here~~
}
  \item{AWAY_FTF}{Number of times to simulate the game.
%%     ~~Describe \code{x} here~~
}
  \item{AWAY_oEFG}{Amount of game clock deciseconds to run each game simulation for.
%%     ~~Describe \code{x} here~~
}
  \item{AWAY_oTOVr}{A conitinuous time Markov-chain generator matrix.
%%     ~~Describe \code{x} here~~
}
  \item{AWAY_oFTF}{Number of times to simulate the game.
%%     ~~Describe \code{x} here~~
}

  \item{HOME_EFG}{A conitinuous time Markov-chain generator matrix.
%%     ~~Describe \code{x} here~~
}
  \item{HOME_TOVr}{Number of times to simulate the game.
%%     ~~Describe \code{x} here~~
}
  \item{HOME_OREBr}{Amount of game clock deciseconds to run each game simulation for.
%%     ~~Describe \code{x} here~~
}
  \item{HOME_DREBr}{A conitinuous time Markov-chain generator matrix.
%%     ~~Describe \code{x} here~~
}
  \item{HOME_FTF}{Number of times to simulate the game.
%%     ~~Describe \code{x} here~~
}
  \item{HOME_oEFG}{Amount of game clock deciseconds to run each game simulation for.
%%     ~~Describe \code{x} here~~
}
  \item{HOME_oTOVr}{A conitinuous time Markov-chain generator matrix.
%%     ~~Describe \code{x} here~~
}
  \item{HOME_oFTF}{Number of times to simulate the game.
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
