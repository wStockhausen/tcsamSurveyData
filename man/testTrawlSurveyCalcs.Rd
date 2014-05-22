\name{testTrawlSurveyCalcs}
\alias{testTrawlSurveyCalcs}
\title{Test trawl survey functions.}
\usage{
testTrawlSurveyCalcs()
}
\description{
This function provides a test of the trawl survey
functions.
}
\details{


This function prompts the user for survey data and strata
files. It then calls selectStrata.TrawlSurvey for Tanner
crab (Bairdi tanner crab, 'BTC') twice, once for the
revised strata, once for the original strata. It then
selects hauls and individuals, calculates biomass time
series, cpue by haul and station, and biomass time series
by stratum, E/W 166W, and total and outputs all (or most)
to csv files in the user's working directory.
}

