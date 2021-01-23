#--create inst/extdata objects

#----create survey grid layers list
surveyGridLayers = gisCreateSurveyGridLayers();
wtsUtilities::saveObj(surveyGridLayers,fn="inst/extdata/surveyGridLayers.RData");
