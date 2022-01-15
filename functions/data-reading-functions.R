setwd("C:/Users/leonh/Documents/Proyectos Personales/Premio INFONAVIT/Github/envi")

set_vid_uid = function(dataframe) {
  dataframe$vid = paste(dataframe$X.U.FEFF.FOLIO, dataframe$VIV_SEL, sep = "_")
  if ('HOGAR' %in% colnames(dataframe)) {
    dataframe$uid = paste(dataframe$X.U.FEFF.FOLIO, dataframe$VIV_SEL, dataframe$HOGAR, sep = "_")
  }
  dataframe = dataframe[ , c("vid",
                           names(dataframe)[names(dataframe) != "vid"])]
  if ('uid' %in% colnames(dataframe)) {
    dataframe = dataframe[ , c("uid",
                             names(dataframe)[names(dataframe) != "uid"])]
  }
  return(dataframe)
}