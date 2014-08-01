

library(devtools)
library(IT2)

all.orchards <- read.csv(file.path(path.expand.2("~"), "ProjectPaths", "projectid_2_directory.csv"), as.is = TRUE)


tdir <- tempdir()

urly <- 'http://images.fineartamerica.com/images-medium-large-5/tree-fog-sunrise-robert-woodward.jpg'


destin <- file.path(gsub("\\\\","/",tdir),"image.jpg")

download.file(urly,destin)

