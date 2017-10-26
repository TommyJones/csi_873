## load RWeka
library(RWeka)
## look for a package providing id3
WPM("refresh-cache")
WPM("list-packages", "available") ## look for id3
## install package providing id3
WPM("install-package", "simpleEducationalLearningSchemes")
## load the package
WPM("load-package", "simpleEducationalLearningSchemes")
## make classifier
ID3 <- make_Weka_classifier("weka/classifiers/trees/Id3")
## test it out.
DF2 <- read.arff(system.file("arff", "contact-lenses.arff",
package = "RWeka"))
ID3(`contact-lenses` ~ ., data = DF2)
