# Install the three plotting dependencies from CRAN when absent.
# Executed release versions are recorded in R_ggplot2_sessionInfo.txt.
required <- c('ggplot2','jsonlite','svglite')
missing <- required[!vapply(required,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) install.packages(missing,repos='https://cloud.r-project.org')
stopifnot(packageVersion('ggplot2') >= '3.4.0')
for(p in required) cat(p,as.character(packageVersion(p)),'\n')
