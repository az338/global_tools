library(knitr)
args <- commandArgs(trailingOnly = TRUE)

# get working dir 
wd = getwd()

# set working directory using second argument
# should be directory where the tex,pdf files
# will be created
setwd(file.path(wd,args[2]))

# name of the rnwfile is the first argument
rnwfile = file.path(wd,args[1])

# get path where Rnw file is located
fpath = unlist(strsplit(rnwfile,"/|.Rnw"))
# get the name of the .Rnw file (without the extension)
fname = fpath[length(fpath)]
# create path to the texfile using previous path and name
texfile = file.path(wd,'/',args[2],paste(fname,".tex",sep=""))

cat(texfile,'\n')

# convert from Rnw to tex
knit(rnwfile)

#convert from tex to pdf
system(paste("pdflatex ",texfile,sep=""))
