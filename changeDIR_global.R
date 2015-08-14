## Author: Azedine Zoufir
## Supervisor : Dr Andreas Bender
## All rights reserved 
## 20/10/14

library(plyr)

## This script is used to modify all paths in the scripts
## of this folder.

## This must be executed when scripts are run for the first time


## PLEASE ONLY MODIFY THE PATHS AND THE VARIABLES BELOW #############


WORK_DIR = system("echo $WORK_DIR",intern=T)

Proj_dirs = c(file.path(WORK_DIR,"/toxCast_lincs_integration/"),
              file.path(WORK_DIR,"/PKD_Leo_Price"),
              file.path(WORK_DIR,"bioseek_Xitong_Ellen/3CpHGn"),
              file.path(WORK_DIR,"bioseek_Xitong_Ellen/NScore_cliques"),
              file.path(WORK_DIR,"bioseek_Xitong_Ellen/targetPred_fullBioMap"))

PKD_DATA_DIR='/scratch/az338/ucc-fileserver/PKD_Project_Data/'


# Tox variable
#TOX = "livrCarcino"
TOX = "BW_decrs"

####### END MODIFICATIONS ######################

#For each project directory
l_ply(Proj_dirs, function(d) {
          # Path to the directory containing the data for this project
          DATA_DIR = file.path(d,"/data/")

          # Data folder on Caluclon is now same as data folder in Rover 
          # thanks to the shared folder ucc-fileserver/Calculon
          CALC_DATA_DIR = DATA_DIR

          ## Path for the figure directory included in the tex/pdf files
          FIG_DIR = file.path(d,'figures/')
          
          # Get scripts
          filenames = c(system(paste("find ",file.path(d,"scripts"),' -name \\*\\.R',sep=""),intern=T),
                        system(paste("find ",file.path(d,"scripts"),' -name \\*\\.Rnw',sep=""),intern=T),
                        system(paste("find ",file.path(d,"scripts"),' -name \\*\\.r',sep=""),intern=T),
                        system(paste("find ",file.path(d,"scripts"),' -name \\*\\.rnw',sep=""),intern=T),
                        system(paste("find ",file.path(d,"scripts"),' -name \\*\\.py',sep=""),intern=T))
          
          idx = grep('\\~$',filenames)
          if(length(idx)>0)
              filenames = filenames[-idx]
          
          
          #For each file : Make the changes
          l_ply(filenames, function(f) {
                    x=readLines(f)
                    x[grep("^PROJECT_DIR=|^PROJECT_DIR =", x )] = paste("PROJECT_DIR=\'",d,"\'",sep="")
                    x[grep("^DATA_DIR=|^DATA_DIR =", x )] = paste("DATA_DIR=\'",DATA_DIR,"\'",sep="")
                    x[grep("^CALC_DATA_DIR=|^CALC_DATA_DIR =", x )] = paste("CALC_DATA_DIR=\'",CALC_DATA_DIR,"\'",sep="")
                    x[grep("^PKD_DATA_DIR=|^PKD_DATA_DIR =", x )] = paste("PKD_DATA_DIR=\'",PKD_DATA_DIR,"\'",sep="")
                    x[grep("^FIG_DIR=|^FIG_DIR =|^FIG_PATH = |^FIG_PATH=",x)] = paste("FIG_DIR=\'",FIG_DIR,"\'",sep="")
                    x[grep("^TOX=|^TOX =",x)] = paste("TOX=\'",TOX,"'",sep='')
                    cat(x, file=file.path(f), sep="\n")
          })
})



