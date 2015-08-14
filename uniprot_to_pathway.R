
## Author: Azedine Zoufir
## Supervisor : Dr Andreas Bender
## All rights reserved
## 14/5/2015

library(org.Hs.eg.db)
library(reactome.db)
library(KEGGREST)
library(plyr)
library(reshape2)


# Helper functions to fetch pathways from target uniprot 

target_to_KEGG = function(uniprot) {
    # convert uniprot to kegg gene id
    hsa_ids = unlist(keggConv('hsa',paste('uniprot:',uniprot,sep='')))
    # get pathway ids for kegg gene id
    if(length(hsa_ids) > 0) path_ids =  keggLink('pathway',hsa_ids)
    else path_ids=NULL
    # get db output for pathway id
    if(!is.null(path_ids)) {
      db_out =  llply(path_ids,keggGet) 
      # get pathways from db output
      pathways = unlist(llply(db_out, function(x) x[[1]]$PATHWAY_MAP))
    }
    else pathways = NULL
    # if no pathway then return nothing string
    if(is.null(pathways) | length(pathways) == 0) {
          pathways = '---'
    }
    return(pathways)
}
 

target_to_reactome = function(uniprot) {   # WARNING: UNTESTED VERSION --- older version is deprecated  
  # Get Entrez id from Uniprot ID of targets selected above
  entrez_ids = try(select(org.Hs.eg.db,
                          keys = uniprot,
                          keytype = "UNIPROT",
                          columns = c("GENENAME","ENTREZID")),silent=T)
  
  if(class(entrez_ids) == 'try-error') return('---')
  
  pathways = llply(entrez_ids$ENTREZID, function(id) {
    if(!is.na(id)) {
      path_ids = reactomeEXTID2PATHID[[id]]
      if(!is.null(path_ids)){
        # pathway ids to pathway name
        pathways = unlist(llply(path_ids, function(pid) reactomePATHID2NAME[[pid]]))
      }
      else {
        # return "nothing-string" if no pathway id
        pathways = '---'
      }
      # remove 'Homo Sapiens' header from pathway names
      pathways = gsub('Homo sapiens: ','',pathways)
      return(pathways)
    }
  })
}
