library(org.Hs.eg.db)


uniprot_to_symbol=function(uni) {
    select(org.Hs.eg.db,
           keys=uni,
           keytype='UNIPROT',
           columns='SYMBOL')$SYMBOL
}


