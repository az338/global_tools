library(org.Hs.eg.db)


uniprot_to_symbol=function(uni) {
    select(org.Hs.eg.db,
           keys=uni,
           keytype='UNIPROT',
           columns='SYMBOL')
}


symbol_to_uniprot = function(sym) {
  select(org.Hs.eg.db,
         keys=sym,
         keytype='SYMBOL',
         columns='UNIPROT')
}