## This script attemps to identify functions using global variables in
## the ftmsRanalysis namespace using the codetools library. However,
## this cannot correctly identify variable names used in non-standard
## evaluation (e.g. column name usage in dplyr), so the printed 
## messages are only an indication of potential problems.
## 
## Amanda M. White (8/10/2018)

library(codetools)

# load all libraries used in ftmsRanalysis
imports <- packageDescription("ftmsRanalysis")$Imports
imports <- gsub(" \\(.*\\)", "", unlist(strsplit(imports, ",?\n")))

suggests <- packageDescription("ftmsRanalysis")$Suggests
suggests <- gsub(" \\(.*\\)", "", unlist(strsplit(suggests, ",?\n")))

for (ll in c(imports, suggests)) do.call(library, list(ll))

library(ftmsRanalysis)

ns_exports <- getNamespaceExports(asNamespace("ftmsRanalysis"))
for (ns_name in ns_exports) {
  obj <- getFromNamespace(ns_name, ns="ftmsRanalysis")
  if (mode(obj) != "function") next
  
  res <- codetools::findGlobals(obj, merge = FALSE)
  
  if (length(res$variables) > 0 ) {
  # expect(length(res$variables) == 0, 
    ok <-unlist(lapply(res$variables, exists, mode="function"))
    if (any(!ok)) {
      #browser()
      cat(
           sprintf("Found possible global variable(s) in function %s: [%s]", ns_name, paste(res$variables[!ok], collapse=", ")), "\n")
    }
  }
}
