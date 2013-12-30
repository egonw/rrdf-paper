snpStore = new.rdf(ontology=FALSE)
add.prefix(snpStore,
  prefix="sio", namespace="http://semanticscience.org/resource/"
)
add.prefix(snpStore, "tmo", "http://www.w3.org/2001/sw/hcls/ns/transmed/")
add.prefix(snpStore, "obo", "http://purl.obolibrary.org/obo/")
add.prefix(snpStore, "bibo", "http://purl.org/ontology/bibo/")
add.prefix(snpStore, "cito", "http://purl.org/spar/cito/")
add.prefix(snpStore, "snp", "http://example.org/snp/")
add.prefix(snpStore, "art", "http://example.org/article/")
add.prefix(snpStore, "loc", "http://example.org/location/")
add.prefix(snpStore, "all", "http://example.org/allele/")
add.prefix(snpStore, "ex", "http://example.org/onto/")
add.prefix(snpStore, "pubmedid", "http://example.org/pubmed/")
add.prefix(snpStore, "snpid", "http://example.org/snpid/")




snpClass = "http://semanticscience.org/resource/SIO_010027"
chromosomeClass = "http://purl.obolibrary.org/obo/GO_0005694"
pubmedIdClass = "http://semanticscience.org/resource/CHEMINF_000302"
snpIdClass = "http://www.w3.org/2001/sw/hcls/ns/transmed/TMO_0161"
articleClass = "http://purl.org/ontology/bibo/Article"
alleleClass = "http://purl.obolibrary.org/obo/SO_0001023"

rdfTypePred = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
isDescribedBy = "http://purl.org/spar/cito/isDescribedBy"
onChromosome = "http://example.org/onto/onChromosome"
hasAttribute = "http://semanticscience.org/resource/CHEMINF_000200"
hasStart = "http://example.org/onto/hasStart"
hasValue = "http://semanticscience.org/resource/SIO_000300"
hasValidation = "http://example.org/onto/hasValidation"
hasAllele = "http://example.org/onto/hasAllele"
hasLocation = "http://example.org/onto/hasLocation"





createEntry <- function(row) {
  snpID = row[1]
  chr = row[2]
  chrStart = row[3]
  validation = row[4]
  pubmedID = row[5]
  allele = row[6]

  snpSubject = paste("http://example.org/snp/", snpID, sep="")
  pubmedObject = paste("http://example.org/article/a", pubmedID, sep="")
  alleleObject = paste("http://example.org/allele/", snpID, sep="")
  chrObject = paste("http://example.org/location/", snpID, sep="")

  snpIDObject = paste("http://example.org/snpid/", snpID, sep="")
  pubmedIDObject = paste("http://example.org/pubmed/a", pubmedID, sep="")

  add.triple(snpStore, snpSubject, rdfTypePred, snpClass)
  add.triple(snpStore, snpSubject, hasAttribute, snpIDObject)
  add.triple(snpStore, snpIDObject, rdfTypePred, snpIdClass)
  add.data.triple(snpStore, snpIDObject, hasValue, snpID)
  # validation information
  sapply(unlist(strsplit(validation, split=",")),
    function(validationItem) {
      if (!is.null(validationItem)) {
        add.data.triple(snpStore,
          snpSubject, hasValidation, validationItem
        )
      }
    }
  )
  # allele information
  add.triple(snpStore, snpSubject, hasAllele, alleleObject)
  add.triple(snpStore, alleleObject, rdfTypePred, alleleClass)
  add.data.triple(snpStore, alleleObject, hasValue, allele)
  # chromosome location information
  add.triple(snpStore, snpSubject, hasLocation, chrObject)
  add.triple(snpStore, chrObject, rdfTypePred, chromosomeClass)
  add.data.triple(snpStore, chrObject, onChromosome, chr)
  add.data.triple(snpStore, chrObject, hasStart, chrStart)
  # pubmed information
  add.triple(snpStore, snpSubject, isDescribedBy, pubmedObject)
  add.triple(snpStore, pubmedObject, rdfTypePred, articleClass)
  add.triple(snpStore, pubmedObject, hasAttribute, pubmedIDObject)
  add.triple(snpStore, pubmedIDObject, rdfTypePred, pubmedIdClass)
  add.data.triple(snpStore, pubmedIDObject, hasValue, pubmedID)
}






apply(data, MARGIN=1, FUN=createEntry)
save.rdf(snpStore, filename="test.n3", format="N3")
cat(asString.rdf(snpStore))
