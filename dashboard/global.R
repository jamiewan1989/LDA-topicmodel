library(memoise)

nps_cats <<-list("promoter" = "promoter","detractor" = "detractor")

getIgraph <- memoise(function(nps_cat) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.

  backbone <- readRDS(file = sprintf("%s.rda", nps_cat))
})