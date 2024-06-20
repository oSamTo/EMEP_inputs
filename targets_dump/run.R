library(targets)
#####################
#### Inspect the pipeline
#####################

#tar_manifest(fields = all_of("command"))
#tar_glimpse()
#tar_visnetwork()
#tar_outdated() # what is out of date?

system.time(tar_make_future(workers = 6L))

tar_meta(fields = warnings, complete_only = T)

