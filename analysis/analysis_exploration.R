pca <- do_pca()

exprs_set <- merge_exprs_and_pca()

umap_coords <- do_umap()

exprs_set <- merge_exprs_and_umap()

exprs_set <- remove_dropped_events()

do_clustering_diagnostics_no_dropped()

do_pca_plots(module = "exploration")

do_umap_plots(module = "exploration")


# Is very slow, so commented out until needed
# do_corrplot()
