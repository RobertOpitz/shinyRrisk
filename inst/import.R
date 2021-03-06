# classes
source(file = "../R/classes/generics.R", local = TRUE)
source(file = "../R/classes/set-class-union.R", local = TRUE)

source(file = "../R/classes/author-class.R", local = TRUE)
source(file = "../R/classes/model-authors-class.R", local = TRUE)

source(file = "../R/classes/item-class.R", local = TRUE)
source(file = "../R/classes/item-class-extension.R", local = TRUE)

source(file = "../R/classes/model-items-class.R", local = TRUE)
source(file = "../R/classes/model-items-class-extension.R", local = TRUE)

source(file = "../R/classes/model-scoring-class.R", local = TRUE)
source(file = "../R/classes/model-uncertainties-class.R", local = TRUE)
source(file = "../R/classes/score-class.R", local = TRUE)
source(file = "../R/classes/uncert-class.R", local = TRUE)

source(file = "../R/classes/basics-class.R", local = TRUE)
source(file = "../R/classes/model-basic-class.R", local = TRUE)
source(file = "../R/classes/model-settings-class.R", local = TRUE)
source(file = "../R/classes/model-name-class.R", local = TRUE)
source(file = "../R/classes/model-version-class.R", local = TRUE)
source(file = "../R/classes/model-references-class.R", local = TRUE)
source(file = "../R/classes/model-parts-class.R", local = TRUE)
source(file = "../R/classes/model-graphs-class.R", local = TRUE)
source(file = "../R/classes/model-glossary-class.R", local = TRUE)
source(file = "../R/classes/model-abbreviations-class.R", local = TRUE)
source(file = "../R/classes/model-validation-class.R", local = TRUE)
source(file = "../R/classes/model-comments-class.R", local = TRUE)
source(file = "../R/classes/model-conclusions-class.R", local = TRUE)
source(file = "../R/classes/model-output-class.R", local = TRUE)
source(file = "../R/classes/model-class.R", local = TRUE)


# rrisk-2.12.0
#source(file = "../R/rrisk-2.12.0/classes.R", local = TRUE)
#source(file = "../R/rrisk-2.12.0/defineItemtype.R", local = TRUE)
source(file = "../R/rrisk-2.12.0/get-data-for-with.R", local = TRUE)
source(file = "../R/rrisk-2.12.0/menu-mcrv-dist-family-function.R", local = TRUE)
source(file = "../R/rrisk-2.12.0/menu-define-rsrv-function.R", local = TRUE)
source(file = "../R/rrisk-2.12.0/menu-bsrv-function.R", local = TRUE)
source(file = "../R/rrisk-2.12.0/bsrv-fit-to-data-function.R", local = TRUE)
source(file = "../R/rrisk-2.12.0/menu-mcrv-fit-to-data-function.R", local = TRUE)
source(file = "../R/rrisk-2.12.0/mcrv-fit-to-data-function.R", local = TRUE)
source(file = "../R/rrisk-2.12.0/get_distribution_description-function.R", local = TRUE) 
source(file = "../R/rrisk-2.12.0/my-value-distr-params-function.R", local = TRUE)
source(file = "../R/rrisk-2.12.0/items-Evaluation-function.R", local = TRUE)
source(file = "../R/rrisk-2.12.0/define-numv-fnrv-function.R", local = TRUE)
source(file = "../R/rrisk-2.12.0/my-value-function.R", local = TRUE)
#source(file = "../R/rrisk-2.12.0/helpFunctions.R", local = TRUE)
#source(file = "../R/rrisk-2.12.0/get-uncertainty-matrix.R", local = TRUE)
source(file = "../R/rrisk-2.12.0/discrete-sampling-util-functions.R", local = TRUE)
#source(file = "../R/rrisk-2.12.0/runFunctions.R", local = TRUE)
source(file = "../R/rrisk-2.12.0/resample-strata.R", local = TRUE)
source(file = "../R/rrisk-2.12.0/reset-simulation-results.R", local = TRUE)
source(file = "../R/rrisk-2.12.0/run-1d-simulation.R", local = TRUE)
source(file = "../R/rrisk-2.12.0/run-2d-simulation.R", local = TRUE)
source(file = "../R/rrisk-2.12.0/plot-tornado-plot-for-pdf.R", local = TRUE) # is basically doubled with get-tornado-plot-data.R
source(file = "../R/rrisk-2.12.0/vecswitch.R", local = TRUE) 

# rriskDistributions-2.1.2
source(file = "../R/rrisk-distributions-2.1.2/rriskDistributions-functions.R", local = TRUE)
source(file = "../R/rrisk-distributions-2.1.2/fit-perc-function.R", local = TRUE)
source(file = "../R/rrisk-distributions-2.1.2/additional_rrisk_distribution_functions.R", local = TRUE)

# modules
source(file = "../R/modules/about/view/ui.R", local = TRUE)

source(file = "../R/modules/analysis/server.R", local = TRUE)
source(file = "../R/modules/analysis/logic/build-tornado-plot.R", local = TRUE)
source(file = "../R/modules/analysis/logic/create-plotly-with-config.R", local = TRUE)
source(file = "../R/modules/analysis/logic/get-gam-plot-data.R", local = TRUE)
source(file = "../R/modules/analysis/logic/get-tornado-plot-data.R", local = TRUE)
source(file = "../R/modules/analysis/view/ui.R", local = TRUE)

source(file = "../R/modules/authors/server.R", local = TRUE)
source(file = "../R/modules/authors/logic/author-set.R", local = TRUE)
source(file = "../R/modules/authors/validation/author.R", local = TRUE)
source(file = "../R/modules/authors/view/ui.R", local = TRUE)
source(file = "../R/modules/authors/view/modals/add.R", local = TRUE)
source(file = "../R/modules/authors/view/modals/edit.R", local = TRUE)

source(file = "../R/modules/documentation/server.R", local = TRUE)
source(file = "../R/modules/documentation/view/ui.R", local = TRUE)

source(file = "../R/modules/body/view/ui.R", local = TRUE)

source(file = "../R/modules/header/server.R", local = TRUE)
source(file = "../R/modules/header/view/ui.R", local = TRUE)

source(file = "../R/modules/introduction/view/ui.R", local = TRUE)

source(file = "../R/modules/items/server.R", local = TRUE)
source(file = "../R/modules/items/logic/build-show-text.R", local = TRUE)
source(file = "../R/modules/items/logic/disable-all-sections.R", local = TRUE)
source(file = "../R/modules/items/logic/fit-data.R", local = TRUE)
source(file = "../R/modules/items/logic/get-item-names.R", local = TRUE)
source(file = "../R/modules/items/logic/get-item-role-code.R", local = TRUE)
source(file = "../R/modules/items/logic/get-item-roles.R", local = TRUE)
source(file = "../R/modules/items/logic/get-item-set.R", local = TRUE)
source(file = "../R/modules/items/logic/get-item-tool-tips.R", local = TRUE)
source(file = "../R/modules/items/logic/get-item-type-codes.R", local = TRUE)
source(file = "../R/modules/items/logic/get-item-types.R", local = TRUE)
source(file = "../R/modules/items/logic/get-probability-density-values.R", local = TRUE)
source(file = "../R/modules/items/logic/get-probability-density.R", local = TRUE)
source(file = "../R/modules/items/logic/hide-all-feedback.R", local = TRUE)
source(file = "../R/modules/items/logic/matrix-add-span.R", local = TRUE)
source(file = "../R/modules/items/logic/set-item.R", local = TRUE)
source(file = "../R/modules/items/logic/set-Dep-Items.R", local = TRUE)
source(file = "../R/modules/items/logic/set-item-score.R", local = TRUE)
source(file = "../R/modules/items/logic/update-probability-density-section.R", local = TRUE)
source(file = "../R/modules/items/logic/get-model-network-plot-data.R", local = TRUE)
source(file = "../R/modules/items/logic/build-network-plot.R", local = TRUE)
source(file = "../R/modules/items/validation/add.R", local = TRUE)
source(file = "../R/modules/items/validation/edit.R", local = TRUE)
source(file = "../R/modules/items/view/ui.R", local = TRUE)
source(file = "../R/modules/items/view/modals/add.R", local = TRUE)
source(file = "../R/modules/items/view/modals/edit.R", local = TRUE)
source(file = "../R/modules/items/view/modals/scoringEdit.R", local = TRUE)
source(file = "../R/modules/items/view/tabs/define.R", local = TRUE)
source(file = "../R/modules/items/view/tabs/fit.R", local = TRUE)

#source(file = "../R/modules/overview/server.R", local = TRUE)
#source(file = "../R/modules/overview/logic/filter-plot-data.R", local = TRUE)
#source(file = "../R/modules/overview/view/ui.R", local = TRUE)

source(file = "../R/modules/run/server.R", local = TRUE)
source(file = "../R/modules/run/view/ui.R", local = TRUE)
source(file = "../R/modules/run/validation/simulation.R", local = TRUE)

source(file = "../R/modules/scoring/server.R", local = TRUE)
source(file = "../R/modules/scoring/logic/display-scoring-system.R", local = TRUE)
source(file = "../R/modules/scoring/logic/get-scoring-values.R", local = TRUE)
source(file = "../R/modules/scoring/logic/set-scoring.R", local = TRUE)
source(file = "../R/modules/scoring/validation/scoring-context.R", local = TRUE)
source(file = "../R/modules/scoring/validation/scoring-modal.R", local = TRUE)
source(file = "../R/modules/scoring/view/ui.R", local = TRUE)
source(file = "../R/modules/scoring/view/modals/add.R", local = TRUE)
source(file = "../R/modules/scoring/view/modals/edit.R", local = TRUE)

source(file = "../R/modules/setup/server.R", local = TRUE)
source(file = "../R/modules/setup/logic/build-csv.R", local = TRUE)
source(file = "../R/modules/setup/logic/build-df-authors.R", local = TRUE)
source(file = "../R/modules/setup/logic/build-df-documentation.R", local = TRUE)
source(file = "../R/modules/setup/logic/build-df-items.R", local = TRUE)
source(file = "../R/modules/setup/logic/get-uncertainty-plots-data-for-report.R", local = TRUE)
source(file = "../R/modules/setup/logic/build-plots.R", local = TRUE)
source(file = "../R/modules/setup/logic/get-export-name.R", local = TRUE)
source(file = "../R/modules/setup/logic/model-load.R", local = TRUE)
source(file = "../R/modules/setup/validation/model.R", local = TRUE)
source(file = "../R/modules/setup/view/ui.R", local = TRUE)

source(file = "../R/modules/sidebar/view/ui.R", local = TRUE)

#source(file = "../R/modules/simulation/server.R", local = TRUE)
#source(file = "../R/modules/simulation/validation/simulation.R", local = TRUE)
#source(file = "../R/modules/simulation/view/ui.R", local = TRUE)

source(file = "../R/modules/summary/server.R", local = TRUE)
source(file = "../R/modules/summary/logic/plot-cdf.R", local = TRUE)
source(file = "../R/modules/summary/logic/plot-histogram.R", local = TRUE)
source(file = "../R/modules/summary/logic/plot-convergence-of-1d-simulation.R", local = TRUE)
source(file = "../R/modules/summary/view/ui.R", local = TRUE)

source(file = "../R/modules/uncertainties/server.R", local = TRUE)
source(file = "../R/modules/uncertainties/logic/prepare-uncertainties-plot-data.R", local = TRUE)
source(file = "../R/modules/uncertainties/logic/build-uncertainties-plot.R", local = TRUE)
source(file = "../R/modules/uncertainties/logic/display-uncertainties.R", local = TRUE)
source(file = "../R/modules/uncertainties/logic/get-uncertainty-types.R", local = TRUE)
source(file = "../R/modules/uncertainties/logic/set-uncertainty.R", local = TRUE)
source(file = "../R/modules/uncertainties/logic/get-scoring-colors.R", local = TRUE)
source(file = "../R/modules/uncertainties/logic/get-scoring-meanings-base.R", local = TRUE)
source(file = "../R/modules/uncertainties/logic/get-scoring-meanings-detailed.R", local = TRUE)
source(file = "../R/modules/uncertainties/logic/get-uncertainties-plot-data.R", local = TRUE)
source(file = "../R/modules/uncertainties/logic/get-uncertainty-sub-cats.R", local = TRUE)
source(file = "../R/modules/uncertainties/logic/init-scoring-system.R", local = TRUE)
source(file = "../R/modules/uncertainties/logic/init-uncertainties.R", local = TRUE)
source(file = "../R/modules/uncertainties/logic/filter-plot-data.R", local = TRUE)
source(file = "../R/modules/uncertainties/validation/add.R", local = TRUE)
source(file = "../R/modules/uncertainties/validation/edit.R", local = TRUE)
source(file = "../R/modules/uncertainties/view/ui.R", local = TRUE)
source(file = "../R/modules/uncertainties/view/modals/add.R", local = TRUE)
source(file = "../R/modules/uncertainties/view/modals/edit.R", local = TRUE)

# utils
source(file = "../R/utils/add-action-column.R", local = TRUE)
source(file = "../R/utils/build-action-button.R", local = TRUE)
source(file = "../R/utils/check-matrix-data-fit.R", local = TRUE)
source(file = "../R/utils/get-error-message.R", local = TRUE)
source(file = "../R/utils/get-object-name.R", local = TRUE)
source(file = "../R/utils/get-rule-set.R", local = TRUE)
source(file = "../R/utils/shiny-alert.R", local = TRUE)
source(file = "../R/utils/check-rules.R", local = TRUE)
source(file = "../R/utils/put-line-breaks.R", local = TRUE)
source(file = "../R/utils/uncertainties-to-matrix.R", local = TRUE)
source(file = "../R/utils/parse-action-button.R", local = TRUE)
