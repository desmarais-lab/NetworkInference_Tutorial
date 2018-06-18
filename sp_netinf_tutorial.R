## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE---------------------------------------------------------
## install.packages('NetworkInference')

## ---- eval=FALSE---------------------------------------------------------
## install.packages(c('dplyr', 'igraph', 'speedglm', 'devtools'))

## ---- message=FALSE------------------------------------------------------
library(NetworkInference)
data('policies')
print(ls())

## ------------------------------------------------------------------------
policies

## ------------------------------------------------------------------------
policies_metadata

## ------------------------------------------------------------------------
print(unique(policies$policy)[100:104])

## ---- message=FALSE------------------------------------------------------
library(dplyr)
filter(policies_metadata, policy %in% unique(policy)[100:104]) %>%
    select(-source)

## ------------------------------------------------------------------------
policy_cascades <- as_cascade_long(policies, cascade_node_name = 'statenam',
                                   event_time = 'adopt_year', 
                                   cascade_id = 'policy')

## ------------------------------------------------------------------------
print(class(policy_cascades))
length(policy_cascades)
print(names(policy_cascades))

## ------------------------------------------------------------------------
selected_policies <- subset_cascade(cascade = policy_cascades, 
                                    selection = c('clinic_access', 'cogrowman'))
print(selected_policies)

## ------------------------------------------------------------------------
time_constrained <- subset_cascade_time(cascade = selected_policies, 
                                        start_time = 1990, end_time = 2000)
print(time_constrained[1:2])

## ------------------------------------------------------------------------
less_nodes <- drop_nodes(cascades = time_constrained, 
                         nodes = c('Maryland', 'Washington'))
print(less_nodes[1:2])

## ------------------------------------------------------------------------
summary(policy_cascades)

## ---- fig.align='center', fig.width=9, fig.height=3----------------------
selection <- c('guncontrol_assaultweapon_ba', 'guncontrol_licenses_dealer')
plot(policy_cascades, label_nodes = TRUE, selection = selection)

## ---- fig.align='center', fig.width=8, fig.height=4----------------------
selection <- c('waiting', 'threestrikes', 'unionlimits', 'smokeban', 
               'paperterror', 'miglab', 'methpre', 'lott', 'lemon', 'idtheft',
               'harass', 'hatecrime', 'equalpay')
plot(policy_cascades, label_nodes = FALSE, selection = selection)

## ------------------------------------------------------------------------
results <- netinf(policy_cascades, trans_mod = "exponential", n_edges = 100, 
                  params = 0.5, quiet = TRUE)
results

## ------------------------------------------------------------------------
npe <- count_possible_edges(policy_cascades)
npe

## ------------------------------------------------------------------------
results <- netinf(policy_cascades, trans_mod = "exponential", 
                  p_value_cutoff = 0.1, params = 0.5, quiet = TRUE)
results

## ------------------------------------------------------------------------
results <- netinf(policy_cascades, trans_mod = "exponential", 
                  p_value_cutoff = 0.1, quiet = TRUE)
attr(results, 'diffusion_model_parameters')

## ------------------------------------------------------------------------
results

## ---- fig.align='center', fig.width=7, fig.height=4----------------------
plot(results, type = "improvement")

## ---- fig.align='center', fig.width=7, fig.height=4----------------------
plot(results, type = 'p-value')

## ---- fig.width=7, fig.height=5.5----------------------------------------
#install.packages('igraph')
# For this functionality the igraph package has to be installed
# This code is only executed if the package is found:
if(requireNamespace("igraph", quietly = TRUE)) {
    plot(results, type = "network")
}

## ---- message=FALSE, eval=FALSE------------------------------------------
## if(requireNamespace("igraph", quietly = TRUE)) {
##     library(igraph)
##     g <- graph_from_data_frame(d = results[, 1:2])
##     plot(g, edge.arrow.size=.3, vertex.color = "grey70")
## }

## ---- message=FALSE------------------------------------------------------
devtools::install_github('desmarais-lab/spid')
library(spid)

## ------------------------------------------------------------------------
n_edges = c(500, 700)
lambdas = c(0.5, 1)
time_windows = c(50, 100)
param_grid = expand.grid(n_edges, lambdas, time_windows)
colnames(param_grid) = c('n_edges', 'lambdas', 'time_windows')
param_grid

## ---- message=FALSE------------------------------------------------------
library(spid)
network = infer_network(time = 1960, time_window = time_windows[1], 
                        cascades = policy_cascades, params = lambdas[1], 
                        n_edges = n_edges[1])
network

## ---- cache=TRUE---------------------------------------------------------
years = 1960:2015
networks = lapply(years, infer_network, time_window = time_windows[1],
                  cascades = policy_cascades, params = lambdas[1], 
                  n_edges = n_edges[1])
# Cast into single dataframe:
networks = do.call(rbind, networks)
networks

## ---- cache=TRUE---------------------------------------------------------
eha_data = make_eha_data(cascades = policy_cascades, networks = networks, 
                         min_time = 1960, decay_parameter = lambdas[1])
eha_data

## ---- cache=TRUE---------------------------------------------------------
library(speedglm)
mod = event ~ events_so_far + n_neighbor_events_decay + cascade_id
res = speedglm(mod, data = eha_data, family = binomial(link = "logit"))
smry = summary(res)
smry$coefficients = head(smry$coefficients)
smry
BIC(res)

## ---- cache=TRUE---------------------------------------------------------
#grid_search_results = grid_search_eha(cascades = policy_cascades, n_jobs = 4,
#                                      n_edges = n_edges, params = lambdas, 
#                                      time_windows = time_windows)
#save(grid_search_results, file = 'grid_search_results.RData')
load('grid_search_results.RData')
arrange(grid_search_results, bic)

