library(NetworkInference)  # Data already in cascades format:

#data(cascades)
## create discrete cascade times
#for(c in 1:length(cascades$cascade_times)){
#    cascades$cascade_times[[c]] <- round(10*cascades$cascade_times[[c]])
#}
data("policies")
cascades = as_cascade_long(policies, cascade_node_name = 'statenam',
                           event_time = 'adopt_year', cascade_id = 'policy')

s1 = Sys.time()

# set the window to 100 time periods
# start estimates at time period 101
# E = 20
# lambda = 1
# maximum time, useful quantity
max.time <- max(unlist(cascades$cascade_times))
# list to hold results objects
netinf.results <- list()
# loop over all times from 101 on
for(t in 1960:max.time){
    # set the minimum time for the window at time t
    window.min <- t-101
    # set the max window time
    window.max <- t-1
    # copy the cascade object to modify for time t
    cascades.t <- cascades
    # new lists for cascade times and nodes within the time window
    cascade_times <- list()
    cascade_nodes <- list()
    cascade_names <- c()
    # list index for storing modified cascades
    index <- 1
    # loop over cascades
    for(c in 1:length(cascades.t$cascade_times)){
        # copy the c'th cascades
        cascade_times.c <- cascades.t$cascade_times[[c]]
        cascade_nodes.c <- cascades.t$cascade_nodes[[c]]
        # find indexes of cascade elements that fit within the window
        keep.times <- which((cascade_times.c >= window.min) & (cascade_times.c <= window.max))
        # see if the cascade should be used in the t'th window
        if(length(keep.times) > 0){
            # store window-specific cascade data
            cascade_times[[index]] <- cascade_times.c[keep.times]
            cascade_nodes[[index]] <- cascade_nodes.c[keep.times]
            # increment the list index
            index <- index + 1
            cascade_names = c(cascade_names, names(cascades.t$cascade_times)[c])
        }
    }
    # store the window-specific cascade times and nodes in the
    # time-specific cascades object
    cascades.t$cascade_times <- cascade_times
    cascades.t$cascade_nodes <- cascade_nodes
    names(cascades.t$cascade_time) = names(cascades.t$cascade_nodes) = cascade_names
    # run netinf on the t'th window
    netinf.results[[t]] <- netinf(cascades.t, trans_mod = "exponential", n_edges = 20, params = 1,
                                  quiet = TRUE)
}
e1 = Sys.time()

s2 = Sys.time()
# create variables to use in EHA
# adoption indicator
y <- NULL
# number of previous adoptions
n.adopted <- NULL
# cascade indicator
cascade.id <- NULL
# weighted number of sources previously adopting
sources.adopted <- NULL
# copy the node list
nodes <- cascades$node_names
# loop over all times
for(t in 1960:max.time){
    # for each time, add observations for each cascade
    for(c in 1:length(cascades$cascade_times)){
        max.t <- max(cascades$cascade_times[[c]])
        min.t <- min(cascades$cascade_times[[c]])
        # proceed to add observations from this cascade if
        # the time point falls withing the cascade's bounds
        if((t >= min.t) & (t <= max.t)){
            # for each cascade and each time, check each node to possibly add data
            for(n in nodes){
                # check if the node was in the cascade
                in.cascade <- is.element(n,cascades$cascade_nodes[[c]])
                # if not in the cascade, add a 'not adopted yet' observation
                if(!in.cascade) in.data <- T
                # if the node is in the cascade, add a data point if adopted
                # on or after time t
                if(in.cascade){
                    in.data <- ifelse(cascades$cascade_times[[c]][which(cascades$cascade_nodes[[c]]==n)]>=t,T,F)
                }
                if(in.data){
                    # add a dependent variable observation
                    if(in.cascade) y <- c(y,1*(cascades$cascade_times[[c]][which(cascades$cascade_nodes[[c]]==n)]==t))
                    if(!in.cascade) y <- c(y,0)
                    # store the cascade id
                    cascade.id <- c(cascade.id,c)
                    # store the number of previous adoptions in cascade c
                    n.adopted <- c(n.adopted,sum(cascades$cascade_times[[c]] < t))
                    # start the sources adopted variable
                    sources.adopted.n <- 0
                    # find node n's sources at time t
                    sources.n <- netinf.results[[t]]$origin_node[which(netinf.results[[t]]$destination_node==n)]
                    # proceed if n has sources
                    if(length(sources.n)>0){
                        # find the times at which n's sources adopted in cascade c
                        source.times <- cascades$cascade_times[[c]][which(is.element(cascades$cascade_nodes[[c]],sources.n))]
                        if(length(source.times)>0) source.times <- source.times[which(source.times < t)]
                        if(length(source.times)>0){
                            # if there are sources of n that adopted prior to t,
                            # add the exponential weights to the sources adopted variable
                            sources.adopted.n <- sources.adopted.n + sum(exp(-(t-source.times)))
                        }
                    }
                    # increment the sources adopted variable
                    sources.adopted <- c(sources.adopted,sources.adopted.n)
                }
            }
        }
    }
    print(t)
}
end = Sys.time()

logit.est <- glm(y~n.adopted+sources.adopted+as.factor(cascade.id),family=binomial)
summary(logit.est)
BIC(logit.est)
end2 = Sys.time()
