probe_dat <- function (object, probes) {
    .Call(apply_probe_data,object,probes)
}
