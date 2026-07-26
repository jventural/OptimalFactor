# Progress reporting shared by the resampling and simulation routines. Both
# spend minutes inside a cluster, and a run that prints nothing is impossible to
# tell apart from a run that has hung.

# cat() alone is not enough: the R console buffers, so in RStudio a message
# emitted just before a long computation may not appear until the computation
# ends, which is precisely when it is useless. Every status line goes through
# here so that it reaches the console immediately.
.of_say <- function(fmt, ...) {
  cat(sprintf(fmt, ...), "\n", sep = "")
  utils::flush.console()
  invisible(NULL)
}

# Bringing up a PSOCK cluster is not instant: every worker starts an R process
# and loads OptimalFactor with lavaan behind it, which with many workers takes
# longer than a few of the fits it is meant to accelerate. Announcing it before
# it starts is the difference between "slow" and "frozen".
.of_start_cluster <- function(n_cores, n_tasks = Inf, verbose = TRUE) {
  # More workers than tasks is pure cost: the extra ones load lavaan, sit idle
  # and still compete for cores with the ones doing the work.
  nw <- max(1L, min(n_cores, parallel::detectCores(), n_tasks))
  if (verbose)
    .of_say("Starting %d workers (each one loads lavaan, this takes a moment)...", nw)
  t0  <- Sys.time()
  clu <- parallel::makePSOCKcluster(nw)

  # A worker does not inherit the master's library paths. It starts a plain R
  # process and builds .libPaths() from the environment, so anything the session
  # added at runtime is invisible to it: RStudio project libraries, renv, a
  # devtools install into a non default library. Pushing the master's paths
  # first is what keeps the workers able to find OptimalFactor at all.
  ok <- tryCatch({
    parallel::clusterCall(clu, function(p) .libPaths(p), .libPaths())
    loaded <- unlist(parallel::clusterEvalQ(
      clu, requireNamespace("OptimalFactor", quietly = TRUE)))
    length(loaded) > 0 && all(loaded)
  }, error = function(e) FALSE)

  if (!ok) {
    parallel::stopCluster(clu)
    stop("The workers could not load OptimalFactor from:\n  ",
         paste(.libPaths(), collapse = "\n  "),
         "\nInstall the package into one of those libraries, or use n_cores = 1.",
         call. = FALSE)
  }
  if (verbose)
    .of_say("  %d workers ready in %s.", nw,
            .of_hms(as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  clu
}

# Arguments for one efa_boosting() call inside a resampling routine.
#
# The timeout is not a detail. Resampling makes the pipeline meet datasets it
# would never be run on by hand: a bootstrap draw of N = 100 repeats rows until
# only 63 are distinct, and on such a sample a WLSMV fit can grind for many
# minutes. One replication like that stalls the whole run, which is why the cap
# is on by default here even though efa_boosting() leaves it off.
#
# Any 'performance' the caller passed through ... is merged on top, so the
# defaults can be overridden instead of colliding with them.
.of_fit_args <- function(dots, timeout = NULL) {
  perf <- list(emit_progress = FALSE)
  if (!is.null(timeout) && is.finite(timeout) && timeout > 0) {
    if (requireNamespace("R.utils", quietly = TRUE)) {
      perf$use_timeouts          <- TRUE
      perf$timeout_optimization  <- timeout
      perf$timeout_efa           <- max(5, timeout / 3)
    } else {
      warning("Package 'R.utils' is needed to cap a replication; running without a timeout.",
              call. = FALSE)
    }
  }
  if (!is.null(dots$performance))
    perf <- utils::modifyList(perf, dots$performance)
  dots$performance <- perf
  dots
}

# Seconds as a compact human duration.
.of_hms <- function(s) {
  if (!is.finite(s)) return("?")
  s <- round(s)
  if (s < 60)   return(sprintf("%ds", s))
  if (s < 3600) return(sprintf("%dm%02ds", s %/% 60, s %% 60))
  sprintf("%dh%02dm", s %/% 3600, (s %% 3600) %/% 60)
}

# One-line bar with elapsed time and a linear estimate of what is left. The
# estimate is only as good as the assumption that tasks cost the same, which is
# roughly true here: every task is one full EFA-Boosting run.
.of_progress <- function(done, total, t0, width = 26L) {
  frac    <- if (total > 0) done / total else 1
  filled  <- round(frac * width)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  eta     <- if (done > 0) elapsed / done * (total - done) else NA_real_
  cat(sprintf("\r  [%s%s] %3.0f%% (%d/%d)  elapsed %s  left ~%s     ",
              strrep("=", filled), strrep(" ", width - filled),
              100 * frac, done, total, .of_hms(elapsed), .of_hms(eta)))
  utils::flush.console()
  invisible(NULL)
}

# parLapply over a list, reporting progress as results come back.
#
# A PSOCK cluster gives no per-task callback: the master sits blocked inside
# parLapply until the whole call returns. Progress is therefore reported one
# round at a time, a round being one task per worker. The price is a barrier per
# round, so a slow task makes its round wait; with tasks of comparable cost, as
# here, that is a small price for knowing whether a twenty-minute run is halfway
# or stuck.
.of_cluster_lapply <- function(clu, x, fun, verbose = TRUE) {
  n  <- length(x)
  nw <- length(clu)
  out <- vector("list", n)
  if (!n) return(out)

  rounds <- split(seq_len(n), ceiling(seq_len(n) / nw))
  t0 <- Sys.time()
  if (verbose) {
    # Being explicit about this matters: when everything fits in one round the
    # bar has nothing to report until the end, and a bar stuck at 0% is
    # indistinguishable from a hung session.
    if (length(rounds) == 1L)
      .of_say("  All %d tasks run at once, so the bar jumps to 100%% when they finish.", n)
    else
      .of_say("  %d rounds of up to %d tasks; the bar advances once per round.",
              length(rounds), nw)
    .of_progress(0, n, t0)
  }
  for (r in rounds) {
    # out[r] <- keeps a NULL result in place; out[[i]] <- NULL would delete it.
    # LB matters because run times are heavy tailed: a resample that makes the
    # greedy loop iterate costs many times what a well behaved one costs.
    out[r] <- parallel::parLapplyLB(clu, x[r], fun)
    if (verbose) .of_progress(max(r), n, t0)
  }
  if (verbose) cat("\n")
  out
}

# The sequential counterpart, so both paths report the same way.
.of_serial_lapply <- function(x, fun, verbose = TRUE) {
  n <- length(x)
  out <- vector("list", n)
  if (!n) return(out)

  t0 <- Sys.time()
  if (verbose) .of_progress(0, n, t0)
  for (i in seq_len(n)) {
    out[i] <- list(fun(x[[i]]))
    if (verbose) .of_progress(i, n, t0)
  }
  if (verbose) cat("\n")
  out
}
