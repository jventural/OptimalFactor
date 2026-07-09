#' Theory-Guided Specification Search for CFA Models
#'
#' @description
#' Theory-aware extension of \code{\link{specification_search}}. The classic
#' specification search (MacCallum, 1986) is driven purely by fit: its loss
#' only looks at RMSEA/CFI/SRMR, so it "capitalizes on chance" and can drift
#' toward models that fit but break the intended theory (moving items to the
#' wrong factor, or dropping core items of the construct). This function adds a
#' \strong{theory-congruence} term to the loss that penalizes departures from
#' the theoretical item-to-factor assignment and penalizes dropping theoretical
#' items. A single parameter \code{theory_weight} grades how much theory counts
#' relative to fit (\code{0} reproduces the fit-only search; larger values make
#' the search progressively more conservative).
#'
#' @details
#' The total loss minimized for each candidate model is
#' \deqn{L = L_{fit} + \code{theory\_weight} \cdot L_{theory}}
#' where \eqn{L_{fit}} is the composite fit loss of \code{specification_search}
#' \deqn{L_{fit} = w_{rmsea}\max(0,(RMSEA-t_{rmsea})/0.03) +
#'       w_{cfi}\max(0,(t_{cfi}-CFI)/0.03) + w_{srmr}\max(0,(SRMR-t_{srmr})/0.03)}
#' and the theory loss is
#' \deqn{L_{theory} = \code{tw\_move}\cdot p_{mis} + \code{tw\_drop}\cdot p_{drop} +
#'       \code{tw\_k}\cdot |k - k_{theory}|/k_{theory}}
#' with \eqn{p_{mis}} the proportion of retained items assigned to a factor that
#' does not match their theoretical dimension, and \eqn{p_{drop}} the proportion
#' of theoretical items dropped from the model. Empirical factors are aligned to
#' theoretical dimensions greedily by maximum item overlap (each factor inherits
#' the most frequent theoretical label among its items), the same criterion used
#' by the project's loading maps. The reported \code{congruence} is
#' \eqn{1 - p_{mis}} (1 = the model fully respects the theory). Default seeds
#' include the theoretical structure itself as a starting point.
#'
#' \strong{Warning.} Like the fit-only search, this remains an EXPLORATORY
#' device; cross-validate the chosen model on an independent sample.
#'
#' @param data Data frame with the observed item responses.
#' @param items Character vector with the item names to search over (must all
#'   exist in \code{data}).
#' @param theory Named list encoding the theoretical structure, of the form
#'   \code{list(FactorA = c("it1","it2",...), FactorB = c(...))}. Items not
#'   listed are treated as having no theoretical factor (neither rewarded nor
#'   penalized).
#' @param theory_weight Non-negative scalar weighting the theory term against
#'   the fit term. \code{0} reproduces the fit-only search; typical values
#'   0.5-1 recover theory-congruent models with equivalent fit. Default 0.5.
#' @param tw_move,tw_drop,tw_k Internal weights of the theory loss: penalty for
#'   misassigned items, for dropped theoretical items, and for deviating from
#'   the theoretical number of factors. Defaults 0.6, 0.4, 0.0.
#' @param seeds,max_factors,min_items_factor,cfi_target,rmsea_target,srmr_target
#'   As in \code{\link{specification_search}}. If \code{seeds = NULL}, the
#'   theoretical structure plus block-partition seeds are generated.
#' @param max_iter_per_config,max_covs,max_items_removed,try_bifactor,operations
#'   As in \code{\link{specification_search}}.
#' @param n_cores Number of CPU cores for parallel evaluation of the candidate
#'   models within each greedy iteration (which are independent). Default 1
#'   (sequential). Values > 1 use a PSOCK cluster (works on Windows, where
#'   \code{fork} is unavailable); results are identical to the sequential run,
#'   only faster. A practical choice is \code{parallel::detectCores() - 1}.
#' @param estimator,ordered,std.lv,mi_min,mi_top,loss_weights,patience,early_stop_after_meet,verbose
#'   As in \code{\link{specification_search}}.
#'
#' @return An object of class \code{specification_search} (so
#'   \code{print.specification_search} applies), a list with \code{table}
#'   (one row per evaluated configuration, including the extra columns
#'   \code{congruence} and \code{theory_loss}), \code{successful}, \code{best}
#'   (the lowest-total-loss model, with its \code{congruence} and
#'   \code{theory_loss}), \code{results}, \code{theory_weight} and \code{call}.
#'
#' @section References:
#' MacCallum, R. C. (1986). Specification searches in covariance structure
#'   modeling. \emph{Psychological Bulletin, 100}(1), 107--120.
#'
#' @examples
#' \dontrun{
#'   data(Data_Personality)
#'   items  <- paste0("PPTQ", 1:15)
#'   theory <- list(F1 = paste0("PPTQ", 1:5),
#'                  F2 = paste0("PPTQ", 6:10),
#'                  F3 = paste0("PPTQ", 11:15))
#'
#'   # Fit-only (drifts from theory) vs theory-guided (keeps the structure)
#'   blind   <- specification_search_theory(Data_Personality, items, theory,
#'                theory_weight = 0, estimator = "MLR", ordered = FALSE)
#'   guided  <- specification_search_theory(Data_Personality, items, theory,
#'                theory_weight = 0.5, estimator = "MLR", ordered = FALSE)
#'
#'   guided$table[, c("config","cfi","rmsea","congruence","loss")]
#'   guided$best$factors
#' }
#'
#' @seealso \code{\link{specification_search}}
#' @export
specification_search_theory <- function(
    data, items, theory,
    theory_weight = 0.5,
    tw_move = 0.6, tw_drop = 0.4, tw_k = 0.0,   # pesos internos del theory_loss
    seeds = NULL, max_factors = 4, min_items_factor = 2,
    cfi_target = 0.95, rmsea_target = 0.08, srmr_target = 0.08,
    max_iter_per_config = 40, max_covs = 5, max_items_removed = 6,
    try_bifactor = TRUE, operations = c("move","drop","cov"),
    estimator = "WLSMV", ordered = TRUE, std.lv = TRUE,
    mi_min = 10, mi_top = 3,
    loss_weights = c(rmsea = 0.5, cfi = 0.3, srmr = 0.2),
    patience = 8, early_stop_after_meet = 3, n_cores = 1, verbose = TRUE) {

  if (!requireNamespace("lavaan", quietly = TRUE)) stop("Package 'lavaan' is required.")
  stopifnot(is.data.frame(data), is.character(items), length(items) >= 3)
  operations <- match.arg(operations, c("move","drop","cov"), several.ok = TRUE)

  # ---- Asignacion teorica item -> etiqueta ---------------------------------
  # theory: lista nombrada factor_teorico -> vector de items
  theo_item <- setNames(rep(NA_character_, length(items)), items)
  for (fac in names(theory)) theo_item[intersect(theory[[fac]], items)] <- fac
  k_theory  <- length(theory)
  n_theory  <- sum(!is.na(theo_item))

  wts <- c(rmsea = 0.5, cfi = 0.3, srmr = 0.2)
  if (!is.null(loss_weights)) for (nm in names(loss_weights))
    if (nm %in% names(wts)) wts[nm] <- loss_weights[[nm]]

  if (isTRUE(verbose)) {
    cat("\n", strrep("=", 74), "\n", sep = "")
    cat(" Specification Search TEORICA (MacCallum 1986 + congruencia teorica)\n")
    cat(strrep("=", 74), "\n", sep = "")
    cat(sprintf(" theory_weight = %.2f  | k_teorico = %d | items teoricos = %d\n",
                theory_weight, k_theory, n_theory))
    cat(" ADVERTENCIA: sigue siendo exploratorio; valide en muestra independiente.\n")
    cat(strrep("=", 74), "\n\n", sep = "")
  }

  # ---- Utilidades de ajuste (identicas al original) -------------------------
  fit_modelo <- function(syntax, items_used) {
    fit <- try(lavaan::cfa(syntax, data = data, estimator = estimator,
                           ordered = if (isTRUE(ordered)) items_used else FALSE,
                           std.lv = std.lv), silent = TRUE)
    if (inherits(fit, "try-error")) return(NULL)
    conv <- try(lavaan::lavInspect(fit, "converged"), silent = TRUE)
    if (inherits(conv, "try-error") || !isTRUE(conv)) return(NULL)
    fit
  }
  get_indices <- function(fit) {
    if (is.null(fit)) return(list(cfi=NA_real_, tli=NA_real_, rmsea=NA_real_,
                                  srmr=NA_real_, chisq=NA_real_, df=NA_real_))
    fm <- suppressWarnings(lavaan::fitMeasures(fit))
    pick <- function(s,n) if (s %in% names(fm)) as.numeric(fm[s]) else if (n %in% names(fm)) as.numeric(fm[n]) else NA_real_
    list(cfi=pick("cfi.scaled","cfi"), tli=pick("tli.scaled","tli"),
         rmsea=pick("rmsea.scaled","rmsea"), srmr=as.numeric(fm["srmr"]),
         chisq=pick("chisq.scaled","chisq"), df=pick("df.scaled","df"))
  }
  es_admisible <- function(fit) {
    if (is.null(fit)) return(FALSE)
    lat <- unique(lavaan::lavNames(fit, type = "lv"))
    pe  <- lavaan::parameterEstimates(fit, standardized = TRUE)
    neg <- any(pe$op=="~~" & pe$lhs==pe$rhs & pe$lhs %in% lat & pe$est < 0)
    std <- try(lavaan::standardizedSolution(fit), silent = TRUE)
    big <- if (inherits(std,"try-error")) FALSE else any(std$op=="=~" & std$lhs %in% lat & abs(std$est.std) > 1)
    !neg && !big
  }
  cumple <- function(idx) !is.na(idx$cfi) && !is.na(idx$rmsea) && idx$cfi >= cfi_target && idx$rmsea <= rmsea_target
  loss_fit <- function(idx) {
    if (is.na(idx$cfi)||is.na(idx$rmsea)||is.na(idx$srmr)) return(Inf)
    rmsea_l <- max(0,(idx$rmsea-rmsea_target)/0.03)
    cfi_l   <- max(0,(cfi_target-idx$cfi)/0.03)
    srmr_l  <- max(0,(idx$srmr-srmr_target)/0.03)
    as.numeric(wts["rmsea"])*rmsea_l + as.numeric(wts["cfi"])*cfi_l + as.numeric(wts["srmr"])*srmr_l
  }

  # ---- CONGRUENCIA TEORICA --------------------------------------------------
  # Alinea cada factor empirico a la etiqueta teorica dominante de sus items
  # (greedy, sin repetir etiqueta) y calcula la proporcion de items mal asignados.
  theory_metrics <- function(asig) {
    fnames <- names(asig)
    items_used <- unique(unlist(asig, use.names = FALSE))
    # matriz de solapamiento factor_empirico x etiqueta_teorica
    labs <- names(theory)
    ov <- matrix(0, length(fnames), length(labs), dimnames = list(fnames, labs))
    for (f in fnames) {
      tl <- theo_item[asig[[f]]]
      for (l in labs) ov[f, l] <- sum(tl == l, na.rm = TRUE)
    }
    # asignacion greedy factor->etiqueta por mayor solapamiento
    assign_lab <- setNames(rep(NA_character_, length(fnames)), fnames)
    ov2 <- ov
    for (step in seq_len(min(length(fnames), length(labs)))) {
      if (all(ov2 == 0)) break
      pos <- which(ov2 == max(ov2), arr.ind = TRUE)[1, ]
      assign_lab[rownames(ov2)[pos[1]]] <- colnames(ov2)[pos[2]]
      ov2[pos[1], ] <- -1; ov2[, pos[2]] <- -1
    }
    # items congruentes: su factor empirico quedo etiquetado con SU etiqueta teorica
    mal <- 0L; tot <- 0L
    for (f in fnames) for (it in asig[[f]]) {
      if (is.na(theo_item[it])) next
      tot <- tot + 1L
      if (is.na(assign_lab[f]) || assign_lab[f] != theo_item[it]) mal <- mal + 1L
    }
    prop_mal  <- if (tot > 0) mal/tot else 0
    prop_drop <- (n_theory - length(intersect(items_used, names(theo_item))))/max(1, n_theory)
    kdev      <- abs(length(fnames) - k_theory)/max(1, k_theory)
    list(prop_mal = prop_mal, prop_drop = prop_drop, kdev = kdev,
         congruence = 1 - prop_mal, assign_lab = assign_lab)
  }
  theory_loss <- function(asig) {
    tm <- theory_metrics(asig)
    tw_move*tm$prop_mal + tw_drop*tm$prop_drop + tw_k*tm$kdev
  }
  loss <- function(idx, asig) loss_fit(idx) + theory_weight * theory_loss(asig)

  # ---- Paralelizacion opcional del ajuste de candidatos (PSOCK, Windows-ok) --
  # n_cores > 1: el ajuste de los modelos candidatos de cada iteracion (que son
  # independientes) se reparte entre workers. Los resultados son identicos a la
  # version secuencial; solo cambia la velocidad.
  .cl <- NULL
  if (n_cores > 1L && requireNamespace("parallel", quietly = TRUE)) {
    .cl <- parallel::makeCluster(n_cores)
    parallel::clusterExport(.cl, c("data", "estimator", "ordered", "std.lv"), envir = environment())
    parallel::clusterEvalQ(.cl, { suppressMessages(library(lavaan)); TRUE })
    on.exit(try(parallel::stopCluster(.cl), silent = TRUE), add = TRUE)
  }
  .fit_worker <- function(spec) {
    fit <- try(lavaan::cfa(spec$syntax, data = data, estimator = estimator,
                           ordered = if (isTRUE(ordered)) spec$items_used else FALSE,
                           std.lv = std.lv), silent = TRUE)
    if (inherits(fit, "try-error")) return(list(ok = FALSE))
    conv <- try(lavaan::lavInspect(fit, "converged"), silent = TRUE)
    if (inherits(conv, "try-error") || !isTRUE(conv)) return(list(ok = FALSE))
    lat <- unique(lavaan::lavNames(fit, type = "lv"))
    pe  <- lavaan::parameterEstimates(fit, standardized = TRUE)
    neg <- any(pe$op == "~~" & pe$lhs == pe$rhs & pe$lhs %in% lat & pe$est < 0)
    std <- try(lavaan::standardizedSolution(fit), silent = TRUE)
    big <- if (inherits(std, "try-error")) FALSE else any(std$op == "=~" & std$lhs %in% lat & abs(std$est.std) > 1)
    if (neg || big) return(list(ok = FALSE))
    fm <- suppressWarnings(lavaan::fitMeasures(fit))
    pick <- function(s, n) if (s %in% names(fm)) as.numeric(fm[s]) else if (n %in% names(fm)) as.numeric(fm[n]) else NA_real_
    list(ok = TRUE, cfi = pick("cfi.scaled", "cfi"), tli = pick("tli.scaled", "tli"),
         rmsea = pick("rmsea.scaled", "rmsea"), srmr = as.numeric(fm["srmr"]),
         chisq = pick("chisq.scaled", "chisq"), df = pick("df.scaled", "df"))
  }
  .eval_specs <- function(specs) {
    if (length(specs) == 0) return(list())
    if (!is.null(.cl)) parallel::parLapply(.cl, specs, .fit_worker) else lapply(specs, .fit_worker)
  }

  # ---- Constructores de sintaxis (identicos al original) --------------------
  asig_to_syntax <- function(asig, covs = character(0)) {
    lines <- vapply(names(asig), function(f) paste0(f, " =~ ", paste(asig[[f]], collapse=" + ")), character(1))
    syn <- paste(lines, collapse = "\n")
    if (length(covs) > 0) syn <- paste(syn, paste(covs, collapse="\n"), sep="\n"); syn
  }
  asig_to_bifactor <- function(asig, covs = character(0)) {
    all_items <- sort(unique(unlist(asig, use.names = FALSE))); fnames <- names(asig)
    lines <- c(paste0("G =~ ", paste(all_items, collapse=" + ")))
    for (f in fnames) lines <- c(lines, paste0(f, " =~ ", paste(asig[[f]], collapse=" + ")))
    for (f in fnames) lines <- c(lines, paste0("G ~~ 0*", f))
    if (length(fnames) > 1) { combs <- utils::combn(fnames,2)
      for (i in seq_len(ncol(combs))) lines <- c(lines, paste0(combs[1,i]," ~~ 0*",combs[2,i])) }
    syn <- paste(lines, collapse="\n")
    if (length(covs) > 0) syn <- paste(syn, paste(covs, collapse="\n"), sep="\n"); syn
  }
  items_en_asig <- function(asig) sort(unique(unlist(asig, use.names = FALSE)))

  # ---- Semillas: por defecto la ESTRUCTURA TEORICA + particiones -----------
  default_seeds <- function() {
    out <- list()
    out[["1"]] <- list(list(G = items))
    if (max_factors >= k_theory) out[[as.character(k_theory)]] <- list(theory)  # semilla teorica
    for (k in 2:max_factors) {
      kk <- as.character(k)
      if (!is.null(out[[kk]])) next
      sizes <- rep(floor(length(items)/k), k); rem <- length(items)-sum(sizes)
      if (rem>0) sizes[seq_len(rem)] <- sizes[seq_len(rem)]+1
      idx <- 1L; asig <- list()
      for (j in seq_len(k)) { asig[[paste0("F",j)]] <- items[idx:(idx+sizes[j]-1L)]; idx <- idx+sizes[j] }
      out[[kk]] <- list(asig)
    }
    out
  }
  if (is.null(seeds)) seeds <- default_seeds()

  # ---- Optimizador greedy de una configuracion ------------------------------
  optimizar_config <- function(asig_inicial, config_id, es_bifactor = FALSE) {
    mk <- function(asig, cv) if (es_bifactor) asig_to_bifactor(asig, cv) else asig_to_syntax(asig, cv)
    covs <- character(0)
    fit  <- fit_modelo(mk(asig_inicial, covs), items_en_asig(asig_inicial))
    if (is.null(fit) || !es_admisible(fit)) return(NULL)
    current_loss <- loss(get_indices(fit), asig_inicial)
    current_asig <- asig_inicial; current_fit <- fit
    mejoras_sin_avance <- 0L; iters_tras_cumplir <- 0L

    for (iter in seq_len(max_iter_per_config)) {
      if (mejoras_sin_avance >= patience) break
      fnames <- names(current_asig); items_actuales <- items_en_asig(current_asig)
      # -- Enumerar candidatos (mismo orden que la version secuencial) --
      cand <- list()
      addc <- function(asig, cv, label) cand[[length(cand) + 1L]] <<-
        list(asig = asig, covs = cv, label = label, syntax = mk(asig, cv), items_used = items_en_asig(asig))
      if ("move" %in% operations && length(fnames) >= 2)
        for (fi in seq_along(fnames)) { f_o <- fnames[fi]; its_f <- current_asig[[f_o]]
          if (length(its_f) <= min_items_factor) next
          for (item in its_f) for (fj in seq_along(fnames)) { if (fi == fj) next
            f_d <- fnames[fj]; nueva <- current_asig
            nueva[[f_o]] <- setdiff(nueva[[f_o]], item); nueva[[f_d]] <- c(nueva[[f_d]], item)
            addc(nueva, covs, paste0("MOVE ", item, ": ", f_o, " -> ", f_d)) } }
      if ("drop" %in% operations && (length(items) - length(items_actuales)) < max_items_removed)
        for (fi in seq_along(fnames)) { f_n <- fnames[fi]; its_f <- current_asig[[f_n]]
          if (length(its_f) <= min_items_factor) next
          for (item in its_f) { nueva <- current_asig; nueva[[f_n]] <- setdiff(nueva[[f_n]], item)
            addc(nueva, covs, paste0("DROP ", item, " from ", f_n)) } }
      if ("cov" %in% operations && length(covs) < max_covs) {
        mi <- tryCatch(lavaan::modificationIndices(current_fit, sort. = TRUE, minimum.value = mi_min),
                       error = function(e) data.frame())
        if (NROW(mi) > 0) { mi_cov <- mi[mi$op == "~~" & mi$lhs %in% items_actuales & mi$rhs %in% items_actuales & mi$lhs != mi$rhs, , drop = FALSE]
          if (NROW(mi_cov) > 0) { mi_cov <- utils::head(mi_cov, mi_top)
            for (r in seq_len(nrow(mi_cov))) {
              new_cov <- paste0(mi_cov$lhs[r], " ~~ ", mi_cov$rhs[r]); rev_cov <- paste0(mi_cov$rhs[r], " ~~ ", mi_cov$lhs[r])
              if (new_cov %in% covs || rev_cov %in% covs) next
              addc(current_asig, c(covs, new_cov), paste0("COV ", new_cov, " (MI=", round(mi_cov$mi[r], 1), ")")) } } }
      }
      # -- Evaluar candidatos (paralelo si n_cores>1) y elegir el mejor --
      if (length(cand) == 0) { mejoras_sin_avance <- mejoras_sin_avance + 1L } else {
        res <- .eval_specs(cand)
        losses <- vapply(seq_along(cand), function(i) { rr <- res[[i]]
          if (is.null(rr) || !isTRUE(rr$ok)) return(Inf)
          loss(rr, cand[[i]]$asig) }, numeric(1))
        bi <- which.min(losses)
        if (length(bi) == 1L && is.finite(losses[bi]) && losses[bi] < current_loss) {
          current_loss <- losses[bi]; current_asig <- cand[[bi]]$asig; covs <- cand[[bi]]$covs
          current_fit  <- fit_modelo(cand[[bi]]$syntax, cand[[bi]]$items_used)
          mejoras_sin_avance <- 0L
          if (verbose) { inew <- get_indices(current_fit); tm <- theory_metrics(current_asig)
            cat(sprintf("    [%s] iter %d: %s -> CFI=%.3f RMSEA=%.3f congr=%.2f loss=%.3f\n",
                        config_id, iter, cand[[bi]]$label, inew$cfi, inew$rmsea, tm$congruence, current_loss)) }
        } else mejoras_sin_avance <- mejoras_sin_avance + 1L
      }
      ich <- get_indices(current_fit)
      if (cumple(ich)) { iters_tras_cumplir <- iters_tras_cumplir + 1L
        if (iters_tras_cumplir >= early_stop_after_meet) break }
    }
    idx_fin <- get_indices(current_fit); tm <- theory_metrics(current_asig)
    list(fit=current_fit, asig=current_asig, covs=covs, indices=idx_fin,
         cumple=cumple(idx_fin), loss=loss(idx_fin, current_asig),
         theory_loss=theory_loss(current_asig), congruence=tm$congruence,
         n_items=length(items_en_asig(current_asig)), n_factors=length(current_asig),
         bifactor=es_bifactor, config_id=config_id)
  }

  # ---- Barrido de semillas --------------------------------------------------
  todos <- list(); contador <- 0L
  ks <- as.character(sort(as.integer(names(seeds))))
  for (k in ks) { if (is.null(seeds[[k]])) next
    if (verbose) cat("--- k =", k, "factor(s) ---\n")
    sl <- seeds[[k]]
    for (s in seq_along(sl)) { asig <- sl[[s]]; cid <- paste0("k",k,"_s",s)
      if (verbose) cat("  Config:", cid, "(standard)\n")
      res <- tryCatch(optimizar_config(asig, cid, FALSE), error=function(e){ if(verbose) cat("    Error:",conditionMessage(e),"\n"); NULL })
      if (!is.null(res)) { contador<-contador+1L; todos[[contador]]<-res }
      if (try_bifactor && as.integer(k) >= 2L) { cid_bi <- paste0("k",k,"_s",s,"_bi")
        if (verbose) cat("  Config:", cid_bi, "(bifactor)\n")
        res_bi <- tryCatch(optimizar_config(asig, cid_bi, TRUE), error=function(e){ if(verbose) cat("    Error:",conditionMessage(e),"\n"); NULL })
        if (!is.null(res_bi)) { contador<-contador+1L; todos[[contador]]<-res_bi } } }
  }
  if (verbose) cat("\nModelos evaluados:", contador, "\n")
  if (contador == 0L) { warning("Ningun modelo ajusto. Resultado vacio.")
    out <- list(table=data.frame(), successful=data.frame(), best=NULL, results=list(),
                theory_weight=theory_weight); class(out) <- c("specification_search","list"); return(out) }

  rows <- lapply(todos, function(r) data.frame(
    config=r$config_id, n_factors=r$n_factors, bifactor=r$bifactor, n_items=r$n_items,
    n_covs=length(r$covs), cfi=round(r$indices$cfi,4), tli=round(r$indices$tli,4),
    rmsea=round(r$indices$rmsea,4), srmr=round(r$indices$srmr,4),
    congruence=round(r$congruence,3), theory_loss=round(r$theory_loss,4),
    loss=round(r$loss,4), meets=r$cumple, stringsAsFactors=FALSE))
  tabla <- do.call(rbind, rows)
  tabla <- tabla[order(tabla$loss, na.last=TRUE), , drop=FALSE]; rownames(tabla) <- NULL
  exitosos <- tabla[isTRUE(tabla$meets) | (!is.na(tabla$meets) & tabla$meets), , drop=FALSE]
  ord <- order(vapply(todos, function(r) r$loss, numeric(1)), na.last=TRUE)
  best_res <- todos[[ord[1]]]
  syn_best <- if (best_res$bifactor) asig_to_bifactor(best_res$asig, best_res$covs) else asig_to_syntax(best_res$asig, best_res$covs)
  best <- list(config_id=best_res$config_id, fit=best_res$fit, syntax=syn_best,
               factors=best_res$asig, covs=best_res$covs, indices=best_res$indices,
               congruence=best_res$congruence, theory_loss=best_res$theory_loss,
               bifactor=best_res$bifactor, meets=best_res$cumple, loss=best_res$loss)
  out <- list(table=tabla, successful=exitosos, best=best, results=todos,
              theory_weight=theory_weight, call=match.call())
  class(out) <- c("specification_search","list")
  out
}
