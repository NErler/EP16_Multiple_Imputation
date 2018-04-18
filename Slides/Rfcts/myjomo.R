myjomo.lmer <- function (formula, data, level = rep(1, ncol(data)), beta.start = NULL,
          l2.beta.start = NULL, u.start = NULL, l1cov.start = NULL,
          l2cov.start = NULL, l1cov.prior = NULL, l2cov.prior = NULL,
          a.start = NULL, a.prior = NULL, nburn = 1000, nbetween = 1000,
          nimp = 5, meth = "common", output = 1, out.iter = 10)
{
  cat("This function is beta software. Use carefully and please report any bug to the package mantainer\n")
  if (nimp < 2) {
    nimp = 2
    cat("Minimum number of imputations:2. For single imputation using function jomo.lmer.MCMCchain\n")
  }
  stopifnot(is.data.frame(data))
  stopifnot(any(grepl("~", deparse(formula))))
  fit.cr <- lmer(formula, data = data, na.action = na.omit)
  betaY.start <- fixef(fit.cr)
  varY.start <- (summary(fit.cr)$sigma)^2
  varY.prior <- (summary(fit.cr)$sigma)^2
  colnamysub <- all.vars(formula[[2]])
  Ysub <- get(colnamysub, pos = data)
  Ycov <- data.frame(mget(all.vars(formula[[3]]), envir = as.environment(data)))
  level <- data.frame(matrix(level, 1, ncol(data)))
  colnames(level) <- colnames(data)
  terms.sub <- attr(terms(formula), "term.labels")
  split.terms <- strsplit(terms.sub, ":")
  length.sub <- length(terms.sub)
  order.sub <- attr(terms(formula), "order")
  submod <- matrix(1, 4, sum(order.sub) - 1)
  Y.con <- NULL
  Y.cat <- NULL
  Y.numcat <- NULL
  Y2.con <- NULL
  Y2.cat <- NULL
  Y2.numcat <- NULL
  for (j in 1:ncol(Ycov)) {
    if (level[1, which(colnames(level) == colnames(Ycov)[j])] ==
        1) {
      if (is.numeric(Ycov[, j])) {
        if (is.null(Y.con))
          Y.con <- data.frame(Ycov[, j, drop = FALSE])
        else Y.con <- data.frame(Y.con, Ycov[, j, drop = FALSE])
      }
      if (is.factor(Ycov[, j])) {
        if (is.null(Y.cat))
          Y.cat <- data.frame(Ycov[, j, drop = FALSE])
        else Y.cat <- data.frame(Y.cat, Ycov[, j, drop = FALSE])
        Y.numcat <- cbind(Y.numcat, nlevels(Ycov[, j]))
      }
    }
    else {
      if (is.numeric(Ycov[, j])) {
        if (is.null(Y2.con))
          Y2.con <- data.frame(Ycov[, j, drop = FALSE])
        else Y2.con <- data.frame(Y2.con, Ycov[, j, drop = FALSE])
      }
      if (is.factor(Ycov[, j])) {
        if (is.null(Y2.cat))
          Y2.cat <- data.frame(Ycov[, j, drop = FALSE])
        else Y2.cat <- data.frame(Y2.cat, Ycov[, j, drop = FALSE])
        Y2.numcat <- cbind(Y2.numcat, nlevels(Ycov[,
                                                   j]))
      }
    }
  }
  h <- 1
  for (j in 1:length.sub) {
    for (k in 1:order.sub[j]) {
      current.term <- split.terms[[j]][k]
      if (grepl("\\|", deparse(current.term))) {
        j.tbd <- j
        clus.name <- sub(".*\\|", "", current.term)
        clus.name <- gsub(" ", "", clus.name)
        current.term <- sub("\\|.*$", "", current.term)
        random.terms <- strsplit(current.term, "+", fixed = T)
        length.ran <- length(random.terms[[1]])
        submod.ran <- matrix(1, 3, length.ran)
        for (t in 1:length.ran) {
          ct <- gsub(" ", "", random.terms[[1]][t])
          if (ct == 1) {
            submod.ran[1:2, t] <- 0
          }
          else if (length(which(colnames(Y.cat) == ct)) !=
                   0) {
            submod.ran[1, t] <- which(colnames(Y.cat) ==
                                        ct)
            submod.ran[2, t] <- 2
            submod.ran[3, t] <- Y.numcat[submod.ran[1,
                                                    t]] - 1
          }
          else if (length(which(colnames(Y.con) == ct)) !=
                   0) {
            submod.ran[1, t] <- which(colnames(Y.con) ==
                                        ct)
            submod.ran[2, t] <- 1
          }
        }
      }
      else {
        current.term <- sub(".*I\\(", "", current.term)
        current.term <- sub("\\)", "", current.term)
        if (grepl("\\^", current.term)) {
          submod[3, h] <- as.integer(sub(".*\\^", "",
                                         current.term))
          current.term <- sub("\\^.*", "", current.term)
        }
        else {
          submod[3, h] <- 1
        }
        if (length(which(colnames(Y.cat) == current.term)) !=
            0) {
          submod[1, h] <- which(colnames(Y.cat) == current.term)
          submod[2, h] <- 2
          submod[4, h] <- Y.numcat[submod[1, h]] - 1
        }
        else if (length(which(colnames(Y.con) == current.term)) !=
                 0) {
          submod[1, h] <- which(colnames(Y.con) == current.term)
          submod[2, h] <- 1
        }
        else if (length(which(colnames(Y2.cat) == current.term)) !=
                 0) {
          submod[1, h] <- which(colnames(Y2.cat) == current.term)
          submod[2, h] <- 4
          submod[4, h] <- Y2.numcat[submod[1, h]] - 1
        }
        else if (length(which(colnames(Y2.con) == current.term)) !=
                 0) {
          submod[1, h] <- which(colnames(Y2.con) == current.term)
          submod[2, h] <- 3
        }
        h <- h + 1
      }
    }
  }
  order.sub <- order.sub[-j.tbd]
  if (!is.null(Y.con) & sum((colnames(Y.con) == clus.name) ==
                            1))
    Y.con <- Y.con[, -which(colnames(Y.con) == clus.name), drop = F]
  if (!is.null(Y.cat) & sum((colnames(Y.cat) == clus.name) ==
                            1))
    Y.cat <- Y.cat[, -which(colnames(Y.cat) == clus.name), drop = F]
  X = matrix(1, max(nrow(Y.cat), nrow(Y.con)), 1)
  if (!is.null(Y2.con) | !is.null(Y2.cat)) {
    X2 = matrix(1, max(nrow(Y2.cat), nrow(Y2.con)), 1)
    if (is.null(l2.beta.start))
      l2.beta.start = matrix(0, ncol(X2), (max(as.numeric(!is.null(Y2.con)),
                                               ncol(Y2.con)) + max(0, (sum(Y2.numcat) - length(Y2.numcat)))))
  }
  Z = matrix(1, nrow(X), 1)
  if (is.null(beta.start))
    beta.start = matrix(0, ncol(X), (max(as.numeric(!is.null(Y.con)),
                                         ncol(Y.con)) + max(0, (sum(Y.numcat) - length(Y.numcat)))))
  clus <- factor(data[, clus.name])
  previous_levels_clus <- levels(clus)
  levels(clus) <- 0:(nlevels(clus) - 1)
  uY.start <- matrix(0, nlevels(clus), length.ran)
  if (is.null(l1cov.start)) {
    if (meth == "common") {
      l1cov.start = diag(1, ncol(beta.start))
    }
    else {
      l1cov.start = matrix(diag(1, ncol(beta.start)), nlevels(clus) *
                             ncol(beta.start), ncol(beta.start), 2)
    }
  }
  if (is.null(l1cov.prior))
    l1cov.prior = diag(1, ncol(l1cov.start))
  diagVar <- as.data.frame(VarCorr(fit.cr))[1:length.ran, 4]
  covuY.start <- VarCorr(fit.cr)[[1]][1:length.ran, 1:length.ran]
  covuY.prior <- VarCorr(fit.cr)[[1]][1:length.ran, 1:length.ran]
  if (kappa(covuY.start) > 10^8) {
    covuY.prior <- diag(1, length.ran)
    covuY.start <- diag(1, length.ran)
  }
  ncolYcon = max(as.numeric(!is.null(Y.con)), ncol(Y.con))
  ncolY2con = max(as.numeric(!is.null(Y2.con)), ncol(Y2.con))
  stopifnot(((!is.null(Y.con)) || (!is.null(Y.cat) & !is.null(Y.numcat))) ||
              ((!is.null(Y2.con)) || (!is.null(Y2.cat) & !is.null(Y2.numcat))))
  if (is.null(u.start))
    u.start = matrix(0, nlevels(clus), ncol(Z) * (ncolYcon +
                                                    max(0, (sum(Y.numcat) - length(Y.numcat)))) + (ncolY2con +
                                                                                                     max(0, (sum(Y2.numcat) - length(Y2.numcat)))))
  if (is.null(l2cov.start))
    l2cov.start = diag(1, ncol(u.start))
  if (is.null(l2cov.prior))
    l2cov.prior = diag(1, ncol(l2cov.start))
  if (!is.null(Y.cat)) {
    isnullcat = 0
    previous_levels <- list()
    Y.cat <- data.frame(Y.cat)
    for (i in 1:ncol(Y.cat)) {
      Y.cat[, i] <- factor(Y.cat[, i])
      previous_levels[[i]] <- levels(Y.cat[, i])
      levels(Y.cat[, i]) <- 1:nlevels(Y.cat[, i])
    }
  }
  else {
    isnullcat = 1
    Y.cat = -999
    Y.numcat = -999
  }
  if (!is.null(Y2.cat)) {
    isnullcat2 = 0
    previous_levels2 <- list()
    Y2.cat <- data.frame(Y2.cat)
    for (i in 1:ncol(Y2.cat)) {
      Y2.cat[, i] <- factor(Y2.cat[, i])
      previous_levels2[[i]] <- levels(Y2.cat[, i])
      levels(Y2.cat[, i]) <- 1:nlevels(Y2.cat[, i])
    }
  }
  else {
    isnullcat2 = 1
    Y2.cat = -999
    Y2.numcat = -999
  }
  if (!is.null(Y.con)) {
    stopifnot(nrow(Y.con) == nrow(clus), nrow(Y.con) == nrow(X),
              nrow(Z) == nrow(Y.con))
  }
  if (isnullcat == 0) {
    stopifnot(nrow(Y.cat) == nrow(clus), nrow(Y.cat) == nrow(X),
              nrow(Z) == nrow(Y.cat))
  }
  if (!is.null(Y2.con)) {
    stopifnot(nrow(Y2.con) == nrow(clus), nrow(Y2.con) ==
                nrow(X), nrow(Z) == nrow(Y2.con))
  }
  if (isnullcat2 == 0) {
    stopifnot(nrow(Y2.cat) == nrow(clus), nrow(Y2.cat) ==
                nrow(X), nrow(Z) == nrow(Y2.cat))
  }
  stopifnot(nrow(beta.start) == ncol(X), ncol(beta.start) ==
              (ncolYcon + max(0, (sum(Y.numcat) - length(Y.numcat)))))
  if (!is.null(Y2.con) || Y2.cat[1] != -999)
    stopifnot(nrow(l2.beta.start) == ncol(X2), ncol(l2.beta.start) ==
                (ncolY2con + max(0, (sum(Y2.numcat) - length(Y2.numcat)))))
  stopifnot(ncol(u.start) == ncol(Z) * (ncolYcon + max(0, (sum(Y.numcat) -
                                                             length(Y.numcat)))) + (ncolY2con + max(0, (sum(Y2.numcat) -
                                                                                                          length(Y2.numcat)))))
  if (meth == "common")
    stopifnot(nrow(l1cov.start) == ncol(l1cov.start), nrow(l1cov.prior) ==
                nrow(l1cov.start), nrow(l1cov.start) == ncol(beta.start))
  stopifnot(nrow(l1cov.prior) == ncol(l1cov.prior))
  stopifnot(ncol(l2cov.start) == ncol(u.start), nrow(l2cov.prior) ==
              nrow(l2cov.start), nrow(l2cov.prior) == ncol(l2cov.prior))
  betait = matrix(0, nrow(beta.start), ncol(beta.start))
  for (i in 1:nrow(beta.start)) {
    for (j in 1:ncol(beta.start)) betait[i, j] = beta.start[i,
                                                            j]
  }
  if (!is.null(Y2.con) || Y2.cat[1] != -999) {
    beta2it = matrix(0, nrow(l2.beta.start), ncol(l2.beta.start))
    for (i in 1:nrow(l2.beta.start)) {
      for (j in 1:ncol(l2.beta.start)) beta2it[i, j] = l2.beta.start[i,
                                                                     j]
    }
  }
  covit = matrix(0, nrow(l1cov.start), ncol(l1cov.start))
  for (i in 1:nrow(l1cov.start)) {
    for (j in 1:ncol(l1cov.start)) covit[i, j] = l1cov.start[i,
                                                             j]
  }
  uit = matrix(0, nrow(u.start), ncol(u.start))
  for (i in 1:nrow(u.start)) {
    for (j in 1:ncol(u.start)) uit[i, j] = u.start[i, j]
  }
  covuit = matrix(0, nrow(l2cov.start), ncol(l2cov.start))
  for (i in 1:nrow(l2cov.start)) {
    for (j in 1:ncol(l2cov.start)) covuit[i, j] = l2cov.start[i,
                                                              j]
  }
  if (!is.null(Y.con)) {
    colnamycon <- colnames(Y.con)
    Y.con <- data.matrix(Y.con)
    storage.mode(Y.con) <- "numeric"
  }
  if (isnullcat == 0) {
    colnamycat <- colnames(Y.cat)
    Y.cat <- data.matrix(Y.cat)
    storage.mode(Y.cat) <- "numeric"
  }
  if (!is.null(Y2.con)) {
    colnamy2con <- colnames(Y2.con)
    Y2.con <- data.matrix(Y2.con)
    storage.mode(Y2.con) <- "numeric"
  }
  if (isnullcat2 == 0) {
    colnamy2cat <- colnames(Y2.cat)
    Y2.cat <- data.matrix(Y2.cat)
    storage.mode(Y2.cat) <- "numeric"
  }
  colnamx <- colnames(X)
  colnamz <- colnames(Z)
  X <- data.matrix(X)
  storage.mode(X) <- "numeric"
  Z <- data.matrix(Z)
  storage.mode(Z) <- "numeric"
  if (!is.null(Y2.con) || Y2.cat[1] != -999) {
    colnamx2 <- colnames(X2)
    X2 <- data.matrix(X2)
    storage.mode(X2) <- "numeric"
  }
  clus <- matrix(as.integer(levels(clus))[clus], ncol = 1)
  if (!is.null(Y.con) & (isnullcat == 0)) {
    Y = cbind(Y.con, Y.cat)
    Yi = cbind(Y.con, matrix(0, nrow(Y.con), (sum(Y.numcat) -
                                                length(Y.numcat))))
  }
  else if (!is.null(Y.con)) {
    Y = Y.con
    Yi = Y.con
  }
  else {
    Y = Y.cat
    Yi = matrix(0, nrow(Y.cat), (sum(Y.numcat) - length(Y.numcat)))
  }
  if (!is.null(Y2.con) & isnullcat2 == 0) {
    Y2 = cbind(Y2.con, Y2.cat)
    Y2i = cbind(Y2.con, matrix(0, nrow(Y2.con), (sum(Y2.numcat) -
                                                   length(Y2.numcat))))
  }
  else if (!is.null(Y2.con)) {
    Y2 = Y2.con
    Y2i = Y2.con
  }
  else if (isnullcat2 == 0) {
    Y2 = Y2.cat
    Y2i = matrix(0, nrow(Y2.cat), (sum(Y2.numcat) - length(Y2.numcat)))
  }
  else {
    Y2 = NULL
  }
  h = 1
  if (isnullcat == 0) {
    for (i in 1:length(Y.numcat)) {
      for (j in 1:nrow(Y)) {
        if (is.na(Y.cat[j, i])) {
          Yi[j, (ncolYcon + h):(ncolYcon + h + Y.numcat[i] -
                                  2)] = NA
        }
      }
      h = h + Y.numcat[i] - 1
    }
  }
  h = 1
  if (isnullcat2 == 0) {
    for (i in 1:length(Y2.numcat)) {
      for (j in 1:nrow(Y2)) {
        if (is.na(Y2.cat[j, i])) {
          Y2i[j, (ncolY2con + h):(ncolY2con + h + Y2.numcat[i] -
                                    2)] = NA
        }
      }
      h = h + Y2.numcat[i] - 1
    }
  }
  ncY2 <- max(0, ncol(Y2))
  Ysubimp <- Ysub
  if (is.null(a.start))
    a.start = 50 + ncol(Y)
  if (is.null(a.prior))
    a.prior = a.start
  if (output != 1)
    out.iter = nburn + nbetween
  imp = matrix(0, nrow(Y) * (nimp + 1), ncol(Y) + ncY2 + 4)
  imp[1:nrow(Y), 1] = Ysub
  imp[1:nrow(Y), 2:(1 + ncol(Y))] = Y
  if (!is.null(Y2))
    imp[1:nrow(Y2), (ncol(Y) + 2):(ncol(Y) + ncY2 + 1)] = Y2
  imp[1:nrow(clus), (ncol(Y) + ncY2 + 2)] = clus
  imp[1:nrow(X), (ncol(Y) + ncY2 + 3)] = c(1:nrow(Y))
  Yimp = Yi
  Yimp2 = matrix(Yimp, nrow(Yimp), ncol(Yimp))
  if (!is.null(Y2)) {
    Y2imp = Y2i
    Y2imp2 = matrix(Y2imp, nrow(Y2imp), ncol(Y2imp))
  }
  imp[(nrow(clus) + 1):(2 * nrow(clus)), (ncol(Y) + ncY2 +
                                            2)] = clus
  imp[(nrow(X) + 1):(2 * nrow(X)), (ncol(Y) + ncY2 + 3)] = c(1:nrow(Y))
  imp[(nrow(X) + 1):(2 * nrow(X)), (ncol(Y) + ncY2 + 4)] = 1
  betapost <- array(0, dim = c(nrow(beta.start), ncol(beta.start),
                               (nimp - 1)))
  betaYpost <- array(0, dim = c(1, length(betaY.start), (nimp -
                                                           1)))
  bpost <- matrix(0, nrow(beta.start), ncol(beta.start))
  bYpost <- matrix(0, 1, length(betaY.start))
  if (!is.null(Y2)) {
    beta2post <- array(0, dim = c(nrow(l2.beta.start), ncol(l2.beta.start),
                                  (nimp - 1)))
    b2post <- matrix(0, nrow(l2.beta.start), ncol(l2.beta.start))
  }
  upost <- matrix(0, nrow(u.start), ncol(u.start))
  uYpost <- matrix(0, nrow(uY.start), ncol(uY.start))
  upostall <- array(0, dim = c(nrow(u.start), ncol(u.start),
                               (nimp - 1)))
  uYpostall <- array(0, dim = c(nrow(uY.start), ncol(uY.start),
                                (nimp - 1)))
  omegapost <- array(0, dim = c(nrow(l1cov.start), ncol(l1cov.start),
                                (nimp - 1)))
  opost <- matrix(0, nrow(l1cov.start), ncol(l1cov.start))
  varYpost <- rep(0, (nimp - 1))
  vYpost <- matrix(0, 1, 1)
  covupost <- array(0, dim = c(nrow(l2cov.start), ncol(l2cov.start),
                               (nimp - 1)))
  cpost <- matrix(0, nrow(l2cov.start), ncol(l2cov.start))
  covuYpost <- array(0, dim = c(nrow(as.matrix(covuY.start)),
                                ncol(as.matrix(covuY.start)), (nimp - 1)))
  cuYpost <- matrix(0, nrow(as.matrix(covuY.start)), ncol(as.matrix(covuY.start)))
  meanobs <- colMeans(Yi, na.rm = TRUE)
  for (i in 1:nrow(Yi)) for (j in 1:ncol(Yi)) if (is.na(Yimp[i,
                                                             j]))
    Yimp2[i, j] = meanobs[j]
  if (!is.null(Y2)) {
    meanobs2 <- colMeans(Y2i, na.rm = TRUE)
    for (i in 1:nrow(Y2i)) for (j in 1:ncol(Y2i)) if (is.na(Y2imp[i,
                                                                  j]))
      Y2imp2[i, j] = meanobs2[j]
  }
  for (i in 1:length(Ysubimp)) if (is.na(Ysubimp[i]))
    Ysubimp[i] = mean(Ysubimp, na.rm = TRUE)
  if (!is.null(Y2)) {
    if (meth == "common") {
      .Call("jomo2lmer", Ysub, Ysubimp, submod, order.sub,
            submod.ran, Y, Yimp, Yimp2, Y.cat, Y2, Y2imp,
            Y2imp2, Y2.cat, X, X2, Z, clus, betaY.start,
            bYpost, betait, beta2it, uit, uY.start, bpost,
            upost, uYpost, b2post, varY.start, vYpost, covit,
            opost, covuY.start, cuYpost, covuit, cpost, nburn,
            varY.prior, covuY.prior, l1cov.prior, l2cov.prior,
            Y.numcat, Y2.numcat, ncolYcon, ncolY2con, out.iter,
            PACKAGE = "jomo")
    }
    else {
      .Call("jomo2lmerhr", Ysub, Ysubimp, submod, order.sub,
            submod.ran, Y, Yimp, Yimp2, Y.cat, Y2, Y2imp,
            Y2imp2, Y2.cat, X, X2, Z, clus, betaY.start,
            bYpost, betait, beta2it, uit, uY.start, bpost,
            upost, uYpost, b2post, varY.start, vYpost, covit,
            opost, covuY.start, cuYpost, covuit, cpost, nburn,
            varY.prior, covuY.prior, l1cov.prior, l2cov.prior,
            Y.numcat, Y2.numcat, ncolYcon, ncolY2con, a.start,
            a.prior, out.iter, PACKAGE = "jomo")
    }
  }
  else {
    if (meth == "common") {
      .Call("jomo1lmer", Ysub, Ysubimp, submod, order.sub,
            submod.ran, Y, Yimp, Yimp2, Y.cat, X, Z, clus,
            betaY.start, bYpost, betait, uit, uY.start, bpost,
            upost, uYpost, varY.start, vYpost, covit, opost,
            covuY.start, cuYpost, covuit, cpost, nburn, varY.prior,
            covuY.prior, l1cov.prior, l2cov.prior, Y.numcat,
            ncolYcon, out.iter, PACKAGE = "jomo")
    }
    else {
      .Call("jomo1lmerhr", Ysub, Ysubimp, submod, order.sub,
            submod.ran, Y, Yimp, Yimp2, Y.cat, X, Z, clus,
            betaY.start, bYpost, betait, uit, uY.start, bpost,
            upost, uYpost, varY.start, vYpost, covit, opost,
            covuY.start, cuYpost, covuit, cpost, nburn, varY.prior,
            covuY.prior, l1cov.prior, l2cov.prior, Y.numcat,
            ncolYcon, a.start, a.prior, out.iter, PACKAGE = "jomo")
    }
  }
  bpost <- matrix(0, nrow(beta.start), ncol(beta.start))
  bYpost <- matrix(0, 1, length(betaY.start))
  opost <- matrix(0, nrow(l1cov.start), ncol(l1cov.start))
  vYpost <- matrix(0, 1, 1)
  upost <- matrix(0, nrow(u.start), ncol(u.start))
  uYpost <- matrix(0, nrow(uY.start), ncol(uY.start))
  cpost <- matrix(0, nrow(l2cov.start), ncol(l2cov.start))
  cuYpost <- matrix(0, nrow(as.matrix(covuY.start)), ncol(as.matrix(covuY.start)))
  if (!is.null(Y2)) {
    b2post <- matrix(0, nrow(l2.beta.start), ncol(l2.beta.start))
  }
  imp[(nrow(Y) + 1):(2 * nrow(Y)), 1] = Ysubimp
  if (!is.null(Y.con)) {
    imp[(nrow(Y) + 1):(2 * nrow(Y)), 2:(1 + ncol(Y.con))] = Yimp2[,
                                                                  1:ncol(Y.con)]
  }
  if (isnullcat == 0) {
    imp[(nrow(Y) + 1):(2 * nrow(Y)), (ncolYcon + 2):(1 +
                                                       ncol(Y))] = Y.cat
  }
  if (!is.null(Y2.con)) {
    imp[(nrow(Y2) + 1):(2 * nrow(Y2)), (ncol(Y) + 2):(1 +
                                                        ncol(Y) + ncol(Y2.con))] = Y2imp2[, 1:ncol(Y2.con)]
  }
  if (isnullcat2 == 0) {
    imp[(nrow(Y2) + 1):(2 * nrow(Y2)), (ncolY2con + ncol(Y) +
                                          2):(ncol(Y) + ncY2 + 1)] = Y2.cat
  }
  if (output == 1)
    cat("First imputation registered.", "\n")
  for (i in 2:nimp) {
    imp[(i * nrow(clus) + 1):((i + 1) * nrow(clus)), (ncol(Y) +
                                                        ncY2 + 2)] = clus
    imp[(i * nrow(X) + 1):((i + 1) * nrow(X)), (ncol(Y) +
                                                  ncY2 + 3)] = c(1:nrow(Y))
    imp[(i * nrow(X) + 1):((i + 1) * nrow(X)), (ncol(Y) +
                                                  ncY2 + 4)] = i
    if (!is.null(Y2)) {
      if (meth == "common") {
        .Call("jomo2lmer", Ysub, Ysubimp, submod, order.sub,
              submod.ran, Y, Yimp, Yimp2, Y.cat, Y2, Y2imp,
              Y2imp2, Y2.cat, X, X2, Z, clus, betaY.start,
              bYpost, betait, beta2it, uit, uY.start, bpost,
              upost, uYpost, b2post, varY.start, vYpost,
              covit, opost, covuY.start, cuYpost, covuit,
              cpost, nbetween, varY.prior, covuY.prior, l1cov.prior,
              l2cov.prior, Y.numcat, Y2.numcat, ncolYcon,
              ncolY2con, out.iter)
      }
      else {
        .Call("jomo2lmerhr", Ysub, Ysubimp, submod, order.sub,
              submod.ran, Y, Yimp, Yimp2, Y.cat, Y2, Y2imp,
              Y2imp2, Y2.cat, X, X2, Z, clus, betaY.start,
              bYpost, betait, beta2it, uit, uY.start, bpost,
              upost, uYpost, b2post, varY.start, vYpost,
              covit, opost, covuY.start, cuYpost, covuit,
              cpost, nbetween, varY.prior, covuY.prior, l1cov.prior,
              l2cov.prior, Y.numcat, Y2.numcat, ncolYcon,
              ncolY2con, a.start, a.prior, out.iter)
      }
    }
    else {
      if (meth == "common") {
        .Call("jomo1lmer", Ysub, Ysubimp, submod, order.sub,
              submod.ran, Y, Yimp, Yimp2, Y.cat, X, Z, clus,
              betaY.start, bYpost, betait, uit, uY.start,
              bpost, upost, uYpost, varY.start, vYpost, covit,
              opost, covuY.start, cuYpost, covuit, cpost,
              nbetween, varY.prior, covuY.prior, l1cov.prior,
              l2cov.prior, Y.numcat, ncolYcon, out.iter)
      }
      else {
        .Call("jomo1lmerhr", Ysub, Ysubimp, submod, order.sub,
              submod.ran, Y, Yimp, Yimp2, Y.cat, X, Z, clus,
              betaY.start, bYpost, betait, uit, uY.start,
              bpost, upost, uYpost, varY.start, vYpost, covit,
              opost, covuY.start, cuYpost, covuit, cpost,
              nbetween, varY.prior, covuY.prior, l1cov.prior,
              l2cov.prior, Y.numcat, ncolYcon, a.start, a.prior,
              out.iter)
      }
    }
    betapost[, , (i - 1)] = bpost
    betaYpost[, , (i - 1)] = bYpost
    upostall[, , (i - 1)] = upost
    uYpostall[, , (i - 1)] = uYpost
    omegapost[, , (i - 1)] = opost
    varYpost[i - 1] = vYpost
    covupost[, , (i - 1)] = cpost
    covuYpost[, , (i - 1)] = cuYpost
    if (!is.null(Y2)) {
      beta2post[, , (i - 1)] = b2post
      b2post <- matrix(0, nrow(l2.beta.start), ncol(l2.beta.start))
    }
    bpost <- matrix(0, nrow(beta.start), ncol(beta.start))
    bYpost <- matrix(0, 1, length(betaY.start))
    opost <- matrix(0, nrow(l1cov.start), ncol(l1cov.start))
    vYpost <- matrix(0, 1, 1)
    upost <- matrix(0, nrow(u.start), ncol(u.start))
    uYpost <- matrix(0, nrow(uY.start), ncol(uY.start))
    cpost <- matrix(0, nrow(l2cov.start), ncol(l2cov.start))
    cuYpost <- matrix(0, nrow(as.matrix(covuY.start)), ncol(as.matrix(covuY.start)))
    imp[(i * nrow(X) + 1):((i + 1) * nrow(X)), 1] = Ysubimp
    if (!is.null(Y.con)) {
      imp[(i * nrow(X) + 1):((i + 1) * nrow(X)), 2:(1 +
                                                      ncol(Y.con))] = Yimp2[, 1:ncol(Y.con)]
    }
    if (isnullcat == 0) {
      imp[(i * nrow(X) + 1):((i + 1) * nrow(X)), (ncolYcon +
                                                    2):(1 + ncol(Y))] = Y.cat
    }
    if (!is.null(Y2.con)) {
      imp[(i * nrow(X) + 1):((i + 1) * nrow(X)), (ncol(Y) +
                                                    2):(ncol(Y) + ncol(Y2.con) + 1)] = Y2imp2[, 1:ncol(Y2.con)]
    }
    if (isnullcat2 == 0) {
      imp[(i * nrow(X) + 1):((i + 1) * nrow(X)), (ncolY2con +
                                                    ncol(Y) + 2):(ncol(Y) + ncY2 + 1)] = Y2.cat
    }
    if (output == 1)
      cat("Imputation number ", i, "registered", "\n")
  }
  betaYpostmean <- apply(betaYpost, c(1, 2), mean)
  varYpostmean <- mean(varYpost)
  covuYpostmean <- apply(covuYpost, c(1, 2), mean)
  uYpostmean <- apply(uYpostall, c(1, 2), mean)
  betapostmean <- apply(betapost, c(1, 2), mean)
  if (!is.null(Y2))
    beta2postmean <- apply(beta2post, c(1, 2), mean)
  upostmean <- apply(upostall, c(1, 2), mean)
  omegapostmean <- apply(omegapost, c(1, 2), mean)
  covupostmean <- apply(covupost, c(1, 2), mean)
  if (output == 1) {
    cat("The posterior mean of the substantive model fixed effects estimates is:\n")
    print(betaYpostmean)
    cat("The posterior mean of the substantive model residual variance is:\n")
    print(varYpostmean)
    cat("The posterior mean of the substantive model random effects covariance matrix is:\n")
    print(covuYpostmean)
    cat("The posterior mean of the substantive model random effects estimates is:\n")
    print(uYpostmean)
    cat("The posterior mean of the fixed effects estimates is:\n")
    print(betapostmean)
    if (!is.null(Y2)) {
      cat("The posterior mean of the level 2 fixed effects estimates is:\n")
      print(beta2postmean)
    }
    cat("The posterior mean of the random effects estimates is:\n")
    print(upostmean)
    cat("The posterior mean of the level 1 covariance matrix is:\n")
    print(omegapostmean)
    cat("The posterior mean of the level 2 covariance matrix is:\n")
    print(covupostmean)
  }
  imp <- data.frame(imp)
  if (isnullcat == 0) {
    for (i in 1:ncol(Y.cat)) {
      imp[, (1 + ncolYcon + i)] <- as.factor(imp[, (1 +
                                                      ncolYcon + i)])
      levels(imp[, (1 + ncolYcon + i)]) <- previous_levels[[i]]
    }
  }
  if (isnullcat2 == 0) {
    for (i in 1:ncol(Y2.cat)) {
      imp[, (1 + ncol(Y) + ncolY2con + i)] <- as.factor(imp[,
                                                            (1 + ncol(Y) + ncolY2con + i)])
      levels(imp[, (1 + ncol(Y) + ncolY2con + i)]) <- previous_levels2[[i]]
    }
  }
  imp[, (ncol(Y) + ncY2 + 2)] <- factor(imp[, (ncol(Y) + ncY2 +
                                                 2)])
  levels(imp[, (ncol(Y) + ncY2 + 2)]) <- previous_levels_clus
  clus <- factor(clus)
  levels(clus) <- previous_levels_clus
  if (ncolYcon > 0) {
    for (j in 1:(ncolYcon)) {
      imp[, j + 1] = as.numeric(imp[, j + 1])
    }
  }
  if (ncolY2con > 0) {
    for (j in 1:(ncolY2con)) {
      imp[, ncol(Y) + j + 1] = as.numeric(imp[, ncol(Y) +
                                                j + 1])
    }
  }
  if (isnullcat == 0) {
    if (is.null(colnamycat))
      colnamycat = paste("Ycat", 1:ncol(Y.cat), sep = "")
  }
  else {
    colnamycat = NULL
    Y.cat = NULL
    Y.numcat = NULL
  }
  if (!is.null(Y.con)) {
    if (is.null(colnamycon))
      colnamycon = paste("Ycon", 1:ncol(Y.con), sep = "")
  }
  else {
    colnamycon = NULL
  }
  if (isnullcat2 == 0) {
    if (is.null(colnamy2cat))
      colnamy2cat = paste("Y2cat", 1:ncol(Y2.cat), sep = "")
  }
  else {
    colnamy2cat = NULL
    Y2.cat = NULL
    Y2.numcat = NULL
  }
  if (!is.null(Y2.con)) {
    if (is.null(colnamy2con))
      colnamy2con = paste("Y2con", 1:ncol(Y2.con), sep = "")
  }
  else {
    colnamy2con = NULL
  }
  if (is.null(colnamysub))
    colnamysub = "Ysub"
  colnames(imp) <- c(colnamysub, colnamycon, colnamycat, colnamy2con,
                     colnamy2cat, "clus", "id", "Imputation")
  return(imp)
}








myjomo.lmer.MCMCchain <- function (formula, data, level = rep(1, ncol(data)), beta.start = NULL,
                                   l2.beta.start = NULL, u.start = NULL, l1cov.start = NULL,
                                   l2cov.start = NULL, l1cov.prior = NULL, l2cov.prior = NULL,
                                   a.start = NULL, a.prior = NULL, betaY.start = NULL, varY.start = NULL,
                                   covuY.start = NULL, uY.start = NULL, nburn = 1000, meth = "common",
                                   start.imp = NULL, start.imp.sub = NULL, l2.start.imp = NULL,
                                   output = 1, out.iter = 10)
{
  cat("This function is beta software. Use carefully and please report any bug to the package mantainer\n")
  stopifnot(is.data.frame(data))
  stopifnot(any(grepl("~", deparse(formula))))
  fit.cr <- lmer(formula, data = data, na.action = na.omit)
  if (is.null(betaY.start))
    betaY.start <- fixef(fit.cr)
  if (is.null(varY.start))
    varY.start <- (summary(fit.cr)$sigma)^2
  varY.prior <- (summary(fit.cr)$sigma)^2
  colnamysub <- all.vars(formula[[2]])
  Ysub <- get(colnamysub, pos = data)
  Ycov <- data.frame(mget(all.vars(formula[[3]]), envir = as.environment(data)))
  level <- data.frame(matrix(level, 1, ncol(data)))
  colnames(level) <- colnames(data)
  terms.sub <- attr(terms(formula), "term.labels")
  split.terms <- strsplit(terms.sub, ":")
  length.sub <- length(terms.sub)
  order.sub <- attr(terms(formula), "order")
  submod <- matrix(1, 4, sum(order.sub) - 1)
  Y.con <- NULL
  Y.cat <- NULL
  Y.numcat <- NULL
  Y2.con <- NULL
  Y2.cat <- NULL
  Y2.numcat <- NULL
  Y2i <- NULL
  Y2imp2 <- NULL
  for (j in 1:ncol(Ycov)) {
    if (level[1, which(colnames(level) == colnames(Ycov)[j])] ==
        1) {
      if (is.numeric(Ycov[, j])) {
        if (is.null(Y.con))
          Y.con <- data.frame(Ycov[, j, drop = FALSE])
        else Y.con <- data.frame(Y.con, Ycov[, j, drop = FALSE])
      }
      if (is.factor(Ycov[, j])) {
        if (is.null(Y.cat))
          Y.cat <- data.frame(Ycov[, j, drop = FALSE])
        else Y.cat <- data.frame(Y.cat, Ycov[, j, drop = FALSE])
        Y.numcat <- cbind(Y.numcat, nlevels(Ycov[, j]))
      }
    }
    else {
      if (is.numeric(Ycov[, j])) {
        if (is.null(Y2.con))
          Y2.con <- data.frame(Ycov[, j, drop = FALSE])
        else Y2.con <- data.frame(Y2.con, Ycov[, j, drop = FALSE])
      }
      if (is.factor(Ycov[, j])) {
        if (is.null(Y2.cat))
          Y2.cat <- data.frame(Ycov[, j, drop = FALSE])
        else Y2.cat <- data.frame(Y2.cat, Ycov[, j, drop = FALSE])
        Y2.numcat <- cbind(Y2.numcat, nlevels(Ycov[,
                                                   j]))
      }
    }
  }
  h <- 1
  for (j in 1:length.sub) {
    for (k in 1:order.sub[j]) {
      current.term <- split.terms[[j]][k]
      if (grepl("\\|", deparse(current.term))) {
        j.tbd <- j
        clus.name <- sub(".*\\|", "", current.term)
        clus.name <- gsub(" ", "", clus.name)
        current.term <- sub("\\|.*$", "", current.term)
        random.terms <- strsplit(current.term, "+", fixed = T)
        length.ran <- length(random.terms[[1]])
        submod.ran <- matrix(1, 3, length.ran)
        for (t in 1:length.ran) {
          ct <- gsub(" ", "", random.terms[[1]][t])
          if (ct == 1) {
            submod.ran[1:2, t] <- 0
          }
          else if (length(which(colnames(Y.cat) == ct)) !=
                   0) {
            submod.ran[1, t] <- which(colnames(Y.cat) ==
                                        ct)
            submod.ran[2, t] <- 2
            submod.ran[3, t] <- Y.numcat[submod.ran[1,
                                                    t]] - 1
          }
          else if (length(which(colnames(Y.con) == ct)) !=
                   0) {
            submod.ran[1, t] <- which(colnames(Y.con) ==
                                        ct)
            submod.ran[2, t] <- 1
          }
        }
      }
      else {
        current.term <- sub(".*I\\(", "", current.term)
        current.term <- sub("\\)", "", current.term)
        if (grepl("\\^", current.term)) {
          submod[3, h] <- as.integer(sub(".*\\^", "",
                                         current.term))
          current.term <- sub("\\^.*", "", current.term)
        }
        else {
          submod[3, h] <- 1
        }
        if (length(which(colnames(Y.cat) == current.term)) !=
            0) {
          submod[1, h] <- which(colnames(Y.cat) == current.term)
          submod[2, h] <- 2
          submod[4, h] <- Y.numcat[submod[1, h]] - 1
        }
        else if (length(which(colnames(Y.con) == current.term)) !=
                 0) {
          submod[1, h] <- which(colnames(Y.con) == current.term)
          submod[2, h] <- 1
        }
        else if (length(which(colnames(Y2.cat) == current.term)) !=
                 0) {
          submod[1, h] <- which(colnames(Y2.cat) == current.term)
          submod[2, h] <- 4
          submod[4, h] <- Y2.numcat[submod[1, h]] - 1
        }
        else if (length(which(colnames(Y2.con) == current.term)) !=
                 0) {
          submod[1, h] <- which(colnames(Y2.con) == current.term)
          submod[2, h] <- 3
        }
        h <- h + 1
      }
    }
  }
  order.sub <- order.sub[-j.tbd]
  if (!is.null(Y.con) & sum((colnames(Y.con) == clus.name) ==
                            1))
    Y.con <- Y.con[, -which(colnames(Y.con) == clus.name), drop = F]
  if (!is.null(Y.cat) & sum((colnames(Y.cat) == clus.name) ==
                            1))
    Y.cat <- Y.cat[, -which(colnames(Y.cat) == clus.name), drop = F]
  X = matrix(1, max(nrow(Y.cat), nrow(Y.con)), 1)
  if (!is.null(Y2.con) | !is.null(Y2.cat)) {
    X2 = matrix(1, max(nrow(Y2.cat), nrow(Y2.con)), 1)
    if (is.null(l2.beta.start))
      l2.beta.start = matrix(0, ncol(X2), (max(as.numeric(!is.null(Y2.con)),
                                               ncol(Y2.con)) + max(0, (sum(Y2.numcat) - length(Y2.numcat)))))
  }
  Z = matrix(1, nrow(X), 1)
  if (is.null(beta.start))
    beta.start = matrix(0, ncol(X), (max(as.numeric(!is.null(Y.con)),
                                         ncol(Y.con)) + max(0, (sum(Y.numcat) - length(Y.numcat)))))
  clus <- factor(data[, clus.name])
  previous_levels_clus <- levels(clus)
  levels(clus) <- 0:(nlevels(clus) - 1)
  if (is.null(uY.start))
    uY.start <- matrix(0, nlevels(clus), length.ran)
  if (is.null(l1cov.start)) {
    if (meth == "common") {
      l1cov.start = diag(1, ncol(beta.start))
    }
    else {
      l1cov.start = matrix(diag(1, ncol(beta.start)), nlevels(clus) *
                             ncol(beta.start), ncol(beta.start), 2)
    }
  }
  if (is.null(l1cov.prior))
    l1cov.prior = diag(1, ncol(l1cov.start))
  if (is.null(covuY.start))
    covuY.start <- VarCorr(fit.cr)[[1]][1:length.ran, 1:length.ran]
  covuY.prior <- VarCorr(fit.cr)[[1]][1:length.ran, 1:length.ran]
  if (kappa(covuY.start) > 10^8) {
    covuY.prior <- diag(1, length.ran)
    covuY.start <- diag(1, length.ran)
  }
  ncolYcon = max(as.numeric(!is.null(Y.con)), ncol(Y.con))
  ncolY2con = max(as.numeric(!is.null(Y2.con)), ncol(Y2.con))
  stopifnot(((!is.null(Y.con)) || (!is.null(Y.cat) & !is.null(Y.numcat))) ||
              ((!is.null(Y2.con)) || (!is.null(Y2.cat) & !is.null(Y2.numcat))))
  if (is.null(u.start))
    u.start = matrix(0, nlevels(clus), ncol(Z) * (ncolYcon +
                                                    max(0, (sum(Y.numcat) - length(Y.numcat)))) + (ncolY2con +
                                                                                                     max(0, (sum(Y2.numcat) - length(Y2.numcat)))))
  if (is.null(l2cov.start))
    l2cov.start = diag(1, ncol(u.start))
  if (is.null(l2cov.prior))
    l2cov.prior = diag(1, ncol(l2cov.start))
  if (!is.null(Y.cat)) {
    isnullcat = 0
    previous_levels <- list()
    Y.cat <- data.frame(Y.cat)
    for (i in 1:ncol(Y.cat)) {
      Y.cat[, i] <- factor(Y.cat[, i])
      previous_levels[[i]] <- levels(Y.cat[, i])
      levels(Y.cat[, i]) <- 1:nlevels(Y.cat[, i])
    }
  }
  else {
    isnullcat = 1
    Y.cat = -999
    Y.numcat = -999
  }
  if (!is.null(Y2.cat)) {
    isnullcat2 = 0
    previous_levels2 <- list()
    Y2.cat <- data.frame(Y2.cat)
    for (i in 1:ncol(Y2.cat)) {
      Y2.cat[, i] <- factor(Y2.cat[, i])
      previous_levels2[[i]] <- levels(Y2.cat[, i])
      levels(Y2.cat[, i]) <- 1:nlevels(Y2.cat[, i])
    }
  }
  else {
    isnullcat2 = 1
    Y2.cat = -999
    Y2.numcat = -999
  }
  if (!is.null(Y.con)) {
    stopifnot(nrow(Y.con) == nrow(clus), nrow(Y.con) == nrow(X),
              nrow(Z) == nrow(Y.con))
  }
  if (isnullcat == 0) {
    stopifnot(nrow(Y.cat) == nrow(clus), nrow(Y.cat) == nrow(X),
              nrow(Z) == nrow(Y.cat))
  }
  if (!is.null(Y2.con)) {
    stopifnot(nrow(Y2.con) == nrow(clus), nrow(Y2.con) ==
                nrow(X), nrow(Z) == nrow(Y2.con))
  }
  if (isnullcat2 == 0) {
    stopifnot(nrow(Y2.cat) == nrow(clus), nrow(Y2.cat) ==
                nrow(X), nrow(Z) == nrow(Y2.cat))
  }
  stopifnot(nrow(beta.start) == ncol(X), ncol(beta.start) ==
              (ncolYcon + max(0, (sum(Y.numcat) - length(Y.numcat)))))
  if (!is.null(Y2.con) || Y2.cat[1] != -999)
    stopifnot(nrow(l2.beta.start) == ncol(X2), ncol(l2.beta.start) ==
                (ncolY2con + max(0, (sum(Y2.numcat) - length(Y2.numcat)))))
  stopifnot(ncol(u.start) == ncol(Z) * (ncolYcon + max(0, (sum(Y.numcat) -
                                                             length(Y.numcat)))) + (ncolY2con + max(0, (sum(Y2.numcat) -
                                                                                                          length(Y2.numcat)))))
  if (meth == "common")
    stopifnot(nrow(l1cov.start) == ncol(l1cov.start), nrow(l1cov.prior) ==
                nrow(l1cov.start), nrow(l1cov.start) == ncol(beta.start))
  stopifnot(nrow(l1cov.prior) == ncol(l1cov.prior))
  stopifnot(ncol(l2cov.start) == ncol(u.start), nrow(l2cov.prior) ==
              nrow(l2cov.start), nrow(l2cov.prior) == ncol(l2cov.prior))
  betait = matrix(0, nrow(beta.start), ncol(beta.start))
  for (i in 1:nrow(beta.start)) {
    for (j in 1:ncol(beta.start)) betait[i, j] = beta.start[i,
                                                            j]
  }
  if (!is.null(Y2.con) || Y2.cat[1] != -999) {
    beta2it = matrix(0, nrow(l2.beta.start), ncol(l2.beta.start))
    for (i in 1:nrow(l2.beta.start)) {
      for (j in 1:ncol(l2.beta.start)) beta2it[i, j] = l2.beta.start[i,
                                                                     j]
    }
  }
  covit = matrix(0, nrow(l1cov.start), ncol(l1cov.start))
  for (i in 1:nrow(l1cov.start)) {
    for (j in 1:ncol(l1cov.start)) covit[i, j] = l1cov.start[i,
                                                             j]
  }
  uit = matrix(0, nrow(u.start), ncol(u.start))
  for (i in 1:nrow(u.start)) {
    for (j in 1:ncol(u.start)) uit[i, j] = u.start[i, j]
  }
  covuit = matrix(0, nrow(l2cov.start), ncol(l2cov.start))
  for (i in 1:nrow(l2cov.start)) {
    for (j in 1:ncol(l2cov.start)) covuit[i, j] = l2cov.start[i,
                                                              j]
  }
  if (!is.null(Y.con)) {
    colnamycon <- colnames(Y.con)
    Y.con <- data.matrix(Y.con)
    storage.mode(Y.con) <- "numeric"
  }
  if (isnullcat == 0) {
    colnamycat <- colnames(Y.cat)
    Y.cat <- data.matrix(Y.cat)
    storage.mode(Y.cat) <- "numeric"
  }
  if (!is.null(Y2.con)) {
    colnamy2con <- colnames(Y2.con)
    Y2.con <- data.matrix(Y2.con)
    storage.mode(Y2.con) <- "numeric"
  }
  if (isnullcat2 == 0) {
    colnamy2cat <- colnames(Y2.cat)
    Y2.cat <- data.matrix(Y2.cat)
    storage.mode(Y2.cat) <- "numeric"
  }
  colnamx <- colnames(X)
  colnamz <- colnames(Z)
  X <- data.matrix(X)
  storage.mode(X) <- "numeric"
  Z <- data.matrix(Z)
  storage.mode(Z) <- "numeric"
  if (!is.null(Y2.con) || Y2.cat[1] != -999) {
    colnamx2 <- colnames(X2)
    X2 <- data.matrix(X2)
    storage.mode(X2) <- "numeric"
  }
  clus <- matrix(as.integer(levels(clus))[clus], ncol = 1)
  if (!is.null(Y.con) & (isnullcat == 0)) {
    Y = cbind(Y.con, Y.cat)
    Yi = cbind(Y.con, matrix(0, nrow(Y.con), (sum(Y.numcat) -
                                                length(Y.numcat))))
  }
  else if (!is.null(Y.con)) {
    Y = Y.con
    Yi = Y.con
  }
  else {
    Y = Y.cat
    Yi = matrix(0, nrow(Y.cat), (sum(Y.numcat) - length(Y.numcat)))
  }
  if (!is.null(Y2.con) & isnullcat2 == 0) {
    Y2 = cbind(Y2.con, Y2.cat)
    Y2i = cbind(Y2.con, matrix(0, nrow(Y2.con), (sum(Y2.numcat) -
                                                   length(Y2.numcat))))
  }
  else if (!is.null(Y2.con)) {
    Y2 = Y2.con
    Y2i = Y2.con
  }
  else if (isnullcat2 == 0) {
    Y2 = Y2.cat
    Y2i = matrix(0, nrow(Y2.cat), (sum(Y2.numcat) - length(Y2.numcat)))
  }
  else {
    Y2 = NULL
  }
  h = 1
  if (isnullcat == 0) {
    for (i in 1:length(Y.numcat)) {
      for (j in 1:nrow(Y)) {
        if (is.na(Y.cat[j, i])) {
          Yi[j, (ncolYcon + h):(ncolYcon + h + Y.numcat[i] -
                                  2)] = NA
        }
      }
      h = h + Y.numcat[i] - 1
    }
  }
  h = 1
  if (isnullcat2 == 0) {
    for (i in 1:length(Y2.numcat)) {
      for (j in 1:nrow(Y2)) {
        if (is.na(Y2.cat[j, i])) {
          Y2i[j, (ncolY2con + h):(ncolY2con + h + Y2.numcat[i] -
                                    2)] = NA
        }
      }
      h = h + Y2.numcat[i] - 1
    }
  }
  ncY2 <- max(0, ncol(Y2))
  Ysubimp <- Ysub
  if (is.null(a.start))
    a.start = 50 + ncol(Y)
  if (is.null(a.prior))
    a.prior = a.start
  if (output != 1)
    out.iter = nburn + 2
  nimp = 1
  imp = matrix(0, nrow(Y) * (nimp + 1), ncol(Y) + ncY2 + 4)
  imp[1:nrow(Y), 1] = Ysub
  imp[1:nrow(Y), 2:(1 + ncol(Y))] = Y
  if (!is.null(Y2))
    imp[1:nrow(Y2), (ncol(Y) + 2):(ncol(Y) + ncY2 + 1)] = Y2
  imp[1:nrow(clus), (ncol(Y) + ncY2 + 2)] = clus
  imp[1:nrow(X), (ncol(Y) + ncY2 + 3)] = c(1:nrow(Y))
  Yimp = Yi
  Yimp2 = matrix(Yimp, nrow(Yimp), ncol(Yimp))
  if (!is.null(Y2)) {
    Y2imp = Y2i
    Y2imp2 = matrix(Y2imp, nrow(Y2imp), ncol(Y2imp))
  }
  imp[(nrow(clus) + 1):(2 * nrow(clus)), (ncol(Y) + ncY2 +
                                            2)] = clus
  imp[(nrow(X) + 1):(2 * nrow(X)), (ncol(Y) + ncY2 + 3)] = c(1:nrow(Y))
  imp[(nrow(X) + 1):(2 * nrow(X)), (ncol(Y) + ncY2 + 4)] = 1
  betapost <- array(0, dim = c(nrow(beta.start), ncol(beta.start),
                               (nburn)))
  betaYpost <- array(0, dim = c(1, length(betaY.start), (nburn)))
  if (!is.null(Y2)) {
    beta2post <- array(0, dim = c(nrow(l2.beta.start), ncol(l2.beta.start),
                                  (nburn)))
  }
  upostall <- array(0, dim = c(nrow(u.start), ncol(u.start),
                               (nburn)))
  uYpostall <- array(0, dim = c(nrow(uY.start), ncol(uY.start),
                                (nburn)))
  omegapost <- array(0, dim = c(nrow(l1cov.start), ncol(l1cov.start),
                                (nburn)))
  varYpost <- rep(0, (nburn))
  covupost <- array(0, dim = c(nrow(l2cov.start), ncol(l2cov.start),
                               (nburn)))
  covuYpost <- array(0, dim = c(nrow(as.matrix(covuY.start)),
                                ncol(as.matrix(covuY.start)), (nburn)))
  meanobs <- colMeans(Yi, na.rm = TRUE)
  if (!is.null(Y2)) {
    meanobs2 <- colMeans(Y2i, na.rm = TRUE)
  }
  if (!is.null(start.imp)) {
    start.imp <- as.matrix(start.imp)
    if ((nrow(start.imp) != nrow(Yimp2)) || (ncol(Yimp2) !=
                                             ncol(start.imp))) {
      cat("start.imp dimensions incorrect. Not using start.imp as starting value for the imputed dataset.\n")
      start.imp = NULL
    }
    else {
      Yimp2 <- start.imp
    }
  }
  if (is.null(start.imp)) {
    for (i in 1:nrow(Yi)) for (j in 1:ncol(Yi)) if (is.na(Yimp[i,
                                                               j]))
      Yimp2[i, j] = meanobs[j]
  }
  if (!is.null(l2.start.imp)) {
    l2.start.imp <- as.matrix(l2.start.imp)
    if ((nrow(l2.start.imp) != nrow(Y2imp2)) || (ncol(Y2imp2) !=
                                                 ncol(l2.start.imp))) {
      cat("l2.start.imp dimensions incorrect. Not using l2.start.imp as starting value for the level 2 imputed dataset.\n")
      l2.start.imp = NULL
    }
    else {
      Y2imp2 <- l2.start.imp
    }
  }
  if (!is.null(Y2i) & is.null(l2.start.imp)) {
    for (i in 1:nrow(Y2i)) for (j in 1:ncol(Y2i)) if (is.na(Y2imp[i,
                                                                  j]))
      Y2imp2[i, j] = meanobs2[j]
  }
  if (!is.null(start.imp.sub)) {
    start.imp <- as.matrix(start.imp)
    if (!is.vector(start.imp.sub)) {
      cat("start.imp.sub must be a vector. Not using start.imp as starting value for the imputed dataset.\n")
      start.imp.sub = NULL
    }
    else {
      Ysubimp = start.imp.sub
    }
  }
  if (is.null(start.imp.sub)) {
    for (i in 1:length(Ysubimp)) if (is.na(Ysubimp[i]))
      Ysubimp[i] = mean(Ysubimp, na.rm = TRUE)
  }
  if (!is.null(Y2)) {
    if (meth == "common") {
      .Call("MCMCjomo2lmer", Ysub, Ysubimp, submod, order.sub,
            submod.ran, Y, Yimp, Yimp2, Y.cat, Y2, Y2imp,
            Y2imp2, Y2.cat, X, X2, Z, clus, betaY.start,
            betaYpost, betait, beta2it, uit, uY.start, betapost,
            upostall, uYpostall, beta2post, varY.start, varYpost,
            covit, omegapost, covuY.start, covuYpost, covuit,
            covupost, nburn, varY.prior, covuY.prior, l1cov.prior,
            l2cov.prior, Y.numcat, Y2.numcat, ncolYcon, ncolY2con,
            out.iter, PACKAGE = "jomo")
    }
    else {
      .Call("MCMCjomo2lmerhr", Ysub, Ysubimp, submod, order.sub,
            submod.ran, Y, Yimp, Yimp2, Y.cat, Y2, Y2imp,
            Y2imp2, Y2.cat, X, X2, Z, clus, betaY.start,
            betaYpost, betait, beta2it, uit, uY.start, betapost,
            upostall, uYpostall, beta2post, varY.start, varYpost,
            covit, omegapost, covuY.start, covuYpost, covuit,
            covupost, nburn, varY.prior, covuY.prior, l1cov.prior,
            l2cov.prior, Y.numcat, Y2.numcat, ncolYcon, ncolY2con,
            a.start, a.prior, out.iter, PACKAGE = "jomo")
    }
  }
  else {
    if (meth == "common") {
      .Call("MCMCjomo1lmer", Ysub, Ysubimp, submod, order.sub,
            submod.ran, Y, Yimp, Yimp2, Y.cat, X, Z, clus,
            betaY.start, betaYpost, betait, uit, uY.start,
            betapost, upostall, uYpostall, varY.start, varYpost,
            covit, omegapost, covuY.start, covuYpost, covuit,
            covupost, nburn, varY.prior, covuY.prior, l1cov.prior,
            l2cov.prior, Y.numcat, ncolYcon, out.iter, PACKAGE = "jomo")
    }
    else {
      .Call("MCMCjomo1lmerhr", Ysub, Ysubimp, submod, order.sub,
            submod.ran, Y, Yimp, Yimp2, Y.cat, X, Z, clus,
            betaY.start, betaYpost, betait, uit, uY.start,
            betapost, upostall, uYpostall, varY.start, varYpost,
            covit, omegapost, covuY.start, covuYpost, covuit,
            covupost, nburn, varY.prior, covuY.prior, l1cov.prior,
            l2cov.prior, Y.numcat, ncolYcon, a.start, a.prior,
            out.iter, PACKAGE = "jomo")
    }
  }
  imp[(nrow(Y) + 1):(2 * nrow(Y)), 1] = Ysubimp
  if (!is.null(Y.con)) {
    imp[(nrow(Y) + 1):(2 * nrow(Y)), 2:(1 + ncol(Y.con))] = Yimp2[,
                                                                  1:ncol(Y.con)]
  }
  if (isnullcat == 0) {
    imp[(nrow(Y) + 1):(2 * nrow(Y)), (ncolYcon + 2):(1 +
                                                       ncol(Y))] = Y.cat
  }
  if (!is.null(Y2.con)) {
    imp[(nrow(Y2) + 1):(2 * nrow(Y2)), (ncol(Y) + 2):(1 +
                                                        ncol(Y) + ncol(Y2.con))] = Y2imp2[, 1:ncol(Y2.con)]
  }
  if (isnullcat2 == 0) {
    imp[(nrow(Y2) + 1):(2 * nrow(Y2)), (ncolY2con + ncol(Y) +
                                          2):(ncol(Y) + ncY2 + 1)] = Y2.cat
  }
  if (output == 1)
    cat("First imputation registered.", "\n")
  betaYpostmean <- apply(betaYpost, c(1, 2), mean)
  varYpostmean <- mean(varYpost)
  covuYpostmean <- apply(covuYpost, c(1, 2), mean)
  uYpostmean <- apply(uYpostall, c(1, 2), mean)
  betapostmean <- apply(betapost, c(1, 2), mean)
  if (!is.null(Y2))
    beta2postmean <- apply(beta2post, c(1, 2), mean)
  upostmean <- apply(upostall, c(1, 2), mean)
  omegapostmean <- apply(omegapost, c(1, 2), mean)
  covupostmean <- apply(covupost, c(1, 2), mean)
  if (output == 1) {
    cat("The posterior mean of the substantive model fixed effects estimates is:\n")
    print(betaYpostmean)
    cat("The posterior mean of the substantive model residual variance is:\n")
    print(varYpostmean)
    cat("The posterior mean of the substantive model random effects covariance matrix is:\n")
    print(covuYpostmean)
    cat("The posterior mean of the substantive model random effects estimates is:\n")
    print(uYpostmean)
    cat("The posterior mean of the fixed effects estimates is:\n")
    print(betapostmean)
    if (!is.null(Y2)) {
      cat("The posterior mean of the level 2 fixed effects estimates is:\n")
      print(beta2postmean)
    }
    cat("The posterior mean of the random effects estimates is:\n")
    print(upostmean)
    cat("The posterior mean of the level 1 covariance matrix is:\n")
    print(omegapostmean)
    cat("The posterior mean of the level 2 covariance matrix is:\n")
    print(covupostmean)
  }
  imp <- data.frame(imp)
  if (isnullcat == 0) {
    for (i in 1:ncol(Y.cat)) {
      imp[, (1 + ncolYcon + i)] <- as.factor(imp[, (1 +
                                                      ncolYcon + i)])
      levels(imp[, (1 + ncolYcon + i)]) <- previous_levels[[i]]
    }
  }
  if (isnullcat2 == 0) {
    for (i in 1:ncol(Y2.cat)) {
      imp[, (1 + ncol(Y) + ncolY2con + i)] <- as.factor(imp[,
                                                            (1 + ncol(Y) + ncolY2con + i)])
      levels(imp[, (1 + ncol(Y) + ncolY2con + i)]) <- previous_levels2[[i]]
    }
  }
  imp[, (ncol(Y) + ncY2 + 2)] <- factor(imp[, (ncol(Y) + ncY2 +
                                                 2)])
  levels(imp[, (ncol(Y) + ncY2 + 2)]) <- previous_levels_clus
  clus <- factor(clus)
  levels(clus) <- previous_levels_clus
  if (ncolYcon > 0) {
    for (j in 1:(ncolYcon)) {
      imp[, j + 1] = as.numeric(imp[, j + 1])
    }
  }
  if (ncolY2con > 0) {
    for (j in 1:(ncolY2con)) {
      imp[, ncol(Y) + j + 1] = as.numeric(imp[, ncol(Y) +
                                                j + 1])
    }
  }
  if (isnullcat == 0) {
    if (is.null(colnamycat))
      colnamycat = paste("Ycat", 1:ncol(Y.cat), sep = "")
  }
  else {
    colnamycat = NULL
    Y.cat = NULL
    Y.numcat = NULL
  }
  if (!is.null(Y.con)) {
    if (is.null(colnamycon))
      colnamycon = paste("Ycon", 1:ncol(Y.con), sep = "")
  }
  else {
    colnamycon = NULL
  }
  if (isnullcat2 == 0) {
    if (is.null(colnamy2cat))
      colnamy2cat = paste("Y2cat", 1:ncol(Y2.cat), sep = "")
  }
  else {
    colnamy2cat = NULL
    Y2.cat = NULL
    Y2.numcat = NULL
  }
  if (!is.null(Y2.con)) {
    if (is.null(colnamy2con))
      colnamy2con = paste("Y2con", 1:ncol(Y2.con), sep = "")
  }
  else {
    colnamy2con = NULL
  }
  if (is.null(colnamysub))
    colnamysub = "Ysub"
  colnames(imp) <- c(colnamysub, colnamycon, colnamycat, colnamy2con,
                     colnamy2cat, "clus", "id", "Imputation")
  if (isnullcat == 0) {
    cnycatcomp <- rep(NA, (sum(Y.numcat) - length(Y.numcat)))
    count = 0
    for (j in 1:ncol(Y.cat)) {
      for (k in 1:(Y.numcat[j] - 1)) {
        cnycatcomp[count + k] <- paste(colnamycat[j],
                                       k, sep = ".")
      }
      count = count + Y.numcat[j] - 1
    }
    if (!is.null(Y.con)) {
      cnamycomp <- c(colnamycon, cnycatcomp)
    }
    else {
      cnamycomp <- c(cnycatcomp)
    }
  }
  else {
    cnamycomp <- c(colnamycon)
  }
  if (isnullcat2 == 0) {
    cny2catcomp <- rep(NA, (sum(Y2.numcat) - length(Y2.numcat)))
    count = 0
    for (j in 1:ncol(Y2.cat)) {
      for (k in 1:(Y2.numcat[j] - 1)) {
        cny2catcomp[count + k] <- paste(colnamy2cat[j],
                                        k, sep = ".")
      }
      count = count + Y2.numcat[j] - 1
    }
    if (!is.null(Y2.con)) {
      cnamy2comp <- c(colnamy2con, cny2catcomp)
    }
    else {
      cnamy2comp <- c(cny2catcomp)
    }
  }
  else {
    cnamy2comp <- c(colnamy2con)
  }
  dimnames(betapost)[1] <- list(colnamx)
  dimnames(betapost)[2] <- list(cnamycomp)
  if (!is.null(Y2)) {
    dimnames(beta2post)[1] <- list(colnamx2)
    dimnames(beta2post)[2] <- list(cnamy2comp)
  }
  dimnames(omegapost)[1] <- list(cnamycomp)
  dimnames(omegapost)[2] <- list(cnamycomp)
  dimnames(Yimp2)[2] <- list(cnamycomp)
  return(list(finimp = imp, collectbeta = betapost, collectu = upostall,
              collectomega = omegapost, collectcovu = covupost, finimp.latnorm = Yimp2,
              l2.finimp.latnorm = Y2imp2, collectbetaY = betaYpost,
              collectvarY = varYpost, collectcovuY = covuYpost, collectuY = uYpostall))
}
