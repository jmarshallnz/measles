jm_estR0 <- function (epid, GT, import = NULL, n.t0 = NULL, t = NULL, begin = NULL, 
    end = NULL, date.first.obs = NULL, time.step = 1, q = c(0.025, 
        0.975), correct = TRUE, nsim = 10000, checked = FALSE, 
    ...) 
{
    if (nsim < 1000) 
        warning("Accurate confidence interval for R(t) requires a large number of simulations. Consider increasing 'nsim'")
    if (nsim > 100) 
        warning("Simulations may take several minutes.")
    DNAME = deparse(substitute(epid))
    CALL = match.call()
    if (checked == FALSE) {
        parameters <- integrity.checks(epid, t, GT, begin, end, 
            date.first.obs, time.step, AR = NULL, S0 = NULL, 
            methods = "TD")
        begin <- parameters$begin
        end <- parameters$end
    }
    epid = check.incid(epid, t, date.first.obs, time.step)
    begin.nb = which(epid$t == begin)
    end.nb = which(epid$t == end)
    epid.bak <- epid
    t <- diff(c(FALSE, epid$incid == 0, FALSE), 1)
    start <- which(t == 1)
    end <- which(t == -1)
    if (length(start) > 0 & length(end) > 0) {
        longest <- max(end - start)
        if (longest > length(GT$GT)) 
            warning(paste("Gap in epidemic curve is longer than the generation interval. Consider using a different GT distribution (maybe with \"truncate=", 
                longest, "\" (length of longest gap))."), sep = "")
    }
    if (is.null(import)) {
        import <- rep(0, length(epid$incid))
    }
    if (!is.null(import) & (length(import) != length(epid$incid))) {
        stop("Vector of imported cases should have the same length as 'epid' data.")
    }
    if (is.null(n.t0)) {
        n.t0 = epid$incid[1]
        warning("Using initial incidence as initial number of cases.")
    }
    if (n.t0 > epid$incid[1]) {
        stop(paste("Provided initial number of cases (n.t0=", 
            n.t0, ") is larger than incidence on begin day (=", 
            epid$incid[begin], ")"))
    }
    Tmax = length(epid$incid)
    GT.pad = GT$GT
    if (length(GT.pad) < Tmax) {
        GT.pad <- c(GT.pad, rep(0, Tmax - length(GT.pad)))
    }
    P <- matrix(0, ncol = Tmax, nrow = Tmax)
    p <- matrix(0, ncol = Tmax, nrow = Tmax)
    multinom.simu = vector("list", Tmax)
    multinom.simu[[1]] = matrix(0, Tmax, nsim)
    if (epid$incid[1] - n.t0 > 0) {
        P[1, 1] <- (epid$incid[1] - n.t0)/(epid$incid[1] - 1)
        p[1, 1] <- 1
        multinom.simu[[1]][1, ] = rmultinom(nsim, epid$incid[1] - 
            n.t0, p[1:1, 1])
    }
    epid.orig <- epid
    epid$incid = epid$incid + import
    for (s in 2:Tmax) {
        multinom.simu[[s]] = matrix(0, Tmax, nsim)
        if ((epid$incid[s] - import[s] > 0)) {
            weight.cases.for.s <- (epid$incid[1:s] - c(rep(0, 
                s - 1), 1 + import[s])) * GT.pad[s:1]
            weight.cases.for.s <- weight.cases.for.s/sum(weight.cases.for.s)
            prob.cases.for.s <- weight.cases.for.s * (epid$incid[s] - 
                import[s])/(epid$incid[1:s] - c(rep(0, s - 1), 
                1 + import[s]))
            prob.cases.for.s[epid$incid[1:(s - 1)] == 0] <- 0
            if (epid$incid[s] - import[s] == 1) {
                prob.cases.for.s[s] <- 0
            }
            P[1:s, s] <- prob.cases.for.s
            p[1:s, s] <- weight.cases.for.s
            multinom.simu[[s]][1:s, ] = multinom.simu[[s - 1]][1:s, 
                ] + rmultinom(nsim, epid$incid[s] - import[s], 
                p[1:s, s])
        }
        else {
            P[1:s, s] <- 0
            p[1:s, s] <- 0
            multinom.simu[[s]][1:s, ] = multinom.simu[[s - 1]][1:s, 
                ]
        }
    }
    R.WT <- apply(P, 1, sum)
    R.corrected <- R.WT/(cumsum(GT.pad[1:Tmax]))[Tmax:1]
    if (is.na(R.corrected[length(epid$incid)])) {
        R.corrected[length(epid$incid)] <- 0
    }
    total.infected.by.time.unit.simu = multinom.simu[[length(epid$incid)]]
    R.simu <- total.infected.by.time.unit.simu/c(epid$incid)
    R.simu.corrected <- R.simu/(cumsum(GT.pad[1:Tmax]))[Tmax:1]
    R.simu.corrected[Tmax, ] <- 0
    quant.simu = matrix(0, Tmax, length(q))
    quant.simu.corrected = matrix(0, Tmax, length(q))
    for (s in 1:Tmax) {
        if (epid$incid[s] == 0) {
            R.WT[s] <- 0
            R.simu[s,] <- 0
            R.corrected[s] <- 0
            R.simu.corrected[s,] <- 0
        }
        quant.simu[s, ] = quantile(R.simu[s, ], q, na.rm = TRUE)
        quant.simu.corrected[s, ] = quantile(R.simu.corrected[s, 
            ], q, na.rm = TRUE)
    }
    # JM: average over time
    R0 <- apply(R.simu.corrected, 2, mean)

    conf.int = matrix(data = NA, nrow = end.nb, ncol = length(q))
    colnames(conf.int) = q
    if (correct == TRUE) {
        R = R.corrected[begin.nb:end.nb]
        conf.int[begin.nb:end.nb,] = quant.simu.corrected[begin.nb:end.nb,]
    }
    else {
        R = R.WT[begin.nb:end.nb]
        conf.int[begin.nb:end.nb,] = quant.simu[begin.nb:end.nb,]
    }
    names(R) = epid$t[begin.nb:end.nb]
    conf.int <- data.frame(na.omit(conf.int))
    rownames(conf.int) = as.character(epid$t[begin.nb:end.nb])
    pred = epid$incid
    pred[2:(length(epid$incid) + length(GT$GT))] = 0
    for (s in 1:end.nb) {
        pred[s:(s + length(GT$GT) - 1)] = pred[s:(s + length(GT$GT) - 
            1)] + R[s] * epid$incid[s] * GT$GT
    }
    pred = pred[1:end.nb]
    return(structure(list(R = R, R0 = R0, conf.int = conf.int, P = P, 
        p = p, GT = GT, epid = epid.bak, import = import, pred = pred, 
        begin = begin, begin.nb = begin.nb, end = end, end.nb = end.nb, 
        data.name = DNAME, call = CALL, method = "Time-Dependent", 
        method.code = "TD"), class = "R0.R"))
}

