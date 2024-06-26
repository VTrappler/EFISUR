rm(list = ls())
library("renv")

# Imports ########################################

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[, 1])) {
    renv::install(p, dep = TRUE)
  }
  require(p, character.only = TRUE)
}

usePackage("DiceDesign")
usePackage("DiceOptim")
usePackage("DiceKriging")
usePackage("parallel")
usePackage("Rsolnp")
usePackage("nloptr")
usePackage("randtoolbox")

library("DiceDesign")
library("DiceOptim")
library("DiceKriging")
library("parallel")
library("Rsolnp")
library("nloptr")
library("randtoolbox")

# Test functions ########

fct1 <- function(input) {
  return((1 / 3 * input[2]^4 - 2.1 * input[2]^2 + 4) * input[2]^2 +
    input[1] * input[2] + 4 * input[1]^2 * (input[1]^2 - 1))
}

fct2 <- function(input) {
  return(((3 * input[1]^2 + 7 * input[1] * input[2] - 3) * exp(-1 * (input[1] * input[2])^2) *
    cos(5 * pi * input[1]^2) - 1.2 * input[2]^2))
}

fct3 <- function(input) {
  return(5 * (input[1]^2 + input[2]^2) - (input[3]^2 + input[4]^2) +
    input[1] * (-input[3] + input[4] + 5) + input[2] * (input[3] - input[4] + 3))
}

fct4 <- function(input) {
  return(-input[1]^2 + 5 * input[2] - input[3] + input[4]^2 - 1)
}

# Test functions ########


#' Surrogate model of f
Objective_function <- function(x, d, m, names, model_joint_objective) {
  control <- matrix(x, nrow = 1, ncol = (d + m))
  control <- data.frame(control)
  names(control) <- names[1:(d + m)]
  p <- predict(model_joint_objective, newdata = data.frame(x = control), type = "UK", checkNames = FALSE)
  return(p$mean)
}

#' Surrogate model F of E[f(x,U)]
Expectation_Objective_function <- function(x, alea, d, m, model_joint_objective) {
  m1 <- matrix(x, nrow = 1, ncol = d)
  m1 <- t(matrix(rep(m1, dim(alea)[1]), d, dim(alea)[1]))
  dat <- data.frame(cbind(m1, alea))
  p <- predict(model_joint_objective, newdata = data.frame(x = dat), type = "UK", checkNames = FALSE)
  return(mean(p$mean))
}

Constraint_function <- function(x, d, m, names, model_joint_constraint) {
  control <- matrix(x, nrow = 1, ncol = (d + m))
  control <- data.frame(control)
  names(control) <- names[1:(d + m)]
  p <- predict(model_joint_constraint, newdata = data.frame(x = control), type = "UK", checkNames = FALSE)
  return(p$mean)
}

Quantile_Constraint_function <- function(x, alea, d, m, model_joint_constraint) {
  m1 <- matrix(x, nrow = 1, ncol = d)
  m1 <- t(matrix(rep(m1, dim(alea)[1]), d, dim(alea)[1]))
  dat <- data.frame(cbind(m1, alea))
  p <- predict(model_joint_constraint, newdata = data.frame(x = dat), type = "UK", checkNames = FALSE)
  return(quantile(p$mean, prob = 0.95))
}

mean_sd_Z <- function(x, model, alea, d, m, select) {
  x <- matrix(x, ncol = d, nrow = 1)
  alea <- matrix(alea, ncol = m)
  m1 <- matrix(x, nrow = 1, ncol = d)
  m2 <- t(matrix(rep(x, dim(alea)[1]), d, dim(alea)[1]))
  dat <- data.frame(cbind(m2, alea))
  pred <- DiceKriging::predict.km(model, dat, checkNames = FALSE, type = "UK", cov.compute = FALSE)
  if (select == "TRUE") {
    return(mean(pred$mean))
  }
  if (select == "FALSE") {
    return(abs(mean(pred$sd)))
  }
}

Phi_G <- function(x, modelConst, alea, seuil, d, m, sign) {
  input <- matrix(0, dim(alea)[1], d + m)
  input[, ((d + 1):(d + m))] <- alea
  for (j in 1:dim(alea)[1]) input[j, 1:d] <- x
  pred <- DiceKriging::predict.km(modelConst, input, checkNames = FALSE, type = "UK", cov.compute = FALSE)
  if (sign == ">") {
    phi <- pnorm((pred$mean - seuil) / pred$sd)
  } else {
    phi <- pnorm((seuil - pred$mean) / pred$sd)
  }
  return(phi)
}

Expectation_A <- function(x, alea, d, m, list_const_km, seuil, sign, alpha) {
  d_c <- length(list_const_km) # constraints models
  PHI <- matrix(NA, nrow = d_c, ncol = dim(alea)[1])
  for (i in 1:d_c) PHI[i, ] <- Phi_G(x, list_const_km[[i]], alea, seuil[i], d, m, sign[i])
  E_A <- 1 - alpha - mean(apply(PHI, 2, prod))
  return(E_A)
}

z_mean_feasible <- function(modelObjective, d, m, alea, list_const_km, seuil, sign, alpha) {
  X <- matrix(modelObjective@X[, 1:d], ncol = d)
  res <- apply(X, 1, mean_sd_Z, modelObjective, alea, d, m, select = "TRUE")
  res1 <- apply(X, 1, Expectation_A, alea, d, m, list_const_km, seuil, sign, alpha)
  index <- which(res1 <= 0)
  if (length(index) > 0) {
    index2 <- which.min(res[index])
    return(list(min = res[index[index2]], opt = index[index2], mar = 1))
  }
  if (length(index) == 0) {
    t <- which.max(res1)
    return(list(min = res[t], opt = t, mar = 2))
  }
}

Probability_A <- function(x, alea, Nsim, d, m, list_const_km, seuil, sign, alpha) {
  input <- matrix(0, dim(alea)[1], d + m)
  input[, (d + 1):(d + m)] <- alea
  for (j in 1:dim(alea)[1]) input[j, 1:d] <- x
  d_c <- length(list_const_km) # constraints models
  realizations <- array(0, dim = c(Nsim, dim(alea)[1], d_c)) # multidimensional array (Nsim,CRN,nbreConstraint)
  for (i in 1:d_c) {
    realizations[, , i] <- DiceKriging::simulate(list_const_km[[i]],
      nsim = Nsim, newdata = input,
      checkNames = FALSE, type = "UK", cond = TRUE
    )
  }
  for (i in 1:d_c) {
    if (sign[i] == ">") {
      realizations[, , i] <- seuil[i] - realizations[, , i]
    } else {
      realizations[, , i] <- realizations[, , i] - seuil[i]
    }
  }
  sim <- apply(realizations, c(1, 2), max) ## max(g1,g2,g3)
  p_sim <- apply(sim, 1, function(vect) length(which(vect <= 0)) / dim(alea)[1])
  c <- length(which(1 - alpha - p_sim <= 0)) / Nsim
  return(c = c)
}

Feas_Expected_Improvement <- function(x, minimum_feas, model, alea, Nsim, d, m, list_const_km, seuil, sign, alpha) {
  sdZ <- abs(mean_sd_Z(x, model, alea, d, m, select = "FALSE"))
  espZ <- mean_sd_Z(x, model, alea, d, m, select = "TRUE")
  v <- (minimum_feas - espZ) / sdZ
  phi <- dnorm(v, mean = 0, sd = 1, log = FALSE)
  PHI <- pnorm(v, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
  EIZ <- sdZ * (v * PHI + phi)
  proba <- Probability_A(x, alea, Nsim, d, m, list_const_km, seuil, sign, alpha)
  return(c(EIZ) * proba)
}

PredVar_Z <- function(xstar, xp1, model, alea, d, m) {
  U_MC <- dim(alea)[1]
  xxstar <- xstar
  objectinit <- model@covariance

  X1 <- (as.matrix(rbind(model@X, as.vector(xp1))))
  X2 <- (as.matrix(rbind(model@X, as.vector(xp1))))
  n1 <- nrow(X1)
  n2 <- nrow(X2)
  dim <- ncol(X1)
  C <- covMat1Mat2(model@covariance, X1, X2, nugget.flag = TRUE) / model@covariance@sd2
  C <- solve(C)

  ########################################################### FIRST TERM
  U1 <- alea
  U2 <- alea
  nU1 <- nrow(U1)
  nU2 <- nrow(U2)
  dimU <- ncol(U1)
  object <- objectinit
  object@range.val <- covparam2vect(objectinit)[(d + 1):(d + m)]
  outU <- covMat1Mat2(object, U1, U2, nugget.flag = TRUE) / model@covariance@sd2
  MU <- mean(matrix(outU, nU1, nU2))
  term1 <- MU

  ########################################################### SECOND TERM

  X1 <- cbind(t(matrix(rep(xstar, U_MC), length(xstar), U_MC)), alea)
  X2 <- (as.matrix(rbind(model@X, as.vector(xp1))))
  n1 <- nrow(X1)
  n2 <- nrow(X2)
  dim <- ncol(X1)
  object <- objectinit
  object@range.val <- covparam2vect(objectinit)
  M <- covMat1Mat2(model@covariance, X1, X2, nugget.flag = TRUE) / model@covariance@sd2

  X1 <- (as.matrix(rbind(model@X, as.vector(xp1))))
  X2 <- cbind(t(matrix(rep(xstar, U_MC), length(xstar), U_MC)), alea)
  n1 <- nrow(X1)
  n2 <- nrow(X2)
  dim <- ncol(X1)
  object <- objectinit
  object@range.val <- covparam2vect(objectinit)
  trans_M <- covMat1Mat2(model@covariance, X1, X2, nugget.flag = TRUE) / model@covariance@sd2

  a <- matrix(apply(M, 2, mean), nrow = 1)
  b <- matrix(apply(trans_M, 1, mean), ncol = 1)
  term2 <- a %*% C %*% (b)
  ###########################################################
  return(term1 - term2)
}

PredMean_Z <- function(x, xp1, observation, model, alea, d, m) {
  m1 <- matrix(x, nrow = 1, ncol = d)
  m2 <- t(matrix(rep(x, dim(alea)[1]), d, dim(alea)[1]))
  dat <- data.frame(cbind(m2, alea))
  colnames(dat) <- NULL
  xp1 <- as.vector(xp1)
  data <- data.frame(rbind(xp1, dat))
  utile <- DiceKriging::predict.km(model, data, checkNames = FALSE, type = "UK", cov.compute = TRUE)
  term1 <- mean_sd_Z(x, model, alea, d, m, "TRUE")
  term2 <- (observation - utile$mean[1]) / utile$sd[1]^2
  term3 <- mean(utile$cov[1, ])
  return(term1 + term2 * term3)
}

PredVar_F <- function(newdata, xp1, model, d, m) {
  newdata <- matrix(newdata, ncol = d + m)
  xp1 <- matrix(xp1, ncol = d + m)
  utile <- DiceKriging::predict.km(model, rbind(xp1, newdata), checkNames = FALSE, type = "UK", cov.compute = TRUE)
  return(utile$sd[2]^2 - utile$cov[1, 2]^2 / utile$sd[1]^2)
}

TotalVar <- function(x, xp1, model, alea, d, m, observation, ming, variZplus1) {
  meanZplus1 <- PredMean_Z(x = x, xp1 = xp1, observation, model = model, alea = alea, d = d, m = m)

  term <- (ming - meanZplus1) / sqrt(variZplus1)
  a <- pnorm(term, mean = 0, sd = 1)
  b <- dnorm(term, mean = 0, sd = 1)

  term1 <- (ming - meanZplus1)^2 + variZplus1
  term2 <- sqrt(variZplus1) * (ming - meanZplus1)

  expected_imprmnt <- (ming - meanZplus1) * a + sqrt(variZplus1) * b

  return(c(expected_imprmnt, term1 * a + term2 * b - expected_imprmnt^2))
}

aggregate_u_tp1 <- function(utilde, xnew, inputsss, rep, ming, modelObjective, alea, m, d, list_const_km, seuil, sign) {
  input4s <- c(xnew, utilde)
  stopping <- 0
  oldlaw <- DiceKriging::predict.km(modelObjective, rbind(input4s), checkNames = FALSE, type = "UK", cov.compute = TRUE)
  oldlaw <- c(oldlaw$mean, oldlaw$sd)
  aleaa <- matrix(randtoolbox::sobol(n = rep, dim = 1, init = TRUE, scrambling = 0, seed = 4711, normal = TRUE), ncol = 1)
  aleaa <- aleaa * oldlaw[2] + oldlaw[1]
  variZplus1 <- abs(PredVar_Z(xnew, input4s, modelObjective, alea, d, m))
  term1 <- term2 <- NULL
  repeat{
    stopping <- stopping + 1
    muet <- TotalVar(xnew, input4s, modelObjective, alea, d, m, aleaa[stopping], ming, variZplus1)
    term1 <- c(term1, muet[1])
    term2 <- c(term2, muet[2])
    if (stopping == rep) break
  }
  res <- var(term1) + mean(term2)
  return(res)

  d_c <- length(list_const_km) # constraints models
  PHI <- matrix(NA, nrow = d_c, ncol = dim(inputsss)[1])
  for (i in 1:d_c)
  {
    pred <- DiceKriging::predict(list_const_km[[i]], newdata = inputsss, checkNames = FALSE, type = "UK", cond = TRUE)
    predicted_sd2 <- apply(inputsss, 1, PredVar_F, input4s, list_const_km[[i]], d, m)
    if (sign[i] == ">") {
      PHI[i, ] <- pnorm((pred$mean - seuil[i]) / sqrt(abs(predicted_sd2)))
    } else {
      PHI[i, ] <- pnorm((seuil[i] - pred$mean) / sqrt(abs(predicted_sd2)))
    }
  }
  pq <- apply(PHI, 2, prod)
  pp <- mean(pq * (1 - pq))
  return(pp * res)
}

EFISUR <- function(model_O, listCon, bornes, d, m, alea, N, alpha, sign, seuil, solver) {
  min_feas_init <- z_mean_feasible(model_O, d, m, alea, listCon, seuil, sign, alpha) # feasible minimum (z_min^{feas})
  hist_feasmin <- c(min_feas_init$min) # feasible minimum value
  hist_feasopt <- c(min_feas_init$opt) # associated point

  f <- function(x) {
    return(-Feas_Expected_Improvement(x, min_feas_init$min, model_O, alea, N, d, m, listCon, seuil, sign, alpha))
  }

  if (solver == "bobyqa") {
    start <- lhsDesign(1, d)$design
    for (a in 1:d) start[, a] <- bornes[a, 1] + (bornes[a, 2] - bornes[a, 1]) * start[, a]

    opt_solver_X <- bobyqa(start,
      fn = f, lower = bornes[1:d, 1], upper = bornes[1:d, 2],
      control = list(maxeval = 500)
    )
    x_tplus1 <- opt_solver_X$par
  }
  if (solver == "discrete") {
    N_xOpt <- 100 * d
    Opt <- lhsDesign(N_xOpt, d)$design
    for (a in 1:d) Opt[, a] <- bornes[a, 1] + (bornes[a, 2] - bornes[a, 1]) * Opt[, a]
    crit <- apply(Opt, 1, f)
    x_tplus1 <- Opt[which.max(crit), ]
  }

  ##### bobyqa in U

  inputsss <- matrix(0, N, (d + m))
  inputsss[, (d + 1):(d + m)] <- alea
  for (j in 1:N) inputsss[j, 1:d] <- x_tplus1

  g <- function(utilde) {
    return(aggregate_u_tp1(utilde, x_tplus1, inputsss,
      rep = 5, min_feas_init$min, model_O,
      alea, m, d, listCon, seuil, sign
    ))
  }

  if (solver == "bobyqa") {
    start <- lhsDesign(1, m)$design
    for (a in 1:m) start[, a] <- bornes[(d + a), 1] + (bornes[(d + a), 2] - bornes[(d + a), 1]) * start[, a]
    opt_solver_U <- bobyqa(start,
      fn = g, lower = rep(bornes[(d + 1):(d + m), 1], m),
      upper = rep(bornes[(d + 1):(d + m), 2], m), control = list(maxeval = 200)
    )
    u_tplus1 <- opt_solver_U$par
  }
  if (solver == "discrete") {
    N_uOpt <- 100 * d
    Opt <- lhsDesign(N_uOpt, d)$design
    for (a in 1:m) Opt[, a] <- bornes[(a + d), 1] + (bornes[(a + d), 2] - bornes[(a + d), 1]) * Opt[, a]
    crit <- apply(Opt, 1, g)
    u_tplus1 <- Opt[which.min(crit), ]
  }
  return(list(
    res = cbind(x_tplus1, u_tplus1),
    FeasMin = min_feas_init$min,
    OptPoint = model_O@X[min_feas_init$opt, 1:d]
  ))
}

renv::snapshot()
