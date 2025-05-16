#' Jim's robust variance esitmator
#'
#' @note Changed the following:
#'   - stats::model.matrix
#'   - lme4::isLMM
#'   - stats::family
#'   - stats::sigma
#'   - stats::vcov
#'   - MASS::ginv
#'   - expm::sqrtm
#'   - stats::nobs
#'   - lme4::fixef
#'   - lme4::predict.merMod
#'   - lme4::getME
#'
#' @param obj Model object
#' @param cluster (Optional) cluster identifier
#' @param type One of c("classic","DF","KC","MD","FG")
#'
#' @return A robust covariance matrix estimate
#' @noRd
vcovCR.glmerMod = function(obj, cluster, type="classic"){

  # Helper functions (from clubSandwich)
  get_outer_group <- function(obj) {
    group_n <- lme4::getME(obj, "l_i")
    group_facs <- lme4::getME(obj, "flist")
    group_facs[[which.min(group_n)]]
  }
  check_nested <- function(inner_grp, outer_grp) {
    n_outer <- tapply(outer_grp, inner_grp, function(x) length(unique(x)))
    all(n_outer == 1)
  }
  is_nested_lmerMod <- function(obj, cluster = get_outer_group(obj)) {
    group_facs <- lme4::getME(obj, "flist")
    nested <- vapply(group_facs, check_nested, outer_grp=cluster, FUN.VALUE=TRUE)
    all(nested)
  }

  # Check if obj is a fitted model from lmer or glmer
  if ("merMod" %in% class(obj)) {
    stop("The 'obj' should be an object fitted using lmer or glmer.")
  }
  if (!is.null(obj@call$weights))
    stop("Models with prior weights are not currently supported.")
  # Check if cluster is manually input and of class factor
  if (!missing(cluster) && !is.factor(cluster)) {
    stop("If 'cluster' is manually input, it must be of class 'factor'.")
  }
  if (missing(cluster))
    cluster <- get_outer_group(obj)
  if (!is_nested_lmerMod(obj, cluster))
    stop("Non-nested random effects detected. Method is not available for such models.")
  ropt = substr(type,1,2)
  if (ropt=="FG") {
    type1="FG"
    if (nchar(type)==2) {
      r = 0.75
    } else {
      r = as.numeric(substr(type,regexpr("\\(",type)[[1]]+1,regexpr("\\)",type)[[1]]-1))
    }
  } else {
    if (ropt=="KC") {
      type1="KC"
      if (nchar(type)==2) {
        exact = FALSE
      } else {
        kcstr = tolower(substr(type,regexpr("\\(",type)[[1]]+1,regexpr("\\)",type)[[1]]-1))
        if (!(kcstr=="exact" | kcstr=="e")) stop("Invalid option for KC")
        exact = TRUE
      }
    } else {
      type1 = type
    }}
  # Check if type is one of the specific allowed values
  allowed_types <- c("classic", "DF", "KC", "MD", "FG")
  if (!(type1 %in% allowed_types)) {
    stop("The 'type' must be one of the following: 'classic', 'DF', 'KC', 'MD', 'FG'.")
  }
  #################
  # helper functions
  #################
  mtx_DA <- function(D,A) {
    matrix(rep(diag(D),ncol(A))*as.numeric(A), ncol=ncol(A))
  }
  mtx_AD <- function(A,D) {
    matrix(rep(diag(D), each=nrow(A))*as.numeric(A), ncol=ncol(A))
  }
  #################
  # extract information from obj
  #################
  n = stats::nobs(obj)
  clusternames = unique(cluster)
  m = length(clusternames)
  #
  X = stats::model.matrix(obj,type="fixed")
  beta=matrix(lme4::fixef(obj),ncol=1)
  np=dim(beta)[1]
  #
  Z = stats::model.matrix(obj,type="random")
  nq=dim(Z)[2]
  #
  Y = obj@resp$y
  # The following allows processing of binomial data
  if (lme4::isLMM(obj)) nden=rep(1,length(Y)) else nden = obj@resp$n
  #
  eta = lme4::predict.merMod(obj,type="link")
  ginv_eta = lme4::predict.merMod(obj,type="response")
  #
  link = stats::family(obj)$link
  #
  sigma2 = stats::sigma(obj)^2
  lambda = lme4::getME(obj,"Lambda")
  R = as.matrix(lambda%*%t(lambda)*sigma2)
  WB_B <- R
  if (Matrix::isDiagonal(WB_B)) diagB=TRUE else diagB=FALSE
  ##################
  # Robust variance calculation
  ##################
  XtVX = stats::vcov(obj)
  WB_C1 = solve(XtVX)
  sum=matrix(0,np,np)
  for (g in clusternames){
    grp = (cluster == g & nden>0)
    ng = sum(grp)
    if (link == "identity") {
      delta = diag(ng)
      deltainv = delta
    } else if (link == "logit") {
      term = ginv_eta[grp]*(1-ginv_eta[grp])
      delta = diag(term,ng,ng)
      deltainv = diag(1/term,ng,ng)
    } else if (link == "log") {
      term = ginv_eta[grp]
      delta = diag(term,ng,ng)
      deltainv = diag(1/term,ng,ng)
    } else {
      stop("Link ",link," not supported")
    }
    #
    P = deltainv%*%(Y[grp]-ginv_eta[grp]) + eta[grp]
    e = matrix(P - X[grp,]%*%beta,ncol=1)
    ete = e[,,drop=FALSE]%*%t(e[,,drop=FALSE])
    #
    Sigma = diag(sigma2*stats::family(obj)$variance(ginv_eta[grp])/nden[grp])

    # this is diagonal, which is the first term of WB

    if (link=="identity") {
      WB_A <- diag(1/diag(Sigma))
    } else {
      WB_A <- diag(1/diag(mtx_AD(mtx_DA(deltainv,Sigma),deltainv)))
    }

    WB_U <- Z[grp,,drop=FALSE]
    WB_Ut <- t(Z[grp,,drop=FALSE])
    # Compute the inverse
    if (diagB) {
      WB_AUB <- mtx_AD(mtx_DA(WB_A,WB_U),WB_B)
    } else {
      WB_AUB <- mtx_DA(WB_A,WB_U)%*%WB_B
    }
    WB_UtA <- mtx_AD(WB_Ut,WB_A)
    Vinv <- WB_A - WB_AUB%*%solve(diag(nq) + WB_Ut%*%WB_AUB)%*%WB_UtA
    #
    if (type1=="MD") {
      WB_U = -t(Vinv)%*%X[grp,]
      WB_V = t(X[grp,])
      # Since WB_A is identity, the following expressions are simplified from general Woodbury
      O = WB_C1 + WB_V%*%WB_U
      #      WB_A = diag(ng)
      #      FF = WB_A - WB_U%*%MASS::ginv(matrix(as.numeric(O),dim(O)))%*%WB_V
      FF = -(WB_U%*%MASS::ginv(matrix(as.numeric(O),dim(O)))%*%WB_V)
      diag(FF) = diag(FF) + 1
      FVX = FF%*%Vinv%*%X[grp,]
      sum = sum + t(FVX)%*%ete%*%FVX
    } else {
      if (type1=="KC") {
        if (exact) {
          H = X[grp,]%*%XtVX%*%t(X[grp,])%*%Vinv
          FF = MASS::ginv(expm::sqrtm(diag(ng) - t(H)))
          if (is.complex(FF)) stop("(I-H_g)^(-1/2) is complex")
          FVX = FF%*%Vinv%*%X[grp,]
          sum = sum + t(FVX)%*%ete%*%FVX
        } else {
          WB_U = -t(Vinv)%*%X[grp,]
          WB_V = t(X[grp,])
          # Since WB_A is identity, the following expressions are simplified from general Woodbury
          O = WB_C1 + WB_V%*%WB_U
          #        WB_A = diag(ng)
          #        FF = WB_A - WB_U%*%MASS::ginv(matrix(as.numeric(O),dim(O)))%*%WB_V
          FF = -(WB_U%*%MASS::ginv(matrix(as.numeric(O),dim(O)))%*%WB_V)
          diag(FF) = diag(FF) + 1
          VX = Vinv%*%X[grp,]
          FVX = FF%*%VX
          term = t(FVX)%*%ete%*%VX
          #        sum = sum + (t(FVX)%*%ete%*%VX + t(VX)%*%ete%*%FVX)/2
          sum = sum + (term + t(term))/2
        }
      } else {
        if (type1=="FG") {
          Q = t(X[grp,])%*%Vinv%*%X[grp,]%*%XtVX
          XAA = mtx_AD(X[grp,],diag(1/sqrt(1-pmin(r,diag(Q)))))
          #      XAA = X[grp,]%*%diag(1/sqrt(1-pmin(r,diag(Q))))
          sum = sum + t(XAA)%*%Vinv%*%ete%*%Vinv%*%XAA
        } else {
          VX = Vinv%*%X[grp,]
          sum = sum + t(VX)%*%ete%*%VX
        }}}
    #
  }
  c = 1
  if (type1=="DF") {
    if (m-np>0) c= m/(m-np) else cat("DF not valid because m-p <= 0; defaulting to classic")
  }
  robustVar = c*XtVX%*%sum%*%XtVX
  robustVar
}
