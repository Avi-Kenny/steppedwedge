#' Jim's robust variance esitmator
#'
#' @param obj Model object
#' @param cluster (Optional) cluster identifier
#' @param type One of c("classic","DF","KC","MD","FG")
#'
#' @return A robust covariance matrix estimate
#' @noRd
vcovCR.glmerMod = function(obj, cluster, type="classic"){
  eps=1e-14
  ######################
  # Helper functions (from clubSandwich)
  ######################
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
  #################
  # other helper functions
  #################
  mtx_DA <- function(D,A) {
    matrix(rep(Matrix::diag(D),ncol(A))*as.numeric(A), ncol=ncol(A))
  }
  mtx_AD <- function(A,D) {
    matrix(rep(Matrix::diag(D), each=nrow(A))*as.numeric(A), ncol=ncol(A))
  }
  
  ##################
  # Function starts here
  ##################
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
  ######################
  # decode options
  #######################
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
      if (ropt=="MB") {
        type1="MBN"
        # defaults
        DF = TRUE
        d = 2
        r = 1
        text = substr(type,regexpr("\\(",type)[[1]]+1,regexpr("\\)",type)[[1]]-1)
        if (text!=""){
          mbnargs = lapply(strsplit(text,","),strsplit,"=")
          numargs = length(mbnargs[[1]])
          for (i in 1:numargs){
            if (!(mbnargs[[1]][[i]][1] %in% c("DF","d","r"))) {
              stop("Allowable arguments for MBN are 'DF','d','r'")
            }
            if (mbnargs[[1]][[i]][1]=="DF") {
              assign(mbnargs[[1]][[i]][1],as.logical(mbnargs[[1]][[i]][2]))
            } else {
              assign(mbnargs[[1]][[i]][1],as.numeric(mbnargs[[1]][[i]][2]))
            }
          }
        }
      } else { 
        type1 = type
      }}}
  # Check if type is one of the specific allowed values
  allowed_types <- c("classic", "DF", "KC", "MD", "FG", "MBN")
  if (!(type1 %in% allowed_types)) {
    stop("The 'type' must be one of the following: 'classic', 'DF', 'KC', 'MD', 'FG', 'MBN'.")
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
  eta = stats::predict(obj,type="link")
  ginv_eta = stats::predict(obj,type="response")
  #
  link = stats::family(obj)$link
  #
  sigma2 = stats::sigma(obj)^2
  lambda = lme4::getME(obj,"Lambda")
  R = Matrix::as.matrix(lambda%*%Matrix::t(lambda)*sigma2)
  WB_B <- R
  if (Matrix::isDiagonal(WB_B)) diagB=TRUE else diagB=FALSE
  ##################
  # Robust variance calculation
  ##################
  XtVX = stats::vcov(obj)
  WB_C1 = Matrix::solve(XtVX)
  sum=matrix(0,np,np)
  # start loop over clusters
  for (g in clusternames){
    grp = (cluster == g & nden>0)
    ng = sum(grp)
    if (link == "identity") {
      delta = Matrix::diag(ng)  
      deltainv = delta 
    } else if (link == "logit") {
      term = ginv_eta[grp]*(1-ginv_eta[grp])
      delta = Matrix::diag(term,ng,ng)
      deltainv = Matrix::diag(1/term,ng,ng)
    } else if (link == "log") {
      term = ginv_eta[grp]
      delta = Matrix::diag(term,ng,ng)
      deltainv = Matrix::diag(1/term,ng,ng)
    } else {
      stop("Link ",link," not supported")
    }
    #
    P = deltainv%*%(Y[grp]-ginv_eta[grp]) + eta[grp]
    e = matrix(P - X[grp,]%*%beta,ncol=1)
    ete = e[,,drop=FALSE]%*%Matrix::t(e[,,drop=FALSE])
    #
    Sigma = Matrix::diag(sigma2*stats::family(obj)$variance(ginv_eta[grp])/nden[grp],ng,ng)
    
    # this is diagonal, which is the first term of WB
    
    if (link=="identity") {
      WB_A <- Matrix::diag(1/Matrix::diag(Sigma),ng,ng)
    } else {
      WB_A <- Matrix::diag(1/Matrix::diag(mtx_AD(mtx_DA(deltainv,Sigma),deltainv)),ng,ng)
    }
    
    WB_U <- Z[grp,,drop=FALSE] 
    WB_Ut <- Matrix::t(Z[grp,,drop=FALSE])
    # Compute the inverse
    if (diagB) {
      WB_AUB <- mtx_AD(mtx_DA(WB_A,WB_U),WB_B)
    } else {
      WB_AUB <- mtx_DA(WB_A,WB_U)%*%WB_B
    }
    WB_UtA <- mtx_AD(WB_Ut,WB_A)
    Vinv <- WB_A - WB_AUB%*%Matrix::solve(Matrix::diag(nq) + WB_Ut%*%WB_AUB)%*%WB_UtA
    #    
    if (type1=="MD") {
      WB_U = -Matrix::t(Vinv)%*%X[grp,,drop=FALSE]
      WB_V = Matrix::t(X[grp,,drop=FALSE])
      # Since WB_A is identity, the following expressions are simplified from general Woodbury
      O = WB_C1 + WB_V%*%WB_U
      #      WB_A = Matrix::diag(ng)
      #      FF = WB_A - WB_U%*%MASS::ginv(matrix(as.numeric(O),dim(O)))%*%WB_V
      FF = -(WB_U%*%MASS::ginv(matrix(as.numeric(O),dim(O)))%*%WB_V) 
      Matrix::diag(FF) = Matrix::diag(FF) + 1
      FVX = FF%*%Vinv%*%X[grp,,drop=FALSE]
      sum = sum + Matrix::t(FVX)%*%ete%*%FVX
    } else {
      if (type1=="KC") {
        if (exact) {
          H = X[grp,,drop=FALSE]%*%XtVX%*%Matrix::t(X[grp,,drop=FALSE])%*%Vinv  
          FF = MASS::ginv(expm::sqrtm(Matrix::diag(ng) - Matrix::t(H)))
          if (is.complex(FF)) stop("(I-H_g)^(-1/2) is complex")
          FVX = FF%*%Vinv%*%X[grp,,drop=FALSE]
          sum = sum + Matrix::t(FVX)%*%ete%*%FVX
        } else {
          WB_U = -Matrix::t(Vinv)%*%X[grp,,drop=FALSE]
          WB_V = Matrix::t(X[grp,,drop=FALSE])
          # Since WB_A is identity, the following expressions are simplified from general Woodbury
          O = WB_C1 + WB_V%*%WB_U
          #        WB_A = Matrix::diag(ng)
          #        FF = WB_A - WB_U%*%MASS::ginv(matrix(as.numeric(O),dim(O)))%*%WB_V
          FF = -(WB_U%*%MASS::ginv(matrix(as.numeric(O),dim(O)))%*%WB_V) 
          Matrix::diag(FF) = Matrix::diag(FF) + 1
          VX = Vinv%*%X[grp,,drop=FALSE]
          FVX = FF%*%VX
          term = Matrix::t(FVX)%*%ete%*%VX
          #        sum = sum + (Matrix::t(FVX)%*%ete%*%VX + Matrix::t(VX)%*%ete%*%FVX)/2
          sum = sum + (term + Matrix::t(term))/2
        }
      } else {
        if (type1=="FG") {
          Q = Matrix::t(X[grp,,drop=FALSE])%*%Vinv%*%X[grp,,drop=FALSE]%*%XtVX
          XAA = mtx_AD(X[grp,],Matrix::diag(1/sqrt(1-pmin(r,Matrix::diag(Q))),np,np))
          #      XAA = X[grp,]%*%Matrix::diag(1/sqrt(1-pmin(r,Matrix::diag(Q))))
          sum = sum + Matrix::t(XAA)%*%Vinv%*%ete%*%Vinv%*%XAA
        } else {
          # classic and MBN
          VX = Vinv%*%X[grp,,drop=FALSE]
          sum = sum + Matrix::t(VX)%*%ete%*%VX 
        }}}
  }
  # end loop over clusters
  c = 1
  deltam = 0
  phi = 0
  if (type1=="DF") {
    if (m-np>0) c = m/(m-np) else cat("DF not valid because m-p <= 0; defaulting to classic")
  }
  if (type1=="MBN") {
    f = sum(nden)
    if (DF) {c = (f-1)/(f-np) * (m/(m-1))}
    if (m > (d+1)*np) {deltam = np/(m-np)} else {deltam = 1/d}
    omega = XtVX %*% sum
    evals = Re(eigen(omega,only.values=TRUE)$values)
    if (m > np) {pstar = np} else {pstar = sum(evals>eps)}
    phi =  max(r,sum(evals)/pstar)
  }
  #  
  robustVar = c*XtVX%*%sum%*%XtVX + deltam*phi*XtVX
  robustVar
}
