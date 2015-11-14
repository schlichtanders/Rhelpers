argsenv <- function(..., parent=parent.frame()) {
  cl <- match.call(expand.dots=TRUE)

  e <- new.env(parent=parent)
  pf <- parent.frame()
  JJ <- seq_len(length(cl) - 1)
  tagnames <- sprintf(".v%d", JJ)
  for (i in JJ) e[[tagnames[i]]] <- eval(cl[[i+1]], envir=pf)

  attr(e, "tagnames") <- tagnames
  attr(e, "formalnames") <- names(cl)[-1]
  class(e) <- c("environment", "argsenv")
  e
}


mapply2 <- function(FUN, ..., MoreArgs=NULL, SIMPLIFY=TRUE, USE.NAMES=TRUE) {
  FUN <- match.fun(FUN)

  args <- argsenv(...)
  tags <- attr(args, "tagnames")
  iexpr <- quote(.v1[[i]])
  iargs <- lapply(tags, function(x) { iexpr[[2]] <- as.name(x); iexpr })
  names(iargs) <- attr(args, "formalnames")
  iargs <- c(iargs, as.name("..."))
  icall <- quote(function(i, ...) FUN())[-4]
  icall[[3]] <- as.call(c(quote(FUN), iargs))
  ifun <- eval(icall, envir=args)

  lens <- sapply(tags, function(x) length(args[[x]]))
  maxlen <- if (length(lens) == 0) 0 else max(lens)
  if (any(lens != maxlen)) stop("Unequal lengths; recycle not implemented")

  answer <- do.call(lapply, c(list(seq_len(maxlen), ifun), MoreArgs))

  # The rest is from the original mapply code.

  if (USE.NAMES && length(tags)) {
    arg1 <- args[[tags[1L]]]
    if (is.null(names1 <- names(arg1)) && is.character(arg1)) names(answer) <- arg1
    else if (!is.null(names1)) names(answer) <- names1
  }

  if (!identical(SIMPLIFY, FALSE) && length(answer))
      simplify2array(answer, higher = (SIMPLIFY == "array"))
  else answer
}


# Original Map code, but calling mapply2 instead.
Map2 <- function (f, ...) {
  f <- match.fun(f)
  mapply2(FUN=f, ..., SIMPLIFY=FALSE)
}
