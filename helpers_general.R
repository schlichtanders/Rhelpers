library(devtools)

rotation = function(x,n, sign=1) ((0:(x-1) - sign* n) %% x) + 1

str = function(o) capture.output(print(o))

funcstr = Vectorize(function(f) format(f)[2])  # function defined by function() will be list of "function(..)" and "function expression"

factorfun = function(data) factor(funcstr(data))


meshgrid = function(...){
    args = list(...)
    # first extract additional parameters:
    flatten = args$flatten
    if (is.null(flatten)) flatten = TRUE
    else args$flatten <- NULL

    # then proceed
    N = length(args)
    nargs = mapply(length, args)
    single_meshgrid = function(o, i){
        Nrep = prod(nargs[-i])
        rot = rotation(N, i-1)
        X = aperm(array(rep(o, Nrep), nargs[rot]), rot)
        if (flatten)
            dim(X) <- prod(nargs) # formerly NULL, which seems to be the official version, however as.data.frame cannot convert lists of functions of they do not have a dimension (while numbers work fine)
        return(X)
    }
    Map(single_meshgrid, args, 1:N)
}



attach.all <- function (x, overwrite = NA, name = "attach.all")  {
    rem <- names(x) %in% ls(.GlobalEnv)
    if (!any(rem)) overwrite <- FALSE
    rem <- names(x)[rem]
    if (is.na(overwrite)) {
        question <- paste("The following objects in .GlobalEnv will mask\nobjects in the attached database:\n", paste(rem, collapse = ", "), "\nRemove these objects from .GlobalEnv?", sep = "")
        if (interactive()) {
            if (.Platform$OS.type == "windows")  overwrite <- "YES" == winDialog(type = "yesno",  question)
            else overwrite <- 1 == menu(c("YES", "NO"), graphics = FALSE, title = question)
        }
        else overwrite <- FALSE
    }
    if (overwrite) remove(list = rem, envir = .GlobalEnv)
    attach(x, name = name)
}
