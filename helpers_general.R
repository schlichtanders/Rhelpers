library(devtools)
source_url("https://raw.githubusercontent.com/schlichtanders/Rhelpers/master/helpers_idx.R") #rotation
source_url("https://raw.githubusercontent.com/schlichtanders/Rhelpers/master/helpers_functional.R") # for argsenv

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
            dim(X) <- NULL
        return(X)
    }
    Map(single_meshgrid, args, 1:N)
}

meshgrid2 = function(...){
    args = argsenv(...)
    # first extract additional parameters:
    i_flatten = match("flatten", attr(args, 'formalnames'))
    if(is.na(i_flatten)){
        flatten = TRUE
        tagnames = attr(args, 'tagnames')
        formalnames = attr(args, 'formalnames')
    } else {
        flatten = args[[ attr(args, 'tagnames')[i_flatten] ]]
        tagnames = attr(args, 'tagnames')[-i_flatten]
        formalnames = attr(args, 'formalnames')[-i_flatten]
    }

    # then proceed
    N = length(tagnames)
    mylength = function(t) length(args[[t]])
    nargs = mapply(mylength, tagnames)
    single_meshgrid = function(i, t){
        Nrep = prod(nargs[names(nargs)!=t])
        rot = rotation(N, i-1)
        X = aperm(array(rep(args[[t]], Nrep), nargs[rot]), rot)
        if (flatten)
            dim(X) <- NULL
        return(X)
    }
    l = Map(single_meshgrid, 1:N, tagnames)

    if(all( formalnames != ""))
        names(l)<-formalnames
    return(l)

}
