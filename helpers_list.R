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
