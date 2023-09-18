
using <- function(...) {
    libs < -unlist(list(...))
    req <- unlist(lapply(libs, require, character.only = TRUE))
    need <- libs[req == FALSE]
    if (length(need) > 0) { 
        install.packages(need)
        lapply(need, require, character.only = TRUE)
    }
}

using_bioconductor <- function(...) {
    libs < -unlist(list(...))
    req <- unlist(lapply(libs, require, character.only = TRUE))
    need <- libs[req == FALSE]
    if (length(need) > 0) { 
        BiocManager::install(need)
        lapply(need, require, character.only = TRUE)
    }
}

using_github <- function(...) {
    libs < -unlist(list(...))
    req <- unlist(lapply(libs, require, character.only = TRUE))
    need <- libs[req == FALSE]
    if (length(need) > 0) { 
        devtools::install_github(need)
        lapply(need, require, character.only = TRUE)
    }
}