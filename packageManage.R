packagesFile = "./data/packages.RData"

packages.save <- function() {
    packages <- as.data.frame(installed.packages())
    packages$LibPath <- NULL
    save(packages, file = packagesFile)
}

packages.restore <- function() {
    load(file = packagesFile)
    install.packages(as.character(packages$Package))
}

packages.save()