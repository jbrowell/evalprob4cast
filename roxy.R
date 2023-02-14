
library(devtools)
library(roxygen2)

# Update package documentation
document(pkg = ".")

# Install from local repository
install(".")
library(IEAwind51RP)


