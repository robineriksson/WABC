# Determine package name and version from DESCRIPTION file
PKG_VERSION=$(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)

# Name of built package
PKG_TAR=$(PKG_NAME)_$(PKG_VERSION).tar.gz


# Install package
all: install

install:
	cd .. && R CMD INSTALL $(PKG_NAME)

rcpp:
	R --vanilla -e 'Rcpp::compileAttributes()'
	# touch R/RcppExports.R ## because compileAttributes have is own makefile system
	# touch src/RcppExports.cpp

check:
	cd .. && R CMD CHECK $(PKG_NAME)

# 2) Generate documentation
roxygen:
	rm -f man/*.Rd
	Rscript -e "library(devtools)" \
                -e "devtools::document()"

build: clean
	cd .. && R CMD build --compact-vignettes=both $(PKG_NAME)

clean:
	./cleanup
