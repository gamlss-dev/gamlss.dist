
# Getting current package version from DESCRIPTION file
VERSION := $(shell grep '^Version:' DESCRIPTION | awk '{print $$2}')

.PHONY: test
test:
	Rscript -e "library('gamlss.dist'); tinytest::test_all()"

.PHONY: install
install:
	@echo Installing current version: $(VERSION)
	(cd ../ && \
		R CMD build --no-build-vignettes gamlss.dist && \
		R CMD INSTALL gamlss.dist_$(VERSION).tar.gz)

# Reto: Let me know how we can write this in a multi-line command in cmake
.PHONY: coverage
coverage:
	#Rscript -e 'covr::package_coverage(type = "none", quiet = FALSE, file  = "_coverage.html")'
	##Rscript -e 'covr::package_coverage(runs = "tests", quiet = FALSE, file  = "_coverage.html")'
	#Rscript -e 'covr::package_coverage(runs = "tests", line_exclusions = list("R/*", "src/*", "chm/*"), quiet = FALSE, file  = "_coverage.html")'
	Rscript -e 'covr::report(covr::package_coverage(type = "none", quiet = FALSE, file  = "_coverage.html"))'

