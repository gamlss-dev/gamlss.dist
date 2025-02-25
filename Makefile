
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
	Rscript -e "covr::report(covr::package_coverage(type = \"tests\", line_exclusions = list('src/gamlss.dist_init.c'), quiet = FALSE), file = \"_coverage.html\")"

.PHONY: cov2
cov2:
	Rscript -e 'options(covr.debug = TRUE); covr::package_coverage(runs = "inst/tests/test_PO.R")'

.PHONY: check clean
clean:
	-rm src/*.gcda src/*.gcno src/*.o src/*.so
check: clean
	@echo Checking current version: $(VERSION)
	(cd ../ && \
		R CMD build --no-build-vignettes gamlss.dist && \
		R CMD check --as-cran gamlss.dist_$(VERSION).tar.gz)

