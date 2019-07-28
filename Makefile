ELPA_DEPENDENCIES=package-lint dash s
ELPA_ARCHIVES=gnu melpa
TEST_ERT_FILES=$(wildcard test/*.el)
LINT_CHECKDOC_FILES=$(wildcard *.el)
LINT_PACKAGE_LINT_FILES=$(wildcard *.el)
LINT_COMPILE_FILES=$(wildcard *.el) $(wildcard test/*.el)

-include makel.mk
