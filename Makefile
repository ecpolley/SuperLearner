# Run CRAN-specific tests.
cran:
  # Set environmental variable before running.
	SL_CRAN=true Rscript tests/cran/revdep.R

clean:
	rm -rf revdep revdep.Rout