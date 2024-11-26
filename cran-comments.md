## R Package Submission: greta.censored 0.1.0

### Summary

This is the initial release of the `greta.censored` package, which provides additional censored distributions for use with Greta, a probabilistic programming framework for Bayesian modeling.

### Package Checks

- The package has been checked using `R CMD check` on the following platforms:
  - Local machine (macOS 12.0.1, R 4.1.2)
  - GitHub Actions (Ubuntu 18.04, macOS-latest, Windows-latest)
  - R-hub (Windows Server 2022, Ubuntu Linux 20.04, Fedora Linux 34)

All checks passed without errors or warnings.

### Test Coverage

- The package includes comprehensive unit tests for all functions.
- Test coverage is reported using `covr` and is available on Codecov.

### Dependencies

- The package depends on the following CRAN packages:
  - `greta (>= 0.4.2)`
  - `R (>= 3.1.0)`
  - `cli`
  - `glue`
  - `tensorflow (>= 1.14.0)`

### Additional Information

- The package includes a `README.md` with installation instructions and usage examples.
- A `CODE_OF_CONDUCT.md` is included to ensure a welcoming community.
- Continuous integration is set up with GitHub Actions for automated testing and code coverage.
- A `CONTRIBUTING.md` is included to guide new contributors.

### Maintainer

- Mlen-Too Wesley <mlen.too.wesley@gmail.com>

Thank you for considering the `greta.censored` package for CRAN.
