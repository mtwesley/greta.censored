## Resubmission: greta.censored 0.1.0

### Summary

This is the initial submission of the 'greta.censored' package, which provides censored distributions for use with 'greta', a probabilistic programming framework for Bayesian modeling.

### Changes Made in Response to CRAN Feedback

1. **Installation Errors**

   - Added checks to ensure that 'TensorFlow' and 'TensorFlow Probability' are available before use. If unavailable, the package now issues a warning and gracefully fails instead of raising an error.
   - Tested installation in environments without 'TensorFlow' to confirm graceful degradation.

2. **Invalid URLs**

   - Updated the Codecov URL in `README.md` to `https://app.codecov.io/gh/mtwesley/greta.censored?branch=main`.
   - Fixed or removed the broken Code of Conduct URL. If unavailable, a relative link to `CODE_OF_CONDUCT.md` is now used.

3. **Title Field in DESCRIPTION**

   - Updated the Title field in the `DESCRIPTION` file preserving the lowercase for 'greta' as used in its official documentation.

4. **Spelling Issues**

   - Confirmed that flagged terms are intentional and correct. Added these to the WORDLIST file for consistency with future checks.

5. **Namespace and Dependencies**

   - Explicitly declared all necessary imports in the `NAMESPACE` file.
   - Removed unused dependencies from the `DESCRIPTION` file.

6. **Global Variable**

   - Fixed undefined use of variables mentioned in a NOTE relating to their use in distribution functions.

7. **References in DESCRIPTION**

   - Added a reference to 'greta' using the correct citation:
     `Golding (2019) <doi:10.21105/joss.01601>`.

### Skipped Tests

Some tests are conditionally skipped during CRAN checks because they require 'TensorFlow' or 'TensorFlow Probability', which may not be available in all environments (e.g., CRAN's test servers). These tests have been thoroughly verified in environments where the required Python modules are available. Skipped tests include:

- `test-beta_censored.R`
- `test-exponential_censored.R`
- `test-gamma_censored.R`
- `test-lognormal_censored.R`
- `test-normal_censored.R`
- `test-pareto_censored.R`
- `test-student_censored.R`
- `test-weibull_censored.R`

All skipped tests pass successfully on systems with the required external libraries installed. The package also provides clear error messages and instructions when dependencies are missing.

### Notes on External Dependencies

The package depends on Python modules 'tensorflow' and 'tensorflow_probability'. While these dependencies are not installed by the package, we provide clear error messages and instructions for users to install them manually. All functionality gracefully degrades when these dependencies are unavailable.

To mitigate potential issues, the `SystemRequirements` field in the `DESCRIPTION` file lists 'Python' and 'TensorFlow' modules as required external software.

### Package Checks

The package has been checked using `R CMD check` with `--as-cran` on the following platforms:

1. **Local Environment**

   - macOS 15.0, R 4.3.2
   - Ubuntu 22.04 (Docker), R 4.3.3
   - Windows 10, R-devel

2. **Continuous Integration**

   - GitHub Actions:
     - Ubuntu-latest
     - macOS-latest
     - Windows-latest

3. **R-hub**
   - Windows Server 2022, R-devel
   - Ubuntu Linux 20.04, R-devel
   - Fedora Linux 34, R-devel

All checks passed without errors or warnings. Notes have been addressed as described above.

### Test Coverage

- The package includes comprehensive unit tests for all core functions.
- Test coverage is reported using 'covr' and available on Codecov at:
  https://app.codecov.io/gh/mtwesley/greta.censored?branch=main

### Additional Information

- The package includes a `README.md` with installation instructions and usage examples.
- A `CODE_OF_CONDUCT.md` is included to ensure a welcoming community.
- Continuous integration is set up with GitHub Actions for automated testing and code coverage.
- A `CONTRIBUTING.md` is included to guide new contributors.

### Maintainer

- Mlen-Too Wesley <mlen.too.wesley@gmail.com>

Thank you for considering the 'greta.censored' package for CRAN.
