# minair

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![license](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-brightgreen.svg?style=flat)](https://www.gnu.org/licenses/gpl-3.0.html)
[![R-CMD-check](https://github.com/umg-minai/minair/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/umg-minai/minair/actions/workflows/R-CMD-check.yaml)
[![codecov.io](https://img.shields.io/codecov/c/github/umg-minai/minair.svg?branch=main)](https://codecov.io/github/umg-minai/minair/?branch=main)
<!-- badges: end -->

R utilities and functions used in the Medizinische Informatik working group at the Klinik für Anästhesie, Universitätsmedizin Greifswald.

## Bootstrap

We assume you have `guix` installed to generate an isolated developement
environment in the following way:

```bash
git clone https://github.com/umg-minai/minair.git
cd minair
echo "$(pwd)" >> ~/.config/guix/shell-authorized-directories
guix shell
```
