# R client to the Rx Studio APIs

This R package provides helper functions to use the [Rx Studio](https://rx.studio) platform right from R -- either for programmatic access (e.g. integrating data sources, PK/PD modeling and reporting), or just for a CLI interface instead of the web GUI provided at https://app.rx.studio

## Authentication

The `simulate` functions requires a free or licensed Rx Studio account to authenticate with the Rx Studio APIs. If you don't have an account yet, please visit https://rx.studio/demo.

To authenticate with your Rx Studio account, you need to generate a long-live access token at https://app.rx.studio/profile/preferences. Please store your token at a secure place, as it provides full access to your Rx Studio account -- including past simulations, and potentially PHI as well! Tokens can be secured by binding to source IP addresses. and unused tokens can (and should) be revoked.

To use an access token in your R session, you need to call `set_access_token` once.

## Simulations

All simulation functions tied to the related Rx Studio API endpoints start with the `simulate` prefix, then the drug, method and PK/PD target names. For example, `simulate_cefepime_mc_emp_fcmin` will use "Empiric therapy" for "Cefepime" targeting "Free minimum concentration".

Example call:

```r
simulate_cefepime_mc_emp_fcmin(
  PATID = "Anonymous",
  AGE = 65,
  HEIGHT = 175,
  WEIGHT = 75,
  GENDER = "Male",
  CREATININE = 1,
  MODEL = "Nicasio et al. (2009) - ICU",
  MIC = 1,
  CMIN = 6,
  LOADINGDOSE = "Yes",
  CRCLCAP = "No cap",
  REGIMENS = list(
    regimen(1000, 8, 1),
    regimen(1000, 12, 1),
    regimen(1000, 24, 1),
    regimen(1250, 8, 1.25),
    regimen(1250, 12, 1.25),
    regimen(1250, 24, 1.25),
    regimen(1500, 8, 1.5),
    regimen(1500, 12, 1.5),
    regimen(1500, 24, 1.5)
  )
)
```

## Helper functions

As the Rx Studio APIs sometimes requires somewhat complex data structures (e.g. when submitting a list of Dosing Regimens for simulations, or specifying Historical Records of a patient, such as past doses or lab results), the following helper functions have been provided to simplify the simulation calls:

- `regimen`
- `dose`
- `concentration`

## Contact

Please feel free to reach out to us at any time either by [email](mailto:team@rx.studio) or by opening a GitHub ticket!
