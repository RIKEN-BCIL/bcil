# Changelog

Repository tags follow the version the scripts report. `bin/hcppipe_qc`,
`bin/hcppipe_gqc` and `bin/bcil_qcsummary.R` are all at 1.2, so this release is
tagged v1.2.0. The only earlier tag, v0.1.1 (2022-09-26), predates that
convention and is not referenced anywhere; 1.0.0 and 1.1.0 were never tagged.

## v1.2.0 (2026-10-18)

First tagged release since v0.1.1 (2022-09-26). Covers 89 commits.

### Changed behaviour — existing outputs change value or meaning

- **`hcppipe_qc` 1.2 — surface defect score.** The robust z for `MyelinMap_BC` is
  now computed after putting the subject's map on the reference's overall level.
  Required for HCP pipelines from the version that introduced
  `global/scripts/MyelinMap_BC.sh`, whose `--use-ind-mean` defaults to YES: the
  reference is rescaled to the individual's mean before subtraction, so
  `MyelinMap_BC` keeps the individual's level rather than moving to the reference
  level. The reference robust SD is only ~0.060, so the residual level difference
  became a z of 8-17, `|z|>3` held over 99-100% of the cortex, and SDS silently
  reported the thickness-only count. Measured: SDS3 9603 -> 185 and 8558 -> 907 on
  two affected datasets; unchanged (332 -> 330) on old-convention data. The factor
  is recorded as `MyelinMap_BC_level_scale` in `SurfaceStats.tsv`.
- **`hcppipe_qc` 1.2 — quartile labels.** The `fslstats` block wrote the quartile
  labels one step high, so `Q2` in `BiasField_acpc_dc_stats.tsv` held the 25th
  percentile while the same name meant the median in every other file of its kind.
  Fixed. **Archived files keep the old labels**; `hcppipe_gqc` reads both (below).
- **`bcil_qcsummary.R` 1.2 — site reliability.** The spreads of the median-polish
  effect vectors are taken with `sd()` instead of `mad()`. The polish already
  protects against a bad session; `mad()` on the ~16-element site-effect vector
  additionally discarded the most deviant devices, which for a scanner coefficient
  discards the signal. It was below `sd()` in 59 of 68 BQM, was exactly 0 in 4
  (leaving `G_crossed` undefined), and moved 25 of 68 verdicts. Agreement with the
  measured split-half reproducibility improves from +0.67 to +0.84. The `mad()`
  values remain as `ICC_mad` / `G_crossed_mad`.

### Added

- **`bcil_qcsummary.R` — the two QC decisions.** A BQM good for excluding
  individuals is not thereby good for evaluating scanners. Each BQM is decomposed
  by robust two-way median polish on the incomplete subject x site table and
  reported as `ICC` (individuals), `ICC_site`, `n_eff` (effective travellers per
  site, Schur complement of the incidence matrix), `G_nested` and `G_crossed`
  (sites). All four use one sigma_e, taken from repeated sessions of the same
  subject at the same site. Outputs: `bqm_site_bias.tsv`, `bqm_icc.png`,
  `bqm_site_reliability.png`, `bqm_cluster_dendro.png`.
- **`bcil_qcsummary.R` — two cautions on `G_crossed`.** `site_conc_top2` gives the
  share of the site sum-of-squares in the two most deviant sites, and
  `G_crossed_1model` recomputes the coefficient inside the largest single-model
  group of sites. A high `G_crossed` means the site means are reproducible, not
  that the sites are spread evenly.
- **`hcppipe_qc` — eight structural varsets and SDS3** added to `struct_varsets`
  (B0 fieldmap `PhaseDiffAbs`/`Max`/`Gradient`/`Q2`, `BiasField_acpc_dc` `Min`/
  `Q3`/`Max`, myelin bias field terms).
- **`hcppipe_qc`** aborts early with a clear message when `FSLDIR` or `CARET7DIR`
  is unset, instead of failing deep inside with "unbound variable" under `set -u`.
- **`hcppipe_gqc` — quartile key compatibility.** `_tsv_val` matches a requested
  key exactly, then as `(KEY)`, `_KEY`, a prefix and a substring, and shifts the
  requested `Qn` by one when a file has `Q4` but no `Q1`, so archives written under
  either labelling convention read correctly.
- **`bcil_qcsummary.R`** now records its version in the generated report footer.

### Notes

- `hcppipe_qc` and `hcppipe_gqc` report their version in `--help`, in the HTML
  report footer and in `qc_metrics.json` (`meta.version`). Check that field when
  comparing SDS values across runs.
