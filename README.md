# LibeRary

LibeRary is the pharmacometric model and literature repository for the LibeR
ecosystem. It combines a curated, versioned catalogue with an autonomous,
auditable publication-ingestion workflow:

LibeRary is distributed as part of the LibeR 0.9 research beta. Use the
[ecosystem installer](../docs/INSTALL.md) and consult
`LibeRation::liber_support_matrix("LibeRary")`; machine-extracted records
remain evidence-linked research candidates until reviewed.

1. Search PubMed and preserve a reproducible search snapshot.
2. Triage titles and abstracts into High, Intermediate, or Low model
   probability. High and Intermediate enter the first pass; Low is retained as
   a durable backlog for later processing.
3. Acquire PDFs through open-access sources or the user's institutional route.
4. Create a content-addressed document bundle with the original PDF, Docling
   standard-pipeline JSON/Markdown/HTML, parser provenance, and selected raw
   PDF page images. A degraded `pdftools` fallback is recorded explicitly.
5. Run an evidence-led parsed-text investigation: map all discussed models,
   search structure, fixed effects, variability, observation model, population,
   dosing, and reproduction evidence separately; challenge the claims; search
   unresolved gaps; and synthesize only from the resulting evidence ledger.
6. Independently extract and verify material claims from original PDF pages.
7. Compare model, population, dosing, and digitized figure/table claims field by
   field and use a third configured LLM to adjudicate discrepancies from the
   source evidence.
8. Represent the canonical compartments, input, elimination, and parameterization;
   retain reported ADVAN/TRANS or infer common implementations with explicit
   confidence, rationale, alternatives, and review status.
9. Publish recoverable models as versioned, reviewable NONMEM control streams
   that are compiled through LibeRation before use.
10. Prepare a conservative LibeRation reproduction plan and, when sufficiently
   evidenced, simulate the reported regimen against extracted concentration data.

Every LLM role is selectable: abstract triage, investigation/synthesis, PDF
vision verification, skeptical evidence review, and adjudication. Audits retain provider, exact
model, prompt/schema version, content hashes, token usage, timing, evidence
locators, reconciliation, and adjudication decisions.

`machine_consistent` and `machine_adjudicated` are machine qualification
states—not claims of human validation. Human attention is reserved for
unresolved major model fields and any later qualification work.

## Persistent storage

Catalogue data and settings are never stored in the installed package, so they
survive reinstalls:

- Windows: `C:/Users/<username>/Documents/LibeR/library`
- Linux/macOS: `~/LibeR/library`

Set `LIBERARY_HOME` or `options(LibeRary.catalog=...)` to override this.

## Browse and ingest

```r
library(LibeRary)

library_shiny()                 # catalogue browser
library_shiny(mode = "ingest") # autonomous literature pipeline
library_shiny(mode = "reference") # systematic-review comparison and curation

library_search("theophylline", status = "validated")
library_get("lib_theo_synthetic")
library_provenance("lib_theo_synthetic")
```

The LibeRation GUI includes a **Model library** popup. An imported catalogue
entry becomes a normal model version with immutable evidence and qualification
provenance.

## Configure models and Docling

Ollama is the local default. OpenAI and OpenAI-compatible endpoints are also
supported. The GUI discovers models from each configured endpoint instead of
depending on hard-coded model names.

```r
cfg <- ingest_load_config()

cfg$llm$triage <- list(provider = "ollama", model = "triage-model", temperature = 0)
cfg$llm$indexing <- list(provider = "ollama", model = "text-model", temperature = 0)
cfg$llm$vision <- list(provider = "ollama", model = "vision-model", temperature = 0)
cfg$llm$adjudication <- list(provider = "ollama", model = "adjudicator-model", temperature = 0)

cfg$deliberative$enabled <- TRUE
cfg$deliberative$visual_verification <- TRUE
cfg$deliberative$cache_stages <- TRUE
cfg$deliberative$max_gap_rounds <- 1
# Final schema synthesis gets a larger context than the investigative calls.
cfg$deliberative$synthesis_num_ctx <- 32768
cfg$deliberative$synthesis_num_predict <- 16384
# Image tokens also receive a stage-specific context budget.
cfg$deliberative$visual_num_ctx <- 32768
cfg$deliberative$visual_num_predict <- 12288

cfg$docling$executable <- "docling"
cfg$triage$high_threshold <- 0.70
cfg$triage$intermediate_threshold <- 0.30
cfg$reproduction$auto_run <- FALSE # review the plan before execution
library_save_config(cfg)
```

Each role also accepts an `instruction` override. In the ingestion GUI, the
edit button beside each provider/model pair opens a full instruction editor.
Saving persists the override in `config.yml`; **Restore default** removes it so
future package prompt improvements apply automatically.

Use `library_llm_models(provider, cfg, role)` to inspect available choices and
`ingest_docling_available(cfg)` to inspect the parser. Set
`llm.require_independent_extraction_models: true` when the text and vision
lanes must use different provider/model combinations. Even with the same model,
the evidence representations remain separate; the audit warns that model errors
may still be correlated.

API keys are read from environment variables and removed by
`library_save_config()`. Remote content transfer is an explicit operational
choice: set `llm.allow_remote_content: true` before sending PDFs or parsed text
to a non-local provider.

The ingestion GUI saves non-secret settings automatically before every job,
including the NCBI/Unpaywall email, request rate, data directory, thresholds,
providers, and selected models. NCBI requires `tool` and `email` on E-utility
requests. An API key is separate and optional at three requests/second or less;
set `ENTREZ_KEY` for persistent use or enter it in the GUI for the current
session. Keys are deliberately never written to `config.yml`.

## Scripted autonomous workflow

```r
cfg <- ingest_load_config()
cfg$entrez$email <- "you@example.org"

found <- ingest_discover(limit = 100, cfg = cfg)

# High + Intermediate are the configured default first pass.
fetched <- ingest_fetch_institutional(found$manifest_path, cfg = cfg)
processed <- ingest_process_batch(
  found$manifest_path,
  cfg = cfg,
  tiers = c("high", "intermediate"),
  resume = TRUE,
  adjudicate = TRUE
)

# Deliberately process the retained Low backlog later.
low_fetched <- ingest_fetch_institutional(
  found$manifest_path, cfg = cfg,
  classes = "deferred_low", tiers = "low"
)
low_processed <- ingest_process_batch(found$manifest_path, cfg = cfg, tiers = "low")
```

`resume = TRUE` does **not** redo a decision made with the same source hash,
pipeline, and prompt version. Each completed investigation stage is also
content-addressed, so an interrupted article resumes without repeating valid
LLM work. Review-required decisions are terminal too; use `resume = FALSE` when
you deliberately want to revisit one after changing evidence or instructions.
Older one-shot decisions are automatically reprocessed by the current
deliberative pipeline. Set `resume = FALSE` to force deliberate re-extraction;
a replacement catalogue entry is versioned and the prior artefact is retained.

Generated NONMEM review drafts preserve the reported variability metric and
convert it explicitly. For an exponential/log-normal random effect,
distributional CV fraction `c` becomes `OMEGA = log(1 + c^2)`; `c^2` is used
only when the source explicitly defines the approximation
`CV% = 100 * sqrt(OMEGA)`. Additive normal ETAs use absolute variance, residual
CVs become SIGMA variances, and reported correlations/covariances are retained
in block OMEGA structures. Unresolved scales remain review-required rather than
being copied silently.

## Model semantics, populations, and article reproduction

Each extraction retains the original population text while adding any number
of cohorts. Each cohort can hold multiple descriptors and multiple simultaneous
summary-statistic forms—for example mean/SD plus range—alongside sex/category
counts, organ-function descriptors, and CYP genotype or phenotype evidence.
Dosing regimens retain their raw source text, numerical amount, unit, route,
interval, duration, repetitions, steady-state flag, and evidence locator.

ADVAN/TRANS is not guessed directly from a prose label. LibeRary first records
the canonical compartment count, input process, elimination, and parameterization.
Common linear one-, two-, and three-compartment structures can then map to
ADVAN1/2/3/4/11/12 and a compatible TRANS. Nonlinear, incomplete, or ambiguous
structures remain unresolved with ADVAN6/13 alternatives for review.

```r
enriched <- library_model_enrich(extraction)
plan <- library_reproduction_plan(enriched)

plan$eligible  # FALSE when required evidence is missing
plan$blockers  # machine-readable reasons; no silent dose/default assumptions

if (plan$eligible) {
  result <- library_reproduction_run(
    plan, output_dir = "reproduction/article-123",
    nsim = 200, n_cores = 4
  )
}
```

The plan blocks unsafe unit conversions, weight/BSA-normalized doses without a
representative covariate, steady-state dosing without an interval, infusions
without a duration, unresolved implementations, compilation failures, and
generated parameter defaults unless deliberately allowed. If concentration
points were extracted from figures or tables, the result includes pointwise
comparisons, NRMSE, relative error, interval coverage, and a saved plot.
Agreement is evidence that the extracted model can reproduce the reported
profile under stated assumptions; it is not independent validation of the
publication or of LibeRation.

`ingest_extract_batch()` remains available for scripts using the original
single-text-lane workflow. New work should use `ingest_process_batch()`.

## LibeRties queues

The same typed, data-only job envelope supports `triage`, `parse`, `index`,
`dual_extract`, `assess`, and `adjudicate`. Credentials are resolved only in the
worker environment. PDF/text transfer requires an explicit confirmation.

```r
job <- library_job(
  "dual_extract",
  metadata = metadata,
  pdf_path = "article.pdf",
  confirm_transfer = TRUE,
  cfg = cfg
)
job_id <- queue$submit(job)
```

Workers return results; they do not silently mutate a client catalogue.

## Independent reference corpora and adapter training

Systematic-review appendices can be converted into a reference corpus that is
physically separate from the live catalogue. The corpus retains each appendix
row verbatim alongside normalized LibeRary extraction targets, source hashes,
quality tiers, and PMID-level train/validation/test partitions.

```r
reference <- "validation/liberary/aed-pkpd-reference/0.2.2"

library_reference_build(
  "AED_PKPD", reference,
  version = "0.2.2"
)
library_reference_validate(reference, check_hashes = TRUE,
                           source_dir = "AED_PKPD")

# The runner sees source PDFs but never the appendix target.
library_reference_run(
  reference, "AED_PKPD", "benchmark/current",
  cfg = cfg, partition = "test", task = "extraction"
)
library_reference_benchmark(
  reference, "benchmark/current/predictions",
  task = "extraction", partition = "test",
  output_dir = "benchmark/current/report"
)
```

The reference-review GUI can also be launched directly. It loads the model
index once and reads full reference/prediction JSON only when a model is
selected. The three source representations are shown side by side, with a
leaf-level difference table, numeric percent deltas, stored text/vision/
reconciled variants, and an action to open the original publication PDF.

```r
library_reference_shiny(
  corpus = "validation/liberary/aed-pkpd-reference/0.2.2",
  predictions = "validation/liberary/aed-pkpd-benchmark/text-current/predictions",
  source_dir = "AED_PKPD"
)
```

Field decisions remain session-local until **Create successor version** is
used. The GUI writes separate model- and field-decision audit files and calls
`library_reference_revise()` to create a new corpus directory. The source
corpus and its `raw` appendix transcription are never edited. Test-partition
records remain ineligible for training even after review.

Initial appendix records are C/D silver-tier and cannot enter the default
training export. Review decisions create a successor corpus version; the source
version remains unchanged. Test records may become strict benchmark targets but
can never become training-eligible.

```r
library_reference_revise(
  reference, "validation/liberary/aed-pkpd-reference/0.2.0",
  decisions = "review-decisions.csv",
  version = "0.2.0", curator = "reviewer-id"
)

library_reference_training_export(
  "validation/liberary/aed-pkpd-reference/0.2.0",
  "AED_PKPD", "training/aed-pkpd",
  partitions = c("train", "validation")
)
```

`library_reference_training_files()` locates the packaged Python trainer and
requirements file. The trainer validates the leakage guard before importing
the optional Hugging Face/PEFT/TRL stack and can train a LoRA or QLoRA adapter.
The exact base-model identity is recorded because an adapter must never be
applied to a different base model.

## Provenance and use

- Source PDFs, content hashes, acquisition paths, parser versions, fallbacks,
  and evidence locators are retained in document bundles and audits.
- Acquisition and extraction are not blocked by a reuse-rights classifier.
  Provenance is still retained so downstream users can apply their own policy.
- LibeRary does not circumvent publisher authentication or access controls.
- Generated control-stream code and defaults remain labelled suggestions.
- Simulation, estimation, predictive checks, and any required scientific or
  regulatory qualification remain separate downstream activities.

## AI-assisted development

GPT-5.6 was used as an AI engineering collaborator to help implement and review
the ingestion pipeline, evidence schemas, model-extraction workflows, GUI, tests, and documentation.
Scientific direction, curation policy, validation criteria, and release decisions remain the responsibility of the project owner.
