# LibeRary 0.7.5

- Publishes LibeRary in the LibeR 0.9 research-beta compatibility set and
  explicitly distinguishes verified catalogue behavior from experimental
  machine-assisted extraction and human-curated evidence.

# LibeRary 0.7.4

- Restores the established high-resolution LibeR dove and visibly harmonises
  the catalogue, ingestion, and reference-review shells, panels, and controls.
- Aligns catalogue, ingestion, and reference-review headers, theme
  persistence, package-version labels, focus indicators, and transparent forest
  green dove assets.
- Groups ingestion settings into collapsible connection, model, and runtime
  sections so the working pipeline remains visible on smaller displays.
- Makes catalogue, ingestion, and reference-review launchers consistently
  return a Shiny application when `launch.browser = NULL`.

# LibeRary 0.7.3

- Adds a fail-closed reference-corpus release gate that admits only independent
  Tier A/B evidence and never treats machine-generated silver data as gold.
- Adds browser startup coverage and makes the catalogue GUI return a Shiny app
  without launching a browser when requested.

# LibeRary 0.7.2

- Quarantines automated catalogue output: ingestion can publish only
  stub/draft/review entries and cannot label machine output as validated.
- Adds a deterministic computational qualification gate covering catalogue
  schema, evidence presence, extraction confidence, mapping review state,
  strict control-stream compilation, and finite residual-free simulation.
  Promotion to validated requires both this gate and explicit human review.
- Exposes qualification status and blockers in catalogue listings, while
  making clear that computational qualification is not scientific or clinical
  validation.

# LibeRary 0.7.0

- Replaced the default one-shot parsed-text extraction with a resumable,
  evidence-led investigation: model reconnaissance; six focused fact-finding
  domains; skeptical falsification; targeted gap searches; optional PDF-page
  verification; deterministic completeness/consistency gates; and final
  evidence-constrained synthesis.
- Added content-addressed per-stage caches and a versioned evidence ledger with
  source locators, claim status, dependencies, contradictions, open questions,
  and audit/runtime metadata. Published catalogue versions preserve a copy of
  the ledger and expose its readiness summary in both LibeRary GUIs.
- Made resume decisions pipeline- and prompt-version aware, so older one-shot
  catalogue records are re-investigated while interrupted current work resumes
  at the last valid stage.
- Added evidence-aware canonical model semantics and conservative ADVAN/TRANS
  inference for common linear one-, two-, and three-compartment models. Every
  mapping is labelled reported, inferred, or unresolved with confidence,
  rationale, alternatives, and review status.
- Added structured multi-cohort population descriptors with preserved summary
  statistics, including age, weight, height, BMI, categorical distributions,
  and CYP genotype/phenotype evidence.
- Added structured dosing and figure/table concentration targets to the text,
  vision, reconciliation, adjudication, catalogue, and reference-corpus paths.
- Added auditable article-reproduction plans and LibeRation-backed simulations.
  Missing dose units, steady-state intervals, infusion durations, unresolved
  implementations, and generated defaults block execution rather than being
  guessed. Digitized targets can be scored and plotted, with an explicit
  distinction between computational reproduction and independent validation.
- Added an enriched AED PK/PD reference corpus schema and version 0.2.2 corpus
  containing 182 models across protected train/validation/test partitions.
- Refined all three LibeRary applications with a calmer forest-green light and
  dark palette and added implementation, population, and reproduction summaries.
- Brightened the dark theme with layered forest-green surfaces, lighter borders,
  and a more vivid mint accent while preserving comfortable contrast.
- Added a Stop current job control to the ingestion GUI. It terminates the
  supervised worker and its child processes, records a distinct cancelled state,
  preserves partial progress and logs, and refreshes any partial catalogue output.

# LibeRary 0.6.1

- Added the systematic-review reference comparison and curation GUI.
- Made installed Shiny applications resolve package helpers directly from the
  namespace, avoiding false "not an exported object" failures when an updated
  package is installed while an older LibeRary namespace remains attached.
- Fixed the reference GUI mistaking an installed package's lazy-load database
  for an R source checkout, which caused `devtools::load_all()` to replace the
  working namespace with an empty one and report every export as missing.
- Exported the complete reference-corpus API, including validation, listing,
  comparison, revision, benchmarking, and training-data helpers.

# LibeRary 0.6.0

- Compacted each ingestion-stage provider/model selector into one row and added
  a persistent instruction editor with save and restore-default actions.
- Made the ingestion catalogue refresh after completed processing jobs and
  added the full selected-entry detail view and source-PDF action to both GUIs.
- Clarified resume semantics: completed decisions and catalogue entries are
  reused when enabled; disabling resume deliberately reprocesses selected
  articles and versions any replacement catalogue entry.
- Hardened variability extraction and control-stream generation: reported
  OMEGA/SIGMA metrics are retained, CV/SD values are converted to NONMEM
  variances, normal versus log-normal ETA forms are preserved, and correlations
  or covariances can be rendered as `$OMEGA BLOCK` records.
- Added friendly green branding to both LibeRary GUIs and their transparent
  favicons.
- Made GUI settings persistent before every job while keeping API keys out of
  the YAML configuration.
- Fixed installed-package background jobs being mistaken for source trees and
  added terminal-state monitoring so bootstrap failures are shown immediately
  instead of leaving Discover at `Starting...` indefinitely.
- Prevented the completed-job progress poller from invalidating itself in a
  tight loop, which had caused high CPU use and sluggish subsequent sessions.
- Normalized unambiguous LLM percentage confidences to 0-1 proportions while
  continuing to reject invalid values instead of silently clamping them.
- Added Ollama capability discovery so the vision-role selector excludes
  text-only models, and made unavailable extraction lanes explicit in job logs.
- Prevented percentage-like variability estimates and surplus structural
  parameters from being silently written into generated NONMEM review drafts.
- Added probability-based abstract triage with a High/Intermediate first pass
  and a durable Low-probability backlog that is never silently discarded.
- Added content-addressed document bundles using Docling's standard pipeline,
  provenance-rich `pdftools` fallback, and selected original-PDF page images.
- Added independent parsed-text and vision extraction lanes, field-level model
  reconciliation, third-model adjudication, and machine-specific qualification
  states that are not confused with human validation.
- Added separate selectable LLM roles for triage, text extraction, PDF vision,
  assessment, and adjudication in configuration and the ingestion GUI.
- Added resumable end-to-end batch processing and typed LibeRties jobs for all
  literature pipeline stages.

- Unified the catalogue and literature-ingestion prototypes into one package.
- Added selectable Ollama, OpenAI, and OpenAI-compatible providers with live
  model discovery and separate indexing/assessment roles.
- Added conservative independent evidence assessment, prompt/model/usage audit
  records, privacy gates, resumable batches, and human review workflow.
- Moved catalogue/configuration state out of installed package directories.
- Added atomic catalogue writes, stable ids, schema validation, version history,
  BibTeX generation, and generated-default labelling.
- Corrected packaged control streams to declare ADVAN/TRANS in `$SUBROUTINES`
  and validate all mapped control streams with LibeRation.
- Added a LibeRation catalogue popup and immutable model-version provenance.
- Added typed LibeRties `library_index` and `library_assess` queue jobs.
## AED-PKPD reference corpus

- Added a dedicated reference-review GUI with searchable model selection,
  side-by-side historical appendix, normalized target, and LibeRary extraction
  panels; text/vision/reconciled variant switching; source-PDF access;
  field-level differences and numeric deltas; and light/dark green branding.
- Added auditable field decisions and immutable successor-corpus creation.
  Accepted LibeRary or custom JSON values update only the normalized extraction
  target; the verbatim systematic-review appendix remains untouched, and the
  locked-test training safeguard is enforced in the interface and backend.
- Added a versioned, catalogue-isolated pharmacometric reference-corpus API.
  The AED PK/PD converter preserves raw systematic-review appendix rows and
  creates normalized model, article, screening, provenance, and source-hash
  JSON records.
- Added deterministic PMID-level train/validation/test partitions, locked-test
  leakage validation, immutable successor-version curation, and guarded
  source-paper-to-JSON training exports.
- Added target-blind text and dual text/vision/adjudication benchmark runners,
  resumable prediction envelopes, per-variant scoring, triage calibration
  metrics, numeric tolerances, and separate strict versus silver reporting.
- Added an optional PEFT/TRL LoRA/QLoRA trainer that validates leakage guards
  before loading the machine-learning stack.
