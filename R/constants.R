LIBRARY_SCHEMA_VERSION <- "1.4.0"
LIBRARY_PROMPT_VERSION <- "3.1.0"
LIBRARY_REFERENCE_SCHEMA_VERSION <- "1.1.0"
LIBRARY_REFERENCE_SCHEMA_SUPPORTED <- c("1.0.0", "1.1.0")

LIBRARY_STATUS_LEVELS <- c(
  "discovered", "stub", "draft", "machine_consistent",
  "machine_adjudicated", "review", "validated", "mbma_source", "deprecated"
)

LIBRARY_TRIAGE_TIERS <- c("high", "intermediate", "low")

# Default pharmacometrics-oriented PubMed query (population PK/PD + NONMEM/nlmixr)
DEFAULT_PM_QUERY <- paste(
  '("population pharmacokinetic"[Title/Abstract] OR',
  '"population pharmacokinetics"[Title/Abstract] OR',
  '"NONMEM"[Title/Abstract] OR',
  '"nlmixr"[Title/Abstract] OR',
  '"pharmacokinetic-pharmacodynamic"[Title/Abstract]) AND',
  '("pharmacokinetic"[Title/Abstract] OR "pharmacodynamic"[Title/Abstract])'
)

MODEL_KEYWORDS <- c(
  "nonmem", "nlmixr", "monolix", "population pk", "population pharmacokinetic",
  "pharmacokinetic model", "pk/pd", "pk-pd", "clearance", "compartment model",
  "foce", "focei", "saem", "bayesian pk"
)

DEFAULT_CONFIG <- list(
  entrez = list(
    tool = "LibeRary",
    email = "",
    api_key = "",
    requests_per_second = 1
  ),
  unpaywall = list(
    email = "",
    requests_per_second = 1
  ),
  europe_pmc = list(
    requests_per_second = 1
  ),
  data_dir = "",
  inbox_dir = "",
  cache_dir = "",
  fetch = list(
    timeout_seconds = 120L,
    user_agent = "LibeRary/0.7.3 (R; pharmacometric literature ingest)",
    use_chromote_fallback = FALSE
  ),
  triage = list(
    enabled = TRUE,
    high_threshold = 0.70,
    intermediate_threshold = 0.30,
    first_pass_tiers = c("high", "intermediate"),
    retain_low_backlog = TRUE
  ),
  docling = list(
    executable = "docling",
    pipeline = "standard",
    output_formats = c("json", "md", "html"),
    ocr = TRUE,
    tables = TRUE,
    table_mode = "accurate",
    timeout_seconds = 1800L,
    allow_pdf_text_fallback = TRUE,
    render_dpi = 140L,
    max_vision_pages = 12L
  ),
  reproduction = list(
    enabled = TRUE,
    auto_run = FALSE,
    nsim = 200L,
    seed = 20260716L,
    n_cores = 1L,
    allow_generated_defaults = FALSE
  ),
  deliberative = list(
    enabled = TRUE,
    cache_stages = TRUE,
    visual_verification = TRUE,
    max_document_chars = 500000L,
    chunk_chars = 4200L,
    chunk_overlap = 350L,
    max_chunks_per_stage = 8L,
    max_gap_rounds = 1L,
    ledger_context_chars = 24000L,
    visual_context_chars = 16000L,
    visual_num_ctx = 32768L,
    visual_num_predict = 12288L,
    synthesis_context_chars = 24000L,
    synthesis_num_ctx = 32768L,
    synthesis_num_predict = 16384L
  ),
  ollama = list(
    base_url = "http://127.0.0.1:11434",
    model = "",
    timeout_seconds = 600L,
    max_pdf_chars = 120000L,
    num_ctx = 16384L,
    num_predict = 8192L,
    think = FALSE
  ),
  llm = list(
    allow_remote_content = FALSE,
    require_independent_extraction_models = FALSE,
    structured_retries = 1L,
    triage = list(provider = "same", model = "", temperature = 0, instruction = ""),
    indexing = list(provider = "ollama", model = "", temperature = 0, instruction = ""),
    vision = list(provider = "same", model = "", temperature = 0, instruction = ""),
    assessment = list(provider = "same", model = "", temperature = 0, instruction = ""),
    adjudication = list(provider = "same", model = "", temperature = 0, instruction = ""),
    providers = list(
      ollama = list(base_url = "http://127.0.0.1:11434", timeout_seconds = 600L,
                    api_key_env = "OLLAMA_API_KEY"),
      openai = list(base_url = "https://api.openai.com", timeout_seconds = 600L,
                    api_key_env = "OPENAI_API_KEY"),
      openai_compatible = list(base_url = "http://127.0.0.1:1234", timeout_seconds = 600L,
                               api_key_env = "LIBERARY_LLM_API_KEY")
    )
  ),
  catalog_dir = ""
)
