#!/usr/bin/env python3
"""Train a leakage-checked LoRA/QLoRA adapter from a LibeRary reference export.

The script intentionally accepts only the JSONL produced by
library_reference_training_export(). It refuses test records before importing
any heavyweight machine-learning libraries, so --validate-only is useful on a
CPU-only workstation or in CI.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any


def read_examples(path: Path | None) -> list[dict[str, Any]]:
    if path is None:
        return []
    if not path.is_file():
        raise SystemExit(f"Training data not found: {path}")
    output: list[dict[str, Any]] = []
    with path.open("r", encoding="utf-8") as handle:
        for line_number, line in enumerate(handle, 1):
            if not line.strip():
                continue
            try:
                item = json.loads(line)
            except json.JSONDecodeError as exc:
                raise SystemExit(f"Invalid JSON at {path}:{line_number}: {exc}") from exc
            if item.get("partition") == "test" or item.get("leakage_guard", {}).get("test_data") is not False:
                raise SystemExit(f"Leakage guard rejected {item.get('id', line_number)} from {path}")
            messages = item.get("messages")
            if not isinstance(messages, list) or [m.get("role") for m in messages] != ["system", "user", "assistant"]:
                raise SystemExit(f"Malformed messages for {item.get('id', line_number)}")
            output.append(item)
    return output


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--train-file", type=Path, required=True)
    parser.add_argument("--eval-file", type=Path)
    parser.add_argument("--base-model", help="Exact Hugging Face base model used for the adapter")
    parser.add_argument("--output-dir", type=Path)
    parser.add_argument("--validate-only", action="store_true")
    parser.add_argument("--qlora", action=argparse.BooleanOptionalAction, default=True)
    parser.add_argument("--max-seq-length", type=int, default=4096)
    parser.add_argument("--epochs", type=float, default=3.0)
    parser.add_argument("--learning-rate", type=float, default=2e-4)
    parser.add_argument("--batch-size", type=int, default=1)
    parser.add_argument("--gradient-accumulation", type=int, default=16)
    parser.add_argument("--lora-r", type=int, default=16)
    parser.add_argument("--lora-alpha", type=int, default=32)
    parser.add_argument("--seed", type=int, default=20260716)
    args = parser.parse_args()
    if not args.validate_only and (not args.base_model or args.output_dir is None):
        parser.error("--base-model and --output-dir are required unless --validate-only is used")
    return args


def main() -> None:
    args = parse_args()
    train = read_examples(args.train_file)
    evaluation = read_examples(args.eval_file)
    if not train:
        raise SystemExit("No leakage-safe training examples were supplied.")
    tasks = sorted({str(item.get("task", "unknown")) for item in train})
    tiers = sorted({str(item.get("quality_tier", "unknown")) for item in train})
    print(json.dumps({"train_examples": len(train), "eval_examples": len(evaluation),
                      "tasks": tasks, "quality_tiers": tiers, "test_examples": 0}, indent=2))
    if args.validate_only:
        return

    try:
        import torch
        from datasets import Dataset
        from peft import LoraConfig, prepare_model_for_kbit_training
        from transformers import AutoModelForCausalLM, AutoTokenizer, BitsAndBytesConfig
        from trl import SFTConfig, SFTTrainer
    except ImportError as exc:
        raise SystemExit(
            "Training dependencies are missing. Install reference-training-requirements.txt "
            "in a dedicated Python environment."
        ) from exc

    if not torch.cuda.is_available():
        raise SystemExit("CUDA is required for this training profile.")
    compute_dtype = torch.bfloat16 if torch.cuda.is_bf16_supported() else torch.float16
    tokenizer = AutoTokenizer.from_pretrained(args.base_model, use_fast=True)
    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token

    model_kwargs: dict[str, Any] = {
        "torch_dtype": compute_dtype,
        "device_map": {"": 0},
        "low_cpu_mem_usage": True,
    }
    if args.qlora:
        model_kwargs["quantization_config"] = BitsAndBytesConfig(
            load_in_4bit=True,
            bnb_4bit_quant_type="nf4",
            bnb_4bit_use_double_quant=True,
            bnb_4bit_compute_dtype=compute_dtype,
        )
    model = AutoModelForCausalLM.from_pretrained(args.base_model, **model_kwargs)
    model.config.use_cache = False
    if args.qlora:
        model = prepare_model_for_kbit_training(model, use_gradient_checkpointing=True)

    def render(item: dict[str, Any]) -> dict[str, str]:
        return {"text": tokenizer.apply_chat_template(
            item["messages"], tokenize=False, add_generation_prompt=False
        )}

    train_dataset = Dataset.from_list(train).map(render, remove_columns=list(train[0].keys()))
    eval_dataset = None
    if evaluation:
        eval_dataset = Dataset.from_list(evaluation).map(render, remove_columns=list(evaluation[0].keys()))

    lora = LoraConfig(
        r=args.lora_r,
        lora_alpha=args.lora_alpha,
        lora_dropout=0.05,
        bias="none",
        task_type="CAUSAL_LM",
        target_modules="all-linear",
    )
    config = SFTConfig(
        output_dir=str(args.output_dir),
        num_train_epochs=args.epochs,
        learning_rate=args.learning_rate,
        per_device_train_batch_size=args.batch_size,
        per_device_eval_batch_size=1,
        gradient_accumulation_steps=args.gradient_accumulation,
        gradient_checkpointing=True,
        bf16=compute_dtype == torch.bfloat16,
        fp16=compute_dtype == torch.float16,
        logging_steps=5,
        save_strategy="epoch",
        eval_strategy="epoch" if eval_dataset is not None else "no",
        optim="paged_adamw_8bit" if args.qlora else "adamw_torch",
        max_length=args.max_seq_length,
        dataset_text_field="text",
        seed=args.seed,
        report_to="none",
    )
    trainer = SFTTrainer(
        model=model,
        args=config,
        train_dataset=train_dataset,
        eval_dataset=eval_dataset,
        processing_class=tokenizer,
        peft_config=lora,
    )
    trainer.train()
    trainer.save_model(str(args.output_dir))
    tokenizer.save_pretrained(str(args.output_dir))
    (args.output_dir / "liberary_training_provenance.json").write_text(
        json.dumps({
            "base_model": args.base_model,
            "train_file": str(args.train_file.resolve()),
            "eval_file": str(args.eval_file.resolve()) if args.eval_file else None,
            "train_examples": len(train),
            "eval_examples": len(evaluation),
            "tasks": tasks,
            "quality_tiers": tiers,
            "test_examples": 0,
            "qlora": args.qlora,
            "seed": args.seed,
        }, indent=2),
        encoding="utf-8",
    )


if __name__ == "__main__":
    main()
