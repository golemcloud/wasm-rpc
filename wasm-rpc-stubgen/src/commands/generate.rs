// Copyright 2024 Golem Cloud
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use crate::cargo::generate_cargo_toml;
use crate::compilation::compile;
use crate::rust::generate_stub_source;
use crate::stub::StubDefinition;
use crate::wit::{copy_wit_files, generate_stub_wit};
use anyhow::Context;
use fs_extra::dir::CopyOptions;
use heck::ToSnakeCase;
use std::fs;
use std::path::{Path, PathBuf};
use wit_parser::Resolve;

pub fn generate(stub_def: &StubDefinition) -> anyhow::Result<()> {
    let _ = generate_stub_wit_dir(stub_def)?;
    generate_cargo_toml(stub_def).context("Failed to generate the Cargo.toml file")?;
    generate_stub_source(stub_def).context("Failed to generate the stub Rust source")?;
    Ok(())
}

pub async fn build(
    stub_def: &StubDefinition,
    dest_wasm: &Path,
    dest_wit_root: &Path,
) -> anyhow::Result<()> {
    let wasm_path = generate_and_build_stub(stub_def).await?;

    if let Some(parent) = dest_wasm.parent() {
        fs::create_dir_all(parent)
            .context("Failed to create parent directory of the target WASM file")?;
    }
    fs::copy(wasm_path, dest_wasm).context("Failed to copy the WASM file to the destination")?;

    fs::create_dir_all(dest_wit_root).context("Failed to create the target WIT root directory")?;

    fs_extra::dir::copy(
        stub_def.target_root.join("wit"),
        dest_wit_root,
        &CopyOptions::new().content_only(true).overwrite(true),
    )
    .context("Failed to copy the generated WIT files to the destination")?;

    Ok(())
}

pub fn generate_stub_wit_dir(stub_def: &StubDefinition) -> anyhow::Result<Resolve> {
    generate_stub_wit(stub_def).context("Failed to generate the stub wit file")?;
    copy_wit_files(stub_def).context("Failed to copy the dependent wit files")?;
    stub_def
        .verify_target_wits()
        .context("Failed to resolve the result WIT root")
}

pub async fn generate_and_build_stub(stub_def: &StubDefinition) -> anyhow::Result<PathBuf> {
    let _ = generate_stub_wit_dir(stub_def)?;
    generate_cargo_toml(stub_def).context("Failed to generate the Cargo.toml file")?;
    generate_stub_source(stub_def).context("Failed to generate the stub Rust source")?;

    compile(&stub_def.target_root)
        .await
        .context("Failed to compile the generated stub")?;

    let wasm_path = stub_def
        .target_root
        .join("target")
        .join("wasm32-wasi")
        .join("release")
        .join(format!(
            "{}.wasm",
            stub_def.target_crate_name()?.to_snake_case()
        ));
    Ok(wasm_path)
}