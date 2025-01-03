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

use clap::{CommandFactory, FromArgMatches};
use colored::Colorize;
use golem_wasm_rpc_stubgen::*;
use std::process::ExitCode;

#[cfg(feature = "app-command")]
use golem_wasm_rpc_stubgen::model::app::ComponentPropertiesExtensionsNone;

#[tokio::main]
async fn main() -> ExitCode {
    pretty_env_logger::init();

    #[cfg(feature = "app-command")]
    let mut clap_command = Command::command();
    #[cfg(not(feature = "app-command"))]
    let clap_command = Command::command();

    // Based on Command::parse, but using cloned command, so we avoid creating clap_command twice
    let parsed_command = {
        let mut matches = clap_command.clone().get_matches();
        let res = Command::from_arg_matches_mut(&mut matches)
            .map_err(|err| err.format(&mut Command::command()));
        res.unwrap_or_else(|e| e.exit())
    };

    #[cfg(feature = "app-command")]
    let show_warning = !matches!(parsed_command, Command::App { .. });
    #[cfg(not(feature = "app-command"))]
    let show_warning = true;

    if show_warning {
        eprintln!(
            "{}",
            "WARNING: THIS COMMAND IS DEPRECATED AND MIGHT MODIFY SOURCE WIT FILES!".yellow()
        );
        eprintln!(
            "{}",
            format!(
                "\nThe recommended new way to handle wasm-rpc stub generation and linking is the {} command.\n",
                "wasm-rpc-stubgen app".bold().underline(),
            ).yellow(),
        )
    }

    let result = match parsed_command {
        Command::Generate(generate_args) => generate(generate_args),
        Command::Build(build_args) => build(build_args).await,
        Command::AddStubDependency(add_stub_dependency_args) => {
            add_stub_dependency(add_stub_dependency_args)
        }
        Command::Compose(compose_args) => compose(compose_args).await,
        Command::InitializeWorkspace(init_workspace_args) => {
            initialize_workspace(init_workspace_args, "wasm-rpc-stubgen", &[])
        }
        #[cfg(feature = "app-command")]
        Command::App { command } => {
            run_app_command::<ComponentPropertiesExtensionsNone>(
                {
                    // TODO: it would be nice to use the same logic which is used by default for handling help,
                    //       and that way include the current context (bin name and parent commands),
                    //       but that seems to be using errors, error formating and exit directly;
                    //       and quite different code path compared to calling print_help
                    clap_command
                        .find_subcommand_mut("app")
                        .unwrap()
                        .clone()
                        .override_usage(format!(
                            "{} [OPTIONS] [COMMAND]",
                            "wasm-rpc-stubgen app".bold()
                        ))
                },
                command,
            )
            .await
        }
    };

    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            eprintln!("{}", format!("Error: {:#}", err).yellow());
            ExitCode::FAILURE
        }
    }
}
