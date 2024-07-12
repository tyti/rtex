use rtex::tex_main;

use std::process::ExitCode;

fn main() -> ExitCode {
    match tex_main() {
        Ok(()) => ExitCode::SUCCESS,
        Err(()) => ExitCode::FAILURE,
    }
}
