
use std::process::Command;
use std::path::PathBuf;

#[test]
fn run_all_examples() {
    let mut script = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    script.push("examples/run-examples.sh");

    let status = Command::new("/bin/bash")
        .args(&[script.to_str().unwrap(), "--silent"]).status();

    assert!(status.is_ok());
    assert_eq!(status.ok().unwrap().code(), Some(0));
}

