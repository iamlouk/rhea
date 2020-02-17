
use std::process::Command;
use std::path::PathBuf;

fn run_example(name: &str) {
    let mut script = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    script.push("examples/run-example.sh");

    let status = Command::new("/bin/bash")
        .args(&[script.to_str().unwrap(), name]).status();

    assert!(status.is_ok());
    assert_eq!(status.ok().unwrap().code(), Some(0));
}

#[test]
fn all_examples() {
    let mut script = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    script.push("examples/run-examples.sh");

    let status = Command::new("/bin/bash")
        .args(&[script.to_str().unwrap(), "--silent"]).status();

    assert!(status.is_ok());
    assert_eq!(status.ok().unwrap().code(), Some(0));
}

#[test]
fn example_fib() {
    run_example("fib");
}

#[test]
fn example_fib2() {
    run_example("fib2");
}

#[test]
fn example_fib3() {
    run_example("fib3");
}

#[test]
fn example_hello_world() {
    run_example("hello-world");
}

#[test]
fn example_loop() {
    run_example("loop");
}

#[test]
fn example_simple_math() {
    run_example("simple-math");
}

#[test]
fn example_structs() {
    run_example("structs");
}

#[test]
fn example_types() {
    run_example("types");
}
