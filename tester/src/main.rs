use std::{
    fs::File,
    io::{self, BufRead},
    path::Path,
    process::{exit, Command},
    thread,
    time::{Duration, Instant},
};

use clap::Parser;
use console::{style, Term};
use glob::glob;
use regex::Regex;

struct ExpectedOuput {
    line: usize,
    output: String,
}

impl ExpectedOuput {
    pub fn new(line: usize, output: String) -> Self {
        ExpectedOuput { line, output }
    }
}

struct Test {
    path: String,
    expected_output: Vec<ExpectedOuput>,
    expected_errors: Vec<String>,
    expected_exit_code: i32,
    runtime_error_line: usize,
    expected_runtime_error: Option<String>,
    expectations: u32,
    failures: Vec<String>,
}

impl Test {
    fn new(path: String) -> Self {
        Test {
            path,
            expected_output: Vec::new(),
            expected_errors: Vec::new(),
            expected_exit_code: 0,
            runtime_error_line: 0,
            expected_runtime_error: None,
            expectations: 0,
            failures: Vec::new(),
        }
    }

    fn parse(&mut self) -> bool {
        let expected_output_pattern = Regex::new(r"// expect: ?(.*)").unwrap();
        let expected_error_pattern = Regex::new(r"// expect error: (Error.*)").unwrap();
        let _error_line_pattern = Regex::new(r"// \[((java|c) )?line (\d+)\] (Error.*)").unwrap();
        let expected_runtime_error_pattern = Regex::new(r"// expect runtime error: (.+)").unwrap();
        let _stack_trace_pattern = Regex::new(r"\[line (\d+)\]").unwrap();
        let non_test_pattern = Regex::new(r"// nontest").unwrap();

        if let Ok(lines) = read_lines(&self.path) {
            for (line_num, line) in lines.flatten().enumerate() {
                // Not a test file at all, so ignore it.
                if non_test_pattern.is_match(&line) {
                    break;
                }

                if let Some(matched) = expected_output_pattern
                    .captures(&line)
                    .and_then(|captures| captures.get(1))
                    .map(|matched| matched.as_str().to_string())
                {
                    self.expected_output
                        .push(ExpectedOuput::new(line_num + 1, matched));
                    self.expectations += 1;
                    continue;
                }

                if let Some(matched) = expected_error_pattern
                    .captures(&line)
                    .and_then(|captures| captures.get(1))
                    .map(|matched| matched.as_str().to_string())
                {
                    let error = format!("[line {}] {}", line_num + 1, matched);
                    self.expected_errors.push(error);
                    self.expected_exit_code = 65;
                    self.expectations += 1;
                    continue;
                }

                if let Some(matched) = expected_runtime_error_pattern
                    .captures(&line)
                    .and_then(|captures| captures.get(1))
                    .map(|matched| matched.as_str().to_string())
                {
                    self.expected_runtime_error =
                        Some(format!("[line {}] {}", line_num + 1, matched));
                    self.expected_exit_code = 70;
                    self.runtime_error_line = line_num + 1;
                    self.expectations += 1;
                    continue;
                }
            }
        }

        if !self.expected_errors.is_empty() && self.expected_runtime_error.is_some() {
            // Cannot expect both compile and runtime errors.
            println!(
                "{}: {}\n\t{}",
                style("TEST ERROR").magenta(),
                self.path,
                style("Cannot expect both compile and runtime errors.").red()
            );
            return false;
        }

        true
    }

    fn run(&mut self) -> &[String] {
        let syntax_error_pattern = Regex::new(r"\[.*line (\d+)\] (Error.+)").unwrap();

        let result = Command::new("./target/release/rox-cli")
            .args([&self.path])
            .output()
            .expect("failed to execute process");

        let error_lines: Vec<String> = result.stderr.lines().map(|l| l.unwrap()).collect();
        if let Some(runtime_error) = &self.expected_runtime_error {
            // Validate runtime errors, can only have one
            let error = error_lines.get(0).unwrap();
            if error != runtime_error {
                self.failures.push(format!(
                    "Expected runtime error '{}' and got:\n{}",
                    runtime_error, error
                ));
            }

            // TODO: Make sure the stack trace has the right line.
            // RegExpMatch match;
            // var stackLines = errorLines.sublist(1);
            // for (var line in stackLines) {
            //   match = _stackTracePattern.firstMatch(line);
            //   if (match != null) break;
            // }
            // if (match == null) {
            //   fail("Expected stack trace and got:", stackLines);
            // } else {
            //   var stackLine = int.parse(match[1]);
            //   if (stackLine != _runtimeErrorLine) {
            //     fail("Expected runtime error on line $_runtimeErrorLine "
            //         "but was on line $stackLine.");
            //   }
            // }
        } else {
            // Validate compile errors
            let mut found_errors = Vec::new();
            let mut unexpected_count: u32 = 0;
            for line in &error_lines {
                if let Some((first, second)) = syntax_error_pattern
                    .captures(line)
                    .and_then(|captures| {
                        captures
                            .get(1)
                            .and_then(|first| captures.get(2).map(|second| (first, second)))
                    })
                    .map(|(first, second)| {
                        (first.as_str().to_string(), second.as_str().to_string())
                    })
                {
                    let error = format!("[line {}] {}", first, second);
                    if self.expected_errors.contains(&error) {
                        found_errors.push(error);
                    } else if unexpected_count < 10 {
                        self.failures.push("Unexpected error:".to_string());
                        self.failures.push(line.clone());
                        unexpected_count += 1;
                    }
                } else if !line.is_empty() && unexpected_count < 10 {
                    self.failures
                        .push("Unexpected output on stderr:".to_string());
                    self.failures.push(line.clone());
                    unexpected_count += 1;
                }
            }

            if unexpected_count > 10 {
                self.failures
                    .push(format!("(truncated {} more...)", unexpected_count - 10));
            }

            let difference: Vec<_> = found_errors
                .into_iter()
                .filter(|item| !self.expected_errors.contains(item))
                .collect();
            for error in difference {
                self.failures
                    .push(format!("Missing expected error: {}", error));
            }
        }

        // Validate exit code
        if result.status.code() != Some(self.expected_exit_code) && error_lines.len() > 10 {
            let mut errors = error_lines.into_iter().take(10).collect::<Vec<_>>();
            errors.push("(truncated...)".to_string());
            self.failures.push(format!(
                "Expected return code {} and got {}. Stderr:",
                self.expected_exit_code,
                result.status.code().unwrap()
            ));
            self.failures.append(&mut errors);
        }

        // Validate output
        let mut output_lines: Vec<String> = result.stdout.lines().map(|l| l.unwrap()).collect();
        // Remove the trailing last empty line.
        if let Some(output) = output_lines.last() {
            if output.is_empty() {
                output_lines.pop();
            }
        }

        let mut index = 0;
        for (line_index, line) in output_lines.iter().enumerate() {
            index += 1;
            if line_index >= self.expected_output.len() {
                self.failures
                    .push(format!("Got output '{}' when none was expected.", line));
                continue;
            }

            let expected = self.expected_output.get(line_index).unwrap();
            if &expected.output != line {
                self.failures.push(format!(
                    "Expected output '{}' on line {} and got '{}'.",
                    expected.output, expected.line, line
                ));
            }
        }

        while index < self.expected_output.len() {
            let expected = self.expected_output.get(index).unwrap();
            self.failures.push(format!(
                "Missing expected output '{}' on line {}.",
                expected.output, expected.line
            ));
            index += 1;
        }

        &self.failures
    }
}

struct Tester {
    filter_path: Option<String>,
    skip: Option<Vec<String>>,
    passed: u32,
    failed: u32,
    skipped: u32,
    expectations: u32,
}

impl Tester {
    fn new(filter_path: Option<String>, skip: Option<Vec<String>>) -> Self {
        Tester {
            filter_path,
            skip,
            passed: 0,
            failed: 0,
            skipped: 0,
            expectations: 0,
        }
    }

    fn run_suite(&mut self, _name: String) -> bool {
        let term = Term::stdout();

        for path in glob("tests/**/*.lox").expect("Failed to read glob pattern") {
            self.run_test(&term, path.unwrap().to_str().unwrap());
        }

        term.clear_last_lines(1).unwrap();

        if self.failed == 0 {
            println!(
                "All {} tests passed ({} expectations).",
                style(self.passed).green(),
                self.expectations
            );
        } else {
            println!(
                "{} tests passed. {} tests failed.",
                style(self.passed).green(),
                style(self.failed).red()
            );
        }

        self.failed == 0
    }

    fn run_test(&mut self, term: &Term, path: &str) {
        // Skip "benchmark" and "limit" tests
        if path.contains("benchmark") || path.contains("limit") {
            self.skipped += 1;
            return;
        }

        // Skip any other tests requested
        if let Some(skip) = &self.skip {
            if skip.iter().any(|t| path.contains(t)) {
                self.skipped += 1;
                return;
            }
        }

        // Check if we are just running a subset of the tests
        if let Some(filter_path) = &self.filter_path {
            let test_path: String = pathdiff::diff_paths(path, "tests")
                .unwrap()
                .into_os_string()
                .into_string()
                .unwrap();
            if !test_path.starts_with(filter_path) {
                return;
            }
        }

        // Update status output
        term.clear_last_lines(1).unwrap();
        term.write_line(&format!(
            "Passed: {} Failed: {} Skipped: {} ({})",
            style(self.passed).green(),
            style(self.failed).red(),
            style(self.skipped).yellow(),
            style(path)
        ))
        .unwrap();

        let mut test = Test::new(path.to_string());
        if !test.parse() {
            return;
        }
        self.expectations += test.expectations;

        let start = Instant::now();
        let failures = test.run();
        let test_duration = start.elapsed();
        if failures.is_empty() {
            self.passed += 1;
        } else {
            self.failed += 1;
            println!("{}: {}", style("FAIL").red(), path);
            println!();
            for failure in failures {
                println!("\t{}", style(failure).blue());
            }
            println!();
        }

        // Fake delay to achieve nice effect on the console
        thread::sleep(Duration::from_millis(
            (100u128
                .checked_sub(test_duration.as_millis())
                .unwrap_or(100))
            .try_into()
            .unwrap(),
        ));
    }
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Optional path to filter tests
    #[arg(short, long)]
    filter_path: Option<String>,

    /// Optional tests to skip, seprated by comma
    #[arg(short, long)]
    skip: Option<String>,
}

fn main() -> io::Result<()> {
    let args = Args::parse();
    let mut tester = Tester::new(
        args.filter_path,
        args.skip
            .map(|skip| skip.split(',').map(str::to_string).collect()),
    );
    if !tester.run_suite("all".to_string()) {
        exit(1);
    }

    Ok(())
}

// The output is wrapped in a Result to allow matching on errors
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
