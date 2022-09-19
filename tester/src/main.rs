use std::{
    fs::File,
    io::{self, BufRead, Write},
    path::Path,
    process::{exit, Command},
    thread,
    time::Duration,
};

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
        }
    }

    fn parse(&mut self) -> bool {
        let expected_output_pattern = Regex::new(r"// expect: ?(.*)").unwrap();
        let expected_error_pattern = Regex::new(r"// (Error.*)").unwrap();
        //let _error_line_pattern = Regex::new(r"// \[((java|c) )?line (\d+)\] (Error.*)").unwrap();
        let expected_runtime_error_pattern = Regex::new(r"// expect runtime error: (.+)").unwrap();
        let _syntax_error_pattern = Regex::new(r"\[.*line (\d+)\] (Error.+)").unwrap();
        let _stack_trace_pattern = Regex::new(r"\[line (\d+)\]").unwrap();
        let non_test_pattern = Regex::new(r"// nontest").unwrap();

        if let Ok(lines) = read_lines(&self.path) {
            for (line_num, line) in lines.flatten().enumerate() {
                // Not a test file at all, so ignore it.
                if non_test_pattern.is_match(&line) {
                    break;
                }

                if let Some(matched) = expected_output_pattern.find(&line).map(|m| m.as_str()) {
                    self.expected_output
                        .push(ExpectedOuput::new(line_num, matched.to_string()));
                    self.expectations += 1;
                    continue;
                }

                if let Some(matched) = expected_error_pattern.find(&line).map(|m| m.as_str()) {
                    self.expected_errors
                        .push(format!("[line {}] {}", line_num + 1, matched));
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

    fn run(&mut self) -> Vec<String> {
        let mut failures = Vec::new();

        let result = Command::new("./target/release/rox-cli")
            .args([&self.path])
            .output()
            .expect("failed to execute process");

        let mut error_lines = result.stderr.lines().map(|l| l.unwrap());
        if let Some(runtime_error) = &self.expected_runtime_error {
            // Validate runtime errors
            let error = error_lines.next().unwrap();
            if &error != runtime_error {
                failures.push(format!(
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
            // TODO: Compile errors
            // var foundErrors = <String>{};
            // var unexpectedCount = 0;
            // for (var line in error_lines) {
            //   var match = _syntaxErrorPattern.firstMatch(line);
            //   if (match != null) {
            //     var error = "[${match[1]}] ${match[2]}";
            //     if (_expectedErrors.contains(error)) {
            //       foundErrors.add(error);
            //     } else {
            //       if (unexpectedCount < 10) {
            //         fail("Unexpected error:");
            //         fail(line);
            //       }
            //       unexpectedCount++;
            //     }
            //   } else if (line != "") {
            //     if (unexpectedCount < 10) {
            //       fail("Unexpected output on stderr:");
            //       fail(line);
            //     }
            //     unexpectedCount++;
            //   }
            // }
            // if (unexpectedCount > 10) {
            //   fail("(truncated ${unexpectedCount - 10} more...)");
            // }
            // // Validate that every expected error occurred.
            // for (var error in _expectedErrors.difference(foundErrors)) {
            //   fail("Missing expected error: $error");
            // }
        }

        // Validate exit code
        //assert_eq!(result.status.code(), Some(self.expected_exit_code));

        // TODO: Validate output
        //let mut output_lines = result.stdout.lines().map(|l| l.unwrap());
        // if (outputLines.isNotEmpty && outputLines.last == "") {
        //     outputLines.removeLast();
        //   }
        //   var index = 0;
        //   for (; index < outputLines.length; index++) {
        //     var line = outputLines[index];
        //     if (index >= _expectedOutput.length) {
        //       fail("Got output '$line' when none was expected.");
        //       continue;
        //     }
        //     var expected = _expectedOutput[index];
        //     if (expected.output != line) {
        //       fail("Expected output '${expected.output}' on line ${expected.line} "
        //           " and got '$line'.");
        //     }
        //   }
        //   while (index < _expectedOutput.length) {
        //     var expected = _expectedOutput[index];
        //     fail("Missing expected output '${expected.output}' on line "
        //         "${expected.line}.");
        //     index++;
        //   }

        failures
    }
}

struct Tester {
    filter_path: Option<String>,
    passed: u32,
    failed: u32,
    skipped: u32,
    expectations: u32,
}

impl Tester {
    fn new(filter_path: Option<String>) -> Self {
        Tester {
            filter_path,
            passed: 0,
            failed: 0,
            skipped: 0,
            expectations: 0,
        }
    }

    fn run_suite(&mut self, name: String) -> bool {
        let term = Term::stdout();

        // TODO: Run all tests
        for path in glob("samples/**/*.lox").expect("Failed to read glob pattern") {
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
        if path.contains("benchmark") {
            return;
        }

        // Check if we are just running a subset of the tests.
        if let Some(filter_path) = &self.filter_path {
            let test_path: String = pathdiff::diff_paths(path, "samples")
                .unwrap()
                .into_os_string()
                .into_string()
                .unwrap();
            if !test_path.starts_with(filter_path) {
                return;
            }
        }

        // Fake delay to achieve nice effect on the console
        thread::sleep(Duration::from_millis(50));
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

        let failures = test.run();
        if failures.is_empty() {
            self.passed += 1;
        } else {
            self.failed += 1;
            println!("{}: {}", style("FAIL").red(), path);
            println!();
            for failure in &failures {
                println!("\t{}", style(failure).blue());
            }
            println!();
        }
    }
}

fn main() -> io::Result<()> {
    //let mut tester = Tester::new(Some("constructor/default_arguments.lox".to_string()));
    let mut tester = Tester::new(Some("constructor".to_string()));
    if !tester.run_suite("test".to_string()) {
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
