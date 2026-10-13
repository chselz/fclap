if(NOT DEFINED FCLAP_CLI)
  message(FATAL_ERROR "FCLAP_CLI was not provided")
endif()

execute_process(
  COMMAND "${FCLAP_CLI}"
  RESULT_VARIABLE success_status
  OUTPUT_VARIABLE success_stdout
  ERROR_VARIABLE success_stderr
)
if(NOT success_status EQUAL 0)
  message(FATAL_ERROR "success path exited with ${success_status}")
endif()
if(NOT success_stdout STREQUAL "" OR NOT success_stderr STREQUAL "")
  message(FATAL_ERROR "success path produced output")
endif()

execute_process(
  COMMAND "${FCLAP_CLI}" --legacy value
  RESULT_VARIABLE warning_status
  OUTPUT_VARIABLE warning_stdout
  ERROR_VARIABLE warning_stderr
)
if(NOT warning_status EQUAL 0)
  message(FATAL_ERROR "warning path exited with ${warning_status}")
endif()
if(NOT warning_stdout STREQUAL "")
  message(FATAL_ERROR "warning path wrote to stdout")
endif()
if(NOT warning_stderr MATCHES "fixture: warning: argument --legacy: use --name")
  message(FATAL_ERROR "warning path did not write the warning to stderr")
endif()

execute_process(
  COMMAND "${FCLAP_CLI}" --help
  RESULT_VARIABLE help_status
  OUTPUT_VARIABLE help_stdout
  ERROR_VARIABLE help_stderr
)
if(NOT help_status EQUAL 0)
  message(FATAL_ERROR "help path exited with ${help_status}")
endif()
if(NOT help_stdout MATCHES "^usage: fixture")
  message(FATAL_ERROR "help path did not write formatted help to stdout")
endif()
if(NOT help_stderr STREQUAL "")
  message(FATAL_ERROR "help path wrote to stderr")
endif()

execute_process(
  COMMAND "${FCLAP_CLI}" --version
  RESULT_VARIABLE version_status
  OUTPUT_VARIABLE version_stdout
  ERROR_VARIABLE version_stderr
)
if(NOT version_status EQUAL 0)
  message(FATAL_ERROR "version path exited with ${version_status}")
endif()
string(STRIP "${version_stdout}" version_text)
if(NOT version_text STREQUAL "fixture 1.0")
  message(FATAL_ERROR "version path returned unexpected text")
endif()
if(NOT version_stderr STREQUAL "")
  message(FATAL_ERROR "version path wrote to stderr")
endif()

execute_process(
  COMMAND "${FCLAP_CLI}" --unknown
  RESULT_VARIABLE failure_status
  OUTPUT_VARIABLE failure_stdout
  ERROR_VARIABLE failure_stderr
)
if(NOT failure_status EQUAL 2)
  message(FATAL_ERROR "failure path exited with ${failure_status}, expected 2")
endif()
if(NOT failure_stdout STREQUAL "")
  message(FATAL_ERROR "failure path wrote to stdout")
endif()
if(NOT failure_stderr MATCHES "^usage: fixture")
  message(FATAL_ERROR "failure path did not write usage to stderr")
endif()
if(NOT failure_stderr MATCHES "fixture: error: unknown option '--unknown'")
  message(FATAL_ERROR "failure path did not write the parser error")
endif()
# A command-line mistake is a normal termination, not a crash report.
if(failure_stderr MATCHES "Backtrace|ERROR STOP|STOP 2")
  message(FATAL_ERROR "failure path reported a runtime termination")
endif()

# A mistyped option names the option the user probably meant.
execute_process(
  COMMAND "${FCLAP_CLI}" --nmae x
  RESULT_VARIABLE typo_status
  ERROR_VARIABLE typo_stderr
)
if(NOT typo_status EQUAL 2)
  message(FATAL_ERROR "typo path exited with ${typo_status}, expected 2")
endif()
if(NOT typo_stderr MATCHES "fixture: error: unknown option '--nmae' \\(did you mean '--name'\\?\\)")
  message(FATAL_ERROR "typo path did not suggest the intended option")
endif()

# An option declared with env= reads the process environment.
execute_process(
  COMMAND "${CMAKE_COMMAND}" -E env FIXTURE_JOBS=4 "${FCLAP_CLI}"
  RESULT_VARIABLE env_status
  OUTPUT_VARIABLE env_stdout
  ERROR_VARIABLE env_stderr
)
if(NOT env_status EQUAL 0 OR NOT env_stderr STREQUAL "")
  message(FATAL_ERROR "valid environment value was rejected: ${env_stderr}")
endif()
execute_process(
  COMMAND "${CMAKE_COMMAND}" -E env FIXTURE_JOBS=abc "${FCLAP_CLI}"
  RESULT_VARIABLE env_status
  ERROR_VARIABLE env_stderr
)
if(NOT env_status EQUAL 2)
  message(FATAL_ERROR "invalid environment value exited with ${env_status}, expected 2")
endif()
if(NOT env_stderr MATCHES "fixture: error: argument --jobs: environment variable FIXTURE_JOBS: invalid integer value \\(got 'abc'\\)")
  message(FATAL_ERROR "invalid environment value was not reported: ${env_stderr}")
endif()

# A subcommand created with help_on_empty shows its help for an empty line.
execute_process(
  COMMAND "${FCLAP_CLI}" show
  RESULT_VARIABLE empty_status
  OUTPUT_VARIABLE empty_stdout
  ERROR_VARIABLE empty_stderr
)
if(NOT empty_status EQUAL 2)
  message(FATAL_ERROR "empty subcommand exited with ${empty_status}, expected 2")
endif()
if(NOT empty_stderr MATCHES "^usage: fixture show" OR
   NOT empty_stderr MATCHES "item to show" OR
   empty_stderr MATCHES "error:")
  message(FATAL_ERROR "empty subcommand did not print only its help: ${empty_stderr}")
endif()

# width_from_env wraps help to the COLUMNS environment variable.
execute_process(
  COMMAND "${CMAKE_COMMAND}" -E env COLUMNS=40 "${FCLAP_CLI}" --help
  RESULT_VARIABLE narrow_status
  OUTPUT_VARIABLE narrow_stdout
)
if(NOT narrow_status EQUAL 0)
  message(FATAL_ERROR "narrow help exited with ${narrow_status}")
endif()
string(REPLACE "\n" ";" narrow_lines "${narrow_stdout}")
foreach(line IN LISTS narrow_lines)
  string(LENGTH "${line}" line_length)
  if(line_length GREATER 40)
    message(FATAL_ERROR "help line exceeds COLUMNS=40: '${line}'")
  endif()
endforeach()

# An application check reported through parser%error looks like a usage error.
execute_process(
  COMMAND "${FCLAP_CLI}" --check
  RESULT_VARIABLE check_status
  OUTPUT_VARIABLE check_stdout
  ERROR_VARIABLE check_stderr
)
if(NOT check_status EQUAL 2)
  message(FATAL_ERROR "parser%error exited with ${check_status}, expected 2")
endif()
if(NOT check_stderr MATCHES "^usage: fixture" OR
   NOT check_stderr MATCHES "fixture: error: check failed")
  message(FATAL_ERROR "parser%error did not print usage and the message")
endif()

if(DEFINED FCLAP_INVALID_CLI)
  # A broken parser definition is a programming error with its own status.
  execute_process(
    COMMAND "${FCLAP_INVALID_CLI}"
    RESULT_VARIABLE invalid_status
    ERROR_VARIABLE invalid_stderr
  )
  if(NOT invalid_status EQUAL 70)
    message(FATAL_ERROR "invalid definition exited with ${invalid_status}, expected 70")
  endif()
  if(NOT invalid_stderr MATCHES "fixture: invalid parser definition: argument -x/--output:")
    message(FATAL_ERROR "invalid definition did not name the option")
  endif()
endif()

if(DEFINED FCLAP_MISUSE_CLI)
  # Retrieving a mistyped key without stat= stops instead of being ignored.
  execute_process(
    COMMAND "${FCLAP_MISUSE_CLI}"
    RESULT_VARIABLE misuse_status
    ERROR_VARIABLE misuse_stderr
  )
  if(misuse_status EQUAL 0)
    message(FATAL_ERROR "namespace misuse did not stop the program")
  endif()
  if(NOT misuse_stderr MATCHES "fclap: namespace key 'nmae': no such key")
    message(FATAL_ERROR "namespace misuse did not name the key")
  endif()
endif()
