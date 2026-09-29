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
if(NOT warning_stderr MATCHES "\\[WARNING\\].*use --name")
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
if(NOT failure_stderr MATCHES "unknown option")
  message(FATAL_ERROR "failure path did not write the parser error")
endif()
