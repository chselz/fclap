## Plan: Modular FCLAP Refactor

Refactor fclap from a monolithic parser implementation to a modular architecture centered on ErrorStack-based error propagation, precision-aware argument typing, and user-friendly nested subparsers. The recommended path is a staged breaking cleanup: migrate core behavior first (errors + typed values), then split parser/actions/subparsers into focused modules, and finally remove legacy `old/` modules once parity is verified.

**Steps**
1. Baseline and freeze current behavior surface for migration safety. Capture current parser semantics from `/home/selzer/projects/p09_fclap_Selzer/fclap/src/fclap/old/fclap_parser.f90`, `/home/selzer/projects/p09_fclap_Selzer/fclap/src/fclap/old/fclap_actions.f90`, and `/home/selzer/projects/p09_fclap_Selzer/fclap/src/fclap/old/fclap_namespace.f90`, including subparser merge behavior and help/version exits. Mark all places where `error stop`/`stop` currently occur and define expected replacements. This is a blocking inventory for steps 2-7.
2. Implement unified parse error policy around ErrorStack (*depends on 1*). Make `parse_args`/`parse_args_array` accept optional user-provided error stack and always parse against an active stack reference (provided stack or internal local stack). Replace direct hard-stop paths from parser/action code with stack entries. Add a single decision point: if fatal unhandled errors exist and `exit_on_error=.true.`, print full stack and abort; otherwise return namespace + stack to caller.
3. Replace legacy single-error type usage and remove mixed error paths (*depends on 2*). Migrate remaining `fclap_error` interactions to `ErrorEntry`/`ErrorStack` equivalents and remove old `self%last_error` coupling. Keep message context quality (argument name + value). Ensure help/version are represented as controlled flow outcomes rather than unconditional process termination.
4. Introduce precision model for numeric argument declarations (*parallel with 3 after 2 is stable*). Extend argument type grammar to accept dual aliases:
   - Explicit: `real32`, `real64`, `real128`, `int32`, `int64`, `int128`.
   - Alias: `real_sp`, `real_dp`, `real_qp`, `int_sp`, `int_dp`, `int_qp`.
   Canonicalize internally to precision-kind metadata. Treat `real` as `real_dp`; treat `int`/`integer` as `int_dp` per your requirement. Add capability checks so unsupported kinds (e.g., `*_qp` on a compiler without matching kind) fail with actionable parser errors.
5. Propagate per-argument precision through parse, storage, and retrieval APIs (*depends on 4*). Extend `Action` metadata to include numeric kind and update execution/parsing logic to read into kind-correct temporaries. Expand `ValueContainer`/`Namespace` to retain kind-tagged numeric values, and add typed getters requiring matching output variable kinds. Define explicit mismatch behavior: raise parse/runtime retrieval error unless user opts into explicit conversion API.
6. Split actions into dedicated modular units with abstract interfaces where valuable (*depends on 5*). Extract action responsibilities into a dedicated `actions/` area (base metadata, execution, bound-check actions, validation helpers). Reuse `formatter/abstract.f90` design style for abstract contracts where multiple implementations are expected; avoid inheritance where a simple dispatcher table or composition is clearer.
7. Rework subparser subsystem for ergonomic nested composition (*depends on 2, parallel with 6*). Extract subparser registration and dispatch from parser core into dedicated modules (registry + dispatcher). Add a hybrid user API:
   - Manual mode: expose command path (`command_path`) and merged namespace values.
   - Callback mode: allow registering handlers for subparser paths and dispatch automatically.
   This removes the need for deeply nested user-side `select case` chains while preserving explicit control paths.
8. Decompose parser into modular files and move utility-only logic (*depends on 6 and 7*). Split parser responsibilities into base type, builder/registration, parse engine, and formatting integration modules. Move `get_prog_name` out of parser modules into a utility module (`utils/system`) because it is parser-agnostic system behavior.
9. Rewire public API and build scripts for new module graph (*depends on 8*). Update `/home/selzer/projects/p09_fclap_Selzer/fclap/src/fclap.f90` exports to point to the new modules and retire `old/` exports. Update Meson/CMake source lists and module dependency ordering for the new file layout.
10. Refresh docs/examples and replace ad hoc tests with targetted regression tests (*depends on 9*). For testing use the testdrive framework only and add also expected fail tests. Update tutorial/API docs for new type names, error-stack behavior, and nested subparser usage. Add dedicated tests for error stack propagation, `exit_on_error` policy, precision roundtrips per kind, and nested subparser dispatch paths.
11. Remove legacy `old/` parser/actions/namespace/error modules and finalize cleanup (*depends on 10*). Delete dead code only after green verification matrix and ensure no public API references legacy modules.

**Relevant files**
- `/home/selzer/projects/p09_fclap_Selzer/fclap/src/fclap/argparser.f90` — current new skeleton parser; target for split/migration anchor.
- `/home/selzer/projects/p09_fclap_Selzer/fclap/src/fclap/old/fclap_parser.f90` — current full parser logic to migrate (parse loop, groups, subparsers, error path).
- `/home/selzer/projects/p09_fclap_Selzer/fclap/src/fclap/old/fclap_actions.f90` — current action execution and type conversion logic.
- `/home/selzer/projects/p09_fclap_Selzer/fclap/src/fclap/old/fclap_namespace.f90` — current value storage and getters; key precision propagation touchpoint.
- `/home/selzer/projects/p09_fclap_Selzer/fclap/src/fclap/error/codes.f90` — canonical error codes/constants.
- `/home/selzer/projects/p09_fclap_Selzer/fclap/src/fclap/error/entry.f90` — single error payload type.
- `/home/selzer/projects/p09_fclap_Selzer/fclap/src/fclap/error/stack.f90` — stack container; target for parse-wide error orchestration.
- `/home/selzer/projects/p09_fclap_Selzer/fclap/src/fclap/utils/accuracy.f90` — kind constants baseline (`sp`, `dp`, `ip`, etc.).
- `/home/selzer/projects/p09_fclap_Selzer/fclap/src/fclap/formatter/abstract.f90` — abstraction pattern reference for modular design.
- `/home/selzer/projects/p09_fclap_Selzer/fclap/src/fclap.f90` — public API re-export surface.
- `/home/selzer/projects/p09_fclap_Selzer/fclap/example/example.f90` — current subparser UX example to modernize.
- `/home/selzer/projects/p09_fclap_Selzer/fclap/test/unit/old_main.f90` — legacy procedural tests and failure cases list (useful migration checklist).
- `/home/selzer/projects/p09_fclap_Selzer/fclap/meson.build` and `/home/selzer/projects/p09_fclap_Selzer/fclap/src/meson.build` — module list/dependency update points.
- `/home/selzer/projects/p09_fclap_Selzer/fclap/CMakeLists.txt` and `/home/selzer/projects/p09_fclap_Selzer/fclap/src/CMakeLists.txt` — CMake source graph update points.

**Verification**
1. Build succeeds with fpm (to make fpm availible make module load fpm) Meson and CMake after each phase boundary (error system, precision system, modular split).
2. Parse error policy tests:
   - user-provided stack receives entries and is returned intact;
   - with `exit_on_error=.false.` parser does not abort and stack contains trace;
   - with `exit_on_error=.true.` parser prints complete stack then aborts.
3. Precision tests per numeric kind:
   - declaration aliases map to canonical kind metadata;
   - parse/store/get roundtrip succeeds for each supported kind;
   - mismatched getter type triggers clear error;
   - `real` and `int` aliases resolve to `real_dp`/`int_dp`.
4. Subparser tests:
   - nested subparsers parse and merge namespace correctly;
   - command path is exposed correctly;
   - callback dispatch path works and reports stack errors consistently.
5. Regression checks for existing features: nargs modes, append, count, groups, mutex validation, help/version output formatting.
6. Docs/examples compile and reflect new APIs and migration notes.

**Decisions**
- Numeric type naming: support dual aliases (explicit width and `*_sp/*_dp/*_qp`).
- Nested subparser ergonomics: hybrid model (manual command-path inspection + callback dispatch support).
- Migration style: breaking cleanup now, not compatibility-first deprecation.
- Scope included: parser core, error flow, actions modularization, typed precision propagation, nested subparser UX, utility relocation for `get_prog_name`.
- Scope excluded (initial pass): non-parser feature additions unrelated to argument typing/error/subparsers, and performance micro-optimization beyond correctness/stability.

**Further Considerations**
1. `*_qp` availability policy recommendation: hard error when unsupported, plus `has_kind("real128")`-style capability query for callers.
2. Retrieval strictness recommendation: strict kind matching by default, explicit conversion API opt-in to avoid silent precision loss.
3. Namespace storage recommendation: use tagged union-style container with kind metadata rather than many loosely coupled scalar fields to keep maintenance manageable.
