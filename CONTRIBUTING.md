# Contributing

Use Emacs 29.1 or newer.  Run `make release-check` before proposing a change.
New scheduler behavior should have deterministic fake-driver coverage; adapter
changes should include mocked contract tests and must keep all private gptel
symbols inside `gptel-runner-gptel.el`.

Do not copy upstream implementation code while the project license remains
unresolved.  Do not publish or tag a release until the license blocker in
`PLAN.md` is closed.

