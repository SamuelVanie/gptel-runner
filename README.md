# gptel-runner

Orchestrate dozens of gptel agents around one goal in Emacs. Use Emacs Lisp
to decide which agents work in parallel, what they pass to the next stage,
and where the pipeline pauses for your input. Follow the whole run from one
dashboard.

![Emacs dashboard running a 12-agent pipeline, from parallel research to the finished v2.0 release brief.](docs/media/workflow-demo.gif)

In this twelve-agent pipeline, six agents research the v2.0 changes in
parallel and four review what those changes mean for users. A writer drafts
the release brief, then an editor checks it against the original changes.
The demo uses scripted replies. [Watch the 20-second video](docs/media/workflow-demo.mp4) · [Read the pipeline](examples/release-brief.el)

## Installation

Use Emacs 29.1 or newer. For live runs, install gptel 0.9.9.6 or newer and
configure your model. Choose one of the setups below. [gptel setup](https://github.com/karthink/gptel#setup)

### use-package with straight.el

```elisp
(use-package gptel-runner
  :after gptel
  :straight (:host github :repo "SamuelVanie/gptel-runner" :files ("*")))
```

### Built-in use-package (Emacs 30+)

```elisp
(use-package gptel-runner
  :after gptel
  :vc (:url "https://github.com/SamuelVanie/gptel-runner.git" :rev :newest))
```

### Manual installation with load-file

Clone the repository, then add the Elisp below to your Emacs init file.

```sh
git clone https://github.com/SamuelVanie/gptel-runner.git ~/.emacs.d/gptel-runner
```

```elisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/gptel-runner"))
(load-file "~/.emacs.d/gptel-runner/gptel-runner.el")
```

**Run the demo locally.** Type `M-: (load "examples/release-brief") RET`,
then run `M-x gptel-runner-example-release-brief-demo`. The dashboard opens
and all twelve agents run without a model connection.

[Workflow guide](docs/guide.md)
