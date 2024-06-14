# Magit Glab

Magit plugin for manipulating GitLab merge requests.

![](images/screenshot.png)

## Installation

```lisp
(use-package magit-gitlab
  :config
  ;; Update magit-mode-map such that pressing @ opens the magit-gitlab-mr transient
  (define-key magit-mode-map (kbd "@") 'magit-gitlab-mr)
  (transient-append-suffix 'magit-dispatch "!" '("@" "Act on MR" magit-gitlab-mr)))
```

## Setup

Setting up a token:

1. set `git config --global gitlab.user USERNAME`
2. add `machine gitlab.com/api/v4 login USERNAME^magit-gitlab password
   glpat-TOKEN` to `~/.authinfo`, or some other file listed in the
   Emacs variable `authinfo-sources`.

Where `USERNAME` is replaced by your GitLab username,
and `glpat-TOKEN` is replaced by your GitLab access token.

## Usage

Call `magit-gitlab-mr` when visiting a `magit-status` buffer for a
branch connected to a GitLab merge request. A transient should open
allowing you to act on the merge request.
