# HOWTO Release Tezt

- [ ] Review the changelog and update the section name with the version number.
- [ ] Push a commit that updates the version number in `lib_core/version.ml`.
- [ ] Tag with `git tag -a` and push this tag.
- [ ] Push a commit that adds the `+dev` suffix to `magit-gitlab-version`.
